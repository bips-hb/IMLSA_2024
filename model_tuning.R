## setup -----------------------------------------------------------------------
# packages
library(mlr3verse)
library(mlr3proba)
library(tensorflow)
library(keras)
library(wesanderson)
library(ggplot2)
library(survivalmodels)

# figure path
fig_path <- here::here("figures_iml")
if (!file.exists(fig_path)) dir.create(fig_path)
fig <- function(x) here::here(fig_path, x)

#------------------------------------------------------------------------------#
####                          Data Preparation                              ####
#------------------------------------------------------------------------------#


## train, test, validation split -----------------------------------------------
# set seed for reproducibility
set.seed(123)

# create a vector of shuffled row indices
n <- nrow(df_ghana)
indices <- sample(1:n)

# define proportions for train and test sets
train_prop <- 0.6
test_prop <- 0.2

# calculate the number of rows for each set
train_size <- round(train_prop * n)
test_size <- round(test_prop * n)

# split the indices into train, test, and validation sets
train_indices <- indices[1:train_size]
test_indices <- indices[(train_size + 1):(train_size + test_size)]
val_indices <- indices[(train_size + test_size + 1):n]



## one-hot encode all factor columns for neural network fitting ----------------
# separate into numeric and factor columns
numeric_cols <- sapply(df_ghana, is.numeric)
num_df <- df_ghana[, numeric_cols, drop = FALSE]
fac_df <- df_ghana[, !numeric_cols, drop = FALSE]

# one-hot encoding of factor columns
encoded_df <- as.data.frame(model.matrix(~ 0 + ., data = fac_df))

# combine the numeric columns with the encoded data
df_ghana_encoded <- cbind(num_df, encoded_df)

# remove whitespace and special characters in column names
names(df_ghana_encoded) <- gsub(" ", "", names(df_ghana_encoded))
names(df_ghana_encoded) <- gsub("/", "", names(df_ghana_encoded))
names(df_ghana_encoded) <- gsub("'", "", names(df_ghana_encoded))

## set up mlr3 tasks with training and validation data -------------------------
# training task with factor columns
task_train <- as_task_surv(
  survival::Surv(df_ghana[train_indices, "survivaltime"],
                 df_ghana[train_indices, "status"]) ~ .,
  time = "survivaltime",
  event = "status",
  data = df_ghana[train_indices,-c(1, 2, 3)]
)

# training task for cox model (delete dpt1_vaccination == "don't know" to avoid convergence issues)

df_train_cox <- df_ghana[train_indices, ]
df_train_cox <- df_train_cox[df_train_cox$dpt1_vaccination != "don't know",]

task_train_cox <- as_task_surv(
    survival::Surv(df_train_cox[, "survivaltime"],
                   df_train_cox[, "status"]) ~ .,
    time = "survivaltime",
    event = "status",
    data = df_train_cox[,-c(1, 2, 3)]
)

# validation task with factor columns
task_val <- as_task_surv(
  survival::Surv(df_ghana[val_indices, "survivaltime"],
                 df_ghana[val_indices, "status"]) ~ .,
  time = "survivaltime",
  event = "status",
  data = df_ghana[val_indices,-c(1, 2, 3)]
)

# test task with factor columns
task_test <- as_task_surv(
  survival::Surv(df_ghana[test_indices, "survivaltime"],
                 df_ghana[test_indices, "status"]) ~ .,
  time = "survivaltime",
  event = "status",
  data = df_ghana[test_indices,-c(1, 2, 3)]
)

# training task with one-hot-encoded columns
task_train_encoded <- as_task_surv(
  survival::Surv(df_ghana_encoded[train_indices, "survivaltime"],
                 df_ghana_encoded[train_indices, "status"]) ~ .,
  time = "survivaltime",
  event = "status",
  data = df_ghana_encoded[train_indices,-c(1, 2, 3)]
)

# validation task with one-hot-encoded columns
task_val_encoded <- as_task_surv(
  survival::Surv(df_ghana_encoded[val_indices, "survivaltime"],
                 df_ghana_encoded[val_indices, "status"]) ~ .,
  time = "survivaltime",
  event = "status",
  data = df_ghana_encoded[val_indices,-c(1, 2, 3)]
)

# test task with one-hot-encoded columns
task_test_encoded <- as_task_surv(
  survival::Surv(df_ghana_encoded[test_indices, "survivaltime"],
                 df_ghana_encoded[test_indices, "status"]) ~ .,
  time = "survivaltime",
  event = "status",
  data = df_ghana_encoded[test_indices,-c(1, 2, 3)]
)

#------------------------------------------------------------------------------#
####                      Hyperparameter Tuning                             ####
#------------------------------------------------------------------------------#



### set up search spaces to tune -----------------------------------------------
## ranger ----------------------------------------------------------------------
learner_ranger <- lrn(
  "surv.ranger",
  num.trees = to_tune(1, 1000),
  # number of trees in the random forest
  mtry = to_tune(1, 12),
  # number of variables randomly sampled at each split in each tree
  max.depth = to_tune(0, 50),
  # maximum depth of each tree in the forest
  min.node.size = to_tune(1, 100),
  # minimum number of data points required to create a new node during the tree-building process
  sample.fraction = to_tune(0.5, 1.0)
) # fraction of the training data used to grow each tree in the forest


## blackboost ------------------------------------------------------------------
learner_blackboost <- lrn(
  "surv.blackboost",
  mstop = to_tune(0, 500),
  # number of boosting iterations or the maximum number of base learners
  nu = to_tune(0, 1),
  # controls the learning rate or shrinkage of each base learner, influences the contribution of each base learner to the ensemble
  minprob = to_tune(0, 1),
  # minimum probability threshold for creating a split in the trees
  maxdepth = to_tune(1, 200),
  # maximum depth of each tree (base learner) in the ensemble
  minsplit = to_tune(0, 200),
  # minimum number of data points required to create a split in a tree
  minbucket = to_tune(0, 200),
  # minimum number of observations in a terminal node (leaf)
  maxsurrogate = to_tune(0, 10),
  # controls the maximum number of surrogate splits to consider at each split point
  mtry = to_tune(0, 12)
) # controls the number of variables randomly sampled at each split in each tree


## deepsurv --------------------------------------------------------------------
search_space <- ps(
  ## p_dbl for numeric valued parameters
  dropout = p_dbl(lower = 0, upper = 1),
  weight_decay = p_dbl(lower = 0, upper = 0.5),
  learning_rate = p_dbl(lower = 0, upper = 1),
  ## p_int for integer valued parameters
  nodes = p_int(lower = 1, upper = 352),
  k = p_int(lower = 1, upper = 4)
)

search_space$trafo <- function(x, param_set) {
  x$num_nodes = rep(x$nodes, x$k)
  x$nodes = x$k = NULL
  return(x)
}



### set up tuning instances ----------------------------------------------------
## ranger ----------------------------------------------------------------------
instance_ranger <- ti(
  task = task_val,
  learner = learner_ranger,
  resampling = rsmp("cv", folds = 10),
  terminator = trm("evals", n_evals = 500, k = 0)
)


## blackboost ------------------------------------------------------------------
instance_blackboost <- ti(
  task = task_val,
  learner = learner_blackboost,
  resampling = rsmp("cv", folds = 10),
  measure = msr("surv.cindex"),
  terminator = trm("evals", n_evals = 500, k = 0)
)


## deepsurv --------------------------------------------------------------------
set_seed(1234)
at_deepsurv <- AutoTuner$new(
  learner = lrn(
    "surv.deepsurv",
    frac = 0.3,
    early_stopping = TRUE,
    epochs = 10,
    optimizer = "adam"
  ),
  search_space = search_space,
  resampling = rsmp("cv", folds = 10),
  measure = msr("surv.cindex"),
  terminator = trm("evals", n_evals = 500),
  tuner = tnr("random_search")
)



### tune learners and determine optimal hyperparameter configuration -----------
## ranger ----------------------------------------------------------------------
set.seed(7832)
tuner_ranger <- tnr("random_search")
tuner_ranger$optimize(instance_ranger)


## blackboost ------------------------------------------------------------------
set.seed(7832)
tuner_blackboost <- tnr("random_search")
tuner_blackboost$optimize(instance_blackboost)


## deepsurv --------------------------------------------------------------------
at_deepsurv$train(task_val_encoded)
at_deepsurv$tuning_result$x_domain



### set up and train learners with tuned hyperparameters -----------------------
## ranger ----------------------------------------------------------------------
# set up learner with hyperparameters
learner_ranger_tuned <- lrn(
  "surv.ranger",
  num.trees = 708,
  mtry = 3,
  max.depth = 23,
  min.node.size = 1,
  sample.fraction = 0.6778257
)

# train learner
learner_ranger_tuned$train(task_train)


## blackboost ------------------------------------------------------------------
# set up learner with hyperparameters
learner_blackboost_tuned <- lrn(
  "surv.blackboost",
  maxdepth = 64,
  minsplit = 74,
  mstop = 81,
  nu = 0.3685141,
  minprob = 0.008189857,
  maxsurrogate = 6,
  mtry = 12,
  minbucket = 29
)

# train learner
learner_blackboost_tuned$train(task_train)


## deepsurv --------------------------------------------------------------------
# set up learner with hyperparameters
learner_deepsurv_tuned <- lrn(
  "surv.deepsurv",
  frac = 0.3,
  early_stopping = TRUE,
  epochs = 10,
  optimizer = "adam",
  dropout = 0.1655618,
  weight_decay = 0.4931967,
  learning_rate = 0.3540075,
  num_nodes = c(147)
)

# train learner
learner_deepsurv_tuned$train(task_train_encoded)



## coxph -----------------------------------------------------------------------
# set up learner with hyperparameters
learner_coxph <- lrn("surv.coxph")

# train learner
learner_coxph$train(task_train_cox)

#------------------------------------------------------------------------------#
####                        Model Performance                               ####
#------------------------------------------------------------------------------#

## predictions on test data ----------------------------------------------------
# ranger
ranger_prediction <- learner_ranger_tuned$predict(task_test)

# blackboost
blackboost_prediction <- learner_blackboost_tuned$predict(task_test)

# deepsurv
deepsurv_prediction <-
  learner_deepsurv_tuned$predict(task_test_encoded)

# coxph
coxph_prediction <- learner_coxph$predict(task_test)

## scoring rule: integrated Brier score ----------------------------------------
# ranger
ranger_prediction$score(msr("surv.graf"))

# blackboost
blackboost_prediction$score(msr("surv.graf"))

# deepsurv
deepsurv_prediction$score(msr("surv.graf"))

# coxph
coxph_prediction$score(msr("surv.graf"))

## discrimination ability: C-index
# ranger
ranger_prediction$score(msr("surv.cindex"))

# blackboost
blackboost_prediction$score(msr("surv.cindex"))

# deepsurv
deepsurv_prediction$score(msr("surv.cindex"))

# coxph
coxph_prediction$score(msr("surv.cindex"))

## calibration ability: D-calibration
# ranger
ranger_prediction$score(msr("surv.dcalib"))

# blackboost
blackboost_prediction$score(msr("surv.dcalib"))

# deepsurv
deepsurv_prediction$score(msr("surv.dcalib"))

# coxph
coxph_prediction$score(msr("surv.dcalib"))


## compute Brier scores for separate time points -------------------------------
# ranger
ranger_brier <- list()
for (i in 1:60) {
  s <-
    as.numeric(ranger_prediction$score(msr(
      "surv.graf", integrated = FALSE, times = i)
    ))
  ranger_brier[i] <- s
}

# blackboost
blackboost_brier <- list()
for (i in 1:60) {
  s <-
    as.numeric(blackboost_prediction$score(msr(
      "surv.graf", integrated = FALSE, times = i)
    ))
  blackboost_brier[i] <- s
}

# deepsurv
deepsurv_brier <- list()
for (i in 1:60) {
  s <-
    as.numeric(
      deepsurv_prediction$score(
        msr("surv.graf", integrated = FALSE, times = i)
      ))
  deepsurv_brier[i] <- s
}

# coxph
coxph_brier <- list()
for (i in 1:60) {
  s <-
    as.numeric(coxph_prediction$score(
      msr("surv.graf", integrated = FALSE, times = i)
    ))
  coxph_brier[i] <- s
}


## plot Brier score for each model over time -----------------------------------
# create dataframe for plotting
brier_scores <- c(
  unlist(coxph_brier),
  unlist(ranger_brier),
  unlist(deepsurv_brier),
  unlist(blackboost_brier)
)
times <- rep(1:60, times = 4)
models <-
  rep(c("coxph", "ranger", "deepsurv", "blackboost"), each = 60)
df_brier_scores <- data.frame(models, times, brier_scores)

# plot Brier scores
plot_brier <-
  ggplot(df_brier_scores, aes(x = times, y = brier_scores, color = models, )) +
  geom_line(size = 0.8) +
  scale_color_manual(values = wes_palette(n = 4, name = "GrandBudapest2")) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  ylab("Brier score") +
  ggtitle("") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      linewidth = 0.3
    )
  )
plot_brier

# save plot
ggsave(
  fig("plot_brier.pdf"),
  plot = plot_brier,
  width = 6,
  height = 6,
  device = "pdf"
)
