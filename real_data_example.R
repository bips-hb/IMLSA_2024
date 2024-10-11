## Setup -----------------------------------------------------------------------
# packages
library(survival)
library(pec)
library(randomForestSRC)
library(survex)
library(ggplot2)
library(survAUC)
library(survminer)
library(ggnewscale)
library(ggbeeswarm)
library(dplyr)
library(data.table)
source("plotting_functions.R")
source("utils.R")

# figure path
fig_path <- here::here("figures_iml")
if (!file.exists(fig_path))
    dir.create(fig_path)
fig <- function(x)
    here::here(fig_path, x)



#------------------------------------------------------------------------------#
####                         Model Training & Set-up                        ####
#------------------------------------------------------------------------------#


## Load data -------------------------------------------------------------------
data("GBSG2")


## Model training --------------------------------------------------------------
# set seed for reproducibility
set.seed(2607)

# define the proportion of the training data
train_proportion <- 2 / 3

# row indices of the training set
train_indices <- sample(1:nrow(GBSG2), size = floor(train_proportion * nrow(GBSG2)))

# split the data into training and test set
train_dat <- GBSG2[train_indices, ]
test_dat <- GBSG2[-train_indices, ]

# define times variable
times <- seq(min(GBSG2$time), max(GBSG2$time), by = 10)

# fit coxph model on training data
coxph <- coxph(Surv(time, cens) ~ .,
               data = train_dat,
               model = TRUE,
               x = TRUE)

# fit random survival forest on training data
ranger_rsf <- rfsrc(Surv(time, cens) ~ .,
                    data = train_dat,
                    mtry = 2,
                    ntime = times)


## Create survex explainer objects ---------------------------------------------
# create explainer object for coxph model on test data
coxph_explainer <- survex::explain(
    coxph,
    times = times,
    data = test_dat[, -c(9, 10)],
    y = Surv(test_dat$time, test_dat$cens)
)


# create explainer object for ranger model on test data
ranger_explainer <- survex::explain(
    ranger_rsf,
    data = test_dat[, -c(9, 10)],
    y = Surv(test_dat$time, test_dat$cens),
    times = times
)


#------------------------------------------------------------------------------#
####                           Model Performance                            ####
#------------------------------------------------------------------------------#


## Time-dependent Brier score  -------------------------------------------------
# evaluation times from test data
test_times <- seq(0, max(test_dat$time), 10)

# compute Brier scores for coxph and ranger models
brier_scores <- pec(
    object = list("Cox Model" = coxph, "Random Forest" = ranger_rsf),
    formula = Surv(time, cens) ~ 1,
    data = test_dat,
    times = test_times
)

# convert the pec object into a data frame
df_brier_data <- as.data.frame(brier_scores$AppErr)

# add the time points to the dataframe
df_brier_data$time <- brier_scores$time

# convert the data frame into long format
df_brier_long <- reshape2::melt(
    df_brier_data,
    id.vars = "time",
    variable.name = "models",
    value.name = "brier_scores"
)

# replace model names
models_vec <- ifelse(
    df_brier_long$models == "Reference",
    "Kaplan-Meier",
    ifelse(df_brier_long$models == "Cox.Model", "coxph", "ranger")
)
df_brier_long$models <- models_vec

# create custom plot of Brier scores over time
plot_brier <-
    ggplot(df_brier_long,
           aes(
               x = time,
               y = brier_scores,
               color = models,
               linetype = models
           )) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("#F0E442", "#0072B2", "#D55E00")) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
    scale_x_continuous(breaks = seq(0, 2700, 500)) +
    ylab("Brier score") +
    ggtitle("") +
    theme_bw() +
    theme(
        legend.position = "bottom",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        legend.background = element_rect(
            colour = "grey34",
            fill = "white",
            linetype = "solid",
            linewidth = 0.3
        )
    )
plot_brier # Figure

# save custom plot
ggsave(
    fig("plot_brier_gbsg2.pdf"),
    plot = plot_brier,
    width = 7,
    height = 6,
    device = "pdf"
)


## Integrated scores -----------------------------------------------------------
# performance
mp_cph <- model_performance(coxph_explainer)
mp_rsf <- model_performance(ranger_explainer)

# C-index
mp_cph$result$`C-index` # Text
mp_rsf$result$`C-index`# Text

# Integrated Brier Score
mp_cph$result$`Integrated Brier score` # Text
mp_rsf$result$`Integrated Brier score` # Text

# Integrated C/D AUC
mp_cph$result$`Integrated C/D AUC` # Text
mp_rsf$result$`Integrated C/D AUC` # Text


## Kaplan-Meier survival curves ------------------------------------------------
# create surv object
surv <- Surv(time = GBSG2$time, event = GBSG2$cens)

# summary of surv object
summary(surv)

# Kaplan Meier
km_fit <- survfit(surv ~ 1, data = GBSG2)

# visualize Kaplan-Meier surves with survminer
plot_km <- ggsurvplot(
    km_fit,
    data = GBSG2,
    risk.table = TRUE,
    ggtheme = theme_bw(),
    palette = c("#604E97", "#E68FAC"),
    font.x = c(20),
    font.y = c(20),
    font.tickslab = c(18),
    legend = "none"
)
plot_km # Figure

# save Kaplan-Meier curve
ggsave_workaround <- function(g) {
    survminer:::.build_ggsurvplot(
        x = g,
        surv.plot.height = NULL,
        risk.table.height = NULL,
        ncensor.plot.height = NULL
    )
}

plot_km_save <- ggsave_workaround(plot_km)

ggsave(
    fig("plot_km_gbsg2.pdf"),
    plot = plot_km_save,
    width = 7,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####                   Permutation Feature Importance                       ####
#------------------------------------------------------------------------------#


## coxph -----------------------------------------------------------------------
# compute permutation feature importance
pfi_coxph <- model_parts(coxph_explainer)

# extract relevant results for plotting
df_list <- c(list(pfi_coxph))
transformed_dfs <- lapply(df_list, function(x) {
    x <- x$result
    label <- unique(x$label)
    x <-
        x[x$`_permutation_` == 0, !colnames(x) %in% c("_permutation_", "label", "_baseline_")]
    plotting_df <-
        with(x, cbind(x[1], stack(x, select = -`_times_`), label, row.names = NULL))
})
df_pfi_coxph <- do.call(rbind, transformed_dfs)

# rename columns
names(df_pfi_coxph)[names(df_pfi_coxph) == "_times_"] <- "time"
names(df_pfi_coxph)[names(df_pfi_coxph) == "ind"] <- "features"

# delete full model results and results
df_pfi_coxph <- subset(df_pfi_coxph, features != "_full_model_")

# create custom plot of permutation feature importance over time
plot_pfi_coxph <- plot_fi(
    df_pfi_coxph,
    color_values = c(
        "#000000",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
    ),
    breaks = c(seq(0, 2600, 500)),
)
plot_pfi_coxph


## ranger ----------------------------------------------------------------------
# compute permutation feature importance
pfi_ranger <- model_parts(ranger_explainer)

# extract relevant results for plotting
df_list <- c(list(pfi_ranger))
transformed_dfs <- lapply(df_list, function(x) {
    x <- x$result
    label <- unique(x$label)
    x <-
        x[x$`_permutation_` == 0, !colnames(x) %in% c("_permutation_", "label", "_baseline_")]
    plotting_df <-
        with(x, cbind(x[1], stack(x, select = -`_times_`), label, row.names = NULL))
})
df_pfi_ranger <- do.call(rbind, transformed_dfs)

# rename columns
names(df_pfi_ranger)[names(df_pfi_ranger) == "_times_"] <-
    "time"
names(df_pfi_ranger)[names(df_pfi_ranger) == "ind"] <-
    "features"

# delete full model results
df_pfi_ranger <-
    subset(df_pfi_ranger, features != "_full_model_")

# create custom plot of permutation feature importance over time
plot_pfi_ranger <- plot_fi(
    df_pfi_ranger,
    model = "ranger",
    color_values = c(
        "#000000",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
    ),
    breaks = c(seq(0, 2600, 500))
)
plot_pfi_ranger


## Create plot grid and save plots ---------------------------------------------
# create grid of plots
pfi_grid <-
    ggarrange(
        plot_pfi_coxph,
        plot_pfi_ranger,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
pfi_grid # Figure

# save grid of plots
ggsave(
    fig("pfi_grid_gbsg2.pdf"),
    plot = pfi_grid,
    width = 14,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####  Individual Conditional Expectation (ICE) & Partial Dependence (PDP)   ####
#------------------------------------------------------------------------------#


## coxph -----------------------------------------------------------------------
# compute partial dependence and individual conditional expectation values
pdp_ice_coxph <- model_profile(coxph_explainer, variables = "horTh", N = NULL)

# extract relevant ice results for plotting
df_ice_coxph <-
    pdp_ice_coxph$cp_profiles$result[(pdp_ice_coxph$cp_profiles$result$`_vname_` == "horTh")  &
                                         (pdp_ice_coxph$cp_profiles$result$`_times_` %in% times), ]

# rename columns
names(df_ice_coxph)[names(df_ice_coxph) == "_times_"] = "time"
names(df_ice_coxph)[names(df_ice_coxph) == "_yhat_"] = "yhat"
names(df_ice_coxph)[names(df_ice_coxph) == "_ids_"] = "ids"

# sample 200 rows
df_ice_coxph$ids <- as.numeric(df_ice_coxph$ids)
set.seed(2607)
sampled_ids <- sample(sort(unique(df_ice_coxph$ids)), 200, replace = FALSE)
df_ice_coxph <- df_ice_coxph[df_ice_coxph$ids %in% sampled_ids, ]

# drop irrelevant columns
df_ice_coxph <-
    df_ice_coxph[, c("horTh", "time", "yhat", "ids")]

# aggregate ice values to obtain pdp values
df_pdp_coxph <- aggregate(yhat ~ time + horTh, data = df_ice_coxph[, c("ids", "horTh", "yhat", "time")], FUN = mean)

# select reference value for centering
df_ice_coxph_center <-
    df_ice_coxph[df_ice_coxph[, "horTh"] == "no", ]

# add reference value for centering to results dataframe
df_ice_coxph_merge <-
    merge(
        x = df_ice_coxph,
        y = df_ice_coxph_center,
        by = c("ids", "time"),
        all = TRUE
    )

# perform centering operation
df_ice_coxph_merge[, "yhat"] <-
    df_ice_coxph_merge[, "yhat.x"] - df_ice_coxph_merge[, "yhat.y"]

# rename double columns
names(df_ice_coxph_merge)[names(df_ice_coxph_merge) == "horTh.x"] = "horTh"

# aggregate centered ice values to obtain centered pdp values
df_pdp_coxph_center <- aggregate(yhat ~ time + horTh, data = df_ice_coxph_merge[, c("ids", "horTh", "yhat", "time")], FUN = mean)


## Create custom plots ---------------------------------------------------------
# create custom plot of centered ice and pdp curves over time
plot_pdp_ice_coxph_c <- plot_ice_pdp(
    df_ice_coxph_merge,
    df_pdp_coxph_center,
    model = "coxph",
    horTh,
    variable_name = "horTh",
    time,
    cens,
    limits = c(-0.1, 0.3),
    breaks_x = c(seq(0, 2600, 500)),
    breaks_y = seq(-0.1, 0.3, by = 0.1)
)
plot_pdp_ice_coxph_c


## ranger -----------------------------------------------------------------------
# compute partial dependence and individual conditional expectation values
pdp_ice_ranger <- model_profile(ranger_explainer, variables = "horTh", N = NULL)

# extract relevant ice results for plotting
df_ice_ranger <-
    pdp_ice_ranger$cp_profiles$result[(pdp_ice_ranger$cp_profiles$result$`_vname_` == "horTh")  &
                                          (pdp_ice_ranger$cp_profiles$result$`_times_` %in% times), ]

# rename columns
names(df_ice_ranger)[names(df_ice_ranger) == "_times_"] = "time"
names(df_ice_ranger)[names(df_ice_ranger) == "_yhat_"] = "yhat"
names(df_ice_ranger)[names(df_ice_ranger) == "_ids_"] = "ids"

# sample 200 rows
df_ice_ranger$ids <- as.numeric(df_ice_ranger$ids)
df_ice_ranger <- df_ice_ranger[df_ice_ranger$ids %in% sampled_ids, ]

# drop irrelevant columns
df_ice_ranger <-
    df_ice_ranger[, c("horTh", "time", "yhat", "ids")]

# aggregate ice values to obtain pdp values
df_pdp_ranger <- aggregate(yhat ~ time + horTh, data = df_ice_ranger[, c("ids", "horTh", "yhat", "time")], FUN = mean)

# select reference value for centering
df_ice_ranger_center <-
    df_ice_ranger[df_ice_coxph[, "horTh"] == "no", ]

# add reference value for centering to results dataframe
df_ice_ranger_merge <-
    merge(
        x = df_ice_ranger,
        y = df_ice_ranger_center,
        by = c("ids", "time"),
        all = TRUE
    )

# perform centering operation
df_ice_ranger_merge[, "yhat"] <-
    df_ice_ranger_merge[, "yhat.x"] - df_ice_ranger_merge[, "yhat.y"]

# rename double columns
names(df_ice_ranger_merge)[names(df_ice_ranger_merge) == "horTh.x"] = "horTh"

# aggregate centered ice values to obtain centered pdp values
df_pdp_ranger_center <- aggregate(yhat ~ time + horTh, data = df_ice_ranger_merge[, c("ids", "horTh", "yhat", "time")], FUN = mean)

## Create custom plots ---------------------------------------------------------
# create custom plot of centered ice and pdp curves over time
plot_pdp_ice_ranger_c <- plot_ice_pdp(
    df_ice_ranger_merge,
    df_pdp_ranger_center,
    model = "ranger",
    horTh,
    variable_name = "horTh",
    time,
    cens,
    limits = c(-0.1, 0.3),
    breaks_x = c(seq(0, 2600, 500)),
    breaks_y = seq(-0.1, 0.3, by = 0.1)
)
plot_pdp_ice_ranger_c

# create grid of centered ice and pdp plots
pdp_ice_grid_c <-
    ggarrange(
        plot_pdp_ice_coxph_c,
        plot_pdp_ice_ranger_c,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
pdp_ice_grid_c # Figure

# save grid of centered ice and pdp plots
ggsave(
    fig("pdp_ice_grid_c_gbsg2.pdf"),
    plot = pdp_ice_grid_c,
    width = 14,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####               Feature Interaction Friedman's H-statistics              ####
#------------------------------------------------------------------------------#


## ranger ----------------------------------------------------------------------
# compute feature interaction values
df_Hjk_pnodes <-
    feature_interaction(explainer = ranger_explainer,
                        feature = "pnodes",
                        N = NULL)

# add row of pnodes for legend
df_Hjk_pnodes = rbind(df_Hjk_pnodes, data.frame(feature = "pnodes", time = 0, H = 0))

# create preset list of correct feature order
preset_list_all <- c("horTh",
                     "age",
                     "menostat",
                     "tsize",
                     "tgrade",
                     "pnodes",
                     "progrec",
                     "estrec")

# reorder the rows based on the preset list
df_Hjk_pnodes$feature <-
    factor(df_Hjk_pnodes$feature, levels = preset_list_all)
df_Hjk_pnodes <-
    df_Hjk_pnodes[order(df_Hjk_pnodes$feature), ]

# create custom plot of H-statistic curves over time
plot_hjk_pnodes <- plot_f_inter(
    df_Hjk_pnodes,
    title = "pnodes",
    color_values = c(
        "#000000",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
    ),
    limits_y = c(0, 0.15),
    breaks_y = seq(0, 0.15, by = 0.05),
    breaks_x = seq(0, 2600, by = 500)
)
plot_hjk_pnodes


## ranger ----------------------------------------------------------------------
# compute feature interaction values
df_Hjk_horTh <-
    feature_interaction(explainer = ranger_explainer,
                        feature = "horTh",
                        N = NULL)

# add row of horTh for legend
df_Hjk_horTh = rbind(df_Hjk_horTh, data.frame(feature = "horTh", time = 0, H = 0))

# reorder the rows based on the preset list
df_Hjk_horTh$feature <-
    factor(df_Hjk_horTh$feature, levels = preset_list_all)
df_Hjk_horTh <- df_Hjk_horTh[order(df_Hjk_horTh$feature), ]

# create custom plot of H-statistic curves over time
plot_hjk_horTh <- plot_f_inter(
    df_Hjk_horTh,
    title = "horTh",
    color_values = c(
        "#000000",
        "#E69F00",
        "#56B4E9",
        "#009E73",
        "#F0E442",
        "#0072B2",
        "#D55E00",
        "#CC79A7"
    ),
    limits_y = c(0, 0.2),
    breaks_y = seq(0, 0.2, by = 0.1),
    breaks_x = seq(0, 2600, by = 500)
)
plot_hjk_horTh


## create plot grid and save plots ---------------------------------------------
# create grid of h-statistic plots
h_grid <-
    ggarrange(
        plot_hjk_pnodes,
        plot_hjk_horTh,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
h_grid # Figure

# save grid of h-statistic plots
ggsave(
    fig("h_grid.pdf"),
    plot = h_grid,
    width = 14,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####                   Accumulated Local Effects (ALE)                      ####
#------------------------------------------------------------------------------#


## tsize --------------------------------------------------------------------------
# compute accumulated local effects values
ale_ranger <- model_profile(
    ranger_explainer,
    N = 1000,
    variables = "tsize",
    type = "accumulated",
    center = TRUE
)

# extract relevant ale results for plotting
df_ale_ranger <-
    ale_ranger$result[(ale_ranger$result$`_vname_` == "tsize") &
                          (ale_ranger$result$`_times_` %in% times), c("_x_", "_yhat_")]

# rename columns
names(df_ale_ranger)[names(df_ale_ranger) == "_x_"] <- "value"
names(df_ale_ranger)[names(df_ale_ranger) == "_yhat_"] <- "prediction"

# add time column
df_ale_ranger$time <- rep(times, times = (nrow(df_ale_ranger) / length(times)))

# create custom plot of ale curves over time
plot_ale_ranger <- plot_ale_pdp(
    df_ale_ranger,
    model = "ranger",
    x_label = "tsize",
    limits = c(-0.15, 0.15),
    breaks_x = seq(0, 80, by = 10),
    breaks_y = seq(-0.15, 0.15, by = 0.1)
)
plot_ale_ranger


## tsize --------------------------------------------------------------------------
# compute accumulated local effects values
ale_coxph <- model_profile(
    coxph_explainer,
    N = 1000,
    variables = "tsize",
    type = "accumulated",
    center = TRUE
)

# extract relevant ale results for plotting
df_ale_coxph <-
    ale_coxph$result[(ale_coxph$result$`_vname_` == "tsize") &
                         (ale_coxph$result$`_times_` %in% times), c("_x_", "_yhat_")]

# rename columns
names(df_ale_coxph)[names(df_ale_coxph) == "_x_"] <- "value"
names(df_ale_coxph)[names(df_ale_coxph) == "_yhat_"] <- "prediction"

# add time column
df_ale_coxph$time <- rep(times, times = (nrow(df_ale_coxph) / length(times)))

# create custom plot of ale curves over time
plot_ale_coxph <- plot_ale_pdp(
    df_ale_coxph,
    model = "coxph",
    x_label = "tsize",
    limits = c(-0.15, 0.15),
    breaks_x = seq(0, 80, by = 10),
    breaks_y = seq(-0.15, 0.15, by = 0.1)
)
plot_ale_coxph

# create grid of centered ice and pdp plots
ale_grid <-
    ggarrange(
        plot_ale_coxph,
        plot_ale_ranger,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
ale_grid # Figure

# save grid of uncentered ice and pdp plots
ggsave(
    fig("ale_grid_gbsg2.pdf"),
    plot = ale_grid,
    width = 14,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####        Local Interpretable Model-Agnostic Explanations (LIME)          ####
#------------------------------------------------------------------------------#


## Select interesting instances ------------------------------------------------
# patient dead at t = 772
individual_5 <- test_dat[5, -c(9, 10)]
# patient censored at t = 2539
individual_33 <- test_dat[33, -c(9, 10)]


## SurvLIME and SurvSHAP individuals table -------------------------------------
individuals_table <- data.frame(
    Individual = c(
        "time",
        "status",
        "horTh",
        "age",
        "menostat",
        "tsize",
        "tgrade",
        "pnodes",
        "progrec",
        "estrec"
    ),
    P1 = c(
        test_dat[5, 9],
        test_dat[5, 10],
        individual_5[[1]],
        individual_5[[2]],
        individual_5[[3]],
        individual_5[[4]],
        individual_5[[5]],
        individual_5[[6]],
        individual_5[[7]],
        individual_5[[8]]
    ),
    P2 = c(
        test_dat[33, 9],
        test_dat[33, 10],
        individual_33[[1]],
        individual_33[[2]],
        individual_33[[3]],
        individual_33[[4]],
        individual_33[[5]],
        individual_33[[6]],
        individual_33[[7]],
        individual_33[[8]]
    )
)
individuals_table # Table

## Compute lime values ---------------------------------------------------------
# compute lime values for individual 5
survlime_ranger_5 <- predict_parts(ranger_explainer, individual_5, type = "survlime")

# compute lime values for individual 33
survlime_ranger_33 <- predict_parts(ranger_explainer, individual_33, type = "survlime")



### Extract relevant lime results for plotting ---------------------------------
## Extract relevant lime results for plotting for individual 5 -----------------
# extract relevant local importance values
local_importance_5 <-
    as.numeric(survlime_ranger_5$result) * as.numeric(survlime_ranger_5$variable_values)

# convert relevant local importance values to dataframe
df_lime_5 <- data.frame(
    variable_names = names(survlime_ranger_5$variable_values),
    variable_values = as.numeric(survlime_ranger_5$variable_values),
    beta = as.numeric(survlime_ranger_5$result),
    sign_beta = as.factor(sign(as.numeric(
        survlime_ranger_5$result
    ))),
    sign_local_importance = as.factor(sign(local_importance_5)),
    local_importance = local_importance_5
)

# sort local importance values
df_lime_5 <-
    df_lime_5[head(order(abs(df_lime_5$local_importance), decreasing = TRUE), 12), ]

# clean feature names
df_lime_5$variable_names <-
    c(
        "pnodes",
        "tgrade.II",
        "tsize",
        "age",
        "estrec",
        "progrec",
        "horTh.yes",
        "menostat.Pre",
        "tgrade.III"
    )

# exclude all features with local importance = 0
df_lime_5 <- subset(df_lime_5, local_importance != 0)

# create dataframe with predicted survival function values
df_sf_5 <- data.frame(
    times = c(
        survlime_ranger_5$black_box_sf_times,
        survlime_ranger_5$expl_sf_times
    ),
    sfs = c(survlime_ranger_5$black_box_sf, survlime_ranger_5$expl_sf),
    type = c(
        rep(
            "black box survival function",
            length(survlime_ranger_5$black_box_sf)
        ),
        rep(
            "SurvLIME explanation survival function",
            length(survlime_ranger_5$expl_sf)
        )
    )
)

## Extract relevant lime results for plotting for individual 33 ----------------
# extract relevant local importance values
local_importance_33 <-
    as.numeric(survlime_ranger_33$result) * as.numeric(survlime_ranger_33$variable_values)

# convert relevant local importance values to dataframe
df_lime_33 <- data.frame(
    variable_names = names(survlime_ranger_33$variable_values),
    variable_values = as.numeric(survlime_ranger_33$variable_values),
    beta = as.numeric(survlime_ranger_33$result),
    sign_beta = as.factor(sign(as.numeric(
        survlime_ranger_33$result
    ))),
    sign_local_importance = as.factor(sign(local_importance_33)),
    local_importance  = local_importance_33
)

# sort local importance values
df_lime_33 <-
    df_lime_33[head(order(abs(df_lime_33$local_importance), decreasing = TRUE), 12), ]

# clean feature names
df_lime_33$variable_names <-
    c(
        "tgrade.II",
        "tsize",
        "menostat.Pre",
        "progrec",
        "horTh.yes",
        "age",
        "pnodes",
        "estrec",
        "tgrade.III"
    )

# exclude all features with local importance = 0
df_lime_33 <- subset(df_lime_33, local_importance != 0)

# exclude all features with local importance = 0
df_sf_33 <- data.frame(
    times = c(
        survlime_ranger_33$black_box_sf_times,
        survlime_ranger_33$expl_sf_times
    ),
    sfs = c(survlime_ranger_33$black_box_sf, survlime_ranger_33$expl_sf),
    type = c(
        rep(
            "black box survival function",
            length(survlime_ranger_33$black_box_sf)
        ),
        rep(
            "SurvLIME explanation survival function",
            length(survlime_ranger_33$expl_sf)
        )
    )
)


## create custom plots of local importance values  -----------------------------
# create custom plots of local importance values for individual 5
plot_lime_5 <- plot_lime(df_lime_5, accuracy = 0.0000001)
plot_lime_5

# create custom plots of local importance values for individual 5
plot_sf_5 <- plot_sf(df_sf_5)
plot_sf_5

# create custom plots of local importance values for individual 33
plot_lime_33 <- plot_lime(df_lime_33, accuracy = 0.0001, title = "P2: Patient censored at t = 2539")
plot_lime_33

# create custom plots of local importance values for individual 33
plot_sf_33 <- plot_sf(df_sf_33, title = "P2: Patient censored at t = 2539")
plot_sf_33


## Create plot grid and save plots ---------------------------------------------
# create grid of survival function plots
surv_grid <-
    ggarrange(
        plot_sf_5 ,
        plot_sf_33 ,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
surv_grid # Figure

# save grid of survival function plots
ggsave(
    fig("surv_grid_gbsg2.pdf"),
    plot = surv_grid,
    width = 14,
    height = 6,
    device = "pdf"
)

# create grid of local importance plots
lime_grid <-
    ggarrange(
        plot_lime_5 ,
        plot_lime_33,
        ncol = 2,
        nrow = 1,
        common.legend = FALSE
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
lime_grid # Figure

# save grid of local importance plots
ggsave(
    fig("lime_grid_gbsg2.pdf"),
    plot = lime_grid,
    width = 14,
    height = 6,
    device = "pdf"
)


#------------------------------------------------------------------------------#
####               Local SHapley Additive exPlanations (SHAP)               ####
#------------------------------------------------------------------------------#


## Compute shap values ---------------------------------------------------------
# compute shap values for individual 5
survshap_ranger_5 <- predict_parts(
    ranger_explainer,
    individual_5,
    type = 'survshap',
    N = 229,
    calculation_method = c("kernelshap"),
    maxvar = 8
)

# compute shap values for individual 33
survshap_ranger_33 <- predict_parts(
    ranger_explainer,
    individual_33,
    type = 'survshap',
    N = 229,
    calculation_method = c("kernelshap"),
    maxvar = 8
)


### Extract relevant shap results for plotting ---------------------------------
## Extract relevant shap results for plotting for individual 5 -----------------
# convert results to list
dfl_shap_5 <- c(list(survshap_ranger_5))

# convert results to dataframe
df_shap_5 <- lapply(dfl_shap_5, function(x) {
    label <- attr(x, "label")
    cols <- sort(head(order(x$aggregate, decreasing = TRUE), 12))
    sv <- x$result[, cols]
    times <- x$eval_times
    transposed <- as.data.frame(cbind(times = times, sv))
    rownames(transposed) <- NULL
    long_df <- cbind(times = transposed$times,
                     stack(transposed, select = -times),
                     label = label)
})
df_shap_5 <- do.call(rbind, df_shap_5)

## Extract relevant shap results for plotting for indiviudal 33 ----------------
# convert results to list
dfl_shap_33 <- c(list(survshap_ranger_33))

# convert results to list
df_shap_33 <- lapply(dfl_shap_33, function(x) {
    label <- attr(x, "label")
    cols <- sort(head(order(x$aggregate, decreasing = TRUE), 12))
    sv <- x$result[, cols]
    times <- x$eval_times
    transposed <- as.data.frame(cbind(times = times, sv))
    rownames(transposed) <- NULL
    long_df <- cbind(times = transposed$times,
                     stack(transposed, select = -times),
                     label = label)
})
df_shap_33 <- do.call(rbind, df_shap_33)


## Create custom plots of shap values ------------------------------------------
# create custom plots of shap values for observation 5
plot_shap_5 <- plot_shap_ind(df_shap_5)
plot_shap_5

# create custom plots of shap values for observation 33
plot_shap_33 <- plot_shap_ind(
    df_shap_33,
    limits = c(-0.1, 0.1),
    breaks = seq(-0.1, 0.1, by = 0.1),
    title = "P2: Patient censored at t = 2539"
)
plot_shap_33


## Create plot grid and save plots ---------------------------------------------
# create grid of ale plots
shap_grid <-
    ggarrange(
        plot_shap_5,
        plot_shap_33,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
shap_grid # Figure

# save grid of shap plots
ggsave(
    fig("shap_grid_gbsg2.pdf"),
    plot = shap_grid,
    width = 14,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####              Global SHapley Additive exPlanations (SHAP)               ####
#------------------------------------------------------------------------------#


## Compute shap values for all observations in test set ------------------------
survshap_global_ranger <-
    model_survshap(ranger_explainer, test_dat[, -c(9, 10)])


## Preprocess results for aggregated shap plot ---------------------------------
# rename results
agg_results <- survshap_global_ranger
# aggregate shap values of all observations
agg_results$result <-
    aggregate_shap_multiple_observations(agg_results$result, colnames(agg_results$result[[1]]), function(x)
        mean(abs(x)))
agg_results$aggregate <-
    apply(do.call(rbind, agg_results$aggregate), 2, function(x)
        mean(abs(x)))
df_agg <- c(list(agg_results))

# convert data to long format
df_agg_long <- lapply(df_agg, function(x) {
    label <- attr(x, "label")
    cols <- sort(head(order(x$aggregate, decreasing = TRUE), 12))
    sv <- x$result[, cols]
    times <- x$eval_times
    transposed <- as.data.frame(cbind(times = times, sv))
    rownames(transposed) <- NULL
    long_df <- cbind(times = transposed$times,
                     stack(transposed, select = -times),
                     label = label)
})
df_agg_long <- do.call(rbind, df_agg_long)


## Create custom plot of shap values -------------------------------------------
plot_shap_agg <- plot_shap_agg_line(df_agg_long)
plot_shap_agg


## Preprocess results for beeswarm plot ----------------------------------------
# rename results
beeswarm_results <- survshap_global_ranger

# convert shap value data to long format
df_beeswarm <- as.data.frame(do.call(rbind, beeswarm_results$aggregate))
df_beeswarm_stacked <- stack(df_beeswarm)

# extract feature values for observations for which shap values were computed
original_values <- as.data.frame(beeswarm_results$variable_values)

# convert factor categories to characters
factor_columns <- sapply(original_values, is.factor)
original_values[factor_columns] <-
    lapply(original_values[factor_columns], as.character)

# convert feature values data to long format
original_values_stacked <- stack(original_values)

# combine shap values of observations with feature values
df_beeswarm_plot <- cbind(df_beeswarm_stacked, original_values_stacked)

# rename columns
colnames(df_beeswarm_plot) <- c("shap_value", "feature", "value", "ind")

# create separate dataframes containing features to be plotted
df_beeswarm_horTh <-
    subset(df_beeswarm_plot, feature == "horTh")
df_beeswarm_tgrade <-
    subset(df_beeswarm_plot, feature == "tgrade")
df_beeswarm_pnodes <-
    subset(df_beeswarm_plot, feature == "pnodes")
df_beeswarm_progrec <-
    subset(df_beeswarm_plot, feature == "progrec")
df_beeswarm_pnodes$value <- as.numeric(df_beeswarm_pnodes$value)
df_beeswarm_progrec$value <- as.numeric(df_beeswarm_progrec$value)

## Create beeswarm plots -------------------------------------------------------
# create custom beeswarm plot for numerical feature
plot_beeswarm_pnodes <- plot_shap_bee_cont(df_beeswarm_pnodes, xlab = "")
plot_beeswarm_pnodes

# create custom beeswarm plot for categorical feature
plot_beeswarm_horTh <- plot_shap_bee_cat(df_beeswarm_horTh, subtitle = "")
plot_beeswarm_horTh


## Create plot grid and save plots ---------------------------------------------
# create grid of beeswarm plots
beeswarm_grid <-
    ggarrange(
        plot_beeswarm_pnodes,
        plot_beeswarm_horTh,
        ncol = 1,
        nrow = 2,
        common.legend = FALSE
    )
beeswarm_grid # Figure

# create grid of aggregate shap plots
shap_agg_grid <-
    ggarrange(
        plot_shap_agg,
        beeswarm_grid,
        ncol = 2,
        nrow = 1,
        common.legend = FALSE
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
shap_agg_grid # Figure

# save grid of shap plots
ggsave(
    fig("shap_agg_grid_gbsg2.pdf"),
    plot = shap_agg_grid,
    width = 14,
    height = 6,
    device = "pdf"
)




