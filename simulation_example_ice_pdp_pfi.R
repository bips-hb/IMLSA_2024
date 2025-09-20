## Setup -----------------------------------------------------------------------
# packages
library(simsurv)
library(survival)
library(survex)
library(ranger)
library(randomForestSRC)
library(ggnewscale)
library(survminer)
library(pec)
source("plotting_functions.R")

# figure path
fig_path <- here::here("figures_iml")
if (!file.exists(fig_path))
    dir.create(fig_path)
fig <- function(x)
    here::here(fig_path, x)



#------------------------------------------------------------------------------#
####                          Data Simulation                               ####
#------------------------------------------------------------------------------#


## Simulate survival data ------------------------------------------------------
# set seed for reproducibility
set.seed(2607)

# set number of simulated observations
n <- 3000

# simulate feature values
x <- data.frame(treatment = rbinom(n, 1, 0.5),
                x1 = rnorm(n),
                x2 = rnorm(n))

# simulate survival times using simsurv package
simdat <- simsurv(
    dist = "weibull",
    lambdas = 0.1,
    gammas = 1.5,
    betas = c(treatment = -2.5, x1 = 0.7),
    x = x,
    tde = c(treatment = 5),
    tdefunction = "log",
    maxt = 5
)

# remove id column from simulation dataframe
y <- simdat[, -1]

# add feature values to simulation dataframe
dat <- cbind(y, x)

# convert binary treatment variable to factor
dat$treatment <- factor(dat$treatment)

# simulate random censoring following a binomial distribution
cen <- rbinom(n, 1, 0.2)
cen_status <- ifelse((cen == 1) | (dat$status == 0), 0, 1)
dat$status <- cen_status


## Kaplan-Meier survival curves ------------------------------------------------
# create surv object
surv <- Surv(time = dat$eventtime, event = dat$status)

# summary of surv object
summary(surv)

# Kaplan Meier
km_fit <- survfit(surv ~ treatment, data = dat)

# visualize Kaplan-Meier plots with survminer
plot_km <- ggsurvplot(
    km_fit,
    data = dat,
    risk.table = TRUE,
    ggtheme = theme_bw(),
    palette = c("#604E97", "#E68FAC"),
    font.x = c(20),
    font.y = c(20),
    font.tickslab = c(18),
    legend = "none"
)
plot_km # Figure 18 a)

# save Kaplan-Meier curve plot
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
    fig("figure_18a.pdf"),
    plot = plot_km_save,
    width = 7,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####                          Model Training & Set-up                       ####
#------------------------------------------------------------------------------#


## Model training --------------------------------------------------------------
# set seed for reproducibility
set.seed(2607)

# define the proportion of the training data
train_proportion <- 2 / 3

# row indices of the training set
train_indices <- sample(1:nrow(dat), size = floor(train_proportion * nrow(dat)))

# split the data into training and test set
train_dat <- dat[train_indices, ]
test_dat <- dat[-train_indices, ]

# define times variable
times <- seq(0, 5, by = 0.1)

# fit coxph model on training data
coxph <- coxph(
    Surv(eventtime, status) ~ .,
    data = train_dat,
    model = TRUE,
    x = TRUE
)

# fit random survival forest on training data
ranger_rsf <- rfsrc(
    Surv(eventtime, status) ~ .,
    data = train_dat,
    mtry = 2,
    ntime = times
)


## Create survex explainer objects ---------------------------------------------
# create explainer object for coxph model on test data
coxph_explainer <- survex::explain(
    coxph,
    times = times,
    data = test_dat[, -c(1, 2)],
    y = Surv(test_dat$eventtime, test_dat$status)
)

# create explainer object for ranger model on test data
ranger_explainer <- survex::explain(
    ranger_rsf,
    data = test_dat[, -c(1, 2)],
    y = Surv(test_dat$eventtime, test_dat$status),
    times = times
)



#------------------------------------------------------------------------------#
####                           Model Performance                            ####
#------------------------------------------------------------------------------#


## Create Brier score plots for coxph and ranger models ------------------------
# compute Brier scores for coxph and ranger models
brier_scores <- pec(
    object = list("Cox Model" = coxph, "Random Forest" = ranger_rsf),
    formula = Surv(eventtime, status) ~ 1,
    data = test_dat,
    times = seq(0, 5, by = 20)
)

# convert the pec object into a data.frame
df_brier_data <- as.data.frame(brier_scores$AppErr)

# add the time points to the data.frame
df_brier_data$time <- brier_scores$time

# convert the data.frame into long format
df_brier_long <- reshape2::melt(
    df_brier_data,
    id.vars = "time",
    variable.name = "models",
    value.name = "brier_scores"
)

# replace model names to abbreviations used in the paper
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
    scale_x_continuous(breaks = seq(0, 5, 0.5)) +
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
plot_brier # Figure 18 b)

# save custom plot
ggsave(
    fig("figure_18b.pdf"),
    plot = plot_brier,
    width = 7,
    height = 6,
    device = "pdf"
)



#------------------------------------------------------------------------------#
####  Individual Conditional Expectation (ICE) & Partial Dependence (PDP)   ####
#------------------------------------------------------------------------------#


## coxph -----------------------------------------------------------------------
# compute partial dependence and individual conditional expectation values
pdp_ice_coxph <- model_profile(coxph_explainer, variables = "treatment", N = NULL)

# extract relevant ice results for plotting
df_ice_coxph <-
    pdp_ice_coxph$cp_profiles$result[(pdp_ice_coxph$cp_profiles$result$`_vname_` == "treatment")  &
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
    df_ice_coxph[, c("treatment", "time", "yhat", "ids")]

# aggregate ice values to obtain pdp values
df_pdp_coxph <- aggregate(yhat ~ time + treatment, data = df_ice_coxph[, c("ids", "treatment", "yhat", "time")], FUN = mean)

# select reference value for centering
df_ice_coxph_center <-
    df_ice_coxph[df_ice_coxph[, "treatment"] == 0, ]

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
names(df_ice_coxph_merge)[names(df_ice_coxph_merge) == "treatment.x"] = "treatment"

# extract ice values for times that should be plotted
df_ice_coxph_center <-
    df_ice_coxph_merge[df_ice_coxph_merge[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]
df_ice_coxph <-
    df_ice_coxph[df_ice_coxph[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]

# aggregate centered ice values to obtain centered pdp values
df_pdp_coxph_center <- aggregate(yhat ~ time + treatment, data = df_ice_coxph_merge[, c("ids", "treatment", "yhat", "time")], FUN = mean)

# extract pdp values for times that should be plotted
df_pdp_coxph_center <-
    df_pdp_coxph_center[df_pdp_coxph_center[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]
df_pdp_coxph <-
    df_pdp_coxph[df_pdp_coxph[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]


## Create custom plots ---------------------------------------------------------
# create custom plot of uncentered ice and pdp curves over time
plot_pdp_ice_coxph_uc <- plot_ice_pdp(
    df_ice_coxph,
    df_pdp_coxph,
    model = "coxph",
    y_label = "ICE/PD value",
    variable_name = "treatment",
    status_name = "status",
    time_var = eventtime,
    limits = c(0, 1),
    breaks_x = seq(0, 5, by = 1),
    breaks_y = seq(0, 1, by = 0.2)
)
plot_pdp_ice_coxph_uc

# create custom plot of centered ice and pdp curves over time
plot_pdp_ice_coxph_c <- plot_ice_pdp(
    df_ice_coxph_center,
    df_pdp_coxph_center,
    model = "coxph",
    y_label = "ICE/PD value",
    variable_name = "treatment",
    status_name = "status",
    time_var = eventtime,
    limits = c(-0.6, 0.1),
    breaks_x = seq(0, 5, by = 1),
    breaks_y = seq(-0.6, 0.1, by = 0.1)
)
plot_pdp_ice_coxph_c

# create custom plot of uncentered ice curves over time
plot_ice_coxph_uc <- plot_ice(df_ice_coxph, breaks_x = seq(0, 5, by = 1))
plot_ice_coxph_uc

# create custom plot of centered ice curves over time
plot_ice_coxph_c <- plot_ice(
    df_ice_coxph_center,
    limits_y = c(-0.6, 0.1),
    breaks_y = seq(-0.6, 0.1, by = 0.1),
    breaks_x = seq(0, 5, by = 1)
)
plot_ice_coxph_c


## ranger ----------------------------------------------------------------------
# compute partial dependence and individual conditional expectation values
pdp_ice_ranger <- model_profile(ranger_explainer, variables = "treatment", N = NULL)

# extract relevant ice results for plotting
df_ice_ranger <-
    pdp_ice_ranger$cp_profiles$result[(pdp_ice_ranger$cp_profiles$result$`_vname_` == "treatment")  &
                                          (pdp_ice_ranger$cp_profiles$result$`_times_` %in% times), ]

# rename columns
names(df_ice_ranger)[names(df_ice_ranger) == "_times_"] = "time"
names(df_ice_ranger)[names(df_ice_ranger) == "_yhat_"] = "yhat"
names(df_ice_ranger)[names(df_ice_ranger) == "_ids_"] = "ids"

# sample 200 rows
df_ice_ranger <- df_ice_ranger[df_ice_ranger$ids %in% sampled_ids, ]

# drop irrelevant columns
df_ice_ranger <-
    df_ice_ranger[, c("treatment", "time", "yhat", "ids")]

# aggregate centered ice values to obtain centered pdp values
df_pdp_ranger <- aggregate(yhat ~ time + treatment, data = df_ice_ranger[, c("ids", "treatment", "yhat", "time")], FUN = mean)

# select reference value for centering
df_ice_ranger_center <-
    df_ice_ranger[df_ice_ranger[, "treatment"] == 0, ]

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

# rename double column
names(df_ice_ranger_merge)[names(df_ice_ranger_merge) == "treatment.x"] <-
    "treatment"

# extract ice values for times that should be plotted
df_ice_ranger_center <-
    df_ice_ranger_merge[df_ice_ranger_merge[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]
df_ice_ranger <-
    df_ice_ranger[df_ice_ranger[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]

# aggregate centered ice values to obtain centered pdp values
df_pdp_ranger_center <- aggregate(yhat ~ time + treatment, data = df_ice_ranger_merge[, c("ids", "treatment", "yhat", "time")], FUN = mean)

# extract pdp values for times that should be plotted
df_pdp_ranger_center <-
    df_pdp_ranger_center[df_pdp_ranger_center[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]
df_pdp_ranger <-
    df_pdp_ranger[df_pdp_ranger[, "time"] %in% c(0, 1, 2, 3, 4, 5), ]


## Create custom plots----------------------------------------------------------
# create custom plot of uncentered ice and pdp curves over time
plot_pdp_ice_ranger_uc <- plot_ice_pdp(
    df_ice_ranger,
    df_pdp_ranger,
    model = "ranger",
    y_label = "ICE/PD value",
    variable_name = "treatment",
    time_var = eventtime,
    status_name = "status",
    limits = c(0, 1),
    breaks_x = seq(0, 5, by = 1),
    breaks_y = seq(0, 1, by = 0.2)
)
plot_pdp_ice_ranger_uc

# create custom plot of centered ice and pdp curves over time
plot_pdp_ice_ranger_c <- plot_ice_pdp(
    df_ice_ranger_center,
    df_pdp_ranger_center,
    model = "ranger",
    y_label = "ICE/PD value",
    variable_name = "treatment",
    time_var = eventtime,
    status_name = "status",
    limits = c(-0.9, 0.4),
    breaks_x = seq(0, 5, by = 1),
    breaks_y = seq(-0.9, 0.4, by = 0.2)
)
plot_pdp_ice_ranger_c

# create custom plot of uncentered ice curves over time
plot_ice_ranger_uc <- plot_ice(df_ice_ranger,
                               model = "ranger",
                               breaks_x = seq(0, 5, by = 1))
plot_ice_ranger_uc

# create custom plot of centered ice curves over time
plot_ice_ranger_c <- plot_ice(
    df_ice_ranger_center,
    model = "ranger",
    limits_y = c(-0.9, 0.4),
    breaks_y = seq(-0.9, 0.4, by = 0.2),
    breaks_x = seq(0, 5, by = 1)
)
plot_ice_ranger_c


## Create plot grids and save plots --------------------------------------------
# create grid of uncentered ice and pdp plots
pdp_ice_grid_uc <-
    ggarrange(
        plot_pdp_ice_coxph_uc,
        plot_pdp_ice_ranger_uc,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
pdp_ice_grid_uc # Figure 4

# save grid of uncentered ice and pdp plots
ggsave(
    fig("figure_4.pdf"),
    plot = pdp_ice_grid_uc,
    width = 14,
    height = 6,
    device = "pdf"
)

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
pdp_ice_grid_c # Figure 16

# save grid of centered ice and pdp plots
ggsave(
    fig("figure_17.pdf"),
    plot = pdp_ice_grid_c,
    width = 14,
    height = 6,
    device = "pdf"
)

# create grid of uncentered ice plots
ice_grid_uc <-
    ggarrange(
        plot_ice_coxph_uc,
        plot_ice_ranger_uc,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
ice_grid_uc # Figure 2

# save grid of uncentered ice plots
ggsave(
    fig("figure_2.pdf"),
    plot = ice_grid_uc,
    width = 14,
    height = 6,
    device = "pdf"
)

# create grid of centered ice plots
ice_grid_c <-
    ggarrange(
        plot_ice_coxph_c,
        plot_ice_ranger_c,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
ice_grid_c # Figure 3

# save grid of centered ice plots
ggsave(
    fig("figure_3.pdf"),
    plot = ice_grid_c,
    width = 14,
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
plot_pfi_coxph <- plot_pfi(
    df_pfi_coxph,
    color_values = c(
        "#0072B2",
        "#D55E00",
        "#CC79A7"
    ),
    ylimits = c(-0.02, 0.13),
    breaks = c(seq(0, 5, 1))
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
plot_pfi_ranger <- plot_pfi(
    df_pfi_ranger,
    model = "ranger",
    color_values = c(
        "#0072B2",
        "#D55E00",
        "#CC79A7"
    ),
    ylimits = c(-0.02, 0.13),
    breaks = c(seq(0, 5, 1))
)
plot_pfi_ranger


## Create plot grid and save plots ---------------------------------------------
# create grid of pfi plots
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
pfi_grid # Figure 8

# save grid of pfi plots
ggsave(
    fig("figure_8.pdf"),
    plot = pfi_grid,
    width = 14,
    height = 6,
    device = "pdf"
)
