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
xx <- runif(n)
x1 <- xx + rnorm(n, 0, 0.01)
x2 <- xx + rnorm(n, 0, 0.01)
x <- data.frame(x1 = x1, x2 = x2)

# simulate survival times using simsurv package
simdat <- simsurv(
    dist = "weibull",
    lambdas = 0.1,
    gammas = 1.5,
    betas = c(x1 = -4.5, x2 = 4.5),
    x = x,
    maxt = 5
)

# remove id column from simulation dataframe
y <- simdat[, -1]

# add feature values to simulation dataframe
dat <- cbind(y, x)

# simulate random censoring
cen <- rbinom(n, 1, 0.2)
cen_status <- ifelse((cen == 1) | (dat$status == 0), 0, 1)
dat$status <- cen_status



#------------------------------------------------------------------------------#
####                        Model Training & Set-up                         ####
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

# fit random survival forest on training data
ranger_rsf <- rfsrc(
    Surv(eventtime, status) ~ .,
    data = train_dat,
    mtry = 2,
    ntime = times
)


## Create survex explainer object ----------------------------------------------
# create explainer object for ranger model on test data
ranger_explainer <- survex::explain(
    ranger_rsf,
    data = test_dat[, -c(1, 2)],
    y = Surv(test_dat$eventtime, test_dat$status),
    times = times
)



#------------------------------------------------------------------------------#
####                   Accumulated Local Effects (ALE)                      ####
#------------------------------------------------------------------------------#


## x1 --------------------------------------------------------------------------
# compute accumulated local effects values
ale_ranger_x1 <- model_profile(
    ranger_explainer,
    N = 1000,
    variables = "x1",
    type = "accumulated",
    center = TRUE
)

# extract relevant ale results for plotting
df_ale_ranger_x1 <-
    ale_ranger_x1$result[(ale_ranger_x1$result$`_vname_` == "x1") &
                             (ale_ranger_x1$result$`_times_` %in% times), c("_x_", "_yhat_")]

# rename columns
names(df_ale_ranger_x1)[names(df_ale_ranger_x1) == "_x_"] <- "value"
names(df_ale_ranger_x1)[names(df_ale_ranger_x1) == "_yhat_"] <- "prediction"

# add time column
df_ale_ranger_x1$time <- rep(times, times = (nrow(df_ale_ranger_x1) / length(times)))

# create custom plot of ale curves over time
plot_ale_x1 <- plot_ale_pdp(
    df_ale_ranger_x1,
    model = "ranger",
    x_label = "x1",
    y_label = "ALE value",
    limits = c(-0.8, 0.8),
    breaks_x = seq(0, 1, by = 0.2),
    breaks_y = seq(-0.8, 0.8, by = 0.2),
    key_width = 0.6,
    key_spacing = 0.1
)
plot_ale_x1


## x2 --------------------------------------------------------------------------
# compute accumulated local effects values
ale_ranger_x2 <- model_profile(ranger_explainer,
                               N = 1000,
                               variables = "x2",
                               type = "accumulated",
                               center = TRUE)

# extract relevant ale results for plotting
df_ale_ranger_x2 <-
    ale_ranger_x2$result[(ale_ranger_x2$result$`_vname_` == "x2") &
                             (ale_ranger_x2$result$`_times_` %in% times), c("_x_", "_yhat_")]

# rename columns
names(df_ale_ranger_x2)[names(df_ale_ranger_x2) == "_x_"] <- "value"
names(df_ale_ranger_x2)[names(df_ale_ranger_x2) == "_yhat_"] <- "prediction"

# add time column
df_ale_ranger_x2$time <- rep(times, times = (nrow(df_ale_ranger_x2)/length(times)))

# create custom plot of ale curves over time
plot_ale_x2 <- plot_ale_pdp(
    df_ale_ranger_x2,
    model = "ranger",
    x_label = "x2",
    y_label = "ALE value",
    limits = c(-0.8, 0.8),
    breaks_x = seq(0, 1, by = 0.2),
    breaks_y = seq(-0.8, 0.8, by = 0.2),
    key_width = 0.6,
    key_spacing = 0.1
)
plot_ale_x2



#------------------------------------------------------------------------------#
####                       Partial Dependence (PDP)                         ####
#------------------------------------------------------------------------------#


## x1 --------------------------------------------------------------------------
# compute partial dependence
pdp_ranger_x1 <- model_profile(ranger_explainer,
                               variables = "x1",
                               N = NULL,
                               center = TRUE)

# extract relevant ice results for plotting
df_ice_ranger_x1 <-
    pdp_ranger_x1$cp_profiles$result[(pdp_ranger_x1$cp_profiles$result$`_vname_` == "x1")  &
                                         (pdp_ranger_x1$cp_profiles$result$`_times_` %in% times), ]

# rename columns
names(df_ice_ranger_x1)[names(df_ice_ranger_x1) == "_times_"] = "time"
names(df_ice_ranger_x1)[names(df_ice_ranger_x1) == "_yhat_"] = "prediction"
names(df_ice_ranger_x1)[names(df_ice_ranger_x1) == "_ids_"] = "ids"
names(df_ice_ranger_x1)[names(df_ice_ranger_x1) == "x1"] = "value"

# drop irrelevant columns
df_ice_ranger_x1 <-
    df_ice_ranger_x1[, c("value", "time", "prediction", "ids")]

# aggregate centered ice values to obtain centered pdp values
df_pdp_ranger_x1 <- aggregate(prediction ~ time + value, data = df_ice_ranger_x1[, c("ids", "value", "prediction", "time")], FUN = mean)

# create custom plot of ale curves over time
plot_pdp_x1 <- plot_ale_pdp(
    df_pdp_ranger_x1,
    model = "ranger",
    x_label = "x1",
    y_label = "PD value",
    limits = c(-0.8, 0.8),
    breaks_x = seq(0, 1, by = 0.2),
    breaks_y = seq(-0.8, 0.8, by = 0.2),
    key_width = 0.6,
    key_spacing = 0.1
)
plot_pdp_x1


## x2 --------------------------------------------------------------------------
# compute partial dependence
pdp_ranger_x2 <- model_profile(ranger_explainer,
                               variables = "x2",
                               N = NULL,
                               center = TRUE)

# extract relevant ice results for plotting
df_ice_ranger_x2 <-
    pdp_ranger_x2$cp_profiles$result[(pdp_ranger_x2$cp_profiles$result$`_vname_` == "x2")  &
                                         (pdp_ranger_x2$cp_profiles$result$`_times_` %in% times), ]

# rename columns
names(df_ice_ranger_x2)[names(df_ice_ranger_x2) == "_times_"] = "time"
names(df_ice_ranger_x2)[names(df_ice_ranger_x2) == "_yhat_"] = "prediction"
names(df_ice_ranger_x2)[names(df_ice_ranger_x2) == "_ids_"] = "ids"
names(df_ice_ranger_x2)[names(df_ice_ranger_x2) == "x2"] = "value"

# drop irrelevant columns
df_ice_ranger_x2 <-
    df_ice_ranger_x2[, c("value", "time", "prediction", "ids")]

# aggregate centered ice values to obtain centered pdp values
df_pdp_ranger_x2 <- aggregate(prediction ~ time + value, data = df_ice_ranger_x2[, c("ids", "value", "prediction", "time")], FUN = mean)

# create custom plot of ale curves over time
plot_pdp_x2 <- plot_ale_pdp(
    df_pdp_ranger_x2,
    model = "ranger",
    x_label = "x2",
    y_label = "PD value",
    limits = c(-0.8, 0.8),
    breaks_x = seq(0, 1, by = 0.2),
    breaks_y = seq(-0.8, 0.8, by = 0.2),
    key_width = 0.6,
    key_spacing = 0.1
)
plot_pdp_x2


## Create plot grids and save plots ---------------------------------------------
# create grid of ale plots
ale_grid <-
    ggarrange(
        plot_ale_x1,
        plot_ale_x2,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
ale_grid # Figure 6

# save grid of ale plots
ggsave(
    fig("figure_6.pdf"),
    plot = ale_grid,
    width = 14,
    height = 6,
    device = "pdf"
)

# create grid of pdp plots
pdp_grid <-
    ggarrange(
        plot_pdp_x1,
        plot_pdp_x2,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
pdp_grid # Figure 5

# save grid of pdp plots
ggsave(
    fig("figure_5.pdf"),
    plot = pdp_grid,
    width = 14,
    height = 6,
    device = "pdf"
)
