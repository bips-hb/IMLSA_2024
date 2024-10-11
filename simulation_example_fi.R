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
####                          Data Simulation                               ####
#------------------------------------------------------------------------------#


## Simulate survival data ------------------------------------------------------
# set seed for reproducibility
set.seed(2607)

# set number of simulated observations
n <- 800

# simulate feature values
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 0, 1)
x3 <- rnorm(n, 0, 1)
x <- data.frame(x1 = x1,
                x2 = x2,
                x3 = x3)

# set parameter values
betas <- data.frame(
    lambda   = rep(0.1, n),
    beta_1 = rep(-0.5, n),
    beta_2 = rep(-0.5, n),
    beta_3 = rep(3, n),
    beta_4 = rep(-0.5, n)
)

# define hazard function for simsurv
haz <- function(t, x, betas) {
    betas[["lambda"]] * exp(betas[["beta_1"]] * x[["x1"]] +
                                betas[["beta_2"]] * x[["x2"]] +
                                betas[["beta_3"]] * x[["x1"]] * x[["x2"]] +
                                betas[["beta_4"]] * x[["x3"]])
}

# simulate survival times using simsurv package
simdat <- simsurv(hazard = haz,
                  x = x,
                  betas = betas,
                  maxt = 20)

# remove id column
y <- simdat[, -1]

# add feature values to simulated survival times
dat <- cbind(y, x)

# simulate random censoring
cen <- rbinom(n, 1, 0.2)
cen_status <- ifelse((cen == 1) | (dat$status == 0), 0, 1)
dat$status <- cen_status



#------------------------------------------------------------------------------#
####                           Model Training                               ####
#------------------------------------------------------------------------------#


## Model training --------------------------------------------------------------
# set seed for reproducibility
set.seed(2607)

# define the proportion of the training data
train_proportion <- 2/3

# row indices of the training set
train_indices <- sample(1:nrow(dat), size = floor(train_proportion * nrow(dat)))

# split the data into training and test set
train_dat <- dat[train_indices, ]
test_dat <- dat[-train_indices, ]


# define times variable
times <- seq(0, 20, by = 1)

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
####               Feature Interaction Friedman's H-statistics              ####
#------------------------------------------------------------------------------#


## coxph -----------------------------------------------------------------------
# compute feature interaction values
df_Hjk_coxph_x1 <-
    feature_interaction(explainer = coxph_explainer,
                        feature = "x1",
                        N = NULL)

# create custom plot of H-statistic curves over time
plot_hjk_coxph_x1 <- plot_f_inter(df_Hjk_coxph_x1, subtitle = "coxph")
plot_hjk_coxph_x1


## ranger ----------------------------------------------------------------------
# compute feature interaction values
df_Hjk_ranger_x1 <-
    feature_interaction(explainer = ranger_explainer,
                        feature = "x1",
                        N = NULL)

# create custom plot of H-statistic curves over time
plot_hjk_ranger_x1 <- plot_f_inter(df_Hjk_ranger_x1)
plot_hjk_ranger_x1


## create plot grid and save plots ---------------------------------------------
# create grid of ale plots
inter_grid <-
    ggarrange(
        plot_hjk_coxph_x1,
        plot_hjk_ranger_x1,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
inter_grid # Figure

# save grid of ale plots
ggsave(
    fig("inter_sim_grid.pdf"),
    plot = inter_grid,
    width = 14,
    height = 6,
    device = "pdf"
)
