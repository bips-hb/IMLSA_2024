## setup -----------------------------------------------------------------------
# packages
library(haven)
library(dplyr)
library(mice)
library(survminer)
library(finalfit)
library(survival)
library(ggplot2)

# figure path
fig_path <- here::here("figures_iml")
if (!file.exists(fig_path))
  dir.create(fig_path)
fig <- function(x)
  here::here(fig_path, x)

#------------------------------------------------------------------------------#
####                          Data Preprocessing                            ####
#------------------------------------------------------------------------------#


## import birth recode data ----------------------------------------------------
path <- "/Users/sophielangbein/Desktop/PhD/Daten"
data_ghbr2014 <-
  read_sas(paste(path, "/Ghana_2014/GHBR72SD/GHBR72FL.SAS7BDAT",
                 sep = ""))


## create survival outcome and censoring indicator -----------------------------
alive <-
  data_ghbr2014$B5 # child alive (=1) or dead (=0) at time of interview
age_alive <-
  (data_ghbr2014$V008 - data_ghbr2014$B3) # age of living child in months
age_dead <-
  data_ghbr2014$B7 # age of dead child in months at time of death in months
survivaltime <-
  ifelse(alive == 1, age_alive, age_dead) # joint survivaltime outcome for dead and alive children
status <-
  ifelse((alive == 0) |
           ((alive == 1) &
              (survivaltime == 60)), 1, 0) # censoring indicator (censored = 0, child is alive at age <60, uncensored = 1, child is dead or alive at age 60)


## select features -------------------------------------------------------------
sex <- data_ghbr2014$B4 # sex of child, male (=1), female (=2)
place_residence <-
  data_ghbr2014$V025 # place of residence, urban (=1), rural (=2)
wealth_idx <-
  data_ghbr2014$V190 # wealth index, poorest (=1),..., richest (=5)
mother_age <- data_ghbr2014$V012 # mothers age in years
age_head <- data_ghbr2014$V152 # age of household head
total_dead_sons <- data_ghbr2014$V206 # total dead sons
total_dead_daughters <- data_ghbr2014$V207 # total dead daughters
multiples <-
  data_ghbr2014$B0 # order number in case of multiple birth, single birth (=0), twins (=1 or 2), triplets (=1,2 or 3)
total_births_5years <-
  data_ghbr2014$V208 # total births in the last 5 years
total_children <-
  data_ghbr2014$V220 # total number of living children including current pregnancy, truncated at 6
place_of_delivery <-
  data_ghbr2014$M15 # place of delivery of the child, 11 = "Respondent's home", 12 = "Other home", 21 = "Government hospital", 22 = "Government health center / clinic", 23 = "Government health post / CHPS", 24 = "Mobile clinic", 26 = "Other public", 31 = "Private hospital, clinic", 32 = "FP/PPAG clinic", 33 = "Mobile clinic", 34 = "Maternity home", 36 = "Other private medical", 96 = "Other"
dpt1_vaccination <-
  data_ghbr2014$H3 # received DPT 1 vaccination, 0 = "No", 1 = "Vaccination date on card", 2 = "Reported by mother", 3 = "Vaccination marked on card", 8 = "Don't know", NA = "No living children born in the last 3 years"


## create analysis dataframe ---------------------------------------------------
df_ghana <- data.frame(
  status,
  survivaltime,
  alive,
  sex,
  place_residence,
  wealth_idx,
  mother_age,
  age_head,
  total_dead_sons,
  total_dead_daughters,
  multiples,
  total_births_5years,
  total_children,
  place_of_delivery,
  dpt1_vaccination
)


## filter under 5 year survival ------------------------------------------------
df_ghana <- df_ghana %>% filter(survivaltime <= 60)


## convert categorical features to factors -------------------------------------
# sex
df_ghana$sex <- factor(df_ghana$sex,
                       levels = 1:2,
                       labels = c("male", "female"))

df_ghana$place_residence <- factor(
  df_ghana$place_residence,
  levels = 1:2,
  labels = c("urban", "rural")
)

# wealth_idx
df_ghana$wealth_idx <- factor(
  df_ghana$wealth_idx,
  levels = 1:5,
  labels = c("poorest",
             "poorer",
             "middle",
             "richer",
             "richest")
)

# multiples
df_ghana$multiples <- factor(
  df_ghana$multiples,
  levels = 0:3,
  labels = c(
    "single child",
    "firstborn multiple",
    "secondborn multiple",
    "thirdborn multiple"
  )
)

# place_of_delivery: collapse 13 detailed categories into 4 broader categories
df_ghana <- df_ghana %>%
  mutate(place_of_delivery = as.numeric(substring(as.character(place_of_delivery),
                                                  1, 1)))
df_ghana$place_of_delivery <- factor(
  df_ghana$place_of_delivery,
  levels = c(1:3, 96),
  labels = c(
    "home delivery",
    "public/government facility",
    "private facility",
    "other"
  )
)

# dpt1_vaccination: create designated category for children >3 years
df_ghana$dpt1_vaccination[df_ghana$survivaltime > 36]  <- 4
df_ghana$dpt1_vaccination <- factor(
  df_ghana$dpt1_vaccination,
  levels = c(0:4, 8),
  labels = c(
    "no",
    "vaccination date on card",
    "reported by mother",
    "vaccination marked on card",
    "child older than 3 years",
    "don't know"
  )
)


## imputation with mice --------------------------------------------------------
# set seed for reproducibility
set.seed(123)

# create an imputation model
imputation_model <- mice(df_ghana, m = 1, method = "pmm")

# rename original dataset with missing values
df_ghana_missing <- df_ghana

# perform single imputation
df_ghana <- complete(imputation_model)


#------------------------------------------------------------------------------#
####                        Exploratory Analysis                            ####
#------------------------------------------------------------------------------#


## summary of analysis dataframe with missing values ---------------------------
summary(df_ghana_missing) # missing: place_of_delivery: 1874 and dpt1_vaccination 1916


## summary of imputed analysis dataframe ---------------------------------------
summary(df_ghana)
# dpt1_vaccination: only 11 children "with "vaccination marked on card", only 9 children in "don't know"
# multiples: only 2 thirdborn multiples


## survivaltime distributions of dead vs alive children ------------------------
# convert "alive" variable to factor for plotting
df_ghana$alive <- factor(df_ghana$alive,
                         levels = c(0, 1),
                         labels = c("dead", "alive"))

# plot 2 histograms of survivaltime distributions
plot_dist <-
  ggplot(df_ghana,
         aes(x = survivaltime)) +
  geom_histogram(bins = 30,
                 color = "white",
                 fill = "#FF99CC") +
  facet_grid(. ~ alive) +
  theme_bw()
plot_dist
# survivaltimes of living children are approximately uniformly distributed in the sample (125-250 children at each age in months)
# survivaltime distribution of dead children is very uneven, a majority of children dies <10 months, with a large majority at birth (>750),
# for ages >20 at many times there are no events, while at some time points there is a large number of events

# determine times at which deaths >12 months occur
sort(unique(df_ghana["survivaltime"][df_ghana["alive"] == "dead"]))
# events only occur for t>12 at t = 24; t = 36; t = 48; t = 60 (full years)


## Kaplan-Meier survival curves ------------------------------------------------
# create surv object
surv <- Surv(time = df_ghana$survivaltime, event = df_ghana$status)

# summary of surv object
summary(surv)

# Kaplan Meier
km_fit <- survfit(surv ~ 1,
                data = df_ghana)

# visualize Kaplan-Meier surves with survminer
plot_km <- ggsurvplot(
  km_fit,
  data = df_ghana,
  risk.table = TRUE,
  ggtheme = theme_bw(),
  palette = "#FF99CC",
  font.x = c(14),
  font.y = c(14),
  font.tickslab = c(12),
  legend = "none"
)
plot_km
# clear pattern of stepwise decrease in survival probability at times that events occur


# convert "alive" variable to back to numeric
df_ghana$alive <- as.numeric(df_ghana$alive)

## save survivaltime distribution plot and Kaplan-Meier curve ------------------
# save survivaltime distribution plot
ggsave(
  fig("plot_dist.pdf"),
  plot = plot_dist,
  width = 7,
  height = 6,
  device = "pdf"
)

# save Kaplan-Meier curve
ggsave(
  fig("plot_km.pdf"),
  plot = plot_km,
  width = 7,
  height = 6,
  device = "pdf"
)

