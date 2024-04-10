## setup -----------------------------------------------------------------------
# packages
library(survex)
library(ggplot2)
library(ranger)
library(ggbeeswarm)



# figure path
fig_path <- here::here("figures_iml")
if (!file.exists(fig_path))
  dir.create(fig_path)
fig <- function(x)
  here::here(fig_path, x)


#------------------------------------------------------------------------------#
####                     Data and Model Preparation                         ####
#------------------------------------------------------------------------------#



### set up explainers with test data -------------------------------------------
## ranger
ranger_explainer <- survex::explain(
  learner_ranger_tuned,
  data = df_ghana[test_indices,-c(1, 2, 3)],
  y = Surv(df_ghana[test_indices, ]$survivaltime,
           df_ghana[test_indices, ]$status),
  times = 0:60,
  label = "ranger"
)


## blackboost
blackboost_explainer <- survex::explain(
  learner_blackboost_tuned,
  data = df_ghana[test_indices,-c(1, 2, 3)],
  y = Surv(df_ghana[test_indices, ]$survivaltime,
           df_ghana[test_indices, ]$status),
  times = 0:60,
  label = "blackboost"
)


## deepsurv
deepsurv_explainer <- survex::explain(
  learner_deepsurv_tuned,
  data = df_ghana_encoded[test_indices,-c(1, 2, 3)],
  y = Surv(df_ghana_encoded[test_indices, ]$survivaltime,
           df_ghana_encoded[test_indices, ]$status),
  times = 0:60,
  label = "deepsurv"
)


## coxph
coxph_explainer <- survex::explain(
  learner_coxph,
  data = df_ghana[test_indices,-c(1, 2, 3)],
  y = Surv(df_ghana[test_indices, ]$survivaltime,
           df_ghana[test_indices, ]$status),
  times = 0:60,
  label = "coxph"
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
    x[x$`_permutation_` == 0,!colnames(x) %in% c("_permutation_", "label", "_baseline_")]
  plotting_df <-
    with(x, cbind(x[1], stack(x, select = -`_times_`), label, row.names = NULL))
})
df_pfi_coxph <- do.call(rbind, transformed_dfs)

# rename columns
names(df_pfi_coxph)[names(df_pfi_coxph) == "_times_"] <- "time"
names(df_pfi_coxph)[names(df_pfi_coxph) == "ind"] <- "features"

# delete full model results and results for t = 60
df_pfi_coxph <- subset(df_pfi_coxph, features != "_full_model_")
df_pfi_coxph <- subset(df_pfi_coxph, time != 60)

# create custom plot of permutation feature importance over time
plot_pfi_coxph <-
  ggplot(df_pfi_coxph, aes(x = time, y = values, color = features)) +
  geom_line(size = 0.8) +
  scale_color_manual(
    values = c(
      "#ffff99",
      "#b15928",
      "#a6cee3",
      "#1f78b4",
      "#fb9a99",
      "#e31a1c",
      "#fdbf6f",
      "#ff7f00",
      "#b2df8a",
      "#33a02c",
      "#cab2d6",
      "#6a3d9a"
    )
  ) +
  ggtitle("", subtitle = "coxph") +
  theme_bw() +
  scale_x_continuous(breaks = c(seq(0, 50, 10), 59)) +
  ylab(expression(
    paste(
      "Brier score loss after permutations\nwith loss of full model subtracted"
    )
  )) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      size = 0.3
    )
  )
plot_pfi_coxph


## blackboost ------------------------------------------------------------------
# compute permutation feature importance
pfi_blackboost <- model_parts(blackboost_explainer)

# extract relevant results for plotting
df_list <- c(list(pfi_blackboost))
transformed_dfs <- lapply(df_list, function(x) {
  x <- x$result
  label <- unique(x$label)
  x <-
    x[x$`_permutation_` == 0,!colnames(x) %in% c("_permutation_", "label", "_baseline_")]
  plotting_df <-
    with(x, cbind(x[1], stack(x, select = -`_times_`), label, row.names = NULL))
})
df_pfi_blackboost <- do.call(rbind, transformed_dfs)

# rename columns
names(df_pfi_blackboost)[names(df_pfi_blackboost) == "_times_"] <-
  "time"
names(df_pfi_blackboost)[names(df_pfi_blackboost) == "ind"] <-
  "features"

# delete full model results and results for t = 60
df_pfi_blackboost <-
  subset(df_pfi_blackboost, features != "_full_model_")
df_pfi_blackboost <- subset(df_pfi_blackboost, time != 60)

# create custom plot of permutation feature importance over time
plot_pfi_blackboost <-
  ggplot(df_pfi_blackboost, aes(x = time, y = values, color = features)) +
  geom_line(size = 0.8) +
  scale_color_manual(
    values = c(
      "#ffff99",
      "#b15928",
      "#a6cee3",
      "#1f78b4",
      "#fb9a99",
      "#e31a1c",
      "#fdbf6f",
      "#ff7f00",
      "#b2df8a",
      "#33a02c",
      "#cab2d6",
      "#6a3d9a"
    )
  ) +
  ggtitle("", subtitle = "blackboost") +
  theme_bw() +
  scale_x_continuous(breaks = c(seq(0, 50, 10), 59)) +
  ylab(expression(
    paste(
      "Brier score loss after permutations\nwith loss of full model subtracted"
    )
  )) +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      size = 0.3
    )
  )
plot_pfi_blackboost


## create plot grid and save plots ---------------------------------------------
# create grid of plots
pfi_grid <-
  ggarrange(
    plot_pfi_coxph,
    plot_pfi_blackboost,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  ) +
  theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
pfi_grid

# save grid of plots
ggsave(
  fig("pfi_grid.pdf"),
  plot = pfi_grid,
  width = 14,
  height = 6,
  device = "pdf"
)



#------------------------------------------------------------------------------#
####  Individual Conditional Expectation (ICE) & Partial Dependence (PDP)   ####
#------------------------------------------------------------------------------#


## deepsurv --------------------------------------------------------------------
# compute partial dependence and individual conditional expectation values
pdp_ice_deepsurv <- model_profile(deepsurv_explainer,
                                  variables = "mother_age")

# specify time variable
times = 0:59

# extract relevant ice results for plotting
df_ice_deepsurv <-
  pdp_ice_deepsurv$cp_profiles$result[(pdp_ice_deepsurv$cp_profiles$result$`_vname_` == "mother_age")  &
                                        (pdp_ice_deepsurv$cp_profiles$result$`_times_` %in% times), ]

# rename columns
names(df_ice_deepsurv)[names(df_ice_deepsurv) == "_times_"] = "time"
names(df_ice_deepsurv)[names(df_ice_deepsurv) == "_yhat_"] = "yhat"
names(df_ice_deepsurv)[names(df_ice_deepsurv) == "_ids_"] = "ids"

# sample 100 rows
df_ice_deepsurv$ids <- as.numeric(df_ice_deepsurv$ids)
set.seed(123)
sampled_ids <- sample(sort(unique(df_ice_deepsurv$ids)), 100, replace = FALSE)
df_ice_deepsurv <- df_ice_deepsurv[df_ice_deepsurv$ids %in% sampled_ids, ]

# drop irrelevant columns
df_ice_deepsurv <-
  df_ice_deepsurv[, c("mother_age", "time", "yhat", "ids")]

# select reference value for centering
df_ice_deepsurv_center <-
  df_ice_deepsurv[df_ice_deepsurv[, "mother_age"] == min(df_ice_ranger$mother_age),]

# add reference value for censoring to results dataframe
df_ice_deepsurv_merge <-
  merge(
    x = df_ice_deepsurv,
    y = df_ice_deepsurv_center,
    by = c("ids", "time"),
    all = TRUE
  )

# perform centering operation
df_ice_deepsurv_merge[, "yhat"] <-
  df_ice_deepsurv_merge[, "yhat.x"] - df_ice_deepsurv_merge[, "yhat.y"]

# rename double columns
names(df_ice_deepsurv_merge)[names(df_ice_deepsurv_merge) == "mother_age.x"] = "mother_age"

# extract ice values for times that should be plotted
df_ice_deepsurv_plot <-
  df_ice_deepsurv_merge[df_ice_deepsurv_merge[, "time"] %in% c(1, 10, 20, 30, 40, 50, 59), ]

# aggregate centered ice values to obtain centered pdp values
df_pdp_deepsurv_center <- aggregate(yhat ~ time + mother_age,
                                    data = df_ice_deepsurv_merge[, c("ids", "mother_age", "yhat", "time")],
                                    FUN = mean)

# extract pdp values for times that should be plotted
df_pdp_deepsurv_center_plot <-
  df_pdp_deepsurv_center[df_pdp_deepsurv_center[, "time"] %in% c(1, 10, 20, 30, 40, 50, 59), ]


## create custom plots
# create custom plot of centered ice and pdp curves over time
plot_pdp_ice_deepsurv <- ggplot() +
  geom_line(
    data = df_ice_deepsurv_plot,
    aes(
      x = mother_age,
      y = yhat,
      group = interaction(ids, time),
      color = time
    ),
    alpha = 0.1
  ) +
  geom_path(
    data = df_pdp_deepsurv_center_plot,
    aes(
      x = mother_age,
      y = yhat,
      color = time,
      group = time
    ),
    linewidth = 1.5,
    lineend = "round",
    linejoin = "round"
  ) +
  geom_path(
    data = df_pdp_deepsurv_center_plot,
    aes(x = mother_age, y = yhat, group = time),
    color = "mistyrose",
    linewidth = 0.5,
    linetype = "dashed",
    lineend = "round",
    linejoin = "round"
  ) +
  geom_rug(
    data = df_ghana[sampled_ids, ],
    aes(x = mother_age, y = max(df_ice_deepsurv_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(
      df_pdp_deepsurv_center[, "mother_age"]
    )))
  ) +
  scale_color_viridis_c() +
  ggtitle("", subtitle = "deepsurv") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.9, 0.25),
                     breaks = seq(-0.9, 0.25, by = 0.2)) +
  ylab("prediction") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      linewidth = 0.3
    )
  )
plot_pdp_ice_deepsurv

# create custom plot of centered ice curves over time
plot_ice_deepsurv <- ggplot() +
  geom_line(
    data = df_ice_deepsurv_plot,
    aes(
      x = mother_age,
      y = yhat,
      group = interaction(ids, time),
      color = time
    ),
    alpha = 0.1
  ) +
  geom_rug(
    data = df_ghana[sampled_ids, ],
    aes(x = mother_age, y = max(df_ice_deepsurv_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(
      df_pdp_deepsurv_center[, "mother_age"]
    )))
  ) +
  scale_color_viridis_c() +
  ggtitle("", subtitle = "deepsurv") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.9, 0.25),
                     breaks = seq(-0.9, 0.25, by = 0.2)) +
  ylab("prediction") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      linewidth = 0.3
    )
  )
plot_ice_deepsurv


## ranger ----------------------------------------------------------------------
# compute partial dependence and individual conditional expectation values
pdp_ice_ranger <- model_profile(ranger_explainer,
                                variables = "mother_age")

# extract relevant ice results for plotting
df_ice_ranger <-
  pdp_ice_ranger$cp_profiles$result[(pdp_ice_ranger$cp_profiles$result$`_vname_` == "mother_age")  &
                                      (pdp_ice_ranger$cp_profiles$result$`_times_` %in% times),]

# rename columns
names(df_ice_ranger)[names(df_ice_ranger) == "_times_"] = "time"
names(df_ice_ranger)[names(df_ice_ranger) == "_yhat_"] = "yhat"
names(df_ice_ranger)[names(df_ice_ranger) == "_ids_"] = "ids"

# sample 100 rows
df_ice_ranger <- df_ice_ranger[df_ice_ranger$ids %in% sampled_ids, ]

# drop irrelevant columns
df_ice_ranger <-
  df_ice_ranger[, c("mother_age", "time", "yhat", "ids")]

# select reference value for censoring
df_ice_ranger_center <-
  df_ice_ranger[df_ice_ranger[, "mother_age"] == min(df_ice_ranger$mother_age), ]

# add reference value for censoring to results dataframe
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
names(df_ice_ranger_merge)[names(df_ice_ranger_merge) == "mother_age.x"] <-
  "mother_age"

# extract ice values for times that should be plotted
df_ice_ranger_plot <-
  df_ice_ranger_merge[df_ice_ranger_merge[, "time"] %in% c(1, 10, 20, 30, 40, 50, 59), ]

# aggregate centered ice values to obtain centered pdp values
df_pdp_ranger_center <- aggregate(yhat ~ time + mother_age,
                           data = ice_df_merge[, c("ids", "mother_age", "yhat", "time")],
                           FUN = mean)

# extract pdp values for times that should be plotted
df_pdp_ranger_center_plot <-
  df_pdp_ranger_center[df_pdp_ranger_center[, "time"] %in% c(1, 10, 20, 30, 40, 50, 59), ]

## create custom plots----------------------------------------------------------
# create custom plot of centered ice and pdp curves over time
plot_pdp_ice_ranger <- ggplot() +
  geom_line(
    data = df_ice_ranger_plot,
    aes(
      x = mother_age,
      y = yhat,
      group = interaction(ids, time),
      color = time
    ),
    alpha = 0.1
  ) +
  geom_path(
    data = df_pdp_ranger_center_plot,
    aes(
      x = mother_age,
      y = yhat,
      color = time,
      group = time
    ),
    linewidth = 1.5,
    lineend = "round",
    linejoin = "round"
  ) +
  geom_path(
    data = df_pdp_ranger_center_plot,
    aes(x = mother_age, y = yhat, group = time),
    color = "mistyrose",
    linewidth = 0.5,
    linetype = "dashed",
    lineend = "round",
    linejoin = "round"
  ) +
  geom_rug(
    data = df_ghana[sampled_ids, ],
    aes(x = mother_age, y = max(df_ice_ranger_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(df_pdp_ranger_center[, "mother_age"])))
  ) +
  scale_color_viridis_c() +
  ggtitle("", subtitle = "ranger") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.25, 0.05),
                     breaks = seq(-0.25, 0.05, by = 0.05)) +
  ylab("prediction") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      linewidth = 0.3
    )
  )
plot_pdp_ice_ranger

# create custom plot of centered ice curves over time
plot_ice_ranger <- ggplot() +
  geom_line(
    data = df_ice_ranger_plot,
    aes(
      x = mother_age,
      y = yhat,
      group = interaction(ids, time),
      color = time
    ),
    alpha = 0.1
  ) +
  geom_rug(
    data = df_ghana[sampled_ids, ],
    aes(x = mother_age, y = max(df_ice_ranger_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(df_pdp_ranger_center[, "mother_age"])))
  ) +
  scale_color_viridis_c() +
  ggtitle("", subtitle = "ranger") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.25, 0.05),
                     breaks = seq(-0.25, 0.05, by = 0.05)) +
  ylab("prediction") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      linewidth = 0.3
    )
  )
plot_ice_ranger

## create plot grid and save plots
# create grid of ice and pdp plots
pdp_ice_grid <-
  ggarrange(
    plot_pdp_ice_ranger,
    plot_pdp_ice_deepsurv,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  ) +
  theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
pdp_ice_grid

# create grid of ice and pdp plots
ggsave(
  fig("pdp_ice_grid.pdf"),
  plot = pdp_ice_grid,
  width = 14,
  height = 6,
  device = "pdf"
)

# create grid of ice plots
ice_grid <-
  ggarrange(
    plot_ice_ranger,
    plot_ice_deepsurv,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  ) +
  theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
ice_grid

# create grid of ice and pdp plots
ggsave(
  fig("ice_grid.pdf"),
  plot = ice_grid,
  width = 14,
  height = 6,
  device = "pdf"
)

#------------------------------------------------------------------------------#
####                   Accumulated Local Effects (ALE)                      ####
#------------------------------------------------------------------------------#


## coxph -----------------------------------------------------------------------
# compute accumulated local effects values
ale_coxph <- model_profile(coxph_explainer,
                           variables = "dpt1_vaccination",
                           type = "accumulated")

# extract relevant ale results for plotting
df_ale_coxph <-
  ale_coxph$result[(ale_coxph$result$`_vname_` == "dpt1_vaccination") &
                     (ale_coxph$result$`_times_` %in% times), c("_x_", "_yhat_")]

# rename columns
names(df_ale_coxph)[names(df_ale_coxph) == "_x_"] <- "value"
names(df_ale_coxph)[names(df_ale_coxph) == "_yhat_"] <- "prediction"

# add time column
df_ale_coxph$time <- rep(0:59, times = 4)

# order rows by feature values (categories)
df_ale_coxph <- df_ale_coxph[order(df_ale_coxph$value), ]

# create custom plot of ale curves over time
plot_ale_coxph <- ggplot() +
  geom_path(
    data = df_ale_coxph,
    aes(
      x = time,
      y = prediction,
      color = value,
      group = value
    ),
    linewidth = 0.8
  ) +
  scale_color_manual(values = c("#a9def9", "#ede7b1", "#ff99c8",
                                "#e4c1f9")) +
  ggtitle("", subtitle = "coxph") +
  theme_bw() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 59)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ylab("prediction") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      size = 0.3
    )
  )
plot_ale_coxph

## blackboost
# compute accumulated local effects values
ale_blackboost <- model_profile(blackboost_explainer,
                                variables = "dpt1_vaccination",
                                type = "accumulated")

# extract relevant ale results for plotting
df_ale_blackboost <-
  ale_blackboost$result[(ale_blackboost$result$`_vname_` == "dpt1_vaccination") &
                          (ale_blackboost$result$`_times_` %in% times), c("_x_", "_yhat_")]

# rename columns
names(df_ale_blackboost)[names(df_ale_blackboost) == "_x_"] <-
  "value"
names(df_ale_blackboost)[names(df_ale_blackboost) == "_yhat_"] <-
  "prediction"

# add time column
df_ale_blackboost$time <- rep(0:59, times = 4)

# order rows by feature values
df_ale_blackboost <-
  df_ale_blackboost[order(df_ale_blackboost$value), ]

# create custom plot of ale curves over time
plot_ale_blackboost <- ggplot() +
  geom_path(
    data = df_ale_blackboost,
    aes(
      x = time,
      y = prediction,
      color = value,
      group = value
    ),
    linewidth = 0.8
  ) +
  scale_color_manual(values = c("#a9def9", "#ede7b1", "#ff99c8",
                                "#e4c1f9")) +
  theme_bw() +
  ggtitle("", subtitle = "blackboost") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 59)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ylab("prediction") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      size = 0.3
    )
  )
plot_ale_blackboost

## create plot grid and save plots ---------------------------------------------
# create grid of ale plots
ale_grid <-
  ggarrange(
    plot_ale_coxph,
    plot_ale_blackboost,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  ) +
  theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
ale_grid

# save grid of ale plots
ggsave(
  fig("ale_grid.pdf"),
  plot = ale_grid,
  width = 14,
  height = 6,
  device = "pdf"
)

#------------------------------------------------------------------------------#
####               Local SHapley Additive exPlanations (SHAP)               ####
#------------------------------------------------------------------------------#

## create new model and explainer ----------------------------------------------
# set up ranger model with tuned hyperparameters using ranger package
ranger_rsf <- ranger(
  Surv(survivaltime, status) ~ .,
  data = df_ghana[train_indices, -c(3)],
  num.trees = 967,
  mtry = 6,
  max.depth = 6,
  min.node.size = 36,
  sample.fraction = 0.8475985
)

# set up new explainer using ranger model from ranger package
ranger_explainer2 <- survex::explain(
    ranger_rsf,
    data = df_ghana[test_indices, -c(1, 2, 3)],
    y = Surv(df_ghana[test_indices,]$survivaltime,
             df_ghana[test_indices,]$status)
)

## select interesting instances ------------------------------------------------
# child dead at t = 24
individual_5287 <- df_ghana[5287, -c(1, 2, 3)]
# child dead at t = 0
individual_53 <- df_ghana[53, -c(1, 2, 3)]

## compute shap values ---------------------------------------------------------
# compute shap values for individual 786
survshap_ranger_5287 <- predict_parts(
  ranger_explainer2,
  individual_5287,
  type = 'survshap',
  N = 500,
  calculation_method = c("kernelshap"),
  maxvar = 12
)

# compute shap values for individual 53
survshap_ranger_53 <- predict_parts(
  ranger_explainer2,
  individual_53,
  type = 'survshap',
  N = 500,
  calculation_method = c("kernelshap"),
  maxvar = 12
)

### extract relevant shap results for plotting ---------------------------------
## extract relevant shap results for plotting for indiviudal 5287 --------------
# convert results to list
dfl_shap_5287 <- c(list(survshap_ranger_5287))

# convert results to dataframe
df_shap_5287 <- lapply(dfl_shap_786, function(x) {
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
df_shap_786 <- do.call(rbind, df_shap_786)

## extract relevant shap results for plotting for indiviudal 786 ---------------
# convert results to list
dfl_shap_53 <- c(list(survshap_ranger_53))

# convert results to list
df_shap_53 <- lapply(dfl_shap_53, function(x) {
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
df_shap_53 <- do.call(rbind, df_shap_53)

## order features --------------------------------------------------------------
# create preset list of correct feature order
preset_list_all <- c(
  "sex",
  "place_residence",
  "wealth_idx",
  "mother_age",
  "age_head",
  "total_dead_sons",
  "total_dead_daughters",
  "multiples",
  "total_births_5years",
  "total_children",
  "place_of_delivery",
  "dpt1_vaccination"
)

# reorder the rows based on the preset list for individual 5287
df_shap_5287$ind <- factor(df_shap_786$ind, levels = preset_list_all)
df_shap_5287 <- df_shap_5287[order(df_shap_53$ind),]

# reorder the rows based on the preset list for individual 53
df_shap_53$ind <-
  factor(df_shap_53$ind, levels = preset_list_all)
df_shap_53 <- df_shap_53[order(df_shap_53$ind),]

## create custom plots of shap values ------------------------------------------
# create custom plots of shap values for observation 5287
plot_shap_5287 <- ggplot() +
  geom_path(data = df_shap_5287,
            aes(
              x = times,
              y = values,
              color = ind,
              group = ind
            ),
            linewidth = 0.8) +
  scale_color_manual(
    "feature",
    values = c(
      "#ffff99",
      "#b15928",
      "#a6cee3",
      "#1f78b4",
      "#fb9a99",
      "#e31a1c",
      "#fdbf6f",
      "#ff7f00",
      "#b2df8a",
      "#33a02c",
      "#cab2d6",
      "#6a3d9a"
    ),
    drop = FALSE
  ) +
  theme_bw() +
  ggtitle("C2: Child censored at t=23", subtitle = "ranger") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  scale_y_continuous(limits = c(-0.05, 0.15),
                     breaks = seq(-0.05, 0.15, by = 0.05)) +
  ylab("SurvSHAP(t) value") +
  xlab("time") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      size = 0.3
    )
  )
p_shap_5287

# create custom plots of shap values for observation 53
plot_shap_53 <- ggplot() +
  geom_path(data = df_shap_53,
            aes(
              x = times,
              y = values,
              color = ind,
              group = ind
            ),
            linewidth = 0.8) +
  scale_color_manual(
    "feature",
    values = c(
      "#ffff99",
      "#b15928",
      "#a6cee3",
      "#1f78b4",
      "#fb9a99",
      "#e31a1c",
      "#fdbf6f",
      "#ff7f00",
      "#b2df8a",
      "#33a02c",
      "#cab2d6",
      "#6a3d9a"
    ),
    drop = FALSE
  ) +
  theme_bw() +
  ggtitle("C1: Child dead at t=0", subtitle = "ranger") +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  scale_y_continuous(limits = c(-0.35, 0.1),
                     breaks = seq(-0.35, 0.1, by = 0.1)) +
  ylab("SurvSHAP(t) value") +
  xlab("time") +
  theme(
    legend.position = "bottom",
    plot.subtitle = element_text(size = 14),
    plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.key.size = unit(1.5, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(
      colour = "grey34",
      fill = "white",
      linetype = "solid",
      size = 0.3
    )
  )
plot_shap_53

## create plot grid and save plots
# create grid of ale plots
shap_grid <-
  ggarrange(
    plot_shap_53,
    plot_shap_5287,
    ncol = 2,
    nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  ) +
  theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
shap_grid

# save grid of shap plots
ggsave(
  fig("shap_grid.pdf"),
  plot = shap_grid,
  width = 14,
  height = 6,
  device = "pdf"
)

#------------------------------------------------------------------------------#
####              Global SHapley Additive exPlanations (SHAP)               ####
#------------------------------------------------------------------------------#


## compute shap values for all observations in test set ------------------------
survshap_global_ranger <-
    model_survshap(ranger_explainer2, df_ghana[test_indices,-c(1, 2, 3)])


## preprocess results for aggregated shap plot ---------------------------------
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


## create custom plot of shap values -------------------------------------------
plot_shap_agg <- ggplot() +
    geom_path(data = df_agg_long,
              aes(
                  x = times,
                  y = values,
                  color = ind,
                  group = ind
              ),
              linewidth = 0.8) +
    scale_color_manual(
        "feature",
        values = c(
            "#ffff99",
            "#b15928",
            "#a6cee3",
            "#1f78b4",
            "#fb9a99",
            "#e31a1c",
            "#fdbf6f",
            "#ff7f00",
            "#b2df8a",
            "#33a02c",
            "#cab2d6",
            "#6a3d9a"
        ),
        drop = FALSE
    ) +
    theme_bw() +
    ggtitle("", subtitle = "ranger") +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
    scale_y_continuous(limits = c(0, 0.15),
                       breaks = seq(0, 0.15, by = 0.05)) +
    ylab("Average |SurvSHAP(t)| value") +
    xlab("time") +
    theme(
        legend.position = "bottom",
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1.5, "lines"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.background = element_rect(
            colour = "grey34",
            fill = "white",
            linetype = "solid",
            linewidth = 0.3
        )
    )
plot_shap_agg


## preprocess results for beeswarm plot ----------------------------------------
# rename results
beeswarm_results <- survshap_global_ranger

# convert shap value data to long format
df_beeswarm <- as.data.frame(do.call(rbind, beeswarm_results$aggregate))
df_beeswarm <- df_beeswarm[, preset_list_all]
df_beeswarm_stacked <- stack(df_beeswarm)

# extract feature values for observations for which shap values were computed
original_values <- as.data.frame(beeswarm_results$variable_values)[, cols]
original_values <- original_values[, preset_list_all]

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

# select interesting numerical features to be plotted
total = c(
    "total_children",
    "total_dead_sons",
    "total_dead_daughters",
    "total_births_5years",
    "total_children"
)

# create dataframe containing only interesting numerical features to be plotted
df_beeswarm_total <- df_beeswarm_plot %>%
    filter(feature %in% total)

# convert feature values to numeric
df_beeswarm_total$value <- as.numeric(df_beeswarm_total$value)

# create dataframe for plotting dpt1_vaccination feature (categorical)
df_beeswarm_dpt1 <-
    subset(df_beeswarm_plot, feature == "dpt1_vaccination")


## create custom beeswarm plot for numerical features  -------------------------
plot_beeswarm_total <-
    ggplot(data = df_beeswarm_total, aes(x = shap_value, y = feature, color = value)) +
    geom_quasirandom() +
    theme_bw() +
    ggtitle("", subtitle = "ranger") +
    xlab("Aggregated SurvSHAP(t) value") +
    ylab("") +
    scale_color_gradientn(colors = c(
        "#fcfdbf",
        "#feb078",
        "#fec9d7",
        "#b73779",
        "#721f81",
        "#2c115f",
        "#000004"
    )) +
    theme(
        legend.position = "right",
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1.5, "lines"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.background = element_rect(
            colour = "grey34",
            fill = "white",
            linetype = "solid",
            size = 0.3
        )
    )
plot_beeswarm_total

## create custom beeswarm plot for dpt1_vaccination feature  -------------------
plot_beeswarm_dpt1 <-
    ggplot(data = df_beeswarm_dpt1, aes(x = shap_value, y = feature, color = value)) +
    scale_color_manual(values = c(
        "#a9def9",
        "#ffa1a1",
        "#ede7b1",
        "#ff99c8",
        "#e4c1f9",
        "#baffc9"
    )) +
    geom_quasirandom() +
    theme_bw() +
    ggtitle("", subtitle = "ranger") +
    xlab("Aggregated SurvSHAP(t) value") +
    ylab("") +
    theme(
        legend.position = "right",
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1.5, "lines"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.background = element_rect(
            colour = "grey34",
            fill = "white",
            linetype = "solid",
            size = 0.3
        )
    )
plot_beeswarm_dpt1


## create plot grid and save plots ---------------------------------------------
# create grid of beeswarm plots
beeswarm_grid <-
    ggarrange(
        plot_beeswarm_total,
        plot_beeswarm_dpt1,
        ncol = 1,
        nrow = 2,
        common.legend = FALSE
    )
beeswarm_grid

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
shap_agg_grid

# save grid of shap plots
ggsave(
    fig("shap_agg_grid.pdf"),
    plot = shap_agg_grid,
    width = 14,
    height = 6,
    device = "pdf"
)

#------------------------------------------------------------------------------#
####        Local Interpretable Model-Agnostic Explanations (LIME)          ####
#------------------------------------------------------------------------------#

## compute lime values ---------------------------------------------------------
# compute lime values for individual 5287
survlime_ranger_5287 <- predict_parts(ranger_explainer2,
                                     individual_5287,
                                     type = "survlime")

# compute lime values for individual 53
survlime_ranger_53 <- predict_parts(ranger_explainer2,
                                      individual_53,
                                      type = "survlime")



### extract relevant lime results for plotting ---------------------------------
## extract relevant lime results for plotting for individual 53 ----------------
# extract relevant local importance values
local_importance_53 <-
    as.numeric(survlime_ranger_53$result) * as.numeric(survlime_ranger_53$variable_values)

# convert relevant local importance values to dataframe
df_lime_53 <- data.frame(
    variable_names = names(survlime_ranger_53$variable_values),
    variable_values = as.numeric(survlime_ranger_53$variable_values),
    beta = as.numeric(survlime_ranger_53$result),
    sign_beta = as.factor(sign(
        as.numeric(survlime_ranger_53$result)
    )),
    sign_local_importance = as.factor(sign(local_importance_53)),
    local_importance = local_importance_53
)

# sort local importance values
df_lime_53 <-
    df_lime_53[head(order(abs(df_lime_53$local_importance), decreasing = TRUE), 12),]

# clean feature names
df_lime_53$variable_names <-
    c(
        "age_head",
        "mother_age",
        "total_dead_daughters",
        "total_children",
        "place_of_delivery.public/government_facility",
        "place_residence.urban",
        "dpt1_vaccination.no",
        "wealth_idx.richer",
        "multiples.secondborn_multiples",
        "total_dead_sons",
        "total_births_5years",
        "sex.male"
    )

# exclude all features with local importance = 0
df_lime_53 <- subset(df_lime_53, local_importance != 0)

# exclude all features with local importance = 0
df_sf_53 <- data.frame(
    times = c(
        survlime_ranger_53$black_box_sf_times,
        survlime_ranger_53$expl_sf_times
    ),
    sfs = c(
        survlime_ranger_53$black_box_sf,
        survlime_ranger_53$expl_sf
    ),
    type = c(
        rep(
            "black box survival function",
            length(survlime_ranger_53$black_box_sf)
        ),
        rep(
            "SurvLIME explanation survival function",
            length(survlime_ranger_53$expl_sf)
        )
    )
)

## extract relevant lime results for plotting for individual 786 ---------------
# extract relevant local importance values
local_importance_5287 <-
    as.numeric(survlime_ranger_5287$result) * as.numeric(survlime_ranger_5287$variable_values)

# convert relevant local importance values to dataframe
df_lime_5287 <- data.frame(
    variable_names = names(survlime_ranger_5287$variable_values),
    variable_values = as.numeric(survlime_ranger_5287$variable_values),
    beta = as.numeric(survlime_ranger_5287$result),
    sign_beta = as.factor(sign(as.numeric(
        survlime_ranger_5287$result
    ))),
    sign_local_importance = as.factor(sign(local_importance_786)),
    local_importance  = local_importance_5287
)

# sort local importance values
df_lime_5287 <-
    df_lime_5287[head(order(abs(df_lime_5287$local_importance), decreasing = TRUE), 12),]

# clean feature names
df_lime_5287$variable_names <-
    c(
        "mother_age",
        "age_head",
        "multiples.single_child",
        "wealth_idx.poorest",
        "total_births_5years",
        "total_children",
        "dpt1_vaccination.vaccination_date_on_card",
        "wealth_idx.poorer",
        "total_dead_sons",
        "total_dead_daughters",
        "sex.male",
        "place_residence.urban"
    )

# exclude all features with local importance = 0
df_lime_5287 <- subset(df_lime_5287, local_importance != 0)

# exclude all features with local importance = 0
df_sf_5287 <- data.frame(
    times = c(
        survlime_ranger_5287$black_box_sf_times,
        survlime_ranger_5287$expl_sf_times
    ),
    sfs = c(
        survlime_ranger_5287$black_box_sf,
        survlime_ranger_5287$expl_sf
    ),
    type = c(
        rep(
            "black box survival function",
            length(survlime_ranger_5287$black_box_sf)
        ),
        rep(
            "SurvLIME explanation survival function",
            length(survlime_ranger_5287$expl_sf)
        )
    )
)


## create custom plots of local importance values  -----------------------------
# create custom plots of local importance values for individual 53
plot_local_53 <-
    ggplot(data = df_lime_53,
           aes(
               x = local_importance,
               y = reorder(variable_names, local_importance, abs),
               fill = sign_local_importance
           )) +
    geom_col() +
    scale_fill_manual("", values = c(
        "-1" = "#cc4778",
        "0" = "#ffffff",
        "1" = "#21918c"
    )) +
    ggtitle("C1: Child dead at t=0", subtitle = "ranger") +
    theme_bw() +
    ylab("") +
    xlab("SurvLIME local importance") +
    theme(
        legend.position = "none",
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
    )
plot_local_53

# create custom plots of local importance values for individual 53
plot_sf_53 <-
    ggplot(data = df_sf_53, aes(
        x = times,
        y = sfs,
        group = type,
        color = type
    )) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    labs(x = "time", y = "survival function value") +
    ggtitle("C1: Child dead at t=0", subtitle = "ranger") +
    scale_color_manual(
        "",
        values = c(
            "black box survival function" = "#440154",
            "SurvLIME explanation survival function" = "#fde725"
        )
    ) +
    theme(
        legend.position = "bottom",
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1.5, "lines"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.background = element_rect(
            colour = "grey34",
            fill = "white",
            linetype = "solid",
            size = 0.3
        )
    )
plot_sf_53

# create custom plots of local importance values for individual 786
plot_local_5287 <-
    ggplot(data = df_lime_5287,
           aes(
               x = local_importance,
               y = reorder(variable_names, local_importance, abs),
               fill = sign_local_importance
           )) +
    geom_col() +
    scale_fill_manual("", values = c(
        "-1" = "#cc4778",
        "0" = "#ffffff",
        "1" = "#21918c"
    )) +
    ggtitle("C1: Child dead at t=0", subtitle = "ranger") +
    theme_bw() +
    ylab("") +
    xlab("SurvLIME local importance") +
    theme(
        legend.position = "none",
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
    )
plot_local_5287

# create custom plots of local importance values for individual 786
plot_sf_5287 <-
    ggplot(data = df_sf_5287, aes(
        x = times,
        y = sfs,
        group = type,
        color = type
    )) +
    geom_line(linewidth = 0.8) +
    theme_bw() +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    labs(x = "time", y = "survival function value") +
    ggtitle("C1: Child dead at t=0", subtitle = "ranger") +
    scale_color_manual(
        "",
        values = c(
            "black box survival function" = "#440154",
            "SurvLIME explanation survival function" = "#fde725"
        )
    ) +
    theme(
        legend.position = "bottom",
        plot.subtitle = element_text(size = 14),
        plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1.5, "lines"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.background = element_rect(
            colour = "grey34",
            fill = "white",
            linetype = "solid",
            size = 0.3
        )
    )
plot_sf_5287


## create plot grid and save plots ---------------------------------------------
# create grid of survival function plots
surv_grid <-
    ggarrange(
        plot_sf_53 ,
        plot_sf_786 ,
        ncol = 2,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
surv_grid

# save grid of survival function plots
ggsave(
    fig("surv_grid.pdf"),
    plot = surv_grid,
    width = 14,
    height = 6,
    device = "pdf"
)

# create grid of local importance plots
lime_grid <-
    ggarrange(
        plot_local_53 ,
        plot_local_5287 ,
        ncol = 2,
        nrow = 1,
        common.legend = FALSE
    ) +
    theme(plot.margin = margin(0.1, 0.1, 0.4, 0.1, "cm"))
lime_grid

# save grid of local importance plots
ggsave(
    fig("lime_grid.pdf"),
    plot = lime_grid,
    width = 14,
    height = 6,
    device = "pdf"
)














