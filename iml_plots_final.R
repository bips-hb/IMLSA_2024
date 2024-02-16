## setup -----------------------------------------------------------------------
# packages
library(survex)
library(ggplot2)



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
  data = df_ghana_encoded[test_indices,-c(1, 2, 3)],
  y = Surv(df_ghana_encoded[test_indices, ]$survivaltime,
           df_ghana_encoded[test_indices, ]$status),
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

# extract relevant ice results for plotting
df_ice_deepsurv <-
  pdp_ice_deepsurv$cp_profiles$result[(pdp_ice_deepsurv$cp_profiles$result$`_vname_` == "mother_age")  &
                                        (pdp_ice_deepsurv$cp_profiles$result$`_times_` %in% times), ]

# rename columns
names(df_ice_deepsurv)[names(df_ice_deepsurv) == "_times_"] = "time"
names(df_ice_deepsurv)[names(df_ice_deepsurv) == "_yhat_"] = "yhat"
names(df_ice_deepsurv)[names(df_ice_deepsurv) == "_ids_"] = "ids"

# drop irrelevant columns
df_ice_deepsurv <-
  df_ice_deepsurv[, c("mother_age", "time", "yhat", "ids")]

# select reference value for centering
df_ice_deepsurv_center <-
  df_ice_deepsurv[df_ice_deepsurv[, "mother_age"] == 15,]

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

# extract relevant pdp results for plotting
#df_pdp_deepsurv <-
#  pdp_ice_deepsurv$result[(pdp_ice_deepsurv$result$`_vname_` == "mother_age") &
#                            (pdp_ice_deepsurv$result$`_times_` %in% times), c("_x_", "_yhat_")]
# rename columns
#names(df_pdp_deepsurv)[names(df_pdp_deepsurv) == "_x_"] = "mother_age"
#names(df_pdp_deepsurv)[names(df_pdp_deepsurv) == "_yhat_"] = "yhat"
# add time column
#df_pdp_deepsurv["time"] = rep(0:59, each = length(unique(df_pdp_deepsurv$mother_age)))

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
    data = df_ghana[unique(ice_df_plot$ids), ],
    aes(x = mother_age, y = max(df_ice_deepsurv_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(
      df_pdp_deepsurv_center[, "mother_age"]
    )))
  ) +
  scale_color_viridis_c() +
  theme_bw() +
  ggtitle("", subtitle = "deepsurv") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.9, 0.25),
                     breaks = seq(-0.9, 0.25, by = 0.2)) +
  ylab(expression(paste("prediction"))) +
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
    data = df_ghana[unique(df_ice_deepsurv_plot$ids), ],
    aes(x = mother_age, y = max(df_ice_deepsurv_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(
      df_pdp_deepsurv_center[, "mother_age"]
    )))
  ) +
  scale_color_viridis_c() +
  theme_bw() +
  ggtitle("", subtitle = "deepsurv") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.9, 0.25),
                     breaks = seq(-0.9, 0.25, by = 0.2)) +
  ylab(expression(paste("prediction"))) +
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

# drop irrelevant columns
df_ice_ranger <-
  df_ice_ranger[, c("mother_age", "time", "yhat", "ids")]

# select reference value for censoring
df_ice_ranger_center <-
  df_ice_ranger[df_ice_ranger[, "mother_age"] == 15, ]

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

#pdp_df <- pdp_ice_ranger$result[(pdp_ice_ranger$result$`_vname_` == "mother_age") &
#                                  (pdp_ice_ranger$result$`_times_` %in% times), c("_x_", "_yhat_")]

#names(pdp_df)[names(pdp_df) == "_x_"] = "mother_age"
#names(pdp_df)[names(pdp_df) == "_yhat_"] = "yhat"
#pdp_df["time"] = rep(0:59, each = length(unique(pdp_df$mother_age)))

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
    data = df_ghana[unique(df_ice_ranger_plot$ids), ],
    aes(x = mother_age, y = max(df_ice_ranger_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(df_pdp_ranger_center[, "mother_age"])))
  ) +
  scale_color_viridis_c() +
  theme_bw() +
  ggtitle("", subtitle = "ranger") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.25, 0.05),
                     breaks = seq(-0.25, 0.05, by = 0.05)) +
  ylab(expression(paste("prediction"))) +
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
    data = df_ghana[unique(df_ice_ranger_plot$ids), ],
    aes(x = mother_age, y = max(df_ice_ranger_merge$yhat)),
    sides = "b",
    alpha = 0.8,
    color = "palevioletred",
    position = position_jitter(width = 0.01 *  diff(range(df_pdp_ranger_center[, "mother_age"])))
  ) +
  scale_color_viridis_c() +
  theme_bw() +
  ggtitle("", subtitle = "ranger") +
  theme_bw() +
  scale_x_continuous(breaks = seq(15, 50, 5)) +
  scale_y_continuous(limits = c(-0.25, 0.05),
                     breaks = seq(-0.25, 0.05, by = 0.05)) +
  ylab(expression(paste("prediction"))) +
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
  theme_bw() +
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
  ylab(expression(paste("prediction"))) +
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
  data = df_ghana[train_indices, ],
  num.trees = 967,
  mtry = 6,
  max.depth = 6,
  min.node.size = 36,
  sample.fraction = 0.8475985
)

# set up new explainer using ranger model from ranger package
ranger_explainer2 <- explain(
  ranger_rsf,
  data = df_ghana[test_indices,-c(1, 2, 3)],
  y = Surv(df_ghana[test_indices, ]$survivaltime,
           df_ghana[test_indices, ]$status)
)

## select interesting instances ------------------------------------------------
# child dead at t = 24
individual_786 <- df_ghana[786, -c(1, 2, 3)]
# child dead at t = 0
individual_1121 <- df_ghana[1121, -c(1, 2, 3)]

## compute shap values ---------------------------------------------------------
# compute shap values for individual 786
survshap_ranger_786 <- predict_parts(
  ranger_explainer2,
  individual_786,
  type = 'survshap',
  N = 500,
  calculation_method = c("kernelshap"),
  maxvar = 12
)

# compute shap values for individual 1121
survshap_ranger_1121 <- predict_parts(
  ranger_explainer2,
  individual_1121,
  type = 'survshap',
  N = 500,
  calculation_method = c("kernelshap"),
  maxvar = 12
)

### extract relevant shap results for plotting ---------------------------------
## extract relevant shap results for plotting for indiviudal 786 ---------------
# convert results to list
dfl_shap_786 <- c(list(survshap_ranger_786))

# convert results to dataframe
df_shap_786 <- lapply(dfl_shap_786, function(x) {
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
dfl_shap_1121 <- c(list(survshap_ranger_1121))

# convert results to list
df_shap_1121 <- lapply(dfl_shap_1121, function(x) {
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
df_shap_1121 <- do.call(rbind, df_shap_1121)

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

# reorder the rows based on the preset list for individual 786
df_shap_786$ind <- factor(df_shap_786$ind, levels = preset_list_all)
df_shap_786 <- df_shap_786[order(df_shap_1121$ind),]

# reorder the rows based on the preset list for individual 1121
df_shap_1121$ind <-
  factor(df_shap_1121$ind, levels = preset_list_all)
df_shap_1121 <- df_shap_1121[order(df_shap_1121$ind),]

## create custom plots of shap values ------------------------------------------
# create custom plots of shap values for observation 786
plot_shap_786 <- ggplot() +
  geom_path(data = df_shap_786,
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
p_shap_786

# create custom plots of shap values for observation 1121
plot_shap_1121 <- ggplot() +
  geom_path(data = df_shap_1121,
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
plot_shap_1121

## create plot grid and save plots
# create grid of ale plots
shap_grid <-
  ggarrange(
    plot_shap_1121,
    plot_shap_786,
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


       plot = ice_grid, width = 14, height = 6, device = "pdf") 