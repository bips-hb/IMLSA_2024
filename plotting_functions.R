## Helper functions for plotting



## Pfi plot --------------------------------------------------------------------
#' Plot permutation feature importance
#'
#' @param df_pfi data.frame - contains pfi values, must contain a time, features and values column.
#' @param model string - name of the machine learning model, by default "coxph".
#' @param color_values vector - vector of colors used for plotting.
#' @param breaks vector - to set ggplot x-axis breaks, by default "seq(0, 5, 1)".
#' @returns A ggplot object.
plot_pfi <- function(df_pfi,
                     model = "coxph",
                     color_values = c("#009E73", "#0072B2", "#CC79A7"),
                     breaks = seq(0, 5, 1)) {
    pfi_plot <- ggplot(df_pfi,
                       aes(
                           x = time,
                           y = values,
                           color = features,
                           linetype = features
                       )) +
        geom_line(linewidth = 0.9) +
        scale_color_manual(values = color_values) +
        scale_linetype_discrete() +
        ggtitle("", subtitle = model) +
        theme_bw() +
        scale_x_continuous(breaks = breaks) +
        ylab(expression(paste("pfi"))) +
        theme(
            legend.position = "bottom",
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
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
    return(pfi_plot)
}



## Ice and pdp plots -----------------------------------------------------------
#' Plot ice and pdp curves in one common plot
#'
#' @param df_ice data.frame - contains ice values, must contain a time, yhat and ids column and one column with the variable name.
#' @param df_pdp data.frame - contains pdp values, must contain a time, yhat  and ids column and one column with the variable name.
#' @param model string - name of the machine learning model, by default "coxph".
#' @param variable unquoted feature argument -  column containing feature values.
#' @param variable_name string - name of feature for which ice and pdp values should be computed, by default "treatment".
#' @param time_var unquoted time argument - column containing the event times
#' @param status_var unquoted status argument - column containing the status indicator
#' @param limits vector - to set ggplot y-axis limits, by default "c(0, 1)".
#' @param breaks_x vector - to set ggplot x-axis breaks, by default "seq(0, 1, by = 0.2)".
#' @param breaks_y vector - to set ggplot y-axis breaks, by default "seq(0, 1, by = 0.2)".
#' @returns A ggplot object.
plot_ice_pdp <- function(df_ice,
                         df_pdp,
                         model = "coxph",
                         variable,
                         variable_name = "treatment",
                         time_var,
                         status_var,
                         limits = c(0, 1),
                         breaks_x = seq(0, 1, by = 0.2),
                         breaks_y = seq(0, 1, by = 0.2)) {
    variable <- enquo(variable)
    time_var <- enquo(time_var)
    status_var <- enquo(status_var)

    pdp_ice_plot <- ggplot() +
        geom_line(
            data = df_ice,
            aes(
                x = time,
                y = yhat,
                group = interaction(ids, !!variable),
                color = !!variable
            ),
            alpha = 0.1
        ) +
        geom_path(
            data = df_pdp,
            aes(
                x = time,
                y = yhat,
                color = !!variable,
                linetype = !!variable,
                group = !!variable
            ),
            linewidth = 1.5,
            lineend = "round",
            linejoin = "round"
        ) +
        scale_color_manual(values = c("#E69F00", "#56B4E9"), name = variable_name) +
        scale_linetype_discrete(name = variable_name) +
        ggtitle("", subtitle = model) +
        theme_bw() +
        new_scale_color() +
        geom_rug(
            data = test_dat,
            aes(
                x = !!time_var,
                y = max(df_ice$yhat),
                color = factor(!!status_var)
            ),
            sides = "b",
            alpha = 0.8,
            position = position_jitter(width = 0.01 * 1)
        ) +
        scale_color_manual(values = c("#BE0032", "#999999"),
                           guide = NULL) +
        scale_x_continuous(breaks = breaks_x) +
        scale_y_continuous(
            limits = limits,
            breaks = breaks_y,
            labels = scales::label_number(accuracy = 0.01)
        ) +
        ylab("prediction") +
        theme(
            legend.position = "bottom",
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                linewidth = 0.3
            )
        )

    return(pdp_ice_plot)
}



## Ice plots -------------------------------------------------------------------
#' Plot ice curves
#'
#' @param df_ice data.frame - contains ice values, must contain a time, yhat and ids column and one column with the variable name.
#' @param model string - name of the machine learning model, by default "coxph".
#' @param limits_y vector - to set ggplot y-axis limits, by default "c(0, 1)".
#' @param breaks_y vector - to set ggplot y-axis breaks, by default "seq(0, 1, by = 0.2)".
#' @param breaks_x vector - to set ggplot x-axis breaks, by default "seq(15, 50, 5)".
#' @returns A ggplot object.
plot_ice <- function(df_ice,
                     model = "coxph",
                     limits_y = c(0, 1),
                     breaks_y = seq(0, 1, by = 0.2),
                     breaks_x = seq(15, 50, 5)) {
    ice_plot <- ggplot() +
        geom_path(
            data = df_ice,
            aes(
                x = time,
                y = yhat,
                color = treatment,
                group = interaction(ids, treatment)
            ),
            linewidth = 0.2,
            lineend = "round",
            linejoin = "round"
        ) +
        ggtitle("", subtitle = model) +
        scale_color_manual(values = c("#E69F00", "#56B4E9"), name = "treatment") +
        theme_bw() +
        new_scale_color() +
        geom_rug(
            data = test_dat,
            aes(
                x = eventtime,
                y = max(df_ice$yhat),
                color = factor(status)
            ),
            sides = "b",
            alpha = 0.8,
            position = position_jitter(width = 0.01 * 1)
        ) +
        scale_color_manual(values = c("#BE0032", "#999999"),
                           guide = NULL) +
        scale_x_continuous(breaks = breaks_x) +
        scale_y_continuous(
            limits = limits_y,
            breaks = breaks_y,
            labels = scales::label_number(accuracy = 0.01)
        ) +
        ylab("prediction") +
        theme(
            legend.position = "bottom",
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                linewidth = 0.3
            )
        )
    return(ice_plot)
}



## Ale/pdp plots ---------------------------------------------------------------
#' Plot ale or pdp curves
#'
#' @param df_ale_pdp data.frame - contains ale or pdp values, must contain a time, value and prediction column.
#' @param model string - name of the machine learning model, by default "ranger".
#' @param x_label string - name of the x-axis label, by default "x1".
#' @param limits vector - to set ggplot y-axis limits, by default "c(0, 1)".
#' @param breaks_x vector - to set ggplot x-axis breaks, by default "seq(0, 5, by = 1)".
#' @param breaks_y vector - to set ggplot y-axis breaks, by default "seq(0, 1, by = 0.2)".
#' @param key_width numeric - to control the width of the keys in the legend of a plot, by default 2.
#' @param key_spacing numeric - to control the spacing between the keys in the legend, by default 0.5.
#' @returns A ggplot object.
plot_ale_pdp <- function(df_ale_pdp,
                         model = "ranger",
                         x_label = "x1",
                         limits = c(0, 1),
                         breaks_x = seq(0, 5, by = 1),
                         breaks_y = seq(0, 1, by = 0.2),
                         key_width = 2,
                         key_spacing = 0.5) {
    ale_pdp_plot <- ggplot() +
        geom_path(
            data = df_ale_pdp,
            aes(
                x = value,
                y = prediction,
                color = time,
                group = time
            ),
            linewidth = 0.8
        ) +
        scale_color_viridis_c() +
        ggtitle("", subtitle = model) +
        theme_bw() +
        scale_x_continuous(breaks = breaks_x) +
        scale_y_continuous(
            limits = limits,
            breaks = breaks_y,
            labels = scales::label_number(accuracy = 0.01)
        ) +
        ylab("prediction") +
        xlab(x_label) +
        theme(
            legend.position = "bottom",
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            legend.key.width = unit(key_width, "cm"),
            legend.spacing.x = unit(key_spacing, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                linewidth = 0.7
            )
        )

    return(ale_pdp_plot)
}



## individual shap plots -------------------------------------------------------
#' Plot SurvSHAP(t) curves for individual observations
#'
#' @param df_shap data.frame - contains SurvSHAP(t) values, must contain a time, value and ind column.
#' @param limits vector - to set ggplot y-axis limits, by default "c(-0.3, 0.1)".
#' @param breaks vector - to set ggplot y-axis breaks, by default "seq(-0.3, 0.1, by = 0.1)".
#' @param title string - plot title, by default "P1: Patient dead at t = 471".
#' @returns A ggplot object.
plot_shap_ind <- function(df_shap,
                          limits = c(-0.3, 0.1),
                          breaks = seq(-0.3, 0.1, by = 0.1),
                          title = "P1: Patient dead at t = 471") {
    shap_ind_plot <- ggplot() +
        geom_path(
            data = df_shap,
            aes(
                x = times,
                y = values,
                color = ind,
                linetype = ind,
                group = ind
            ),
            linewidth = 0.8
        ) +
        scale_linetype_discrete(name = "feature") +
        scale_color_manual(
            "feature",
            values = c(
                "#000000",
                "#E69F00",
                "#56B4E9",
                "#009E73",
                "#F0E442",
                "#0072B2",
                "#D55E00",
                "#CC79A7"
            ),
            name = "feature"
        ) +
        theme_bw() +
        ggtitle(title, subtitle = "ranger") +
        scale_x_continuous(breaks = seq(0, 2600, by = 500)) +
        scale_y_continuous(
            limits = limits,
            breaks = breaks,
            labels = scales::label_number(accuracy = 0.01)
        ) +
        ylab("SurvSHAP(t) value") +
        xlab("time") +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                size = 0.3
            )
        )

    return(shap_ind_plot)
}



## Aggregated shap line plot ---------------------------------------------------
#' Plot aggregated SurvSHAP(t) over all observations
#'
#' @param df_shap data.frame - contains aggregated SurvSHAP(t) values, must contain a times, values and ind column.
#' @returns A ggplot object.
plot_shap_agg_line <- function(df_shap) {
    shap_plot <- ggplot() +
        geom_path(
            data = df_agg_long,
            aes(
                x = times,
                y = values,
                color = ind,
                group = ind,
                linetype = ind
            ),
            linewidth = 0.8
        ) +
        scale_linetype_discrete(name = "feature") +
        scale_color_manual(
            "feature",
            values = c(
                "#000000",
                "#E69F00",
                "#56B4E9",
                "#009E73",
                "#F0E442",
                "#0072B2",
                "#D55E00",
                "#CC79A7"
            ),
            name = "feature"
        ) +
        theme_bw() +
        ggtitle("", subtitle = "ranger") +
        scale_x_continuous(breaks = seq(0, 2600, by = 500)) +
        scale_y_continuous(limits = c(0, 0.1),
                           breaks = seq(0, 0.1, by = 0.05)) +
        ylab("Average |SurvSHAP(t)| value") +
        xlab("time") +
        theme(
            legend.position = "bottom",
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                linewidth = 0.3
            )
        )

    return(shap_plot)
}



## Shap beeswarm plot for continuous feature -----------------------------------
#' Plot beeswarm plots of SurvSHAP(t) values for continuous feature
#'
#' @param df_beeswarm data.frame - contains SurvSHAP(t) values of continuous feature, must contain a shap_value, value and feature column.
#' @param x-lab string - x-lab title, by default "Aggregated SurvSHAP(t) value".
#' @param subtitle string - plot subtitle, by default "ranger".
#' @returns A ggplot object.
plot_shap_bee_cont <- function(df_beeswarm,
                               xlab = "Aggregated SurvSHAP(t) value",
                               subtitle = "ranger") {
    shap_plot <- ggplot(data = df_beeswarm, aes(x = shap_value, y = feature, color = value)) +
        geom_quasirandom(orientation = 'y') +
        theme_bw() +
        ggtitle("", subtitle = subtitle) +
        xlab(xlab) +
        ylab("") +
        scale_color_viridis_c() +
        theme(
            legend.position = "right",
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                size = 0.3
            )
        )

    return(shap_plot)
}


## shap beeswarm plot for categorical feature ----------------------------------
#' Plot beeswarm plots of SurvSHAP(t) values for categorical feature
#'
#' @param df_beeswarm data.frame - contains SurvSHAP(t) values of categorical feature, must contain a shap_value, value and feature column.
#' @param x-lab string - x-lab title, by default "Aggregated SurvSHAP(t) value".
#' @param subtitle string - plot subtitle, by default "ranger".
#' @returns A ggplot object.
plot_shap_bee_cat <- function(df_beeswarm,
                              xlab = "Aggregated SurvSHAP(t) value",
                              subtitle = "ranger") {
    shap_plot <- ggplot(data = df_beeswarm, aes(x = shap_value, y = feature, color = value)) +
        scale_color_viridis_d() +
        geom_quasirandom(orientation = 'y') +
        theme_bw() +
        ggtitle("", subtitle = subtitle) +
        xlab(xlab) +
        ylab("") +
        theme(
            legend.position = "right",
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                size = 0.3
            )
        )

    return(shap_plot)
}



## Survival function plot ------------------------------------------------------
#' Plot survival curves
#'
#' @param df_sf data.frame - contains values to plot survival curves, must contain times, sfs and type column
#' @param title string - plot title, by default "P1: Patient dead at t = 471".
#' @returns A ggplot object.
plot_sf <- function(df_sf, title = "P1: Patient dead at t = 471") {
    sf_plot <- ggplot(data = df_sf, aes(
        x = times,
        y = sfs,
        group = type,
        color = type
    )) +
        geom_line(linewidth = 0.8) +
        theme_bw() +
        scale_x_continuous(breaks = seq(0, 2600, by = 500)) +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
        labs(x = "time", y = "survival function value") +
        ggtitle(title, subtitle = "ranger") +
        scale_color_manual(
            "",
            values = c(
                "black box survival function" = "#440154",
                "SurvLIME explanation survival function" = "#fde725"
            )
        ) +
        theme(
            legend.position = "bottom",
            plot.subtitle = element_text(size = 20),
            plot.title = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.key.size = unit(1.5, "lines"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18),
            legend.background = element_rect(
                colour = "grey34",
                fill = "white",
                linetype = "solid",
                size = 0.3
            )
        )

    return(sf_plot)
}


## Lime plot -------------------------------------------------------------------
#' Plot SurvLIME importance values
#'
#' @param df_lime data.frame - contains values to plot SurvLIME importance values, must contain local_importance, variable_names, abs and sign_local_importance column
#' @param title string - plot title, by default "P1: Patient dead at t = 471".
#' @param accuracy numeric - to control the rounding precision of the labels.
#' @returns A ggplot object.
plot_lime <- function(df_lime,
                      title = "P1: Patient dead at t = 471",
                      accuracy = 0.0000001) {
    lime_plot <- ggplot(data = df_lime,
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
        ggtitle(title, subtitle = "ranger") +
        theme_bw() +
        ylab("") +
        xlab("SurvLIME local importance") +
        scale_x_continuous(labels = scales::label_number(accuracy = accuracy)) +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20)
        )

    return(lime_plot)
}

# feature interaction plot
#' Plot SurvLIME importance values
#'
#' @param df_Hjk data.frame - contains values to plot h-statistics values, must contain time, H and feature column
#' @param title string - plot title, by default "x1".
#' @param subtitle string - plot subtitle, by default "ranger".
#' @param color_values vector - vector of colors used for plotting.
#' @param limits_y vector - to set ggplot y-axis limits, by default "c(0, 1)".
#' @param breaks_y vector - to set ggplot y-axis breaks, by default "seq(0, 1, by = 0.25)".
#' @param breaks_x vector - to set ggplot x-axis breaks, by default "seq(15, 20, 5)".
#' @returns A ggplot object.
plot_f_inter <- function(df_Hjk,
                         title = "x1",
                         subtitle = "ranger",
                         color_values = c("#E69F00", "#56B4E9"),
                         limits_y = c(0, 1),
                         breaks_y = seq(0, 1, by = 0.25),
                         breaks_x = seq(0, 20, by = 5)) {
    fi_plot <- ggplot(data = df_Hjk,
                      aes(
                          x = time,
                          y = H,
                          color = feature,
                          group = feature,
                          linetype = feature
                      )) +
        geom_line(linewidth = 0.8) +
        scale_color_manual(values = color_values, name = "feature") +
        scale_linetype_discrete(name = "feature") +
        ggtitle(title, subtitle = subtitle) +
        theme_bw() +
        scale_y_continuous(limits = limits_y, breaks = breaks_y) +
        scale_x_continuous(breaks = breaks_x) +
        ylab(expression(paste("H-statistic value"))) +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 20),
            plot.margin = margin(0.7, 0.7, 0.7, 0.7, "cm"),
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

    return(fi_plot)
}


