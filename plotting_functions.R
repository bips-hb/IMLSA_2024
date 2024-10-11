## Helper functions for plotting



## Pfi plot --------------------------------------------------------------------
plot_fi <- function(df_pfi,
                    model = "coxph",
                    color_values = c("#009E73", "#0072B2", "#CC79A7"),
                    breaks = c(seq(0, 5, 1))) {
    fi_plot <- ggplot(df_pfi,
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
    return(fi_plot)
}



## Ice and pdp plots -----------------------------------------------------------
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
plot_lime <- function(df_lime,
                      title = "P1: Patient dead at t = 471",
                      accuracy = accuracy) {
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


