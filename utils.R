## helper functions



## Shap aggregation ------------------------------------------------------------
# this code is taken from the R survex package
aggregate_shap_multiple_observations <-
    function(shap_res_list,
             feature_names,
             aggregation_function) {
        if (length(shap_res_list) > 1) {
            shap_res_list <- lapply(shap_res_list, function(x) {
                x$rn <- rownames(x)
                x
            })

            full_survshap_results <- do.call("rbind", shap_res_list)
            rownames(full_survshap_results) <- NULL

            # compute arithmetic mean for each time-point and feature across
            # multiple observations

            tmp_res <-
                aggregate(
                    full_survshap_results[, !colnames(full_survshap_results) %in% c("rn")],
                    by = list(full_survshap_results$rn),
                    FUN = aggregation_function
                )
            rownames(tmp_res) <- tmp_res$Group.1
            ordering <-
                order(as.numeric(substring(rownames(tmp_res), 3)))

            tmp_res <-
                tmp_res[ordering, !colnames(tmp_res) %in% c("rn", "Group.1")]
        } else {
            # no aggregation required
            tmp_res <- shap_res_list[[1]]
        }
        shap_values <- tmp_res
        # transform to data.frame to make everything compatible with
        # previous code
        return(shap_values)
    }



## Feature interaction ---------------------------------------------------------
#' Compute Friedman's H_jk statistic
#'
#' @param explainer an explainer object - model preprocessed by the `survex::explain()` function.
#' @param feature string - name of the feature for which h-statistic should be computed
#' @param N numeric - number of observations that should be sampled for calculation. If `NULL` then variable importance will be calculated on the whole dataset.
#' @returns A data.frame containing H_jk statistics value ready for plotting.
feature_interaction <- function(explainer, feature, categorical_variables = NULL, N = NULL) {
    # create list of features
    feature_list <- colnames(explainer$data)
    # remove feature of interest
    feature_list_nf <- feature_list[feature_list != feature]
    # create tuple list for 2d pdp computation c(feature of interest, other feature)
    tuples_list <-
        lapply(feature_list_nf, create_tuple <-
                   function(f) {
                       c(feature, f)
                   })
    # compute pd of all features
    pdp_results <- model_profile(explainer,
                                 variables = feature_list,
                                 center = TRUE,
                                 N = NULL)
    # extract results
    df_results <- pdp_results$result
    # rename columns
    names(df_results)[names(df_results) == "_vname_"] = "feature"
    names(df_results)[names(df_results) == "_times_"] = "time"
    names(df_results)[names(df_results) == "_yhat_"] = "yhat_nf"
    names(df_results)[names(df_results) == "_x_"] = "feature_value"
    # remove unnecessary columns
    df_results <-
        df_results[, c("feature", "feature_value", "time", "yhat_nf")]
    # create separate dataframes for feature of interest and other features
    feature_of_interest = feature
    df_pdp_f <- subset(df_results, feature == feature_of_interest)
    df_pdp_nf <- df_results[!(df_results$feature == feature), ]
    # rename columns
    names(df_pdp_f)[names(df_pdp_f) == "yhat_nf"] <- "yhat_f"
    names(df_pdp_f)[names(df_pdp_f) == "feature"] <-
        "feature_name_v1"
    names(df_pdp_f)[names(df_pdp_f) == "feature_value"] <-
        "feature_value_v1"
    names(df_pdp_nf)[names(df_pdp_nf) == "feature_value"] <-
        "feature_value_v2"
    # compute 2d pd for feature tuples
    data <- explainer$data
    if (!is.null(N) && N < nrow(data)) {
        ndata <- data[sample(1:nrow(data), N), ]
    } else {
        ndata <- data
    }
    df_results_2d <- surv_pdp_2d(explainer,
                                  data = ndata,
                                  variables = tuples_list,
                                  categorical_variables = categorical_variables,
                                  grid_points = 25,
                                  variable_splits_type = "uniform",
                                  center = TRUE,
                                  output_type = "survival")
    # rename columns
    names(df_results_2d)[names(df_results_2d) == "_times_"] = "time"
    names(df_results_2d)[names(df_results_2d) == "_yhat_"] = "yhat_2d"
    names(df_results_2d)[names(df_results_2d) == "_v1value_"] = "feature_value_v1"
    names(df_results_2d)[names(df_results_2d) == "_v2value_"] = "feature_value_v2"
    names(df_results_2d)[names(df_results_2d) == "_v1name_"] = "feature_name_v1"
    names(df_results_2d)[names(df_results_2d) == "_v2name_"] = "feature"
    # delete unnecessary columns
    df_results_2d <-
        df_results_2d[, c(
            "feature",
            "feature_name_v1",
            "feature_value_v1",
            "feature_value_v2",
            "time",
            "yhat_2d"
        )]
    # merge 2d pd values with pd values of other features
    df_merge1 <-
        merge(
            x = df_results_2d,
            y = df_pdp_nf,
            by = c("time", "feature", "feature_value_v2")
        )
    # merge with pd features of feature of interest
    df_merge2 <-
        merge(
            x = df_merge1,
            y = df_pdp_f,
            by = c("time", "feature_name_v1", "feature_value_v1")
        )
    # mean center pd values
    df_merge2$yhat_2d <- df_merge2$yhat_2d - mean(df_merge2$yhat_2d)
    df_merge2$yhat_f <- df_merge2$yhat_f - mean(df_merge2$yhat_f)
    df_merge2$yhat_nf <- df_merge2$yhat_nf - mean(df_merge2$yhat_nf)
    # convert to datatable
    dt_merge <- as.data.table(df_merge2)
    # compute Hjk statistics
    df_Hjk <-
        dt_merge[, .(H = sum((yhat_2d - yhat_f - yhat_nf) ^ 2) / sum(yhat_2d ^
                                                                         2)), by = .(feature, time)]
    # return results
    return(df_Hjk)
}



surv_pdp_2d <- function(x,
                        data,
                        variables,
                        categorical_variables,
                        grid_points,
                        variable_splits_type,
                        center,
                        output_type) {
    model <- x$model
    label <- x$label
    if (output_type == "survival"){
        predict_survival_function <- x$predict_survival_function
    } else {
        predict_survival_function <- x$predict_cumulative_hazard_function
    }
    times <- x$times

    unique_variables <- unlist(variables)
    variable_splits <- calculate_variable_split(data,
                                                variables = unique_variables,
                                                categorical_variables = categorical_variables,
                                                grid_points = grid_points,
                                                variable_splits_type = variable_splits_type
    )

    profiles <- lapply(variables, FUN = function(variables_pair) {
        var1 <- variables_pair[1]
        var2 <- variables_pair[2]
        expanded_data <- merge(variable_splits[[var1]], data[, !colnames(data) %in% variables_pair])
        names(expanded_data)[colnames(expanded_data) == "x"] <- var1
        remaining_cols <- colnames(data)[!colnames(data) %in% variables_pair]
        names(expanded_data)[names(expanded_data) != var1] <- remaining_cols
        expanded_data <- merge(variable_splits[[var2]], expanded_data)
        names(expanded_data)[colnames(expanded_data) == "x"] <- var2
        expanded_data <- expanded_data[, colnames(data)]


        predictions_original <- predict_survival_function(
            model = model,
            newdata = data,
            times = times
        )
        mean_pred <- colMeans(predictions_original)

        predictions <- predict_survival_function(
            model = model,
            newdata = expanded_data,
            times = times
        )

        preds <- c(t(predictions))
        if (center) {
            preds <- preds - mean_pred
        }

        res <- data.frame(
            "_v1name_" = var1,
            "_v2name_" = var2,
            "_v1type_" = ifelse(var1 %in% categorical_variables, "categorical", "numerical"),
            "_v2type_" = ifelse(var2 %in% categorical_variables, "categorical", "numerical"),
            "_v1value_" = as.character(rep(expanded_data[, var1], each = length(times))),
            "_v2value_" = as.character(rep(expanded_data[, var2], each = length(times))),
            "_times_" = rep(times, nrow(expanded_data)),
            "_yhat_" = preds,
            "_label_" = label,
            check.names = FALSE
        )
        return(aggregate(`_yhat_` ~ ., data = res, FUN = mean))
    })

    profiles <- do.call(rbind, profiles)
    profiles
}


calculate_variable_split <- function(data, variables = colnames(data), categorical_variables = NULL, grid_points = 101, variable_splits_type = "quantiles", new_observation = NA) {
    variable_splits <- lapply(variables, function(var) {
        selected_column <- na.omit(data[, var])

        if (!(var %in% categorical_variables)) {
            probs <- seq(0, 1, length.out = grid_points)
            if (variable_splits_type == "quantiles") {
                selected_splits <- unique(quantile(selected_column, probs = probs))
            } else {
                selected_splits <- seq(min(selected_column, na.rm = TRUE), max(selected_column, na.rm = TRUE), length.out = grid_points)
            }
            if (!any(is.na(new_observation))) {
                selected_splits <- sort(unique(c(selected_splits, na.omit(new_observation[, var]))))
            }
        } else {
            if (any(is.na(new_observation))) {
                selected_splits <- sort(unique(selected_column))
            } else {
                selected_splits <- sort(unique(rbind(
                    data[, var, drop = FALSE],
                    new_observation[, var, drop = FALSE]
                )[, 1]))
            }
        }
        selected_splits
    })
    names(variable_splits) <- variables
    variable_splits
}

