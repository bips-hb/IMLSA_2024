## helper functions



## SHAP Aggregation ------------------------------------------------------------
#' Aggregate SHAP results across multiple observations
#'
#' This helper function aggregates SHAP values from multiple observations
#' (e.g., multiple individuals) into a single data frame. This is typically
#' done by taking the arithmetic mean (or another user-specified aggregation
#' function) across observations for each feature and time point.
#'
#' @param shap_res_list list - a list of SHAP results, where each element
#'   is a data.frame containing SHAP values for one observation.
#' @param feature_names character vector - names of the features for which
#'   SHAP values are provided.
#' @param aggregation_function function - function used to aggregate SHAP
#'   values across observations (e.g. `mean`, `median`).
#'
#' @returns A data.frame with aggregated SHAP values per feature and time point.
#'   If only a single observation is passed, the result is returned unchanged.
aggregate_shap_multiple_observations <-
    function(shap_res_list,
             feature_names,
             aggregation_function) {
        if (length(shap_res_list) > 1) {
            # Add rownames as a column to preserve ordering after rbind
            shap_res_list <- lapply(shap_res_list, function(x) {
                x$rn <- rownames(x)
                x
            })

            # Combine all SHAP result data.frames into one
            full_survshap_results <- do.call("rbind", shap_res_list)
            rownames(full_survshap_results) <- NULL

            # Aggregate SHAP values across multiple observations
            # by feature/timepoint using the specified function
            tmp_res <-
                aggregate(
                    full_survshap_results[, !colnames(full_survshap_results) %in% c("rn")],
                    by = list(full_survshap_results$rn),
                    FUN = aggregation_function
                )

            # Restore rownames and ensure correct ordering (numeric)
            rownames(tmp_res) <- tmp_res$Group.1
            ordering <- order(as.numeric(substring(rownames(tmp_res), 3)))

            # Reorder and drop helper columns
            tmp_res <- tmp_res[ordering, !colnames(tmp_res) %in% c("rn", "Group.1")]
        } else {
            # Only one observation → no aggregation needed
            tmp_res <- shap_res_list[[1]]
        }

        # Return as data.frame to keep compatibility with plotting functions
        shap_values <- tmp_res
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



## 2D Partial Dependence for Survival Models -----------------------------------
#' Compute two-dimensional partial dependence profiles for survival models.
#'
#' This function generates pairwise partial dependence profiles (2D PDPs) for
#' a set of variable pairs in a survival model. It can output survival or
#' cumulative hazard predictions, optionally mean-centered.
#'
#' @param x explainer object created by `survex::explain()`, containing model,
#'   prediction functions, and survival times.
#' @param data data.frame - input dataset used for generating profiles.
#' @param variables list - list of variable pairs (tuples) for which to compute 2D PDPs.
#' @param categorical_variables character vector - names of categorical variables.
#' @param grid_points numeric - number of grid points for numeric variables.
#' @param variable_splits_type string - either `"quantiles"` or `"uniform"`, controls
#'   how grid points are selected for numeric variables.
#' @param center logical - if `TRUE`, predictions are mean-centered across the dataset.
#' @param output_type string - `"survival"` (default) or `"cumhaz"` for type of prediction.
#'
#' @returns A data.frame containing the pairwise PDP results with columns for
#'   variable names, grid values, times, and predicted outcomes.
surv_pdp_2d <- function(x,
                        data,
                        variables,
                        categorical_variables,
                        grid_points,
                        variable_splits_type,
                        center,
                        output_type) {
    # extract model and prediction function
    model <- x$model
    label <- x$label
    if (output_type == "survival"){
        predict_survival_function <- x$predict_survival_function
    } else {
        predict_survival_function <- x$predict_cumulative_hazard_function
    }
    times <- x$times

    # compute variable-specific grid splits (quantile- or uniform-based)
    unique_variables <- unlist(variables)
    variable_splits <- calculate_variable_split(
        data,
        variables = unique_variables,
        categorical_variables = categorical_variables,
        grid_points = grid_points,
        variable_splits_type = variable_splits_type
    )

    # compute profiles for each pair of variables
    profiles <- lapply(variables, FUN = function(variables_pair) {
        var1 <- variables_pair[1]
        var2 <- variables_pair[2]

        # expand data to cover grid for var1 and var2
        expanded_data <- merge(variable_splits[[var1]], data[, !colnames(data) %in% variables_pair])
        names(expanded_data)[colnames(expanded_data) == "x"] <- var1
        remaining_cols <- colnames(data)[!colnames(data) %in% variables_pair]
        names(expanded_data)[names(expanded_data) != var1] <- remaining_cols
        expanded_data <- merge(variable_splits[[var2]], expanded_data)
        names(expanded_data)[colnames(expanded_data) == "x"] <- var2
        expanded_data <- expanded_data[, colnames(data)]

        # baseline predictions for centering
        predictions_original <- predict_survival_function(
            model = model,
            newdata = data,
            times = times
        )
        mean_pred <- colMeans(predictions_original)

        # predictions on expanded grid
        predictions <- predict_survival_function(
            model = model,
            newdata = expanded_data,
            times = times
        )

        # flatten and optionally mean-center predictions
        preds <- c(t(predictions))
        if (center) {
            preds <- preds - mean_pred
        }

        # build results data.frame
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

        # average predictions in case of duplicates (categorical grids)
        return(aggregate(`_yhat_` ~ ., data = res, FUN = mean))
    })

    # combine all profiles into single data.frame
    profiles <- do.call(rbind, profiles)
    profiles
}



## Variable Split Calculation --------------------------------------------------
#' Calculate grid splits for variables for partial dependence computation.
#'
#' This helper function generates grid points for numeric and categorical
#' variables, used for creating partial dependence profiles (1D or 2D).
#' For numeric variables, grid points can be chosen based on quantiles or
#' uniformly spaced values. For categorical variables, all unique values
#' (or values observed in `new_observation`) are used.
#'
#' @param data data.frame - dataset from which to calculate variable splits.
#' @param variables character vector - names of variables to calculate splits for.
#' @param categorical_variables character vector - names of categorical variables.
#' @param grid_points numeric - number of grid points to generate for numeric variables.
#' @param variable_splits_type string - `"quantiles"` (default) or `"uniform"`, defines
#'   how numeric splits are selected.
#' @param new_observation data.frame (optional) - observation(s) whose values should
#'   be included in the grid to guarantee coverage.
#'
#' @returns A named list where each element contains the grid values for a variable.
calculate_variable_split <- function(data,
                                     variables = colnames(data),
                                     categorical_variables = NULL,
                                     grid_points = 101,
                                     variable_splits_type = "quantiles",
                                     new_observation = NA) {
    variable_splits <- lapply(variables, function(var) {
        # extract non-missing values for the variable
        selected_column <- na.omit(data[, var])

        if (!(var %in% categorical_variables)) {
            # numeric variable → create grid points
            probs <- seq(0, 1, length.out = grid_points)
            if (variable_splits_type == "quantiles") {
                # use quantile-based grid
                selected_splits <- unique(quantile(selected_column, probs = probs))
            } else {
                # use uniformly spaced grid
                selected_splits <- seq(min(selected_column, na.rm = TRUE),
                                       max(selected_column, na.rm = TRUE),
                                       length.out = grid_points)
            }
            # ensure new_observation values are included in grid (if provided)
            if (!any(is.na(new_observation))) {
                selected_splits <- sort(unique(c(selected_splits,
                                                 na.omit(new_observation[, var]))))
            }
        } else {
            # categorical variable → use all unique observed levels
            if (any(is.na(new_observation))) {
                selected_splits <- sort(unique(selected_column))
            } else {
                # include new_observation levels if provided
                selected_splits <- sort(unique(rbind(
                    data[, var, drop = FALSE],
                    new_observation[, var, drop = FALSE]
                )[, 1]))
            }
        }
        selected_splits
    })

    # assign variable names to list elements
    names(variable_splits) <- variables
    variable_splits
}

