## helper functions



## Shap aggregation ------------------------------------------------------------
# # this code is taken from the R survex package
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
# function to compute Friedman's H_jk statistic
feature_interaction <- function(explainer, feature, N = NULL) {
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
    ndata = explainer$data
    pdp_2d_results <- survex::model_profile_2d(explainer,
                                               variables = tuples_list,
                                               N = NULL,
                                               center = TRUE)
    # extract results
    df_results_2d <- pdp_2d_results$result
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
