## helper functions
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
                    full_survshap_results[,!colnames(full_survshap_results) %in% c("rn")],
                    by = list(full_survshap_results$rn),
                    FUN = aggregation_function
                )
            rownames(tmp_res) <- tmp_res$Group.1
            ordering <-
                order(as.numeric(substring(rownames(tmp_res), 3)))

            tmp_res <-
                tmp_res[ordering,!colnames(tmp_res) %in% c("rn", "Group.1")]
        } else {
            # no aggregation required
            tmp_res <- shap_res_list[[1]]
        }
        shap_values <- tmp_res
        # transform to data.frame to make everything compatible with
        # previous code
        return(shap_values)
    }

