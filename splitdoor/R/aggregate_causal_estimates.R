#' Aggregates pairwise effects to product treatment-level effects. Helpful when a single treatment variable affects multiple outcome variables.
#'
#' @param pairwise_causal_estimate_df A data.frame with causal estimates for each (time-period, treatment, outcome) triplet.
#'
#' @return A data.frame with causal estimates for each (time-period, treatment) pair.
#' @export
#'
#' @examples
aggregate_by_treatment_id<- function(pairwise_causal_estimate_df){

  valid_pairwise_causal_estimate_df = filter(pairwise_causal_estimate_df,
                                             !is.na(causal_estimate))
  if("date_factor" %in% colnames(valid_pairwise_causal_estimate_df)){
    ctr_iv_data_x = group_by(valid_pairwise_causal_estimate_df,
                             date_factor, treatment_tseries_id) %>%
      summarize(
        treatment_group=first(treatment_group),
        agg_causal_estimate = sum(total_outcome_val)/first(total_treatment_val)
      ) %>%
      ungroup()
  } else {
  ctr_iv_data_x = group_by(valid_pairwise_causal_estimate_df,
                           treatment_tseries_id) %>%
    summarize(
      treatment_group = first(treatment_group),
      agg_causal_estimate= sum(total_outcome_val)/first(total_treatment_val)
      ) %>%
    ungroup()
  }
  print(summary(ctr_iv_data_x))
  return(ctr_iv_data_x)
}

#' Aggregates estimates for (time-period, treatment) pairs over treatment groups. Useful when treatments can be classified in a fixed number of groups.
#'
#' @param ctr_df
#' @param estimate_colname
#'
#' @return A data.frame containing mean causal estimates for each treatment group.
#'
#' @examples
breakup_est_by_treatment_group <- function(ctr_df, estimate_colname){
  cond_ests = group_by(ctr_df,
                       treatment_group) %>%
    summarize_(
      num_products = lazyeval::interp(~length(var), var=as.name(estimate_colname)),
      mean_estimate = lazyeval::interp(~mean(var), var=as.name(estimate_colname)),
      sd_estimate = lazyeval::interp(~sd(var), var=as.name(estimate_colname))
    )
  return(cond_ests)
}
