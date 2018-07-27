#' @import dplyr



#' Estimate causal effect of multiple treatment variables on outcome variables, given data for their timeseries.
#' 
#' Estimate causal effect.
#'
#' @param tseries_df
#' @param fn_add_time_period_factor
#' @param fn_independence_test
#' @param test_time_period
#' @param num_discrete_levels
#' @param independence_threshold
#'
#' @return A data.frame containing causal estimates for each pair of (treatment, outcome) variables.
#' @export
#'
#' @examples
splitdoor_causal_estimate <- function(tseries_df,
                                      fn_independence_test=dcor_independence_test,
                                      num_discrete_levels=4,
                                      independence_threshold=0.05,
                                      ...){
  tseries_tbl <- as.tbl(tseries_df)
  distinct_ids = select(tseries_tbl,treatment_tseries_id) %>%
    distinct()

  aug_tseries_tbl = tseries_tbl


  by_pgroup = FALSE
  draw_plots = FALSE

  treatment_outcome_pairs_with_indep_info = fn_independence_test(aug_tseries_tbl,
                                    p_value=independence_threshold,
                                    num_discrete_levels=num_discrete_levels,
                                    ...)
  tseries_tbl_with_indep_info = inner_join(aug_tseries_tbl, treatment_outcome_pairs_with_indep_info,
                                        by=c("date_factor", "treatment_tseries_id", "outcome_tseries_id"))
  print(paste(nrow(tseries_tbl_with_indep_info),nrow(aug_tseries_tbl)))
  print(tseries_tbl_with_indep_info)
  ctr_xy_indep = est_causal_effect(tseries_tbl_with_indep_info,
                                    method="simple" # regression_cov, simple
                                    )
  return(ctr_xy_indep)
}

#' Compute correlational estimate based on all available data. 
#' Provided as a baseline for comparison. Do not use for causal analysis. 
#' 
#' @param tseries_df
#' @param test_time_period
#' @param fn_add_time_period_factor
#'
#' @return
#' @export
#'
#' @examples
correlational_estimate <- function(tseries_df,
                                   test_time_period=NULL,
                                   fn_add_time_period_factor=assign_time_periods){

  tseries_tbl <- as.tbl(tseries_df)
  
  # Check if the data needs to be separated into smaller time periods for analysis 
  if(is.null(test_time_period)){
    ctr_xy_naive= est_naive_nodatefactors(tseries_tbl)
  } else {
    aug_tseries_tbl = fn_add_time_period_factor(tseries_tbl, test_time_period)
    ctr_xy_naive = est_naive(aug_tseries_tbl)
  }

  return(ctr_xy_naive)
}

#' Compare a baseline correlational measure to causal estimates using the split-door criterion.
#'
#' @param splitdoor_estimates_df
#' @param correlational_estimates_df
#' @param by_group
#' @param estimate_colname
#'
#' @return If by_group is TRUE, a data.frame containing the mean estimates for each group using split-door and the baseline method.
#' @export
#'
#' @examples
compare_splitdoor_correlational_estimate <-function(splitdoor_estimates_df, correlational_estimates_df, by_group=TRUE,
                                                    estimate_colname="causal_estimate",
                                                    min_group_frequency=30){
  tseries_with_valid_splitdoor = unique(splitdoor_estimates_df$treatment_tseries_id)#unique(filter(splitdoor_estimates_df, pass_splitdoor_criterion)$treatment_tseries_id)
  ctr_corr_tseries_with_valid_splitdoor = filter(correlational_estimates_df, treatment_tseries_id %in% tseries_with_valid_splitdoor)
  if(by_group){
      # breaking up into product groups
      ctr_corr_bygroup = splitdoor:::breakup_est_by_treatment_group(ctr_corr_tseries_with_valid_splitdoor, estimate_colname)
      ctr_splitdoor_by_group= splitdoor:::breakup_est_by_treatment_group(splitdoor_estimates_df, estimate_colname)

      compare_estimates = left_join(ctr_corr_bygroup, ctr_splitdoor_by_group, by="treatment_group") %>%
        mutate(diff = mean_estimate.y - mean_estimate.x,
               abs_diff = abs(diff))
      print(plot_ctr_compare(compare_estimates, min_group_frequency))
  } else {
    #TODO.
    ctr_corr_average = summarize(ctr_corr_tseries_with_valid_splitdoor,
                                 mean_estimate=mean(agg_causal_estimate))

  }
  return(compare_estimates)
}
