#' @import dplyr


#' Check the variation of causal estimate as the threshold for independence used by split-door criterion is varied.
#'
#' @param tseries_df
#' @param independence_thresholds
#' @param do_plot
#' @param ..
#'
#' @return A data.frame containing average causal estimates with different user-provided values of the independence threshold.
#' @export
#'
#' @examples
check_robustness_independence_threshold <- function(tseries_df,
                                                    independence_thresholds=c(0.001, 0.005, 0.01, 0.05, 0.10, 0.15, 0.20),
                                                    do_plot=TRUE,
                                                    ...){
  mean_estimates_df_list = lapply(independence_thresholds,
        function(indep_thres){
          ctr_xy = splitdoor_causal_estimate(tseries_df,
                    independence_threshold=indep_thres,
                    ...);
          ctr_x = aggregate_by_treatment_id(ctr_xy)
          average_estimates = summarize(ctr_x,
                                        independence_threshold=indep_thres,
                                        mean_causal_estimate=mean(agg_causal_estimate),
                                        sd_causal_estimate=sd(agg_causal_estimate),
                                        se_causal_estimate=sd(agg_causal_estimate)/sqrt(n()),
                                        num_unique_treatments=n()
                                      )

        })
  mean_estimates_by_thres = do.call(rbind.data.frame, mean_estimates_df_list)
  if(do_plot){
    p_est = plot_estimate_by_indep_threshold(mean_estimates_by_thres)
    p_num = plot_num_splitdoors_by_indep_threshold(mean_estimates_by_thres)
    #TODO check if cowplot is installed and give a warning, include in suggests for package
    cowplot::plot_grid(p_num, p_est, labels=c("Split-door Estimates", "Number of Valid Split-door Pairs"), ncol = 2, nrow = 1)
  }
  return(mean_estimates_by_thres)

}

#' Compare distribution of all treatments and treatments with a valid split-door estimate.
#'
#' @param causal_estimate_df
#' @param ordered_treatment_groups_vec
#' @param do_plot
#'
#' @return
#' @export
#'
#' @examples
check_distribution_splitdoors_by_group <- function(causal_estimate_df,
                                                   ordered_treatment_groups_vec=NULL,
                                                   do_plot=TRUE){
  valid_splitdoor_treatments = filter(causal_estimate_df, pass_splitdoor_criterion)
  freq_by_pgroup = group_by(causal_estimate_df, treatment_group) %>%
      summarize(num_treatments = length(unique(treatment_tseries_id))) %>%
      ungroup()
  if(is.null(ordered_treatment_groups_vec)){
    freq_by_top_pgroup = freq_by_pgroup %>%
      arrange(-num_treatments) %>%
      slice(1:10)
    ordered_treatment_groups_vec = freq_by_top_pgroup$treatment_group
  }


  indepfreq_by_pgroup = group_by(valid_splitdoor_treatments, treatment_group) %>%
    summarize(num_treatments = length(unique(treatment_tseries_id))) %>%
    ungroup()

  allfreq_by_top_pgroup = freq_by_pgroup %>%
    filter(treatment_group %in% ordered_treatment_groups_vec) %>%
    mutate(treatment_group = factor(treatment_group, levels=ordered_treatment_groups_vec),
           dataset="With >=10 visits on a day")

  indepfreq_by_top_pgroup = indepfreq_by_pgroup %>%
    filter(treatment_group %in% ordered_treatment_groups_vec) %>%
    mutate(treatment_group = factor(treatment_group, levels=ordered_treatment_groups_vec),
           dataset="Satisfy split-door criterion")
  pgroup_plotdata = rbind(allfreq_by_top_pgroup, indepfreq_by_top_pgroup) %>%
    mutate(dataset = factor(dataset, levels=c("With >=10 visits on a day", "Satisfy split-door criterion")))
  pgroup_plotdata2 = group_by(pgroup_plotdata, dataset) %>%
    mutate(frac_asins = num_treatments/sum(num_treatments)) %>%
    ungroup()
  if(do_plot){
    plot_pgroup_distribution(pgroup_plotdata2)
  }
  return(pgroup_plotdata2)
}

#' Inspect manually timeseries for  <treatment, aux_outcome> pairs that are returned by the split-door criterion.
#'
#' @param treatment_outcome_pairs
#' @param tseries_df
#' @param n_treatment_outcome_pairs
#'
#' @return
#' @export
#'
#' @examples
inspect_splitdoor_pairs <- function(treatment_outcome_pairs, tseries_df,
                                       n_treatment_outcome_pairs=NULL){
  if (!is.null(n_treatment_outcome_pairs)){
    treatment_outcome_pairs=sample_n(treatment_outcome_pairs, n_treatment_outcome_pairs)
  }
  rel_treatment_outcome_pairs = select(treatment_outcome_pairs,
                                               date_factor, treatment_tseries_id,
                                               outcome_tseries_id, independence_probability, pass_splitdoor_criterion)
  subset_data  = inner_join(rel_treatment_outcome_pairs, tseries_df,by=c("date_factor", "treatment_tseries_id", "outcome_tseries_id"))

  plot_treatment_aux_outcome_timeseries(subset_data)
  return(subset_data
  )

}
