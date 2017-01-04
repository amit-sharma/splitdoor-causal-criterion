
splitdoor_causal_estimate <- function(tseries_df, 
                                      fn_add_time_period_factor=assign_time_periods,
                                      fn_independence_test=fisher_independence_test,
                                      test_time_period=15,
                                      num_discrete_levels=4,
                                      independence_threshold=0.05){
  tseries_tbl <- as.tbl(tseries_df)  
  distinct_ids = select(tseries_tbl,treatment_tseries_id) %>%
    distinct()
  #pgroup_distr_eligible = plot_pgroup_distribution(visits_data_unique_asin, f_product_info)
  
  aug_tseries_tbl = fn_add_time_period_factor(tseries_tbl, test_time_period)
  
  
  
  
  

  #TODO yes compare to shock iv too
  #ctr_df_x = est_constant_demand(full_aug_alldays_iv_data) #Just for comparison to shock-iv, not doing it now
  
  # Now computing estimates by the independence criterion
  # relcols_alldays_iv_data = select(aug_alldays_iv_data, 
  #                                  date_factor, date, treatment_tseries_id, outcome_tseries_id,
  #                                  x_visits, ytotal_visits, discrete_x,
  #                                  num_clickthroughs, aux_outcome_val, discrete_yd, 
  #                                  is_constant_demand, num_demand_levels)
  # 
  by_pgroup = FALSE
  draw_plots = FALSE
  
  treatment_outcome_pairs_with_indep_info = fn_independence_test(aug_tseries_tbl,
                                    p_value=independence_threshold,
                                    num_discrete_levels=num_discrete_levels)
  tseries_tbl_with_indep_info = inner_join(aug_tseries_tbl, treatment_outcome_pairs_with_indep_info, 
                                        by=c("date_factor", "treatment_tseries_id", "outcome_tseries_id"))
  print(paste(nrow(tseries_tbl_with_indep_info),nrow(aug_tseries_tbl)))
  print(tseries_tbl_with_indep_info)
  ctr_xy_indep = est_causal_effect(tseries_tbl_with_indep_info,
                                    method="simple" # regression_cov, simple
                                    )
 
  # valid_tseries_tbl = group_by(tseries_tbl_with_indep_info, 
  #                                    date_factor, treatment_tseries_id) %>%
  #   mutate(all_yj_indep = !any(is.na(num_values))) %>% 
  #   ungroup()
  # #todo change: back to num_diff_yds

#}
# return(list(
#   indep_asin_targets_visits = fvalid_asin_targets_visits,
#   dep_asin_targets_visits = fnotvalid_asin_targets_visits,
#   summarized_asin_targets = summarized_asin_targets
#   
#   )
# )

#    indep_asin_targets_visits = valid_tseries_tbl
#    dep_asin_targets_visits =  notvalid_tseries_tbl
  
  #zero_yd_asin_targets_visits = ret_list$zero_yd_asin_targets_visits
  if(draw_plots){
    #indep_unique_asins = (indep_asin_targets_visits, treatment_tseries_id) %>% distinct()
    # plot for paper
    pgroup_distr_splitdoor = plot_pgroup_distribution(aug_tseries_tbl,
                                                      valid_tseries_tbl,
                                                      f_product_visits,
                                                      f_product_info)
    pgroup_distr_splitdoor_weighted = plot_pgroup_distribution(aug_tseries_tbl,
                                                               valid_tseries_tbl,
                                                               f_product_visits,
                                                               f_product_info,
                                                               weight_by_numvisits = TRUE)
    # Again, for paper, plot this:
    plot_invalid_splitdoors(notvalid_tseries_tbl)
    plot_valid_splitdoors(valid_tseries_tbl)
    
    indep_x=plot_sample_xy_pairs(valid_tseries_tbl) # commenting for loop
    dep_x=plot_sample_xy_pairs(filter(notvalid_tseries_tbl, is_constant_demand==FALSE)) # commenting for loop
    #plot_valid_splitdoors
    }
    
   



    #compare_distribution(alldays_iv_data, indep_asin_targets_visits)# commenting for loop
    #breakup_x_yd_by_productgroup(zero_yd_asin_targets_visits, f_product_info)
    
    #TODO: bring it back? commented for now.
    #tt=est_independent_x_yd_pearl(filter(indep_asin_targets_visits, treatment_tseries_id %in% ctr_x_indep$treatment_tseries_id[1:3000]), 
    #                              prob_equal_precision = 0.05)
    
  return(ctr_xy_indep)    
}

correlational_estimate <- function(tseries_df,
                                   test_time_period=NULL,
                                   fn_add_time_period_factor=assign_time_periods){
                                   
  tseries_tbl <- as.tbl(tseries_df)  
  
  if(is.null(test_time_period)){
    ctr_xy_naive= est_naive_nodatefactors(tseries_tbl) 
  } else {
    aug_tseries_tbl = fn_add_time_period_factor(tseries_tbl, test_time_period)
    ctr_xy_naive = est_naive(aug_tseries_tbl)
  }
  
  return(ctr_xy_naive)
}

compare_splitdoor_correlational_estimate <-function(splitdoor_estimates_df, correlational_estimates_df, by_group=TRUE){
  tseries_with_valid_splitdoor = unique(splitdoor_estimates_df$treatment_tseries_id)
  ctr_corr_tseries_with_valid_splitdoor = filter(correlational_estimates_df, treatment_tseries_id %in% tseries_with_valid_splitdoor)
  if(by_group){
      # breaking up into product groups
      ctr_corr_bygroup = breakup_est_by_productgroup(ctr_corr_tseries_with_valid_splitdoor, f_product_info)
      ctr_splitdoor_by_group= breakup_est_by_productgroup(splitdoor_estimates_df, f_product_info)
      
      compare_estimates = left_join(ctr_corr_bygroup, ctr_splitdoor_by_group_indep, by="product_group") %>%
        mutate(diff = mean_estimate.y - mean_estimate.x,
               abs_diff = abs(diff))
      print(plot_ctr_compare(compare_estimates))
  } else {
    ctr_corr_average = summarize(ctr_corr_tseries_with_valid_splitdoor, 
                                 mean_estimate=mean(agg_causal_estimate))
                                 
  }
    #x=plot_sample_xy_pairs(zero_yd_asin_targets_visits)# commenting for loop
  return(compare_estimates)
}