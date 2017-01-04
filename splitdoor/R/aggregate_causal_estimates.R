aggregate_by_treatment_id<- function(pairwise_causal_estimate_df){

  valid_pairwise_causal_estimate_df = filter(pairwise_causal_estimate_df,
                                             !is.na(causal_estimate))
  # if(check_all){
  #     filtered_alldays_iv_data4 = filter(valid_asin_targets_visits, all_yj_indep )
  # } else{
  #     filtered_alldays_iv_data4 = valid_asin_targets_visits
  # }
  if("date_factor" %in% colnames(valid_pairwise_causal_estimate_df)){
    ctr_iv_data_x = group_by(valid_pairwise_causal_estimate_df,
                             date_factor, treatment_tseries_id) %>%
     # arrange_("outcome_tseries_id", "date") %>%
      summarize(
        #total_xvisits_inperiod = sum(total_treatment_val)/length(unique(outcome_tseries_id)),
        treatment_group=first(treatment_group),
        agg_causal_estimate = sum(total_outcome_val)/first(total_treatment_val)
        #py1_x0 = sum(aux_outcome_val)/first(total_aux_outcome_val_inperiod)
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
