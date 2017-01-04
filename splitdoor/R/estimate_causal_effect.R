library(tidyr)

compute_pairwise_causal_effect <- function(valid_tseries_tbl, is_valid_splitdoor=TRUE){
  grouped_df = valid_tseries_tbl %>%
    group_by(date_factor, treatment_tseries_id, outcome_tseries_id)
  
  if(is_valid_splitdoor){
    causal_effect_df = grouped_df %>%
    summarize(
      treatment_group=first(treatment_group),
      independence_probability=first(independence_probability), 
      pass_splitdoor_criterion = first(pass_splitdoor_criterion),
      total_treatment_val=sum(treatment_val),
      total_outcome_val=sum(outcome_val),
      total_aux_outcome_val=sum(aux_outcome_val),
      causal_estimate = sum(outcome_val)/sum(treatment_val)
    )
  } else {
    causal_effect_df = grouped_df %>%
      summarize(    
        treatment_group=first(treatment_group),
        independence_probability=first(independence_probability), 
        pass_splitdoor_criterion = first(pass_splitdoor_criterion),
        total_treatment_val=sum(treatment_val),
        total_outcome_val=sum(outcome_val),
        total_aux_outcome_val=sum(aux_outcome_val),
        causal_estimate = NA
     ) 
  }
  causal_effect_df = ungroup(causal_effect_df)
  return(causal_effect_df)
}
est_causal_effect <- function(tseries_tbl_with_indep_info, method="simple"){
  #TODO add support for accessing valid_asin_targets_visits for ifelse statement in this function
  if (method=="simple"){  
    valid_tseries_tbl = filter(tseries_tbl_with_indep_info, pass_splitdoor_criterion)#!is.na(num_diff_yds))  
    invalid_tseries_tbl = filter(tseries_tbl_with_indep_info, !pass_splitdoor_criterion)
    ctr_valid_xy = compute_pairwise_causal_effect(valid_tseries_tbl) 
    ctr_invalid_xy = compute_pairwise_causal_effect(invalid_tseries_tbl, is_valid_splitdoor=FALSE) 
    ctr_overall_df = rbind(ctr_valid_xy,ctr_invalid_xy)
    print(summary(filter(ctr_overall_df, !is.na(causal_estimate))))
  }
  return(ctr_overall_df)
} 

# est_naive_nodatefactors <- function(aug_alldays_iv_data) {
#   # 1: compute estimates per asin, target_asin pair
#   ctr_iv_data_xy = group_by(aug_alldays_iv_data,
#                             treatment_tseries_id, outcome_tseries_id) %>%
#     arrange(date) %>%
#     summarize(      
#       total_treatment_val=sum(treatment_val),
#       total_outcome_val=sum(outcome_val),
#       total_aux_outcome_val=sum(aux_outcome_val),
#       causal_estimate = sum(outcome_val)/sum(treatment_val)
#       #pyj1_x0 = sum(aux_outcome_val)/first(total_aux_outcome_val_inperiod))
#     ) %>% ungroup()
#   print(summary(ctr_iv_data_xy))
#   
#  return(ctr_iv_data_xy)
# }

est_naive <- function(tseries_tbl) {
  ctr_iv_data_xy = group_by(tseries_tbl,
                            date_factor, treatment_tseries_id, outcome_tseries_id) %>%
    summarize(
      treatment_group=first(treatment_group),
      total_treatment_val=sum(treatment_val),
      total_outcome_val=sum(outcome_val),
      total_aux_outcome_val=sum(aux_outcome_val),
      causal_estimate = sum(outcome_val)/sum(treatment_val)
    )
  print(summary(ctr_iv_data_xy))
  
  return(ctr_iv_data_xy)
}