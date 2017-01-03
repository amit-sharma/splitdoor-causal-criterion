library(lme4)
library(MASS)
library(tidyr)

est_causal_effect <- function(valid_asin_targets_visits, method="simple",
                              check_all=FALSE){
  #TODO add support for accessing valid_asin_targets_visits for ifelse statement in this function
  if (method=="simple"){
    ctr_binary_iv_xy2 = valid_asin_targets_visits %>%
      group_by(date_factor, asin_id, target_asin_id) %>%
      summarize(
        py1_x1 = sum(num_clickthroughs)/sum(x_visits)
        #yd_visits = first(yd_visits)
        #pyj1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod)
      )
    print(summary(ctr_binary_iv_xy2))
    
    # Now computing ctr by asin_id
    #TODO make it work with pvalue and fisher test
    if(check_all){
      #indep_yall_demand_by_xi = group_by(valid_asin_targets_visits, 
      #                                   date_factor, asin_id) %>%
      #  mutate(all_yj_indep = !any(is.na(num_diff_probs))) %>% ungroup()
      filtered_alldays_iv_data4 = filter(valid_asin_targets_visits, all_yj_indep )
      #jfiltered_alldays_iv_data4 = inner_join(filtered_alldays_iv_data4, sum_yd_df, by="date_factor")
    } else{
      filtered_alldays_iv_data4 = valid_asin_targets_visits
      #filtered_alldays_iv_data4 = filter(valid_asin_targets_visits, !is.na(num_diff_yds))
    }
    
    ctr_binary_iv_x4 = group_by(filtered_alldays_iv_data4,
                                date_factor, asin_id) %>%
      summarize(
        total_xvisits_inperiod = sum(x_visits)/length(unique(target_asin_id)),
        py1_x1 = sum(num_clickthroughs)/(sum(x_visits)/length(unique(target_asin_id)))
        #yd_visits = first(yd_visits)
        #py1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod)
      )
    print(summary(ctr_binary_iv_x4))
  } else if (method=="regression_cov"){
    ctr_binary_iv_xy2 = valid_asin_targets_visits %>%
      group_by(date_factor, asin_id, target_asin_id) %>%
      distinct(date) %>%
      summarize(
        py1_x1 = cov(x_visits, num_clickthroughs)/var(x_visits)
        #yd_visits = first(yd_visits)
        #pyj1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod)
      )
    print(summary(ctr_binary_iv_xy2))
    
    
    # Now computing ctr by asin_id
    #TODO change for fisher test or MI
    if(check_all){
      #indep_yall_demand_by_xi = group_by(valid_asin_targets_visits, 
      #                                   date_factor, asin_id) %>%
      #  mutate(all_yj_indep = !any(is.na(num_diff_probs))) %>% ungroup()
      #filtered_alldays_iv_data4 = filter(indep_yall_demand_by_xi, all_yj_indep )
      filtered_alldays_iv_data4 = filter(valid_asin_targets_visits, all_yj_indep )
      #jfiltered_alldays_iv_data4 = inner_join(filtered_alldays_iv_data4, sum_yd_df, by="date_factor")
    } else{
      filtered_alldays_iv_data4 = valid_asin_targets_visits
      #filtered_alldays_iv_data4 = filter(valid_asin_targets_visits, !is.na(num_diff_yds))
    }
    
    ctr_binary_iv_x4_t = group_by(filtered_alldays_iv_data4,
                                  date_factor, asin_id,date) %>%
      summarize(total_xvisits_onday = first(x_visits),
                total_num_clickthroughs = sum(num_clickthroughs)) %>%
      ungroup()
    ctr_binary_iv_x4 =  ctr_binary_iv_x4_t  %>%
      group_by(date_factor, asin_id) %>%
      summarize(
        total_xvisits_inperiod = sum(total_xvisits_onday),
        py1_x1 = cov(total_xvisits_onday, total_num_clickthroughs)/var(total_xvisits_onday)
        #yd_visits = first(yd_visits)
        #py1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod)
      )
    print(summary(ctr_binary_iv_x4))
  }
  return(list(
    ctr_xy = ctr_binary_iv_xy2,
    ctr_x = ctr_binary_iv_x4))
} 

est_naive_nodatefactors <- function(aug_alldays_iv_data) {
  # 1: compute estimates per asin, target_asin pair
  ctr_iv_data_xy = group_by(aug_alldays_iv_data,
                            asin_id, target_asin_id) %>%
    arrange(date) %>%
    summarize(
      pyj1_x1 = sum(num_clickthroughs)/sum(x_visits)
      #pyj1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod))
    ) %>% ungroup()
  print(summary(ctr_iv_data_xy))
  
  # 2 compute estimates per asin.
  ctr_iv_data_x = group_by(aug_alldays_iv_data,
                           date_factor, asin_id) %>%
    arrange(target_asin_id, date) %>%
    summarize(
      total_xvisits_inperiod = sum(x_visits)/length(unique(target_asin_id)),
      total_num_clickthroughs = sum(num_clickthroughs)
      #py1_x1 = sum(num_clickthroughs)/(sum(x_visits)/length(unique(target_asin_id)))
      #py1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod)
    ) %>% ungroup()
  
  print(summary(ctr_iv_data_x))
  
  ctr_iv_data_x = group_by(ctr_iv_data_x,
                           asin_id) %>%
    summarize(py1_x1 = sum(total_num_clickthroughs)/sum(total_xvisits_inperiod)) %>%
    ungroup()
  
  print(summary(ctr_iv_data_x))
  return(list(ctr_iv_data_xy=ctr_iv_data_xy,
              ctr_iv_data_x = ctr_iv_data_x))
}

est_naive <- function(aug_alldays_iv_data) {
  # 1: compute estimates per asin, target_asin pair
  ctr_iv_data_xy = group_by(aug_alldays_iv_data,
                            date_factor, asin_id, target_asin_id) %>%
    arrange(date) %>%
    summarize(
      pyj1_x1 = sum(num_clickthroughs)/sum(x_visits)
      #pyj1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod))
    )
  print(summary(ctr_iv_data_xy))
  
  # 2 compute estimates per asin.
  ctr_iv_data_x = group_by(aug_alldays_iv_data,
                           date_factor, asin_id) %>%
    arrange(target_asin_id, date) %>%
    summarize(
      total_xvisits_inperiod = sum(x_visits)/length(unique(target_asin_id)),
      py1_x1 = sum(num_clickthroughs)/(sum(x_visits)/length(unique(target_asin_id)))
      #py1_x0 = sum(yd_visits)/first(total_yd_visits_inperiod)
    )
  print(summary(ctr_iv_data_x))
  return(list(ctr_iv_data_xy=ctr_iv_data_xy,
              ctr_iv_data_x = ctr_iv_data_x))
}