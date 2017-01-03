discretize_treatment_aux_outcome <-function(tseries_tbl, num_discrete_levels){
  # II. Creating discrete yd and discrete x based on max x value on any day in period.
  by_period_asin = group_by(tseries_tbl, date_factor, asin_id,target_asin_id)
  aug_exp_iv_data = by_period_asin %>%
    mutate(
           #discrete_yd = round(yd_multiplier*yd_visits/max(x_visits))/yd_multiplier,
           max_yd_visits = max(yd_visits),
           discrete_yd = floor((yd_visits/max(yd_visits))*num_discrete_levels),
           discrete_x = floor((x_visits/max(x_visits))*num_discrete_levels),
           maxt= max(x_visits)) %>%
    ungroup()
  
  # computing the number of distinct levels for yd
  by_period_asin_target = group_by(aug_exp_iv_data, 
                                   date_factor, asin_id, target_asin_id) 
  
  aug_alldays_iv_data = by_period_asin_target %>%
    mutate(is_constant_demand = (length(unique(discrete_yd))==1),
           num_demand_levels =length(unique(discrete_yd))) %>%
    ungroup()
 return(aug_alldays_iv_data)
}

fisher_independence_test <- function(tseries_tbl, p_value, num_discrete_levels) {
  discrete_tseries_tbl = discretize_treatment_aux_outcome(tseries_tbl, num_discrete_levels)
  treatment_outcome_pairs = discrete_tseries_tbl %>%
  group_by(date_factor, asin_id, target_asin_id) %>%
  mutate(
    num_diff_yds = length(unique(discrete_yd)),
    num_diff_xds = length(unique(discrete_x))
  ) %>%
  ungroup()
  f_treatment_outcome_pairs = filter(treatment_outcome_pairs,
                           num_diff_yds >1,
                           num_diff_xds >1)
  summarized_asin_targets = f_treatment_outcome_pairs %>%
        group_by(date_factor, asin_id, target_asin_id) %>%
        summarize(
          num_values = n(),
          sum_xvisits = sum(x_visits),
          pvalue = fisher.test(discrete_x, discrete_yd)$p.value
        ) %>%
        ungroup()
  print(paste("Filtering indep pairs by p-value=", p_value))
  indep_asin_targets = filter(summarized_asin_targets, pvalue>=1-p_value)
  
  return(indep_asin_targets)
}