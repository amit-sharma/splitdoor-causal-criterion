#' Test for independence between pairs of discrete time-series.
#'
#' @param tseries_tbl 
#' @param p_value 
#' @param num_discrete_levels 
#'
#' @return A data.frame containing test results for each (treatment, outcome) pair. 
#' Probability of independence of the two time-series is provided along with Pass/Fail result of the test based on the user-provided independence_threshold.
#' @export
#'
#' @examples
fisher_independence_test <- function(tseries_tbl, p_value, num_discrete_levels) {
  discrete_tseries_tbl = discretize_treatment_aux_outcome(tseries_tbl, num_discrete_levels)
  treatment_outcome_pairs = discrete_tseries_tbl %>%
  group_by(date_factor, treatment_tseries_id, outcome_tseries_id) %>%
  mutate(
    num_diff_yds = length(unique(discrete_yd)),
    num_diff_xds = length(unique(discrete_x))
  ) %>%
  ungroup()
  f_treatment_outcome_pairs = filter(treatment_outcome_pairs,
                           num_diff_yds >1,
                           num_diff_xds >1)
  constant_treatment_outcome_pairs = filter(treatment_outcome_pairs,
                                            num_diff_yds<=1 | num_diff_xds<=1)
  
  f_summarized_asin_targets = f_treatment_outcome_pairs %>%
        group_by(date_factor, treatment_tseries_id, outcome_tseries_id) %>%
        summarize(
          num_values = n(),
          sum_xvisits = sum(treatment_val),
          pvalue = fisher.test(discrete_x, discrete_yd)$p.value
        ) %>%
        ungroup()
  constant_summarized_asin_targets = constant_treatment_outcome_pairs %>%
    group_by(date_factor, treatment_tseries_id, outcome_tseries_id) %>%
    summarize(
      num_values = n(),
      sum_xvisits = sum(treatment_val),
      pvalue = NA
    ) %>%
    ungroup()
  summarized_asin_targets = rbind(f_summarized_asin_targets,constant_summarized_asin_targets)
  
  
  print(paste("Filtering indep pairs by p-value=", p_value))
  indep_asin_targets = filter(summarized_asin_targets, pvalue>=1-p_value)
  summarized_asin_targets = mutate(summarized_asin_targets,
                                   independence_probability=ifelse(is.na(pvalue), NA, 1-pvalue),
                                   pass_splitdoor_criterion = ifelse(is.na(independence_probability),
                                                                            FALSE,
                                                                            ifelse(independence_probability<=p_value,TRUE, FALSE))) 
  return(summarized_asin_targets)
}

#' Discretize treatment and outcome timeseries data into user-provided number of levels. Used by the fisher independence function.
#'
#' @param tseries_tbl 
#' @param num_discrete_levels 
#'
#' @return A data.frame containing discretized values of the user-provided time-series.
#'
#' @examples
discretize_treatment_aux_outcome <-function(tseries_tbl, num_discrete_levels){
  # II. Creating discrete yd and discrete x based on max x value on any day in period.
  by_period_asin = group_by(tseries_tbl, date_factor, treatment_tseries_id,outcome_tseries_id)
  aug_exp_iv_data = by_period_asin %>%
    mutate(
           #discrete_yd = round(yd_multiplier*aux_outcome_val/max(treatment_val))/yd_multiplier,
           max_aux_outcome_val = max(aux_outcome_val),
           discrete_yd = floor((aux_outcome_val/max(aux_outcome_val))*num_discrete_levels),
           discrete_x = floor((treatment_val/max(treatment_val))*num_discrete_levels),
           maxt= max(treatment_val)) %>%
    ungroup()
  
  # computing the number of distinct levels for yd
  by_period_asin_target = group_by(aug_exp_iv_data, 
                                   date_factor, treatment_tseries_id, outcome_tseries_id) 
  
  aug_alldays_iv_data = by_period_asin_target %>%
    mutate(is_constant_demand = (length(unique(discrete_yd))==1),
           num_demand_levels =length(unique(discrete_yd))) %>%
    ungroup()
 return(aug_alldays_iv_data)
}
