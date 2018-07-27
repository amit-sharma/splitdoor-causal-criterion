#' @import energy
NULL

#' Fisher's test for independence between pairs of discrete time-series.
#' 
#' Use fisher's test for discrete variables.
#' If num_discrete_levels is provided, this function also discretizes the data before applying Fisher's test. 
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
  if (!is.na(num_discrete_levels)) {
    discrete_tseries_tbl = discretize_treatment_aux_outcome(tseries_tbl, num_discrete_levels)
  }
  treatment_outcome_pairs = discrete_tseries_tbl %>%
  group_by(date_factor, treatment_tseries_id, outcome_tseries_id) %>%
  mutate(
    num_diff_yds = length(unique(discrete_yd)),
    num_diff_xds = length(unique(discrete_x))
  ) %>%
  ungroup()
  
  # Fisher's test does not work well when there is a single discrete level.
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

#' Randomization Test for independence between pairs of time-series using distance correlation.
#' 
#' Uses randomization test based on distance correlation. 
#' 
#' @param tseries_tbl
#' @param p_value
#'
#' @return A data.frame containing test results for each (treatment, outcome) pair.
#' Probability of independence of the two time-series is provided along with Pass/Fail result of the test based on the user-provided independence_threshold.
#' @export
#'
#' @examples
dcor_independence_test <- function(tseries_tbl, p_value, num_discrete_levels=NA) {
  
  x_vals = table(tseries_tbl$treatment_val)
  names_xvals = as.numeric(names(x_vals))
  #1. Filter those periods where yd or x does not change.
  treatment_outcome_pairs = tseries_tbl %>%
    group_by(date_factor, treatment_tseries_id, outcome_tseries_id) %>%
    mutate(
      num_diff_cont_yds=length(unique(aux_outcome_val)),
      num_diff_cont_xds = length(unique(treatment_val))
    ) %>%
    ungroup()
  faug_asin_targets = filter(treatment_outcome_pairs,
                               num_diff_cont_yds >1,
                               num_diff_cont_xds >1)
  
  summarized_asin_targets = faug_asin_targets %>%
      group_by(date_factor, treatment_tseries_id, outcome_tseries_id) %>%
      dplyr::summarize(
        num_values = n(),
        sum_xvisits = sum(treatment_val),
        pvalue = x_yd_randomize_test(treatment_val, aux_outcome_val, 
                                     asin_id, cor_metric="compare_distance_cor", 
                                     x_vals, names_xvals, DOPRINT=FALSE)
      ) %>%
    ungroup()
  print(paste("Filtering indep pairs by p-value=", p_value))
  summarized_asin_targets = mutate(summarized_asin_targets,
                                   independence_probability=ifelse(is.na(pvalue), NA, 1-pvalue),
                                   pass_splitdoor_criterion = ifelse(is.na(independence_probability),
                                                                            FALSE,
                                                                            ifelse(independence_probability<=p_value,TRUE, FALSE)))
  return(summarized_asin_targets)
}

#' General Randomization Test for independence between pairs of vectors.
#' 
#' Uses randomization test based on distance correlation (preferred) or simple correlation. 
#' 
#' @param tseries_tbl
#' @param p_value
#'
#' @return A data.frame containing test results for each (treatment, outcome) pair.
#' Probability of independence of the two time-series is provided along with Pass/Fail result of the test based on the user-provided independence_threshold.
#'
#' @examples
x_yd_randomize_test <- function(x_vec, y_vec, asin_id, cor_metric, 
                                x_vals, names_xvals, 
                                num_data_simulations=1000, DO_ECDF=F, DOPRINT=F){

  cor_vec = replicate(num_data_simulations, gen_cor_samples(x_vec,y_vec,cor_metric, x_vals, names_xvals))
  cor_vec = cor_vec[!is.na(cor_vec)]  
  
  if(cor_metric=="compare_cor"){
    cor_actual=abs(cor(x_vec, y_vec))
  } else if (cor_metric=="compare_distance_cor"){
    cor_actual=max(dcor(x_vec, y_vec), 0) 
  }
  
  if(DO_ECDF){
    cor_vec_ecdf_fn= ecdf(cor_vec)
    computed_p_value = 1- cor_vec_ecdf_fn(cor_actual)
  } else {
    sorted_cor_vec = sort(cor_vec)
    
    computed_temp_value = which(abs(sorted_cor_vec -cor_actual)<10^-10)[1]
    if(is.na(computed_temp_value)){
      computed_temp_value = which.min(abs(sorted_cor_vec-cor_actual))
    }
    if(DOPRINT){
      print(sorted_cor_vec)
      print(abs(sorted_cor_vec-cor_actual))
      print(cor_actual - sorted_cor_vec[1])
      print(computed_temp_value)
      print(paste(cor_actual, sorted_cor_vec[which(abs(sorted_cor_vec -cor_actual)<10^-10)[1]],sorted_cor_vec[which.min(abs(sorted_cor_vec -cor_actual))] ))
    }
    computed_p_value = 1 - computed_temp_value/length(sorted_cor_vec)
    
  }
  if(DOPRINT){
    print(summary(cor_vec))
    print(paste("Actual correlation is", cor_actual))
    print(paste("p-value is", computed_p_value))
  }
  return(computed_p_value)
  
}

#' Simulate two vectors and compute correlation between them.
#' 
#' A part of the randomization test procedure. 
#' 
#' @param tseries_tbl
#' @param p_value
#'
#' @return A data.frame containing test results for each (treatment, outcome) pair.
#' Probability of independence of the two time-series is provided along with Pass/Fail result of the test based on the user-provided independence_threshold.
#'
#' @examples
gen_cor_samples <- function(x_vec, y_vec,cor_metric, x_vals, names_xvals){
  sim_x_values=sample(names_xvals, length(x_vec), prob=x_vals) #gen_sim_values(x_vec, sim="preserve_sum")#gen_sim_values(x_vec)#gen_sim_values(x_vec) #TODO probably need only one
  sim_y_values = y_vec
  if(cor_metric=="compare_cor"){
    cor_value=abs(cor(sim_x_values, sim_y_values)) 
  } else if (cor_metric=="compare_distance_cor"){
    cor_value = dcor(sim_x_values, sim_y_values)
  }
  return(cor_value)
}
