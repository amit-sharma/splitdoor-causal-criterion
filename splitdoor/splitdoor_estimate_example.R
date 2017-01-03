library(splitdoor)
library(dplyr)
  
load("../../binary-iv/Rscripts/splitdoor/expanded_splitdoor_data_withaddtocart.Rdat")
tseries_df = select(alldays_splitdoor_data,
                   date_factor, date,
                   asin_id, target_asin_id,
                   x_visits, num_clickthroughs, num_addtocarts,
                   yd_visits
                   ) %>%
  slice(1:3000)

naive_estimate_df = correlational_estimate(tseries_df,
                                           test_time_period=15,
                                           fn_add_time_period_factor = assign_time_periods)
print("Running splitdoor")
causal_estimate_df = splitdoor_causal_estimate(tseries_df,
                                             fn_add_time_period_factor=assign_time_periods,
                                             fn_independence_test=fisher_independence_test,
                                             test_time_period=15,
                                             num_discrete_levels=4,
                                             independence_threshold=0.05)

compare_splitdoor_correlational_estimate(causal_estimate_df, naive_estimate_df,
                                         by_group=TRUE)