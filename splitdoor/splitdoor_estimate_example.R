library(splitdoor)
library(dplyr)
  
#load("../../binary-iv/Rscripts/splitdoor/expanded_splitdoor_data_withaddtocart.Rdat")
load("../../binary-iv/Rscripts/splitdoor/recommender.log")
tseries_df = select(recommender.log,
                   date_factor, date,
                   asin_id, target_asin_id,
                   treatment_tseries_id, outcome_tseries_id,
                   x_visits, num_clickthroughs,
                   norm_x_visits,norm_num_clickthroughs,norm_yd_visits,
                   yd_visits
                   ) #%>%
  #slice(1:3000)
tseries_df = select(recommender.log,
                    -asin_id,
                    -target_asin_id,
                    -x_visits,
                    -yd_visits,
                    -num_clickthroughs
                    ) %>%
  rename(
    treatment_val=norm_x_visits,
    aux_outcome_val=norm_yd_visits,
    outcome_val=norm_num_clickthroughs
  )
  
naive_estimate_df = correlational_estimate(tseries_df,
                                           test_time_period=15,
                                           fn_add_time_period_factor = assign_time_periods)
agg_naive_estimate_df = aggregate_by_treatment_id(naive_estimate_df)
print("Running splitdoor")
causal_estimate_df = splitdoor_causal_estimate(tseries_df,
                                             fn_add_time_period_factor=assign_time_periods,
                                             fn_independence_test=fisher_independence_test,
                                             test_time_period=15,
                                             num_discrete_levels=4,
                                             independence_threshold=0.05)
agg_causal_estimate_df = aggregate_by_treatment_id(causal_estimate_df)

compare_splitdoor_correlational_estimate(causal_estimate_df, naive_estimate_df,
                                         by_group=TRUE)
