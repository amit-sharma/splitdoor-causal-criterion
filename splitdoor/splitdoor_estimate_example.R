library(splitdoor)
library(dplyr)
  
#load("../../binary-iv/Rscripts/splitdoor/expanded_splitdoor_data_withaddtocart.Rdat")
load("../../binary-iv/Rscripts/splitdoor/recommender.log")
tseries_df = recommender.log  #%>%
  #slice(1:3000)
  
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

compare_splitdoor_correlational_estimate(agg_causal_estimate_df, agg_naive_estimate_df,
                                         by_group=TRUE, estimate_colname="agg_causal_estimate")
