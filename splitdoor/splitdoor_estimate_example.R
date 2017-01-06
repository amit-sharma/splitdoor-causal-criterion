library(splitdoor)
library(dplyr)

load("../../binary-iv/Rscripts/splitdoor/recommender.log")
tseries_df = recommender.log  %>%
  slice(1:30000)
tseries_df = assign_time_periods(tseries_df, time_period = 15)

naive_estimate_df = correlational_estimate(tseries_df,
                                           test_time_period=15,
                                           fn_add_time_period_factor = assign_time_periods)
agg_naive_estimate_df = aggregate_by_treatment_id(naive_estimate_df)


causal_estimate_df = splitdoor_causal_estimate(tseries_df,
                                             fn_add_time_period_factor=assign_time_periods,
                                             fn_independence_test=fisher_independence_test,
                                             test_time_period=15,
                                             num_discrete_levels=4,
                                             independence_threshold=0.05)
agg_causal_estimate_df = aggregate_by_treatment_id(causal_estimate_df)

    plot_invalid_splitdoors(notvalid_tseries_tbl)
    plot_valid_splitdoors(valid_tseries_tbl)

    indep_x=plot_sample_xy_pairs(valid_tseries_tbl) # commenting for loop
    dep_x=plot_sample_xy_pairs(filter(notvalid_tseries_tbl, is_constant_demand==FALSE)) # commenting for loop


compare_splitdoor_correlational_estimate(agg_causal_estimate_df, agg_naive_estimate_df,
                                         by_group=TRUE, estimate_colname="agg_causal_estimate")
check_distribution_splitdoors_by_group(causal_estimate_df)

inspect_splitdoor_pairs(filter(causal_estimate_df,pass_splitdoor_criterion),
                        tseries_df,
                        n_treatment_outcome_pairs = 4)
