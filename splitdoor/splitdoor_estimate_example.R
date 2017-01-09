library(splitdoor)
library(dplyr)

# A data.frame in the following format.
load("recommender.log.sample")

# Optional: Sampling for faster run.
tseries_df = recommender.log.sample  %>%
  slice(1:15000)

# Optional: Splitting data based on a user-specified time-interval.
tseries_df = assign_time_periods(tseries_df, time_period = 15)

# 1a. Computing a naive estimate first using correlations.
naive_estimate_df = correlational_estimate(tseries_df,
                                           test_time_period=15,
                                           fn_add_time_period_factor = assign_time_periods)
# 1b. Aggregating to each unique treatment
agg_naive_estimate_df = aggregate_by_treatment_id(naive_estimate_df)

# 2a. Estimating causal effect using the split-door criterion.
causal_estimate_df = splitdoor_causal_estimate(tseries_df,
                                             fn_independence_test=fisher_independence_test,
                                             num_discrete_levels=4,
                                             independence_threshold=0.05)
# 2b. Aggregating to each unique treatment
agg_causal_estimate_df = aggregate_by_treatment_id(causal_estimate_df)

# 3. Analysis of the obtained estimates
# 3a. Comparing correlational and split-door estimates
compare_splitdoor_correlational_estimate(agg_causal_estimate_df, agg_naive_estimate_df,
                                         by_group=TRUE, estimate_colname="agg_causal_estimate")

# 3b. Visualizing valid split-door <treatment, outcome> pairs
inspect_splitdoor_pairs(filter(causal_estimate_df,pass_splitdoor_criterion),
                        tseries_df,
                        n_treatment_outcome_pairs = 4)

#3c. Checking robustness of the split-door estimate to
#    change in threshold for independence test.
check_robustness_independence_threshold(tseries_df,
                                        independence_thresholds = c(0.05,0.01,0.001))


# 3d. Checking whether distribution of sample treatments selected by the
#     split-door criterion is similar to the overall distribution over treatment groups.
check_distribution_splitdoors_by_group(causal_estimate_df)

