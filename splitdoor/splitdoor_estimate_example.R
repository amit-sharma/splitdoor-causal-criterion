# Supplementary material for the paper:
# Split-door criterion: Identification of causal effects through auxiliary outcomes. 
# Amit Sharma, Jake Hofman, Duncan Watts.
# https://arxiv.org/abs/1611.09414

# Latest version of this code available at: https://www.github.com/amit-sharma/splitdoor-causal-criterion

# General example to illustrate use of the splitdoor R package


library(splitdoor)
library(dplyr)

# A data.frame in the following format.
load("recommender.log.sample")
## Data format
# date_factor: 15-day time periods
# date
# treatment_tseries_id: Focal product id (anonymized)
# treatment_group: Product category
# outcome_tseries_id: Recommended product id (anonymized)
# treatment_val: Visits to focal product (rescaled)
# outcome_val: Referral visits to recommended product
# aux_outcome_val: Direct visits to recommended product
# Optional: Sampling for faster run.

tseries_df = recommender.log.sample  %>%
  slice(1:4500)  # optional, to reduce running time

# Optional: Splitting data based on a user-specified time-interval.
# Not implemented.
tseries_df = assign_time_periods(tseries_df, time_period = 15)

# 1a. Computing a naive estimate first using correlations.
naive_estimate_df = correlational_estimate(tseries_df,
                                           test_time_period=15,
                                           fn_add_time_period_factor = assign_time_periods)
# 1b. Aggregating to each unique treatment
agg_naive_estimate_df = aggregate_by_treatment_id(naive_estimate_df)

# 2a. Estimating causal effect using the split-door criterion.
# Available independence tests: fisher_independence_test, dcor_independence_test
causal_estimate_df = splitdoor_causal_estimate(tseries_df,
                                             fn_independence_test=dcor_independence_test,
                                             num_discrete_levels=NA,
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