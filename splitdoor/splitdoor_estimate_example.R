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
# treatment_tseries_id: ID for each treatment
# treatment_group: Category of treatment
# outcome_tseries_id: ID of the corresponding outcome
# treatment_val: Time-series for values of the treatment
# outcome_val: Time-series for "Referred" part of the outcome
# aux_outcome_val: Time-series for "Direct" part of the outcome

# Optional: Sampling 100 treatments to reduce run-time.
top_categories = count(recommender.log.sample, treatment_group) %>% arrange(-n) %>% slice(1:2)
sample_treatment_ids = filter(recommender.log.sample, treatment_group %in% top_categories$treatment_group) %>%
  distinct(treatment_tseries_id) %>% sample_n(100)
tseries_df = filter(recommender.log.sample, treatment_tseries_id %in% sample_treatment_ids$treatment_tseries_id) 

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
                                         by_group=TRUE, estimate_colname="agg_causal_estimate",
                                         min_group_frequency = 10)

# 3b. Visualizing valid split-door <treatment, outcome> pairs
inspect_splitdoor_pairs(filter(causal_estimate_df,pass_splitdoor_criterion),
                        tseries_df,
                        n_treatment_outcome_pairs = 4)

#3c. Checking robustness of the split-door estimate to
#    change in threshold for independence test.
check_robustness_independence_threshold(tseries_df,
                                        independence_thresholds = c(0.05,0.01))


# 3d. Checking whether distribution of sample treatments selected by the
#     split-door criterion is similar to the overall distribution over treatment groups.
check_distribution_splitdoors_by_group(causal_estimate_df)
