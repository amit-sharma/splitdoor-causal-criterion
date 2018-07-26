# Reproducing results in the original paper
# Split-door criterion for causal identification: Automatic search for
# natural experiments. Amit Sharma, Jake Hofman, Duncan Watts.
# https://arxiv.org/abs/1611.09414

# Latest version of this code available at: https://www.github.com/amit-sharma/splitdoor-causal-criterion

# WARN: Note results will be different because the dataset provided is a subset of the full dataset.
library(splitdoor)
library(dplyr)
library(ggplot2)

## Loading data from a recommender system's logs
load("recommender.log.sample")
tseries_df = recommender.log.sample

## Computing estimates

# 1a. Computing a naive estimate first using correlations.
naive_estimate_df = correlational_estimate(tseries_df,
                                           test_time_period=15,
                                           fn_add_time_period_factor = assign_time_periods)
# 1b. Aggregating to each focal product.
agg_naive_estimate_df = aggregate_by_treatment_id(naive_estimate_df)

# 2a. Estimating causal effect using the split-door criterion.
causal_estimate_df = splitdoor_causal_estimate(tseries_df,
                                             fn_independence_test=dcor_independence_test,
                                             num_discrete_levels=NA,
                                             independence_threshold=0.05)
# 2b. Aggregating to each focal product.
agg_causal_estimate_df = aggregate_by_treatment_id(causal_estimate_df)

## Now generating figures from the paper.

# Figure 7
check_robustness_independence_threshold(tseries_df)

# Figure 8
compare_splitdoor_correlational_estimate(agg_causal_estimate_df, agg_naive_estimate_df,
                                         by_group=TRUE, estimate_colname="agg_causal_estimate")

# Figure 9
ordered_tgroups_vec = c("Book","eBooks","Toy", "Home","Sports","Apparel",
                        "Wireless","DVD", "Health and Beauty","Shoes")
check_distribution_splitdoors_by_group(causal_estimate_df,
                                       ordered_treatment_groups_vec = ordered_tgroups_vec)
