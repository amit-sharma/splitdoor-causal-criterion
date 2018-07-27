# Split-door causal criterion: Automatic search for natural experiments

Split-door criterion provides a method for identifying causal effect between two time-series.
This repository provides an R package for calculating causal effects from observational data using the split-door criterion.

For more details, see: 
Split-door criterion: Identification of causal effects through auxiliary
outcomes. Amit Sharma, Jake Hofman, Duncan Watts.
[https://arxiv.org/abs/1611.09414](https://arxiv.org/abs/1611.09414)

## Installation
Use devtools to install the package. 
```r
install_packages("devtools")
devtools::install_github("amit-sharma/splitdoor-causal-criterion")
```

For a quick introduction to applying the split-door criterion, check out
'splitdoor_estimate_example.R'. 

```r
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

tseries_df = recommender.log.sample

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
```

The package also includes methods for checking robustness of the obtained estimate.
```r
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
```
