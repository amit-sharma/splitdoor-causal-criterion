library(splitdoor)
#library(dplyr)

load("../../binary-iv/Rscripts/splitdoor/recommender.log")
tseries_df = recommender.log

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

compare_splitdoor_correlational_estimate(agg_causal_estimate_df, agg_naive_estimate_df,
                                         by_group=TRUE, estimate_colname="agg_causal_estimate")

check_robustness_independence_threshold(tseries_df)

ordered_tgroups_vec = c("Book","eBooks","Toy", "Home","Sports","Apparel",
                       "Wireless","DVD", "Health and Beauty","Shoes")
check_distribution_splitdoors_by_group(causal_estimate_df,
                                       ordered_treatment_groups_vec = ordered_tgroups_vec)

valid_examples = data.frame(
  date_factor = c("2013-10.1","2013-11.1"),
  treatment_tseries_id = c(10798,42836),#c("B00E9N67MO","B0090SI56Y"),
  outcome_tseries_id = c(20407, 772)#c("B00DZZ2KYQ", "B0034G4P80")
)

inspect_splitdoor_pairs(inner_join(causal_estimate_df,valid_examples,
                                   by=c("date_factor","treatment_tseries_id","outcome_tseries_id")),
                        tseries_df)

invalid_examples = data.frame(
    date_factor = c("2013-12.1","2013-09.1"),
    treatment_tseries_id = c(44085,42728),
    outcome_tseries_id = c(39019, 42879)
  )

inspect_splitdoor_pairs(inner_join(causal_estimate_df,invalid_examples, by=c("date_factor","treatment_tseries_id","outcome_tseries_id")),
                        tseries_df)
