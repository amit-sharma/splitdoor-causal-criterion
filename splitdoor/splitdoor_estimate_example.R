library(splitdoor)
library(dplyr)

load("../../binary-iv/Rscripts/splitdoor/recommender.log")
tseries_df = recommender.log  %>%
  slice(1:30000)

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

test <- function(){
dep_visits = filter(dep_asin_targets_visits, is_constant_demand==FALSE)
  #invalid_examples = data.frame(
    #date_factor = c("2013-12.1","2013-09.1" ,"2014-03.1", "2013-09.0"),
    #asin_id = c("B001OXRTX8","B00DJZR4A2" , "B00H7KJTMG", "B009XNBFJK"),
    #target_asin_id = c("B003WFKOSS", "B00D6DVSAE", "B00HCR8VEQ","B007M50PTM")
  #)

  # for paper, restricting to two examples
  invalid_examples = data.frame(
    date_factor = c("2013-12.1","2013-09.1"), #,"2014-03.1", "2013-09.0"),
    asin_id = c("B001OXRTX8","B00DJZR4A2"),# , "B00H7KJTMG", "B009XNBFJK"),
    target_asin_id = c("B003WFKOSS", "B00D6DVSAE")#, "B00HCR8VEQ","B007M50PTM")
  )
  subset_data = inner_join(dep_visits, invalid_examples,
                           by=c("date_factor", "asin_id", "target_asin_id"))


  ans_df = filter(indep_asin_targets_visits,
                  date_factor=="2013-10.1", treatment_tseries_id=="B00E9N67MO", outcome_tseries_id=="B00DZZ2KYQ")
    ans_df = rbind(ans_df, filter(indep_asin_targets_visits, date_factor=="2013-11.1", treatment_tseries_id=="B0090SI56Y", outcome_tseries_id=="B0034G4P80"))
    # removing the next two for paper
    #ans_df = rbind(ans_df, filter(indep_asin_targets_visits, date_factor=="2014-01.0", treatment_tseries_id=="B00GOFP9NS", outcome_tseries_id=="B00GV1V8HQ"))
    #ans_df = rbind(ans_df, filter(indep_asin_targets_visits, date_factor=="2013-11.1", treatment_tseries_id=="B005I2GCXY", outcome_tseries_id=="B001PGP34G"))

}
