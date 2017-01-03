breakup_est_by_productgroup <- function(ctr_df, f_product_info, outcome_colname="py1_x1"){
  ctr_df_with_cat = inner_join(ctr_df, f_product_info, by="asin_id")
  cond_ests = group_by(ctr_df_with_cat, 
                       product_group) %>%
    summarize_(
      num_products = interp(~length(var), var=as.name(outcome_colname)),
      mean_estimate = interp(~mean(var), var=as.name(outcome_colname)),
      sd_estimate = interp(~sd(var), var=as.name(outcome_colname))
    ) 
  return(cond_ests)
}
