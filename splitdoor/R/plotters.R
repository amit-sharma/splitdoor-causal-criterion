plot_ctr_compare <- function(compare_estimates, filter_out_zeroyd=TRUE, yname="Click-through Estimate",ylim_max=0.2) {
  compare_estimates = compare_estimates %>%
    filter(!is.na(num_products.x), !is.na(num_products.y)) %>%
    #filter(product_group %in% ordered_pgroup_vec)
    filter(num_products.x >= 30, num_products.y >= 30) %>%
    arrange(-num_products.x) %>%
    slice(1:10)
  
  #compare_estimates$product_group2 = factor(compare_estimates$product_group, levels=ordered_pgroup_vec)
  compare_estimates$product_group2 = factor(compare_estimates$product_group, levels=compare_estimates$product_group)
  #compare_estimates$product_group2 = factor(compare_estimates$product_group, 
  #                                          levels=compare_estimates[order(-compare_estimates$num_products.x), "product_group"]$product_group)
  melted_c_estimates = melt(compare_estimates, 
                            id.vars=c("product_group2", "num_products.x", "num_products.y", 
                                      "sd_estimate.x", "sd_estimate.y"), 
                            measure.vars=c("mean_estimate.y",
                                           "mean_estimate.x")
  )
  melted_c_estimates = melted_c_estimates %>% 
    mutate(num_products = ifelse(variable=="mean_estimate.x", num_products.x, num_products.y),
           sd_estimate = ifelse(variable=="mean_estimate.x", sd_estimate.x, sd_estimate.y),
           se_error = sd_estimate/sqrt(num_products))
  # if(filter_out_zeroyd){
  #    melted_c_estimates = filter(melted_c_estimates, variable!="mean_estimate.y")
  # }
  p=ggplot(melted_c_estimates, aes(x=product_group2, y=value, group=variable, color=variable,linetype=variable)) + 
    geom_line() + 
    geom_point(aes(size=num_products)) + 
    geom_errorbar(aes(ymin=value - se_error, ymax=value+se_error))+
    scale_color_discrete(name="Method", labels=c("Split-door", "Naive"))+
    scale_linetype_discrete(name="Method", labels=c("Split-door", "Naive"))+
    scale_size_continuous(name="Number of products")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=16),
          axis.title.y=element_text(size=16),
          legend.key.width=unit(1.2, "cm")
    ) +
    ylim(0, ylim_max) +
    xlab("") +ylab(yname)
  print(p)
  ggsave("causal_ctr_naive.pdf", width=6, height=4.5)
  return(p)
}

show_only_date_labeller <- function(labels_df) {
  return(select(labels_df, 1))
}

plot_sample_xy_pairs <- function(asin_targets_visits, do_sample=TRUE){
  if (do_sample){
    d_asin_targets_visits = distinct(asin_targets_visits, date_factor, asin_id) %>%
      sample_n(20) %>% 
      select(date_factor, asin_id)
  } else {
    d_asin_targets_visits = distinct(asin_targets_visits, date_factor, asin_id) %>%
      select(date_factor, asin_id)
  }
  f_asin_targets_visits = inner_join(asin_targets_visits, d_asin_targets_visits, by=c("date_factor", "asin_id"))
  asin_targets_visits_bydate = group_by(f_asin_targets_visits, date_factor, asin_id, target_asin_id, date) %>%
    summarize(#x_visits_sum = sum(binary_x_visits, na.rm=TRUE),
      x_visits = first(x_visits),
      #num_clickthroughs_sum = sum(binary_num_clickthroughs),
      num_clickthroughs = first(num_clickthroughs),
      discrete_yd = first(discrete_yd),
      yd_visits = first(yd_visits)) %>% 
    ungroup()  # CAUTION CAUTION : ADDED ON OCT 13 2016 FOR PLOTTING BEWARE
  #melted_df = melt(asin_targets_visits_bydate)
  p1=ggplot(asin_targets_visits_bydate, aes(x=date, group=target_asin_id)) + 
    geom_line(aes(y=x_visits), color="black") +
    geom_line(aes(y=yd_visits), color="blue") +
    facet_wrap(~date_factor+asin_id+target_asin_id, scales="free", labeller=show_only_date_labeller)+
    xlab("") + ylab("Number of page visits")
    theme_bw()+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16, face="bold")) 
  print(p1)
  return(asin_targets_visits_bydate)
  
}

compare_distribution <- function(alldays_iv_data, indep_asin_targets_visits, zero_yd_asin_targets_visits){
  num_visits_all = group_by(alldays_iv_data, asin_id) %>%
    summarize(total_visits = sum(x_visits)) %>% ungroup()
  indep_asins = unique(indep_asin_targets_visits$asin_id)
  
  num_visits_indep = filter(num_visits_all, asin_id %in% indep_asins)
  ggplot(num_visits_all, aes(x=total_visits)) + geom_density() + scale_x_log10() +
    geom_density(data=num_visits_indep, aes(x=total_visits), color="red") +
    xlab("Total visits to focal product")
  
  num_yd_visits_all = group_by(alldays_iv_data, target_asin_id) %>%
    summarize(total_yd_visits = sum(yd_visits)) %>% ungroup()
  indep_targetasins = unique(indep_asin_targets_visits$target_asin_id)
  zeroyd_targetasins = unique(zero_yd_asin_targets_visits$target_asin_id)
  
  num_yd_visits_indep = filter(num_yd_visits_all, target_asin_id %in% c(indep_targetasins))#, zeroyd_targetasins))
  ggplot(filter(num_yd_visits_all, total_yd_visits!=0), aes(x=total_yd_visits)) + geom_density() + scale_x_log10()+
    geom_density(data=num_yd_visits_indep, aes(x=total_yd_visits), color="red") +
    xlab("Total visits to recommended product with indep strategy")
  
  
}

placehold_plotter <- function(){
  mean_est_df = as.data.frame(mean_est_valmat) %>% filter(!is.na(V1))
  #melted_df = melt(mean_est_df, 
  #                 id.vars=c("V2", "V6"), 
  #                 measure.vars=c("V4"))
  mean_est_df = mean_est_df %>% 
    mutate(se_est = V5/sqrt(V7))
  
}

plot_estimate_by_indep_threshold <- function(mean_est_df, saveto_filename=NULL){
  p1=ggplot(mean_est_df,
            aes(x=1-V1, y=V3)) +
    geom_line() + geom_point()+
    geom_vline(xintercept=0.95, linetype=2,alpha=0.4)+
    geom_errorbar(aes(ymin=V3-se_est, ymax=V3+se_est)) +
    ylim(0, 0.04) +
    ylab("Causal click-through estimate") + xlab("Likelihood of independence") +
    theme_bw() +
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16, face="bold"))
  if(!is.null(saveto_filename)){
    ggsave(saveto_filename, plot=p1, width=6, height=4.5 )
  }
  return(p1)
}
 
plot_num_splitdoors_by_indep_threshold <- function(mean_est_df, saveto_filename=NULL) {
  p1=ggplot(mean_est_df,
            aes(x=1-V1, y=V6)) +
    geom_line() + geom_point()+
    geom_vline(xintercept=0.95, linetype=2,alpha=0.4)+
    ylab("Number of natural experiments") + xlab("Likelihood of independence") +
    ylim(0, 35000)+
    theme_bw() +
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16, face="bold"))
  if(!is.null(saveto_filename)){
    ggsave(saveto_filename, plot=p1, width=6, height=4.5 )
  }
  return(p1)
}