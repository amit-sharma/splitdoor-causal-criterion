#' @import dplyr ggplot2

plot_ctr_compare <- function(compare_estimates, filter_out_zeroyd=TRUE, yname="Click-through Estimate",ylim_max=0.2) {
  compare_estimates = compare_estimates %>%
    filter(!is.na(num_products.x), !is.na(num_products.y)) %>%
    #filter(treatment_group %in% ordered_pgroup_vec)
    filter(num_products.x >= 30, num_products.y >= 30) %>%
    arrange(-num_products.x) %>%
    slice(1:10)

  #compare_estimates$treatment_group2 = factor(compare_estimates$treatment_group, levels=ordered_pgroup_vec)
  compare_estimates$treatment_group2 = factor(compare_estimates$treatment_group, levels=compare_estimates$treatment_group)
  #compare_estimates$treatment_group2 = factor(compare_estimates$treatment_group,
  #                                          levels=compare_estimates[order(-compare_estimates$num_products.x), "treatment_group"]$treatment_group)
  melted_c_estimates = reshape2::melt(compare_estimates,
                            id.vars=c("treatment_group2", "num_products.x", "num_products.y",
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
  p=ggplot(melted_c_estimates, aes(x=treatment_group2, y=value, group=variable, color=variable,linetype=variable)) +
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
}

show_only_date_labeller <- function(labels_df) {
  return(select(labels_df, 1))
}

# plot_sample_xy_pairs <- function(asin_targets_visits, do_sample=TRUE){
#   if (do_sample){
#     d_asin_targets_visits = distinct(asin_targets_visits, date_factor, treatment_tseries_id) %>%
#       sample_n(20) %>%
#       select(date_factor, treatment_tseries_id)
#   } else {
#     d_asin_targets_visits = distinct(asin_targets_visits, date_factor, treatment_tseries_id) %>%
#       select(date_factor, treatment_tseries_id)
#   }
#   f_asin_targets_visits = inner_join(asin_targets_visits, d_asin_targets_visits, by=c("date_factor", "treatment_tseries_id"))
#   asin_targets_visits_bydate = group_by(f_asin_targets_visits, date_factor, treatment_tseries_id, outcome_tseries_id, date) %>%
#     summarize(#treatment_val_sum = sum(binary_treatment_val, na.rm=TRUE),
#       treatment_val = first(treatment_val),
#       #outcome_val_sum = sum(binary_outcome_val),
#       outcome_val = first(outcome_val),
#       discrete_yd = first(discrete_yd),
#       aux_outcome_val = first(aux_outcome_val)) %>%
#     ungroup()  # CAUTION CAUTION : ADDED ON OCT 13 2016 FOR PLOTTING BEWARE
#   #melted_df = melt(asin_targets_visits_bydate)
#   p1=ggplot(asin_targets_visits_bydate, aes(x=date, group=outcome_tseries_id)) +
#     geom_line(aes(y=treatment_val), color="black") +
#     geom_line(aes(y=aux_outcome_val), color="blue") +
#     facet_wrap(~date_factor+treatment_tseries_id+outcome_tseries_id, scales="free", labeller=show_only_date_labeller)+
#     xlab("") + ylab("Number of page visits")
#     theme_bw()+
#     theme(axis.text = element_text(size=16),
#           axis.title = element_text(size=16, face="bold"))
#   print(p1)
#   return(asin_targets_visits_bydate)
#
# }

compare_distribution <- function(alldays_iv_data, indep_asin_targets_visits, zero_yd_asin_targets_visits){
  num_visits_all = group_by(alldays_iv_data, treatment_tseries_id) %>%
    summarize(total_visits = sum(treatment_val)) %>% ungroup()
  indep_asins = unique(indep_asin_targets_visits$treatment_tseries_id)

  num_visits_indep = filter(num_visits_all, treatment_tseries_id %in% indep_asins)
  ggplot(num_visits_all, aes(x=total_visits)) + geom_density() + scale_x_log10() +
    geom_density(data=num_visits_indep, aes(x=total_visits), color="red") +
    xlab("Total visits to focal product")

  num_aux_outcome_val_all = group_by(alldays_iv_data, outcome_tseries_id) %>%
    summarize(total_aux_outcome_val = sum(aux_outcome_val)) %>% ungroup()
  indep_targetasins = unique(indep_asin_targets_visits$outcome_tseries_id)
  zeroyd_targetasins = unique(zero_yd_asin_targets_visits$outcome_tseries_id)

  num_aux_outcome_val_indep = filter(num_aux_outcome_val_all, outcome_tseries_id %in% c(indep_targetasins))#, zeroyd_targetasins))
  ggplot(filter(num_aux_outcome_val_all, total_aux_outcome_val!=0), aes(x=total_aux_outcome_val)) + geom_density() + scale_x_log10()+
    geom_density(data=num_aux_outcome_val_indep, aes(x=total_aux_outcome_val), color="red") +
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
            aes(x=independence_threshold, y=mean_causal_estimate)) +
    geom_line() + geom_point()+
    geom_vline(xintercept=0.95, linetype=2,alpha=0.4)+
    geom_errorbar(aes(ymin=V3-se_causal_estimate, ymax=V3+se_causal_estimate)) +
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
            aes(x=independence_threshold, y=num_unique_treatments)) +
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

## 1. PLOTTING DISTRIBUTION OF FOCAL PRODUCTS BY PRODUCT GROUP
plot_pgroup_distribution <- function(pgroup_plotdata){
  p=ggplot(pgroup_plotdata, aes(x=treatment_group, y=frac_asins, group=dataset, color=dataset, linetype=dataset))
  filename="distr_by_pgroup_asins.pdf"
  ylabel="Fraction of products"

  p_full = p +geom_line() + geom_point(aes(shape=dataset),size=3) +
    xlab("") + ylab(ylabel) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
          axis.title.y = element_text(size=16),
          legend.title=element_blank(),
          legend.position=c(0.74, 0.8),
          legend.key.width=unit(1.6, "cm"))
  #p_full2 = p_full + guides(shape = guide_legend(override.aes = list(shape=1)))
  p_full2=p_full
  print(p_full2)
  ggsave(filename=filename, plot=p_full2, width=6, height=5)
  return(pgroup_plotdata)
}


## 2. PLOTTING VALID AND INVALID EXAMPLES OF SPLITDOORS
plot_treatment_aux_outcome_timeseries <- function(plot_df){
  label_names = c("Focal product visits (X)", "Direct visits to Recommended Product (Y_D)")
  melted_plot_df = reshape2::melt(plot_df, id.vars=c("date_factor", "date", "treatment_tseries_id", "outcome_tseries_id"), measure.vars=c("treatment_val", "aux_outcome_val"),
                        variable.name = "visit_type", value.name = "num_visits_perday") %>%
    mutate(visit_type_string=ifelse(visit_type=="treatment_val", label_names[1], label_names[2])) %>%
    mutate(visit_type_string = factor(visit_type_string, levels=label_names))
  p1=ggplot(melted_plot_df, aes(x=date, y=num_visits_perday, group=visit_type,color=visit_type_string)) +
    geom_line(aes(linetype=visit_type_string)) +
    facet_wrap(~date_factor+treatment_tseries_id+outcome_tseries_id, scales="free", labeller=show_only_date_labeller)+
    xlab("") + ylab("Number of page visits")+
    scale_color_discrete(labels=expression(Focal~product~visits~(X), Direct~visits~to~recommended~product~(Y[D])))+
    scale_linetype_discrete(labels=expression(Focal~product~visits~(X), Direct~visits~to~recommended~product~(Y[D])))+
    theme_bw()+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16, face="bold"),
          legend.title=element_blank(),
          legend.text=element_text(size=13),
          legend.position="bottom",
          legend.key.width=unit(1.2, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_blank())
  print(p1)
}
