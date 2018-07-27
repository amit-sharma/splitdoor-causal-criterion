#' @import ggplot2
NULL

#' Compare correlational and split-door estimates for different treatment groups.
#'
#' @param compare_estimates
#' @param filter_out_zeroyd
#' @param yname
#' @param ylim_max
#'
#' @return
#' @export
#'
#' @examples
plot_ctr_compare <- function(compare_estimates, 
                             min_group_frequency, filter_out_zeroyd=TRUE, 
                             yname="Click-through Estimate",ylim_max=0.2) {
  compare_estimates = compare_estimates %>%
    filter(!is.na(num_products.x), !is.na(num_products.y)) %>%
    filter(num_products.x >= min_group_frequency, num_products.y >= min_group_frequency) %>%
    arrange(-num_products.x) %>%
    slice(1:10)

  compare_estimates$treatment_group2 = factor(compare_estimates$treatment_group, levels=compare_estimates$treatment_group)
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


#' Plot mean split-door estimate for different values of the independence threshold.
#'
#' @param mean_est_df
#' @param saveto_filename
#'
#' @return
#' @export
#'
#' @examples
plot_estimate_by_indep_threshold <- function(mean_est_df, saveto_filename=NULL){
  p1=ggplot(mean_est_df,
            aes(x=independence_threshold, y=mean_causal_estimate)) +
    geom_line() + geom_point()+
    geom_vline(xintercept=0.95, linetype=2,alpha=0.4)+
    geom_errorbar(aes(ymin=mean_causal_estimate-se_causal_estimate, ymax=mean_causal_estimate+se_causal_estimate)) +
    ylim(0, 0.04) +
    xlim(0.75, 1) +
    ylab("Causal click-through estimate") + xlab("p-value") +
    theme_bw() +
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16, face="bold"))
  if(!is.null(saveto_filename)){
    ggsave(saveto_filename, plot=p1, width=6, height=4.5 )
  }
  return(p1)
}

#' Plot number of unique treatments that have at least one time-period when the split-door criterion is satisfied.
#'
#' Compare this value as the independence threshold varies.
#' @param mean_est_df
#' @param saveto_filename
#'
#' @return
#'
#' @examples
plot_num_splitdoors_by_indep_threshold <- function(mean_est_df, saveto_filename=NULL) {
  max_num_nat_experiments = max(mean_est_df$num_splitdoor_instances)*1.1
  p1=ggplot(mean_est_df,
            aes(x=independence_threshold, y=num_splitdoor_instances)) +
    geom_line() + geom_point()+
    geom_vline(xintercept=0.95, linetype=2,alpha=0.4)+
    ylab("Number of split-door instances") + xlab("p-value") +
    ylim(0, max_num_nat_experiments)+
    xlim(0.75, 1) +
    theme_bw() +
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16, face="bold"))
  if(!is.null(saveto_filename)){
    ggsave(saveto_filename, plot=p1, width=6, height=4.5 )
  }
  return(p1)
}

#' Plot distribution of treatments w.r.t. pre-specified treatment groups that have at least one valid time-period when the split-door criterion is satisfied.
#'
#' Also compares to overall distribution of treatments over treatment groups.
#' @param pgroup_plotdata
#'
#' @return
#' @export
#'
#' @examples
plot_pgroup_distribution <- function(pgroup_plotdata){
  p=ggplot(pgroup_plotdata, aes(x=treatment_group, y=frac_asins, group=dataset, color=dataset, linetype=dataset))
  filename="distr_by_pgroup_asins.pdf"
  ylabel="Fraction of products"

  p_full = p +geom_line() + geom_point(aes(shape=dataset),size=3) +
    xlab("") + ylab(ylabel) +
    ylim(0,1)+
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=16),
          axis.title.y = element_text(size=16),
          legend.title=element_blank(),
          legend.position=c(0.74, 0.8),
          legend.key.width=unit(1.6, "cm"))
  p_full2=p_full
  print(p_full2)
  ggsave(filename=filename, plot=p_full2, width=6, height=5)
  return(pgroup_plotdata)
}


#' Plot examples of <treatment,outcome> time-series.
#'
#' Used to manually inspect timeseries for <treatment,outcome> pairs returned by
#' the split-door criterion.
#' @param plot_df
#'
#' @return
#' @export
#'
#' @examples
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
