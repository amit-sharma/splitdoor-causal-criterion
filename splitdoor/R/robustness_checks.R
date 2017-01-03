check_robustness_independence_threshold <- function(tseries_df, 
                                                    independence_thresholds=c(0(0.001, 0.005, 0.01, 0.05, 0.10, 0.15, 0.20),
                                                    do_plot=TRUE,
                                                    ..){
  mean_estimates_df_list = lapply(independence_thresholds,
        function(indep_thres){
          ctr_x = splitdoor_causal_estimate(tseries_df, 
                    independence_threshold=indep_thres,
                    ..);
          average_estimates = summarize(ctr_x,
                                        indep_thres,
                                        mean(py1_x1),
                                        #mean(py1_x1),
                                        sd(py1_x1),
                                        #sd(py1_x1),
                                        n()
                                        #nrow(ctr_x_indep)
                                        #                                             mean(ctr_xy_zeroyd$py1_x1),
                                        #                                             mean(ctr_x_zeroyd$py1_x1)
                                      )
           
        })
  mean_estimates_by_thres = rbind(estimates_df_list)
  if(do_plot){
    p_est = plot_estimate_by_indep_threshold(mean_estimates_by_thres)
    p_num = plot_num_splitdoors_by_indep_threshold(mean_estimates_by_thres)
    #TODO check if cowplot is installed and give a warning, include in suggests for package
    plot_grid(p_num, p_est, labels=c("Split-door Estimates", "Number of Valid Split-door Pairs"), ncol = 2, nrow = 1)
  }
  return(mean_estimates_by_thres)
  
}