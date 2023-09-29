

uni_signature_illness <- function(plot_df, layout_ncol){
  
  
  if(!"x_label" %in%colnames(plot_df) ){
    plot_df$x_label <- plot_df$x_name
  }
  plot_df_cuts <- NULL
  plot_df_base <- NULL
  
  # prepare red curves
  plot_df$y_hat_red <- ifelse(plot_df$y_logodds_signif==1,1,NA)*plot_df$y_hat
  # get baseline hline for x
  plot_df_base <- plot_df %>% group_by(x_label) %>% summarise(y_hat_baseline = mean(y_hat_baseline,na.rm=TRUE)) %>% as.data.frame()
  
  
  plot_obj <- ggplot(data = plot_df, aes(x=x_raw, y=y_hat))+
    theme_bw() +
    geom_line(color="white")+
    geom_line(aes(y=y_hat_red), color="red") +
    geom_hline(data = plot_df_base, mapping = aes(yintercept=y_hat_baseline), linetype="solid") + 
    facet_wrap(~x_label, scales = "free", ncol=layout_ncol) + #"free_x"
    geom_ribbon(aes(ymin=y_hat_lower, ymax=y_hat_upper), alpha=0.2) +
    coord_cartesian(ylim=c(min(plot_df$y_hat_red,na.rm=TRUE)-0.1,max(plot_df$y_hat_red,na.rm=TRUE)+0.1))
  
  
  # add upper and lower cuts for x value
  if(n_distinct(plot_df$x_pctl)==201 & !(2.5 %in% unique(plot_df$x_pctl) & 97.5 %in% unique(plot_df$x_pctl)) ){
    plot_df$x_pctl <- (plot_df$x_pctl-1)/2
  }
  if(2.5 %in% unique(plot_df$x_pctl) & 97.5 %in% unique(plot_df$x_pctl)){
    plot_df_cuts <- plot_df %>% group_by(x_label) %>%
      summarise(x_raw_qt_l =unique(x_raw[x_pctl==2.5]),
                x_raw_qt_l_txt = "2.5th",
                x_raw_qt_u=unique(x_raw[x_pctl==97.5]),
                x_raw_qt_u_txt = "97.5th") %>% 
      as.data.frame()
    plot_obj <- plot_obj + 
      geom_vline(data=plot_df_cuts, mapping = aes(xintercept=x_raw_qt_l), linetype="dashed")+
      geom_text(data=plot_df_cuts, mapping=aes(x=x_raw_qt_l, y=max(plot_df$y_hat_red,na.rm=TRUE)-0.05, label=x_raw_qt_l_txt),hjust=0)+
      geom_vline(data=plot_df_cuts, mapping = aes(xintercept=x_raw_qt_u),linetype="dashed")+
      geom_text(data=plot_df_cuts, mapping=aes(x=x_raw_qt_u, y=max(plot_df$y_hat_red,na.rm=TRUE)-0.05, label=x_raw_qt_u_txt),hjust=1)
    
  }else{
    print("2.5th and 97.5th not found")
  }
  
  
  # merge additional lines back to data frame
  if(!is.null(plot_df_base)){
    plot_df <- merge(plot_df[,c("x_label",setdiff(colnames(plot_df), colnames(plot_df_base)))], plot_df_base, all.x = TRUE, all.y = FALSE)
    
  }
  if(!is.null(plot_df_cuts)){
    plot_df <- merge(plot_df[,c("x_label",setdiff(colnames(plot_df), colnames(plot_df_cuts)))], plot_df_cuts, all.x = TRUE, all.y = FALSE)
  }
  
  return(list(plot_obj = plot_obj,
              plot_df = plot_df))
}

