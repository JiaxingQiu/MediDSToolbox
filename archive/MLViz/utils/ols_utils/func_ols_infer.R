ols_infer <-  function(df, df_org, dict_df, fml, cluster_col, num_col2="ga_days"){
  
  df <- assign.dict(df, dict_df)
  dd <- datadist(df)
  options(datadist=dd, na.action=na.omit)
  mdl_final <- rms::robcov(rms::ols(as.formula(fml),x=TRUE, y=TRUE, data=df),cluster=df[,cluster_col])
  
  # --- inference ---- (1d marginal effects)
  eff_plot <- ggplot(rms::Predict(mdl_final), anova=anova(mdl_final), pval=TRUE, size.anova=2, sepdiscrete='list')
  
  # --- prediction over time ---
  time_pred_plot <- NULL
  y_col <- strsplit(as.character(fml), " ~")[[1]][1]
  base_mean <- mean(df_org[,y_col],na.rm=TRUE)
  df_org$base_mean <- NA
  for (t in unique(df_org$rel_time)){
    df_org[which(df_org$rel_time==t),'base_mean'] <- mean(df_org[which(df_org$rel_time==t),y_col],na.rm=TRUE)
  }
  df_org$y_hat <- predictrms(mdl_final, newdata = df_org)
  df_org$rel_risk <- df_org$y_hat / df_org$base_mean
  time_pred_plot <- ggplot(df_org, aes(x=rel_time, y=rel_risk))+
    stat_summary(geom = "line", fun = mean) +
    stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
    ylab("response ratio") +
    xlab("relative time before event")
  
  
  
  # --- inference ---- (2d marginal effects)
  bplot_obj_all <- NULL
  if (!is.null(num_col2)){
    if(grepl(num_col2, strsplit(as.character(fml), " ~")[[1]][2])){
      for (num_col1 in colnames(df)){
        if (length(dict_df$type[which(dict_df$varname==num_col1)]!="num")==0)  next
        if (dict_df$type[which(dict_df$varname==num_col1)]!="num")  next
        if (num_col1==num_col2) next
        if(grepl(num_col1, strsplit(as.character(fml), " ~")[[1]][2])){
          df$num_col1 <- df[,num_col1]
          df$num_col2 <- df[,num_col2]
          label(df$num_col1) <- dict_df$label[which(dict_df$varname==num_col1)]
          label(df$num_col2) <- dict_df$label[which(dict_df$varname==num_col2)]
          
          fml_tmp <- gsub(num_col2, "num_col2", gsub(num_col1, "num_col1", fml))
          dd <- datadist(df)
          options(datadist=dd, na.action=na.omit)
          mdl_tmp <- rms::robcov(rms::ols(as.formula(fml_tmp),x=TRUE, y=TRUE, data=df), cluster = df[,cluster_col])
          bplot_obj <- rms::bplot(rms::Predict(mdl_tmp, num_col1, num_col2, fun=est_pctl),
                                  ylabrot=90,adj.subtitle=FALSE)
          if (is.null(bplot_obj_all)) {
            bplot_obj_all <- bplot_obj
            nrow <- 1
            next
          }else{
            nrow <- nrow + 1
            bplot_obj_all <- ggpubr::ggarrange(bplot_obj_all,bplot_obj,nrow=2,ncol=1,heights = c(nrow-1,1))
          }
        }
      }
    }
  }
  if (!is.null(bplot_obj_all)){ eff_plot$continuous <- bplot_obj_all }
  # rearrange eff_plot object
  eff_plot_final <- NULL
  if (!is.null(eff_plot$continuous)){
    if (is.null(eff_plot_final)) eff_plot_final <- eff_plot$continuous
  }
  if (!is.null(eff_plot$discrete)) {
    if (is.null(eff_plot_final)) {eff_plot_final <- eff_plot$discrete}
    else {
      eff_plot_final <- ggpubr::ggarrange(eff_plot_final, eff_plot$discrete,nrow=2,ncol=1, heights = c(1.5,1))
    }
  }
  if(is.null(eff_plot$continuous) & is.null(eff_plot$discrete)){
    eff_plot_final <-  eff_plot
  }
  
  
  
  # anova_df <- as.data.frame(plot(anova(mdl_final)))
  # colnames(anova_df)<-c("chi_square")
  # anova_df$varname <- rownames(anova_df)
  # 
  # anova_plot <- anova_df %>%
  #   mutate(varname=forcats::fct_reorder(varname, desc(chi_square))) %>% 
  #   ggplot( aes(x=chi_square,  y=varname))+
  #   geom_point()+
  #   #scale_fill(trans = 'reverse')+
  #   theme(axis.title.y=element_blank())+
  #   ggtitle("ANOVA (F Statistics)") +
  #   xlab("Chi-Square - d.f.")
  # 
  
  return(list("effect_plot_final"=eff_plot_final, 
              "time_pred_plot" = time_pred_plot,
              "mdl_obj"= mdl_final))
  
  
}

