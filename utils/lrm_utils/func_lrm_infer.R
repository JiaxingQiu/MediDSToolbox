lrm_infer <-  function(
  df, 
  df_org, 
  dict_df, 
  fml, 
  cluster_col, 
  penalty,
  num_col2=NULL,
  fold_risk=FALSE
){
  
  
  dd <- datadist(df)
  options(datadist=dd, na.action=na.omit)
  mdl_final <- NULL
  while(is.null(mdl_final)){
    print(penalty)
    try({
      mdl_final <- rms::robcov(rms::lrm(as.formula(fml),x=TRUE, y=TRUE, data=df, penalty=penalty),cluster=df[,cluster_col])
    },TRUE)
    penalty <- penalty - 0.5
  }
  
  y_col <- strsplit(as.character(fml), " ~")[[1]][1]
  # global mean of outcome
  base_mean <- mean(df_org[,y_col],na.rm=TRUE)
  
  # --- inference ---- (1d marginal effects)
  eff_plot <- ggplot(rms::Predict(mdl_final, fun=plogis), anova=anova(mdl_final), pval=TRUE, size.anova=2, sepdiscrete='list')
  if(fold_risk){
    # redefine mapping funtion
    foldrisk <- function(y_logodds){
      y_fold_risk <- plogis(y_logodds)/base_mean
      return(y_fold_risk)
    }
    eff_plot <- ggplot(rms::Predict(mdl_final, fun=foldrisk), anova=anova(mdl_final), pval=TRUE, size.anova=2, sepdiscrete='list')
  }
  
  # --- prediction over time ---
  time_pred_plot <- NULL
  
  df_org$y_hat <- logit2prob(predictrms(mdl_final, newdata = df_org))
  df_org$rel_risk <- df_org$y_hat / base_mean
  df_org$y_true <- as.factor(df_org[,y_col])
  time_pred_plot <- ggplot(df_org, aes(x=rel_time, y=rel_risk, group=y_true, fill=y_true))+
    stat_summary(geom = "line", fun = mean) +
    stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
    ylab("relative risk") +
    xlab("time to events") +
    scale_fill_discrete(name = y_col)+
    theme(legend.position="top")
  
  
  
  
  nrow <- 3
  bplot_obj_all <- NULL
  # --- inference ---- (2d marginal effects)
  if (!is.null(num_col2)){
    if(grepl(num_col2, strsplit(as.character(fml), " ~")[[1]][2])){
      for (num_col1 in colnames(df)){
        if (dict_df$type[which(dict_df$varname==num_col1)]!="num") next
        if (num_col1==num_col2) next
        if(grepl(num_col1, strsplit(as.character(fml), " ~")[[1]][2])){
          df$num_col1 <- df[,num_col1]
          df$num_col2 <- df[,num_col2]
          label(df$num_col1) <- dict_df$label[which(dict_df$varname==num_col1)]
          label(df$num_col2) <- dict_df$label[which(dict_df$varname==num_col2)]
          
          fml_tmp <- gsub(num_col2, "num_col2", gsub(num_col1, "num_col1", fml))
          dd <- datadist(df)
          options(datadist=dd, na.action=na.omit)
          mdl_tmp <- rms::robcov(rms::lrm(as.formula(fml_tmp),x=TRUE, y=TRUE, data=df, penalty = penalty), cluster = df[,cluster_col])
          bplot_obj <- rms::bplot(rms::Predict(mdl_tmp, num_col1, num_col2, fun=plogis),
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
      eff_plot_final <- ggpubr::ggarrange(eff_plot_final, eff_plot$discrete,nrow=2,ncol=1, heights = c(nrow-1,1))
    }
  }
  if(is.null(eff_plot$continuous) & is.null(eff_plot$discrete)){
    eff_plot_final <-  eff_plot
  }
  
  anova_obj <- anova(mdl_final)
  
  return(list("effect_plot_final"=eff_plot_final,
              "time_pred_plot" = time_pred_plot,
              "mdl_obj"= mdl_final))
  
  
}

