lrm_infer <-  function(
  df, 
  df_org, 
  dict_df, 
  fml, 
  cluster_col, 
  penalty,
  num_col2=NULL,
  fold_risk=TRUE,
  y_max=3
){
  
  eff_plot_final <- NULL
  fitted_eff_plot <- NULL
  mdl_final <- NULL
  
  # train the final model on the whole dataset
  dd <- datadist(df)
  options(datadist=dd, na.action=na.omit)
  while(is.null(mdl_final)){
    print(penalty)
    try({
      mdl_final <- rms::robcov(rms::lrm(as.formula(fml),x=TRUE, y=TRUE, data=df, penalty=penalty),cluster=df[,cluster_col])
    },TRUE)
    penalty <- penalty - 0.5
  }
  
  y_col <- dict_df$varname[which(dict_df$mlrole=="output")]
  x_num_cols <- dict_df$varname[which(dict_df$mlrole=="input" & dict_df$type=="num")]
  
  # global mean of outcome
  base_mean <- mean(df_org[,y_col],na.rm=TRUE)
  print(paste0("-- baseline response mean -- ", base_mean))
  
  if(!fold_risk){
    # --- inference ---- (1d marginal effects)
    # redefine mapping funtion
    plogis_fix <- function(y_logodds){
      y_prob <- plogis(y_logodds)
      y_prob[which(y_prob>y_max)] <- y_max
      return(y_prob)
    }
    eff_plot <- ggplot(rms::Predict(mdl_final, fun=plogis_fix), anova=anova(mdl_final), pval=TRUE, size.anova=2, sepdiscrete='list')
    
    # --- prepare fitted y and x to plot ---
    df_org$y_pred  <- plogis_fix(predictrms(mdl_final, newdata = df_org))
    df_org$y_true <- as.factor(df_org[,y_col])
    df_org$relative_time <- df_org$rel_time
    x_num_cols <- union("relative_time", x_num_cols)
  
    
    # --- prediction over time and each numeric variable ---
    df_plot <- data.frame()
    for (x_col in x_num_cols){
      df_plot_pred <-  df_org[,c(x_col, "y_pred", "y_true")]
      colnames(df_plot_pred) <- c("predictor_value", "y_pred", "y_true")
      df_plot_pred$predictor_name <- x_col
      df_plot <- bind_rows(df_plot, df_plot_pred)
    }
    fitted_eff_plot <- ggplot(df_plot, aes(x=predictor_value, y=y_pred, group=y_true)) +
      stat_summary(geom = "line", fun = mean, size=0.3, color='grey') +
      geom_smooth(aes(color=y_true))+
      facet_wrap(~predictor_name, ncol=3, scales = "free_x") + 
      ylab("Fitted Probability") +
      xlab("Predictor Value") +
      scale_color_discrete(name = y_col)+
      theme(legend.position="top") +
      ylim(0, min(max(df_org$y_pred),y_max) )
    
  }else{
    
    # --- inference ---- (1d marginal effects)
    # redefine mapping funtion
    foldrisk <- function(y_logodds){
      y_fold_risk <- plogis(y_logodds)/base_mean
      y_fold_risk[which(y_fold_risk>y_max)] <- y_max
      return(y_fold_risk)
    }
    eff_plot <- ggplot(rms::Predict(mdl_final, fun=foldrisk), anova=anova(mdl_final), pval=TRUE, size.anova=2, sepdiscrete='list')
    
    # --- prepare fitted y and x to plot ---
    df_org$y_pred  <- foldrisk(predictrms(mdl_final, newdata = df_org))
    df_org$y_true <- as.factor(df_org[,y_col])
    df_org$relative_time <- df_org$rel_time
    x_num_cols <- union("relative_time", x_num_cols)
    
    
    # --- prediction over time and each numeric variable ---
    df_plot <- data.frame()
    for (x_col in x_num_cols){
      df_plot_pred <-  df_org[,c(x_col, "y_pred", "y_true")]
      colnames(df_plot_pred) <- c("predictor_value", "y_pred", "y_true")
      df_plot_pred$predictor_name <- x_col
      df_plot <- bind_rows(df_plot, df_plot_pred)
    }
    fitted_eff_plot <- ggplot(df_plot, aes(x=predictor_value, y=y_pred, group=y_true)) +
      stat_summary(geom = "line", fun = mean, size=0.3, color='grey') +
      geom_smooth(aes(color=y_true))+
      facet_wrap(~predictor_name, ncol=3, scales = "free_x") + 
      ylab("Fold Increase Risk") +
      xlab("Predictor Value") +
      scale_color_discrete(name = y_col)+
      theme(legend.position="top") +
      ylim(0,  min(max(df_org$y_pred),y_max))
  }
  
  
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
    eff_plot_final <- eff_plot
  }
  
  
  return(list("effect_plot_final"=eff_plot_final,
              "fitted_effect_plot"=fitted_eff_plot,
              "mdl_obj"= mdl_final))
  
  
}

