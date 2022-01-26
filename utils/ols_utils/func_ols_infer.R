ols_infer <-  function(df, df_org, dict_df, fml, cluster_col, num_col2="ga_days"){
  
  df <- assign.dict(df, dict_df)
  dd <- datadist(df)
  options(datadist=dd, na.action=na.omit)
  mdl_final <- rms::robcov(rms::ols(as.formula(fml),x=TRUE, y=TRUE, data=df),cluster=df[,cluster_col])
  
  # --- inference ---- (1d marginal effects)
  # 1d marginal effects plot by default rms package
  eff_plot_1d_list <- list()
  i=0
  tryCatch({
    eff_plot <- ggplot(rms::Predict(mdl_final), anova=anova(mdl_final), pval=TRUE, size.anova=2, sepdiscrete='list')
    if(!is.null(eff_plot$continuous)){
      i=i+1
      eff_plot_1d_list[[i]] <- eff_plot$continuous
    }
    if(!is.null(eff_plot$discrete)){
      i=i+1
      eff_plot_1d_list[[i]] <- eff_plot$discrete
    }
    if (is.null(eff_plot$continuous) & is.null(eff_plot$discrete)){
      i=i+1
      eff_plot_1d_list[[i]] <- eff_plot
    }
    eff_plot_1d_list <- eff_plot_1d_list[!sapply(eff_plot_1d_list,is.null)]
    eff_plot_1d <- ggpubr::ggarrange(plotlist = eff_plot_1d_list, ncol=1)
  },error=function(e){print(e)})
  
  
  # # --- prediction over time ---
  # time_pred_plot <- NULL
  # y_col <- strsplit(as.character(fml), " ~")[[1]][1]
  # base_mean <- mean(df_org[,y_col],na.rm=TRUE)
  # df_org$base_mean <- NA
  # for (t in unique(df_org$rel_time)){
  #   df_org[which(df_org$rel_time==t),'base_mean'] <- mean(df_org[which(df_org$rel_time==t),y_col],na.rm=TRUE)
  # }
  # df_org$y_hat <- predictrms(mdl_final, newdata = df_org)
  # df_org$rel_risk <- df_org$y_hat / df_org$base_mean
  # time_pred_plot <- ggplot(df_org, aes(x=rel_time, y=rel_risk))+
  #   stat_summary(geom = "line", fun = mean) +
  #   stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3) +
  #   ylab("response ratio") +
  #   xlab("relative time before event")
  # 
  
  
  # --- inference ---- (2d marginal effects)
  joint_col2 <- num_col2
  mdl_obj <- mdl_final
  eff_plot_2d_list <- list()
  tryCatch({
    i=0
    if (!is.null(joint_col2) & !is.null(df)){
      fml_x <- as.character(mdl_obj$sformula)[3]
      fml_y <- as.character(mdl_obj$sformula)[2]
      fml <- paste0(fml_y, " ~ ", fml_x)
      if(grepl(joint_col2, fml_x)){
        for(joint_col1 in setdiff(mdl_obj$Design$name, c(fml_y, joint_col2)) ){
          i = i+1
          eff_plot_2d_list[[i]] <- NULL
          tryCatch({
            df$joint_col1 <- df[,joint_col1]
            df$joint_col2 <- df[,joint_col2]
            label(df$joint_col1) <- mdl_obj$Design$label[which(mdl_obj$Design$name==joint_col1)]
            label(df$joint_col2) <- mdl_obj$Design$label[which(mdl_obj$Design$name==joint_col2)]
            
            fml_tmp <- gsub(joint_col2, "joint_col2", gsub(joint_col1, "joint_col1", fml))
            dd <- datadist(df)
            options(datadist=dd, na.action=na.omit)
            mdl_tmp <- rms::ols(as.formula(fml_tmp),x=TRUE, y=TRUE, data=df)
            eff_plot_2d_list[[i]] <- rms::bplot(rms::Predict(mdl_tmp, joint_col2, joint_col1),
                                                xlab = label(df$joint_col2),
                                                ylab = label(df$joint_col1),
                                                ylabrot=90,adj.subtitle=FALSE)
          },error=function(e){print(e)})
        }
      }
    }
  },error=function(e){print(e)}) 
  
  eff_plot_2d_list <- eff_plot_2d_list[!sapply(eff_plot_2d_list,is.null)]
  if(length(eff_plot_2d_list)>0){
    eff_plot_2d <- ggpubr::ggarrange(plotlist = eff_plot_2d_list)
  }
  
 
  return(list(eff_plot_1d = eff_plot_1d,
              eff_plot_2d = eff_plot_2d,
              eff_plot = eff_plot))
  
  
}

