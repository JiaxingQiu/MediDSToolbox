lrm_perform <- function(
  mdl_obj,
  df,
  y_map_func = "fold_risk",
  y_map_max = 3,
  rel_time_col=NULL
){
  
  library(tidytext)
  library(ggplot2)
  # initiate the return objects
  fitted_eff_plot <- NULL
  scores_plot <- NULL
  scores_all_final <- NULL
  df_hat <- NULL
  
  
  
  # --------- fitted marginal effects -------
  # define logit reversing function
  logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
  }
  
  # find column name for response variable
  y_col <- as.character( mdl_obj$sformula )[2]
  x_cols <- setdiff(mdl_obj$Design$name, y_col)
  if (!is.null(rel_time_col) ){ x_cols <- union(x_cols, intersect(colnames(df), rel_time_col) ) }
  # global mean of outcome
  base_mean <- mean(mdl_obj$y, na.rm=TRUE)#mean(df[,y_col],na.rm=TRUE)
  print(paste0("--- baseline responce mean --- ", base_mean))
  # define the right mapping function for y-axis
  if(y_map_func == "probability"){
    ymap <- function(y_logodds){
      y_prob <- ifelse(logit2prob(y_logodds) > y_map_max, y_map_max,logit2prob(y_logodds))
      return(y_prob)
    }
  }else if(y_map_func == "fold_risk"){
    ymap <- function(y_logodds){
      y_fold_risk <- ifelse(logit2prob(y_logodds)/base_mean > y_map_max, y_map_max, logit2prob(y_logodds)/base_mean)
      return(y_fold_risk)
    }
  }else{
    ymap <- function(y_logodds){
      y_logodds <- ifelse(y_logodds > y_map_max, y_map_max, y_logodds)
      return(y_logodds)
    }
  }
  # prediction on dataframe
  df$y_pred  <- ymap(rms::predictrms(mdl_obj, newdata = df))
  df$y_logodds  <- rms::predictrms(mdl_obj, newdata = df)
  df$y_true <- as.factor(df[,y_col])
  df_hat <- df
  
  # reformat plot dataframes by group
  tryCatch({
    fit_eff_plot_list <- list()
    i=0
    df_plot_num <- data.frame()
    df_plot_fct <- data.frame()
    for (x_col in x_cols){
      df_plot_pred <- df[,c(x_col, "y_pred", "y_true")]
      colnames(df_plot_pred) <- c("predictor_value", "y_pred", "y_true")
      df_plot_pred$predictor_name <- x_col
      if ( n_distinct(df_plot_pred$predictor_value) < 5 | !is.numeric(df_plot_pred$predictor_value)){
        df_plot_pred$predictor_value <- as.character(df_plot_pred$predictor_value)
        df_plot_fct <- bind_rows(df_plot_fct, df_plot_pred)
      }else{
        df_plot_num <- bind_rows(df_plot_num, df_plot_pred)
      }
    }
    if(nrow(df_plot_num)>0){
      i = i+1
      # fitted version of marginal effect plots
      fit_eff_plot_list[[i]] <- ggplot(df_plot_num, aes(x=predictor_value, y=y_pred, group=y_true, color=y_true)) +
        #stat_summary(geom = "point", fun = mean, size=0.3, alpha=0.7) +
        geom_smooth(method = "glm", formula = "y ~ poly(x, 5)")+
        facet_wrap(~predictor_name, ncol=3, scales = "free_x") + 
        ylab(y_map_func) +
        xlab("Predictor Value") +
        scale_color_discrete(name = y_col)+
        scale_color_manual(values=c( "blue", "orange"))+
        theme(legend.position="top") +
        ylim(0, min(max(df$y_pred),y_map_max) )
    }
    if(nrow(df_plot_fct)>0){
      i = i+1
      # fitted version of marginal effect plots
      fit_eff_plot_list[[i]] <- ggplot(df_plot_fct, aes(x=predictor_value, y=y_pred, group=y_true, color=y_true)) +
        #stat_summary(geom = "point", fun = mean, size=0.3, alpha=0.7) +
        geom_boxplot()+
        facet_wrap(~predictor_name, ncol=3, scales = "free_x") + 
        ylab(y_map_func) +
        xlab("Predictor Value") +
        scale_color_discrete(name = y_col)+
        scale_color_manual(values=c( "blue", "orange"))+
        theme(legend.position="top") +
        ylim(0, min(max(df$y_pred),y_map_max) )
    }
    
    fit_eff_plot_list <- fit_eff_plot_list[!sapply(fit_eff_plot_list,is.null)]
    fitted_eff_plot <- ggpubr::ggarrange(plotlist = fit_eff_plot_list, ncol=1)
    
  },error=function(e){print(e)})
  
  # ----------- feature permutation importance -------------
  tryCatch({
    test_obj_raw <- lrm_test(test_data = df,
                             y_col = y_col,
                             mdl_obj = mdl_obj)
    scores_raw <- test_obj_raw$res_df[1,c("logloss", "AUROC", "AUPRC", "accuracy", "f1score")]
    scores_all <- data.frame()
    for (x_col in x_cols){
      df_shuffled <- df
      # shuffle the predictor
      df_shuffled[,x_col] <- sample(df[,x_col], size=length(df[,x_col]))
      test_obj <- lrm_test(test_data = df_shuffled,
                           y_col=y_col,
                           mdl_obj = mdl_obj)
      scores <- test_obj$res_df[1,c("logloss", "AUROC", "AUPRC", "accuracy", "f1score")]
      scores$varname <- x_col
      scores_all <- bind_rows(scores_all, scores)
    }
    scores_all <- scores_all[,union("varname", colnames(scores_all))]
    scores_raw$varname <- "none"
    scores_all_final <- bind_rows(scores_all, scores_raw)
    colnames(scores_all_final)[which(colnames(scores_all_final) == "varname")] <- "removed_variable"
    
    # make permutation importance plot 
    df_plot_scores_all <- data.frame()
    for (score_col in setdiff(colnames(scores_all_final), "removed_variable") ){
      df_plot_scores <- scores_all_final[,c("removed_variable", score_col)]
      colnames(df_plot_scores) <- c("removed_variable", "score_value")
      df_plot_scores$score_by <- score_col
      df_plot_scores_all <- bind_rows(df_plot_scores_all, df_plot_scores) 
    }
    
    scores_plot <- ggplot(data=df_plot_scores_all, aes(x=score_value, y=tidytext::reorder_within(removed_variable, score_value, score_by) )) +
      geom_point()+
      geom_vline(data=df_plot_scores_all[which(df_plot_scores_all$removed_variable=="none"),], aes(xintercept=score_value))+
      tidytext::scale_y_reordered() +
      facet_wrap(~score_by, scales = "free", ncol = 2)+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())
    
  },error=function(e){print(e)})
  
  return(list(
    df_hat = df_hat,
    fitted_eff_plot = fitted_eff_plot,
    scores_plot = scores_plot,
    scores_all_final = scores_all_final
  ))
  
}
