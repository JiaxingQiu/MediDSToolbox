# non-temporal information in the dataset
lrm_infer <-  function(
  mdl_obj, # a rms model object
  y_map_func = "log_odds",
  y_map_max=3,
  joint_col2 = NULL,
  df = NULL # optinal, finally engineered modeling df underneath mdl_obj 
){
  
  # define logit reversing function
  logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
  }
  
  # define objects to return
  eff_plot_1d <- NULL
  eff_plot_2d <- NULL
  
  # calculate baseline mean of modeling data
  base_mean <- mean(mdl_obj$y,na.rm=TRUE)
  print(paste0("-- baseline response mean -- ", base_mean))
  
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
  
  # 1d marginal effects plot
  eff_plot <- ggplot(rms::Predict(mdl_obj, fun=ymap), anova=anova(mdl_obj), pval=TRUE, size.anova=2, sepdiscrete='list')
  eff_plot_1d_list <- list(eff_plot$continuous, eff_plot$discrete)
  eff_plot_1d_list <- eff_plot_1d_list[!sapply(eff_plot_1d_list,is.null)]
  eff_plot_1d <- ggpubr::ggarrange(plotlist = eff_plot_1d_list, ncol=1)
  
  
  # 2d marginal effects if joint_col2 given
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
            mdl_tmp <- rms::lrm(as.formula(fml_tmp),
                                x=TRUE, y=TRUE,
                                data=df, 
                                penalty = mdl_obj$penalty)
            eff_plot_2d_list[[i]] <- rms::bplot(rms::Predict(mdl_tmp, joint_col1, joint_col2, fun=ymap),
                                                xlab = label(df$joint_col1),
                                                ylab = label(df$joint_col2),
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
  
  
  return(list(eff_plot_1d=eff_plot_1d,
              eff_plot_2d=eff_plot_2d))
  
  
}

