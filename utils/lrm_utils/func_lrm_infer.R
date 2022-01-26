lrm_infer <-  function(
  mdl_obj, # a rms model object
  y_map_func = "fold_risk",
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
  eff_plot <- NULL
  eff_plot_diy <- NULL
  
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
  
  # 1d marginal effects plot by default rms package
  eff_plot_1d_list <- list()
  i=0
  tryCatch({
    eff_plot <- ggplot(rms::Predict(mdl_obj, fun=ymap), anova=anova(mdl_obj), pval=TRUE, size.anova=2, sepdiscrete='list')
    if(!is.null(eff_plot$continuous)) eff_plot$continuous <- eff_plot$continuous + ylab(y_map_func)
    if(!is.null(eff_plot$discrete)) eff_plot$discrete <- eff_plot$discrete + ylab(y_map_func)
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
  
  tryCatch({
    eff_plot_diy <- ggplot_rms_diy(mdl_obj=mdl_obj, ymap=ymap) 
    if(!is.null(eff_plot_diy$continuous)) eff_plot_diy$continuous <- eff_plot_diy$continuous + ylab(y_map_func)
    if(!is.null(eff_plot_diy$discrete)) eff_plot_diy$discrete <- eff_plot_diy$discrete + ylab(y_map_func)
  },error=function(e){print(e)})
  
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
            eff_plot_2d_list[[i]] <- rms::bplot(rms::Predict(mdl_tmp, joint_col2, joint_col1, fun=ymap),
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
  
  
  return(list(eff_plot_1d=eff_plot_1d, # all in one
              eff_plot_2d=eff_plot_2d, # all in one
              eff_plot = eff_plot,
              eff_plot_diy = eff_plot_diy))
  
  
}


ggplot_rms_diy <- function(
  mdl_obj,
  ymap # function object
){
  
  pdf <- as.data.frame(rms::Predict(mdl_obj, fun=ymap))
  colnames(pdf)[which(colnames(pdf)==".predictor.")] <- "xname"
  pdf_all <- data.frame()
  for (xname in mdl_obj$Design$name){
    tryCatch({
      xlabel <- mdl_obj$Design$label[which(mdl_obj$Design$name==xname)]
      stopifnot(nchar(xlabel)>0)
    },error=function(e){
      print("--- no label for current variable, using name instead ---")
      xlabel <- xname
    })
    pdf_x <- pdf[which(pdf[,"xname"]==xname),] # find chunk for current x
    pdf_x$xvalue <- as.character( pdf_x[, xname] )
    pdf_x$xtype <- "fct" # default
    if(length(as.numeric(pdf_x$xvalue))>0){
      if(length(unique(pdf_x$xvalue))>=10 ){
        pdf_x$xtype <- "num"
      }
    }
    pdf_x$yupper <- pdf_x[,"upper"]
    pdf_x$ylower <- pdf_x[,"lower"]
    pdf_x$xlabel <- xlabel
    pdf_x$yhat <- pdf_x[,"yhat"]
    pdf_x <- pdf_x[,c("xname", "xlabel", "xvalue", "xtype", "yupper","ylower","yhat")]
    rownames(pdf_x) <- NULL
    pdf_all <- bind_rows(pdf_all, pdf_x)
  }
  
  # add anova info 
  anova_df <- as.data.frame(anova(mdl_obj,indnl=FALSE))[mdl_obj$Design$name,]
  colnames(anova_df) <- c("chi_square", "dof","p_value")
  anova_df$xname <- rownames(anova_df)
  anova_df$xlabel <- mdl_obj$Design$label
  anova_df$rank <- -(rank(anova_df$chi_square)-max(rank(anova_df$chi_square))-1) # rank by chi-squre
  anova_df$signif <- "> 0.05"
  anova_df$signif[which(anova_df$p_value<=0.001)] <- "<= 0.001"
  anova_df$signif[which(anova_df$p_value>0.001&anova_df$p_value<=0.01)] <- "(0.001, 0.01]"
  anova_df$signif[which(anova_df$p_value<=0.05&anova_df$p_value>0.01)] <- "(0.01, 0.05]"
  anova_df <- anova_df[order(anova_df$rank),]
  
  
  pdf_all <- merge(pdf_all, anova_df[,c("xname", "signif")])
  # rank xlabel by order of chisquare
  pdf_all$xlabel <- factor(pdf_all$xlabel,levels=anova_df$xlabel)
  pdf_all$signif <- factor(pdf_all$signif,levels=c("<= 0.001", "(0.001, 0.01]", "(0.01, 0.05]", "> 0.05"))
  
  eff_plot_1d_diy <- NULL
  eff_plot_1d_diy$continuous <- ggplot(pdf_all[which(pdf_all$xtype=="num"),], aes(x=as.numeric(xvalue), y=yhat)) +
    geom_rect(aes(fill=signif, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)) +
    geom_line() +
    geom_ribbon(aes(ymin=ylower, ymax=yupper), alpha=0.3, linetype=0)+
    facet_wrap(~ xlabel, ncol=5, scales = "free_x") +
    scale_fill_manual(values=c("<= 0.001" ="#FFCCCC", 
                               "(0.001, 0.01]" = "#FFF0F5",# lavender blush
                               "(0.01, 0.05]" = "#FFEFD5", 
                               "> 0.05" = "#F5F5F5")) +
    labs(x=NULL,y=NULL,fill="p")+
    theme(legend.position ="top")

  eff_plot_1d_diy$discrete <- ggplot(pdf_all[which(pdf_all$xtype=="fct"),], aes(x=xvalue, y=yhat)) +
    geom_rect(aes(fill=signif, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin=ylower, ymax=yupper), width=.2) +
    facet_wrap(~ xlabel, ncol=5, scales = "free_x") +
    scale_fill_manual(values=c("<= 0.001" ="#FFCCCC", 
                               "(0.001, 0.01]" = "#FFF0F5",# lavender blush
                               "(0.01, 0.05]" = "#FFEFD5", 
                               "> 0.05" = "#F5F5F5")) +
    labs(x=NULL,y=NULL,fill="p")+
    theme(legend.position ="bottom")

  return(eff_plot_1d_diy)
  # eff_plot_1d_diy_combind <- ggplot() +
  #   geom_line(data=pdf_all[which(pdf_all$xtype=="num"),], aes(x=as.numeric(xvalue), y=yhat)) +
  #   geom_ribbon(data=pdf_all[which(pdf_all$xtype=="num"),], aes(x=as.numeric(xvalue), y=yhat, ymin=ylower, ymax=yupper), alpha=0.2, linetype=0)+
  #   geom_point(data=pdf_all[which(pdf_all$xtype=="fct"),], aes(x=as.numeric(as.factor(xvalue)), y=yhat), size = 1) +
  #   geom_errorbar(data=pdf_all[which(pdf_all$xtype=="fct"),], aes(x=as.numeric(as.factor(xvalue)), y=yhat, ymin=ylower, ymax=yupper), width=.1) +
  #   facet_wrap(~ xlabel, ncol=3, scales = "free_x") +
  #   xlab(NULL) +
  #   ylab(NULL)

}
