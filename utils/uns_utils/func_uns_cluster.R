# This script use unsupervised machine learning methods (1. k-means 2. KNN(under construction)) to make clusters of a given dataset
uns_cluster_kmeans <- function(
  data, # dataframe with one row per one observation subject (i.e. baby day / baby)
  dict_data, # dictionary for data
  input_cols,# =colnames(data_ml%>%select(starts_with("cpd_"))), # input variables to be used to train clustering model
  nc_max=4, # maximum number of clusters, default 15, must be [1,20]
  nc_min=2, # minimum number of cluster you expect kmeans to split your observations [1,20]
  plot_wss=FALSE, # whethr or not to plot trace of within group sum of squares in console
  min_nobs_per_clst=2, # mininal number of observations allowed in a cluster, if a cluster has less than this number of observation, it will be removed
  max_iter=3
){
  # ---- Usage ----
  # unsupervised learning mathod -- Kmeans 
  # cluster observations by selected columns 
  # minor group / the cluster with only small numbers of observations might indicate potential outliers
  # 
  
  # ---- Arguments ----
  # data: a dataframe object for modeling
  # dict_data: dictionary object along with modeling dataframe
  # predictor_cols: colnames of predfictors used to clustering the cohort
  
  # ---- Value ----
  # wss: a within group sum of squares, you can plot the wss against number of clusters for tuning and trace evidence
  # km_mdl: final kmeans model object
  # df_cluster_info: a dataframe with cluster size and cluster centriod information from the fine-tuned kmeans model object
  # cluster_pca_plot：clusters been ploted in a pca 2d space
  # df_minor_org_trace: rows in the original dataframe from minor cluster if clustered by more than 2, containing traces of each iteration 
  # uni_heat_plot: univariate heatmap for binary cluster label
  
  
  # initiate objects to return
  wss <- NULL
  km_mdl <- NULL
  df_cluster_info <- NULL
  cluster_pca_plot <- NULL
  df_minor_org_trace <- data.frame()
  uni_heat_plot <- NULL
  tryCatch({
    # validate inputs
    nc_max <- max(1,as.numeric(nc_max),na.rm=TRUE)
    nc_max <- min(nc_max,20)
    nc_min <- max(1,as.numeric(nc_min),na.rm=TRUE)
    nc_min <- min(nc_min,20)
    if(nc_min>nc_max){
      warning("minimum number of clusters is greater than maximum number of clusters, using [1,20] instead")
      nc_min <- 1
      nc_max <- 20
    }
    min_nobs_per_clst <- max(0, as.numeric(min_nobs_per_clst), na.rm=TRUE)# min_nobs_per_cluster>=0
    max_iter <- min(as.numeric(max_iter),10, na.rm=TRUE)
    
    # refine input columns 
    input_cols <- intersect(input_cols,colnames(data))
    # keep the dataframe with all original columns having the same number of rows as modeling df
    df_org <- data[which(complete.cases(data[,input_cols])),]
    rownames(df_org) <- 1:nrow(df_org)
  },error=function(e){
    print(e)
    print('Error --- validating input arguments or pre-processing modeling dataframe.')
  })
  
  # ------ unsupervised clustering -----
  iter <- 0
  while(TRUE){
    iter <- iter + 1
    print(paste0("--- unsuperivsed clustering iteration ", iter, " ---"))
    # keep complete cases and input variable columns only in a dataframe for modeling 
    df_mdl <- df_org[,input_cols]
    rownames(df_mdl) <- 1:nrow(df_mdl)
    tryCatch({
      # within group sum of squares save wss object
      wss <- (nrow(df_mdl)-1)*sum(apply(df_mdl,2,stats::var)) # baseline (start with 1 cluster)
      # train a Kmean model on different Ks, calculate within group sum of squares
      for (i in nc_min:nc_max) {wss[i] <- sum(kmeans(df_mdl,centers=i,iter.max=100)$withinss)}
      # if you want to see the trace of decreasing in wss
      if(plot_wss){
        par(mfrow=c(2,2))
        plot(1:nc_max, wss, type="b", xlab="Number of Clusters",
             ylab="Within groups sum of squares")
        plot(1:nc_max, log(wss), type="b", xlab="Number of Clusters",
             ylab="log of Within groups sum of squares")
        plot(2:nc_max, diff(wss), type="b", xlab="Number of Clusters",
             ylab="diff( Within groups sum of squares )")
        plot(2:nc_max, diff(log(wss)), type="b", xlab="Number of Clusters",
             ylab="diff( log of Within groups sum of squares )")
        par(mfrow=c(1,1))
      }
      print("Success! wss is created. ")
      # find the biggest drop in wss
      if (nc_min == nc_max) {
        n_centers <- nc_min
      }else{
        n_centers <- c(max(1,nc_min):max(1,nc_max))[which(diff(wss)==min(diff(wss),na.rm=TRUE))]
      }
      # if there is only one cluster
      if (n_centers==1){warning("K-means suggests all the observations came from only one cluster")}
    },error = function(e){
      print(e)
      print("Error --- generating wss trace.")
    })
    tryCatch({
      # train model on all df_mdl
      km_mdl <- kmeans(df_mdl, centers = n_centers)
      print("Success! K-means model is finalized.")
    },error=function(e){
      print(e)
      print('Error --- finalizing kmeans model km_mdl object')
    })
    tryCatch({
      df_cluster_info <- bind_cols()
      # get the frequency table of clusters in a data.frame
      df_freq <- as.data.frame(table(km_mdl$cluster))
      colnames(df_freq) <- c('cluster','Freq')
      df_centers <- as.data.frame(round(km_mdl$centers,4))
      df_centers$cluster <- rownames(df_centers)
      df_cluster_info <- merge(df_freq,df_centers)
      df_cluster_info <- as.data.frame(t(df_cluster_info))
      colnames(df_cluster_info) <- 1:ncol(df_cluster_info)
      print("Success! df_cluster_info has been derived and you can find clues for the clusters")
    },error = function(e){
      print(e)
      print("Error --- deriving df_cluster_info -- cluster info dataframe.")
    })
    tryCatch({
      # plot clusters in pca space
      km_pca_plot <- ggplot2::autoplot(km_mdl, df_mdl, frame = TRUE) # remember to install latest version ggplot2 and library it inside the function again
      # create result clustered dataframe
      df_res <- km_pca_plot$data
      df_res <- df_res %>% select(!starts_with("PC")) %>% as.data.frame() # select non pc columns
      df_res <- bind_cols(df_res, km_pca_plot$data[,c('PC1','PC2')]) # only keep pc1 and pc2 columns
      # add frequency of clusters in a column
      df_res <- merge(df_res, df_freq, all.x = TRUE)
      # add cluster id combind by cluster and frequency
      df_res$cluster_id <- paste0(df_res$cluster," (#obs ",df_res$Freq,")")
      # plot cluster in a 2d pca space
      cluster_pca_plot <- ggplot(df_res, aes(x=PC1, y=PC2, color=cluster_id)) + 
        geom_point(size=1.5) + 
        labs(x = km_pca_plot$labels$x,
             y = km_pca_plot$labels$y)+
        theme(legend.position = "top",
              legend.title = element_blank()) 
      # if number of clusters is greater than 1
      if (n_centers>=2){
        # add a new columns for minor cluster rowname/index to result clustered dataframe
        df_res <- df_res %>% mutate(minor_rownames = ifelse(Freq==min(Freq,na.rm=TRUE), rownames, as.numeric(NA))) %>% as.data.frame()
        # update cluster pca plot by adding a text label of rownames for all the data point/rows in the minor cluster
        cluster_pca_plot <- ggplot(df_res, aes(x=PC1, y=PC2, color=cluster_id)) + 
          geom_point(size=1.5) + 
          labs(x = km_pca_plot$labels$x,
               y = km_pca_plot$labels$y)+
          theme(legend.position = "top",
                legend.title = element_blank()) + 
          xlim(0,1) + ylim(-1,1) + 
          geom_text(aes(label=minor_rownames), hjust = 1, vjust = -1.5, size = 2)
        # locate original data rows coming from the minor cluster
        df_minor_org <- NULL
        df_minor_org <- df_org[which(rownames(df_org)%in%c(df_res$minor_rownames[which(!is.na(df_res$minor_rownames))])),]
        # add the cluster index and iteration number 
        df_minor_org$cluster_iter <- paste0("cluster_",unique(df_res$cluster[which(!is.na(df_res$minor_rownames))]), "_iter_",iter)
        df_minor_org$freq <- unique(df_res$Freq[which(!is.na(df_res$minor_rownames))])
        # default for every minor cluster is not to be removed
        df_minor_org$removed <- FALSE
        # if the minor failed the requirement of min number of observation within a cluster
        if (unique(df_res$Freq[which(!is.na(df_res$minor_rownames))]) < min_nobs_per_clst){ 
          # set removed to be TRUE because it failed the minimum requirement for number of observations in a cluster 
          df_minor_org$removed <- TRUE
          # reorder the columns
          df_minor_org <- df_minor_org[,union(c('cluster_iter','freq','removed'),colnames(df_minor_org))]
          # update df_org by remove the minor cluster affected rows
          df_org <- df_org[which(!rownames(df_org)%in%c(df_res$minor_rownames[which(!is.na(df_res$minor_rownames))])),]
          # reset index / rowname
          rownames(df_org) <- 1:nrow(df_org)
        }
        # combine current minor cluster dataframe into the global one
        df_minor_org_trace <- bind_rows(df_minor_org_trace, df_minor_org)
      }
    },error=function(e){
      print(e)
      print('Error --- creating cluster_pca_plot')
    })
    
    ########  if no more wihtin-cluster number of observation drop under minimum requirement, exit
    if(min(df_freq$Freq)>=min_nobs_per_clst) break
    ########  maximum iteration number, exit if more than 10 
    if(iter >= max_iter) break 
  }
  
  tryCatch({
    # assert original dataframe and result clustered dataframe have same nrows
    stopifnot(nrow(df_org)==nrow(df_res))
    # assign dictionary to the dataframe as attributes
    df_org <- assign.dict(df_org, dict_data, overwrite = TRUE)
    # add cluster id column
    df_org$clustered_label <- df_res$cluster
    # modify dictionary
    attr(df_org$clustered_label,'varname') <- 'clustered_label'
    attr(df_org$clustered_label,'label') <- 'unsupervised cluster labels'
    attr(df_org$clustered_label,'type') <- 'fct'
    attr(df_org$clustered_label,'unit') <- ''
    attr(df_org$clustered_label,'mlrole') <- 'output'
    attr(df_org$clustered_label,'unique_per_sbj') <- FALSE
    attr(df_org$clustered_label,'source_file') <- 'drvd (unsuper)'
    attr(df_org$clustered_label,'label_front') <- 'unsupervised cluster labels'
    attr(df_org$clustered_label,'varname_dict') <- 'clustered_label'
    
    
    # find all the cluster ids
    clst_ids <- unique(df_org$clustered_label)
    # dummmy clusters except one baseline
    for (lvl in clst_ids){
      df_org[,paste0("clustered_label_",lvl,"_tag")] <- ifelse(df_org$clustered_label==lvl, 1, 0)
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")], 'varname') <- paste0("clustered_label_",lvl,"_tag")
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")], 'label') <- paste0('unsupervised cluster ',lvl)
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")], 'type') <- 'fct'
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")],'unit') <- 'tag01'
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")],'mlrole') <- 'output'
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")],'unique_per_sbj') <- FALSE
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")],'source_file') <- 'drvd (unsuper)'
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")],'label_front') <- paste0('unsupervised cluster ',lvl)
      attr(df_org[,paste0("clustered_label_",lvl,"_tag")],'varname_dict') <- 'clustered_label'
    }
    
    # update the dict_data with new added column
    dict_df_org_clustered <- get.dict(df_org)
    # save df to return
    df_org_clustered <- df_org
  },error=function(e){
    print(e)
    print("Error --- finalizing refined and cluster-labeled original dataset")
  })
  
  
  # --- final reformating -----
  colnames(df_cluster_info) <- paste0("cluster___",colnames(df_cluster_info))
  df_cluster_info$rownames <- rownames(df_cluster_info)
  df_cluster_info <- df_cluster_info[,union("rownames", colnames(df_cluster_info))]
  
  return(list("wss"=wss,
              "nc_min"=nc_min,
              "nc_max"=nc_max,
              "km_mdl"=km_mdl,
              "df_cluster_info" = df_cluster_info,
              "cluster_pca_plot"=cluster_pca_plot,
              "df_minor_org_trace"=df_minor_org_trace,
              "df_org_clustered"=df_org_clustered,
              "dict_df_org_clustered"=dict_df_org_clustered
  ))
}
