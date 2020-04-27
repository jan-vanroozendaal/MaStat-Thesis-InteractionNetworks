#complete table
#df_programmed_total <- data.frame(node_from=as.character(), node_to=as.character(), occur=as.numeric(), case=as.character(),
                                  #threshold=as.character(), model=as.character(), path_name=as.character())

#df_nonprogrammed_total <- data.frame(node_from=as.character(), node_to=as.character(), occur=as.numeric(), case=as.character(),
                                  #threshold=as.character(), model=as.character(), path_name=as.character())

#df_weight_total <- data.frame(node_from=as.character(), node_to=as.character(), avg_weight=as.numeric(), occur=as.numeric(), 
                              #case=as.character(), threshold=as.character(), model=as.character(), path_name=as.character())

#df_cluster_total <- data.frame(node_from=as.character(), node_to=as.character(), occur=as.numeric(), case=as.character(),
                                  #threshold=as.character(), model=as.character(), path_name=as.character())

#df_noncluster_total <- data.frame(node_from=as.character(), node_to=as.character(), occur=as.numeric(), case=as.character(),
                                  #threshold=as.character(), model=as.character(), path_name=as.character())

#df_spurcluster_total <- data.frame(node_from=as.character(), node_to=as.character(), occur=as.numeric(), case=as.character(),
                                  #threshold=as.character(), model=as.character(), path_name=as.character())








#load in workspace
workspace <- "SimModel_Ex7_V4_Threshold_Redun.RData"
load(workspace)

df_programmed_temp <- as.data.frame(list_results[5])
df_nonprogrammed_temp <- as.data.frame(list_results[6])

if (grepl("Ex2", workspace) == TRUE | grepl("Ex3", workspace) == TRUE | grepl("Ex6", workspace) == TRUE) {
  df_cluster_temp <- as.data.frame(list_results[7])
  df_noncluster_temp <- as.data.frame(list_results[8])
  df_spurcluster_temp <- as.data.frame(list_results[9])
  df_weight_temp <- as.data.frame(list_results[10])
  }  else {
  df_weight_temp <- as.data.frame(list_results[7])}

#prepare three additional columns with metadata of workspace scenario
case <- "Redun"
threshold <- "True"
model <- "Model 7"







#add those to df_programmed_temp
df_programmed_temp <- cbind(df_programmed_temp, rep(case, nrow(df_programmed_temp)))
df_programmed_temp <- cbind(df_programmed_temp, rep(threshold, nrow(df_programmed_temp)))
df_programmed_temp <- cbind(df_programmed_temp, rep(model, nrow(df_programmed_temp)))
colnames(df_programmed_temp) = c("node_from", "node_to", "occur", "case", "threshold", "model")

for (i in 1:nrow(df_programmed_temp)) {
  path_name <- c(df_programmed_temp$node_from[i], " to ", df_programmed_temp$node_to[i])
  df_programmed_temp$path_name[i] <- paste(path_name, collapse="")
}

#add those to df_nonprogrammed_temp
df_nonprogrammed_temp <- cbind(df_nonprogrammed_temp, rep(case, nrow(df_nonprogrammed_temp)))
df_nonprogrammed_temp <- cbind(df_nonprogrammed_temp, rep(threshold, nrow(df_nonprogrammed_temp)))
df_nonprogrammed_temp <- cbind(df_nonprogrammed_temp, rep(model, nrow(df_nonprogrammed_temp)))
colnames(df_nonprogrammed_temp) = c("node_from", "node_to", "occur", "case", "threshold", "model")

for (i in 1:nrow(df_nonprogrammed_temp)) {
  path_name <- c(df_nonprogrammed_temp$node_from[i], " to ", df_nonprogrammed_temp$node_to[i])
  df_nonprogrammed_temp$path_name[i] <- paste(path_name, collapse="")
}

#add those to df_weight_temp
df_weight_temp <- cbind(df_weight_temp, rep(case, nrow(df_weight_temp)))
df_weight_temp <- cbind(df_weight_temp, rep(threshold, nrow(df_weight_temp)))
df_weight_temp <- cbind(df_weight_temp, rep(model, nrow(df_weight_temp)))
colnames(df_weight_temp) = c("node_from", "node_to", "avg_weight", "occur", "case", "threshold", "model")

for (i in 1:nrow(df_weight_temp)) {
  path_name <- c(df_weight_temp$node_from[i], " to ", df_weight_temp$node_to[i])
  df_weight_temp$path_name[i] <- paste(path_name, collapse="")
}


if (grepl("Ex2", workspace) == TRUE | grepl("Ex3", workspace) == TRUE | grepl("Ex6", workspace) == TRUE) {
  #add those to df_cluster_temp
  df_cluster_temp <- cbind(df_cluster_temp, rep(case, nrow(df_cluster_temp)))
  df_cluster_temp <- cbind(df_cluster_temp, rep(threshold, nrow(df_cluster_temp)))
  df_cluster_temp <- cbind(df_cluster_temp, rep(model, nrow(df_cluster_temp)))
  colnames(df_cluster_temp) = c("node_from", "node_to", "occur", "case", "threshold", "model")
  
  for (i in 1:nrow(df_cluster_temp)) {
    path_name <- c(df_cluster_temp$node_from[i], " to ", df_cluster_temp$node_to[i])
    df_cluster_temp$path_name[i] <- paste(path_name, collapse="")
  }
  
  
  #add those to df_noncluster_temp
  df_noncluster_temp <- cbind(df_noncluster_temp, rep(case, nrow(df_noncluster_temp)))
  df_noncluster_temp <- cbind(df_noncluster_temp, rep(threshold, nrow(df_noncluster_temp)))
  df_noncluster_temp <- cbind(df_noncluster_temp, rep(model, nrow(df_noncluster_temp)))
  colnames(df_noncluster_temp) = c("node_from", "node_to", "occur", "case", "threshold", "model")
  
  for (i in 1:nrow(df_noncluster_temp)) {
    path_name <- c(df_noncluster_temp$node_from[i], " to ", df_noncluster_temp$node_to[i])
    df_noncluster_temp$path_name[i] <- paste(path_name, collapse="")
  }
  
  #add those to df_spurcluster_temp
  df_spurcluster_temp <- cbind(df_spurcluster_temp, rep(case, nrow(df_spurcluster_temp)))
  df_spurcluster_temp <- cbind(df_spurcluster_temp, rep(threshold, nrow(df_spurcluster_temp)))
  df_spurcluster_temp <- cbind(df_spurcluster_temp, rep(model, nrow(df_spurcluster_temp)))
  colnames(df_spurcluster_temp) = c("node_from", "node_to", "occur", "case", "threshold", "model")
  
  for (i in 1:nrow(df_spurcluster_temp)) {
    path_name <- c(df_spurcluster_temp$node_from[i], " to ", df_spurcluster_temp$node_to[i])
    df_spurcluster_temp$path_name[i] <- paste(path_name, collapse="")
  }
}

#append rows to complete tables
df_programmed_total <- rbind(df_programmed_total, df_programmed_temp)
df_nonprogrammed_total <- rbind(df_nonprogrammed_total, df_nonprogrammed_temp)
df_weight_total <- rbind(df_weight_total, df_weight_temp)


if (grepl("Ex2", workspace) == TRUE | grepl("Ex3", workspace) == TRUE | grepl("Ex6", workspace) == TRUE) {
  df_cluster_total <- rbind(df_cluster_total, df_cluster_temp)
  df_noncluster_total <- rbind(df_noncluster_total, df_noncluster_temp)
  df_spurcluster_total <- rbind(df_spurcluster_total, df_spurcluster_temp)
}

#wipe workspace clean except complete table
rm(list=setdiff(ls(), c("df_programmed_total", "df_nonprogrammed_total", "df_weight_total",
                        "df_cluster_total", "df_noncluster_total", "df_spurcluster_total")))



#=-=-=-=-=-=-=-=-=-=-=
#df_total$specificity <- as.numeric(as.character(df_total$specificity))
#df_total$sensitivity <- as.numeric(as.character(df_total$sensitivity))


#change the order of the factor df_total$threshold
#df_total$threshold <- factor(df_total$threshold , levels=c("False", "0.005", "0.01", "0.015", "0.02", "0.025",
                                                            #"0.03", "0.035", "0.04", "0.045", "0.05", "True"))

#change the order of the factor df_total$case
#df_total$case <- factor(df_total$case , levels=c("Syn", "Zero", "Redun"))

#library(ggplot2)
#bp <- ggplot(df_total, aes(x=threshold, y=specificity, group=threshold)) + 
  #geom_boxplot(aes(fill=threshold)) +
  #scale_y_continuous(limits=c(0,1))
#bp

# Split in horizontal direction
#bp + facet_grid(model ~ case)