library(dplyr)
library(ggplot2)

#change data types
df_weight_total$avg_weight <- as.numeric(as.character(df_weight_total$avg_weight))

#subset on Model
df_weight_model <- merge(x=df_noncluster_total[df_noncluster_total$model == "Model 2"
                                                  & df_noncluster_total$occur != 0,],
                         y=df_weight_total, all.x=TRUE)
row.names(df_weight_model) <- NULL

#change data types
df_weight_model$avg_weight <- as.numeric(as.character(df_weight_model$avg_weight))

#change the order of the factor df_weight_model$threshold
thresholds <- c("False", "0.005", "0.01", "0.015", "0.02", "0.025", "0.03", "0.035", "0.04", "0.045", "0.05", "True")
df_weight_model$threshold <- factor(df_weight_model$threshold , levels=thresholds)

#change the order of the factor df_weight_model$case
cases <- c("Syn", "Zero", "Redun")
df_weight_model$case <- factor(df_weight_model$case , levels=cases)

#prep dataframe for plot
df_plot <- df_weight_model[0,]
df_plot$cluster <- rep(0, 0)

for (i in 1:length(thresholds)) {
  #for each threshold level, subset the data first on the synergy case for clustering
  df_subset <- df_weight_model[df_weight_model$threshold == thresholds[i]
                               & df_weight_model$case == "Syn",]
  
  # use kmeans a first time to get the centers
  centers <- kmeans(df_subset[,c(8)], centers = 5)$centers
  
  # order the centers
  centers <- sort(centers)
  
  # call kmeans again but this time passing the centers calculated in the previous step
  clusteridx <- kmeans(x = df_subset[,c(8)], centers = centers)$cluster
  
  #add cluster results to subset dataframe
  df_subset$cluster <- as.character(clusteridx)
  
  #now, create a subset of the same model data without the synergy case
  df_subset_add <- df_weight_model[df_weight_model$threshold == thresholds[i]
                                   & df_weight_model$case != "Syn",]
  #& abs(df_weight_model$avg_weight) > 0.005,]
  
  #map the path names with the same cluster indexes found in the synergy case
  df_subset_add <- merge(x=df_subset_add,y=df_subset[,c(1,2,5,6,7,9)], all.x=TRUE)
  #there can be <NA> values during the merge, because spurious edges are found not present in the synergy case
  #give those a new cluster category
  df_subset_add[is.na(df_subset_add)] <- "other"
  
  #appends rows to df_subset to create a single dataframe
  df_subset <- rbind(df_subset, df_subset_add)
  
  #append results to master dataframe
  df_plot <- rbind(df_plot, df_subset)
}

cluster_overview <- ggplot(df_plot[df_plot$cluster != "other",], aes(x=occur, y=avg_weight, colour=cluster)) + 
  geom_point() +
  facet_grid(threshold ~ case, scales = "free")
cluster_overview

#=-= drill-down plots =-=#
for (i in 1:nrow(df_plot)) {
  if (substr(df_plot$path_name[i],3,3) == "x" & substr(df_plot$path_name[i],10,10) == "y"
      | substr(df_plot$path_name[i],3,3) == "y" & substr(df_plot$path_name[i],10,10) == "x") {
    df_plot$path_type[i] <- "X to Y"
  }
  else if (substr(df_plot$path_name[i],3,3) == "x" & substr(df_plot$path_name[i],10,10) == "z"
           | substr(df_plot$path_name[i],3,3) == "z" & substr(df_plot$path_name[i],10,10) == "x") {
    df_plot$path_type[i] <- "X to Z"
  }
  else if (substr(df_plot$path_name[i],3,3) == "y" & substr(df_plot$path_name[i],10,10) == "z"
           | substr(df_plot$path_name[i],3,3) == "z" & substr(df_plot$path_name[i],10,10) == "y") {
    df_plot$path_type[i] <- "Y to Z"
  }
  else if (substr(df_plot$path_name[i],3,3) == "x" & substr(df_plot$path_name[i],10,10) == "x") {
    df_plot$path_type[i] <- "X to X"
  }
  else if (substr(df_plot$path_name[i],3,3) == "y" & substr(df_plot$path_name[i],10,10) == "y") {
    df_plot$path_type[i] <- "Y to Y"
  }
  else if (substr(df_plot$path_name[i],3,3) == "z" & substr(df_plot$path_name[i],10,10) == "z") {
    df_plot$path_type[i] <- "Z to Z"
  }
}

#save images of drill-down plots automatically in current directory
#iterate over threshold levels
for (i in 1:length(thresholds)) {
  #create file name
  plot_name <- paste(paste(i,"_Model2_NonClusterEdgeCat_", sep=""), paste(thresholds[i], ".jpg", sep=""), sep="")
  
  #open jpeg file
  jpeg(plot_name, width=1920, height=1309)
  
  #Create the plot
  plot(ggplot(df_plot[df_plot$threshold == thresholds[i] & df_plot$cluster != "other",], 
              aes(x=occur, y=avg_weight, colour=cluster)) + 
         geom_point() +
         expand_limits(x = c(0,0.6)) +
         expand_limits(y = c(-0.075, 0.075)) +
         facet_grid(case ~ path_type))
  
  #close the file
  dev.off()
}


#=-= Other category =-=#
#this category holds only edges not occurring in the full synergy case
#exploring the relation of these edges between zero/full redundancy case

other_overview <- ggplot(df_plot[df_plot$cluster == "other",], aes(x=occur, y=avg_weight, colour=cluster)) + 
  geom_point() +
  facet_grid(threshold ~ case, scales = "free")

other_drilldown <- ggplot(df_plot[df_plot$cluster == "other" & df_plot$threshold == "0.035",], 
                          aes(x=occur, y=avg_weight, colour=cluster)) + 
  geom_point() +
  facet_grid(case ~ path_type)