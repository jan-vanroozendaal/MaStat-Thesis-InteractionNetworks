#subset by Models 3, edges all appear within cluster
df_total <- df_spurcluster_total[df_spurcluster_total$model =="Model 3"
                                 & df_spurcluster_total$occur != 0,]


#find path types
for (i in 1:nrow(df_total)) {
  if (substr(df_total$path_name[i],3,3) == "x" & substr(df_total$path_name[i],10,10) == "y"
      | substr(df_total$path_name[i],3,3) == "y" & substr(df_total$path_name[i],10,10) == "x") {
    df_total$path_type[i] <- "X to Y"
  }
  else if (substr(df_total$path_name[i],3,3) == "x" & substr(df_total$path_name[i],10,10) == "z"
           | substr(df_total$path_name[i],3,3) == "z" & substr(df_total$path_name[i],10,10) == "x") {
    df_total$path_type[i] <- "X to Z"
  }
  else if (substr(df_total$path_name[i],3,3) == "y" & substr(df_total$path_name[i],10,10) == "z"
           | substr(df_total$path_name[i],3,3) == "z" & substr(df_total$path_name[i],10,10) == "y") {
    df_total$path_type[i] <- "Y to Z"
  }
  else if (substr(df_total$path_name[i],3,3) == "x" & substr(df_total$path_name[i],10,10) == "x") {
    df_total$path_type[i] <- "X to X"
  }
  else if (substr(df_total$path_name[i],3,3) == "y" & substr(df_total$path_name[i],10,10) == "y") {
    df_total$path_type[i] <- "Y to Y"
  }
  else if (substr(df_total$path_name[i],3,3) == "z" & substr(df_total$path_name[i],10,10) == "z") {
    df_total$path_type[i] <- "Z to Z"
  }
}

#filter by path type Y-Y
df_total <- df_total[df_total$path_type == "Y to Y",]

#add avg weight data
df_total <- merge(df_total, df_weight_total, all.x=TRUE)
df_total$avg_weight <- as.numeric(as.character(df_total$avg_weight))

#change the order of the factor df_total$threshold
df_total$threshold <- factor(df_total$threshold , levels=c("False", "0.005", "0.01", "0.015", "0.02", "0.025",
                                                           "0.03", "0.035", "0.04", "0.045", "0.05", "True"))

#change the order of the factor df_total$case
df_total$case <- factor(df_total$case , levels=c("Syn", "Zero", "Redun"))


models <- unique(df_total$model)

for (i in 1:length(models)) {
  df_plot <- df_total[df_total$model == models[i],]
  
  
  #summary metrics
  # (A) weight
  median_col_weight <- aggregate(df_plot$avg_weight, by = list(df_plot$case, df_plot$threshold), median)
  median_col_weight$x <- format(round(median_col_weight$x, 4), nsmall = 4)

  
  # (B) Occurrences
  median_col_occur <- aggregate(df_plot$occur, by = list(df_plot$case, df_plot$threshold), median)
  median_col_occur$x <- format(round(median_col_occur$x, 4), nsmall = 4)
  median_col_occur$x <- as.numeric(as.character(median_col_occur$x))
  median_col_occur$x <- median_col_occur$x * 100

  # (C) count of edges
  num_edges <- aggregate(df_plot, by = list(df_plot$case, df_plot$threshold), FUN=length)[,c(1,2,3)]
  
  #merge
  Merge1 <- merge(median_col_weight, median_col_occur, by = c("Group.1", "Group.2"), all.x = TRUE)
  Merge_Final <- merge(Merge1, num_edges, by = c("Group.1", "Group.2"), all.x = TRUE)
  colnames(Merge_Final) <- c("Case", "Threshold", "Weight", "Occur", "Count")

  #change data types
  Merge_Final$Weight <- as.numeric(as.character(Merge_Final$Weight))

  
  
  #create dynamic sentence using summary statistics per entry
  # (A) Weight
  sentence_part1 <- paste("(", trimws(as.character(format(round(Merge_Final$Occur, digits=2)), nsmall=2)), sep = "")
  sentence_part2 <- paste(sentence_part1, ", ", sep = "")
  sentence_part3 <- paste(sentence_part2, trimws(as.character(format(round(Merge_Final$Weight, digits=4)), nsmall=4)), sep = "")
  sentence_part4 <- paste(sentence_part3, "), n=", sep = "")
  sentence_part5 <- paste(sentence_part4, trimws(as.character(format(round(Merge_Final$Count, digits=2)), nsmall=2)), sep = "")

  Merge_Final$sentence <- sentence_part5
 
  
  #plot scatter plots of condensed data
  #create file name
  plot_name <- paste(models[i], "YY_Condensed.jpg", sep = "")
  
  #find max range for occur
  max_x <- (max(Merge_Final$Occur) %/% 5 + 1) * 5
  
  #find min/max range for weight
  max_y <- (max(Merge_Final$Weight) %/% 0.02 + 1) * 0.02
  min_y <- (min(Merge_Final$Weight) %/% 0.02) * 0.02
  
  #open jpeg file
  jpeg(plot_name, width=600, height=220)
  
  library(ggplot2)
  plot(ggplot(Merge_Final, 
              aes(x=Occur, y=Weight)) +
         geom_point(aes(colour = Threshold, size = Count)) +
         geom_point(shape = 1, colour = "black", aes(size = Count)) +
         scale_colour_grey(start = 1, end = 0.0) +
         expand_limits(x = c(0,max_x)) +
         expand_limits(y = c(min_y, max_y)) +
         geom_hline(yintercept=0, linetype="dashed", color = "black") +
         labs(x = "Median % of occurrence", y = "Median Edge Weight") +
         theme(legend.position = "none") +
         facet_grid(. ~ Case) + 
         
         #format axis labels and ticks
         theme(
           axis.title.x = element_text(size=14),
           axis.title.y = element_text(size=14),
           axis.text.x = element_text(size=10),
           axis.text.y = element_text(size=10)) +
         
         #format facet titles
         theme(strip.text.x = element_text(face = "bold", size = 14)))
  
  #close the file
  dev.off()
  
  
  
  #pivot data
  library(reshape)

  df_result <- Merge_Final[, c(1,2,6)]
  df_result <- cast(df_result, Threshold ~ Case)
    
  file_name_part1 <- paste(models[i], "_", sep="")
  file_name_part2 <- paste(file_name_part1, "Y-Y_V2", sep="")
  file_name_part3 <- paste(file_name_part2, ".csv", sep="")
    
    
  write.csv(df_result, paste("C:\\Users\\janvr\\Desktop\\Visualization\\Edges\\Y-Y\\", file_name_part3, sep=""), row.names = FALSE)
}