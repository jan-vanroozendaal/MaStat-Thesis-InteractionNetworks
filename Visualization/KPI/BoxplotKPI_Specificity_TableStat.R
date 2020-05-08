df_total$specificity <- as.numeric(as.character(df_total$specificity))
df_total$sensitivity <- as.numeric(as.character(df_total$sensitivity))
df_total$`%_programmed` <- as.numeric(as.character(df_total$`%_programmed`))
df_total$`%_incluster` <- as.numeric(as.character(df_total$`%_incluster`))
df_total$size <- as.numeric(as.character(df_total$size))


#change the order of the factor df_total$threshold
df_total$threshold <- factor(df_total$threshold , levels=c("False", "0.005", "0.01", "0.015", "0.02", "0.025",
                                                           "0.03", "0.035", "0.04", "0.045", "0.05", "True"))

#change the order of the factor df_total$case
df_total$case <- factor(df_total$case , levels=c("Syn", "Zero", "Redun"))


median_col <- aggregate(df_total$specificity, by = list(df_total$model, df_total$case, df_total$threshold), median)
Q1_col <- aggregate(df_total$specificity, by = list(df_total$model, df_total$case, df_total$threshold), quantile, probs = 0.25)
Q3_col <- aggregate(df_total$specificity, by = list(df_total$model, df_total$case, df_total$threshold), quantile, probs = 0.75)

Merge1 <- merge(x = median_col, y = Q1_col, by = c("Group.1", "Group.2", "Group.3"), all.x = TRUE)
Merge2 <- merge(x = Merge1, y = Q3_col, by = c("Group.1", "Group.2", "Group.3"), all.x = TRUE)
colnames(Merge2) <- c("Model", "Case", "Threshold", "Median", "Q1", "Q3")

#create summary statistics
Merge2$Median <- format(round(Merge2$Median, 4), nsmall = 4)
Merge2$Q1 <- format(round(Merge2$Q1, 4), nsmall = 4)
Merge2$Q3 <- format(round(Merge2$Q3, 4), nsmall = 4)

#multiply by 100 to get actual percentages (improves readability)
Merge2$Median <- as.numeric(Merge2$Median) * 100
Merge2$Q1 <- as.numeric(Merge2$Q1) * 100
Merge2$Q3 <- as.numeric(Merge2$Q3) * 100

#create dynamic sentence using summary statistics per entry
sentence_part1 <- paste(trimws(as.character(format(round(Merge2$Median, digits=2)), nsmall=2)), " (", sep = "")
sentence_part2 <- paste(sentence_part1, trimws(as.character(format(round(Merge2$Q1, digits=2)), nsmall=2)), sep= "")
sentence_part3 <- paste(sentence_part2, "-", sep = "")
sentence_part4 <- paste(sentence_part3, trimws(as.character(format(round(Merge2$Q3, digits=2)), nsmall=2)), sep = "")
sentence_part5 <- paste(sentence_part4, ")", sep = "")
Merge2$sentence <- sentence_part5

#pivot data
library(reshape)

#for each combination of model/case/threshold, pivot data, and write separate .csv files per model
models <- unique(Merge2$Model)

for (i in 1:length(models)) {
  df_result <- Merge2[Merge2$Model == models[i], c(2,3,7)]
  df_result <- cast(df_result, Threshold ~ Case)
  
  file_name_part1 <- paste(models[i], "_", sep="")
  file_name_part2 <- paste(file_name_part1, "SpecificityKPI", sep="")
  file_name_part3 <- paste(file_name_part2, ".csv", sep="")
  
  your_directory <-"C:\\Users\\..." #edit file location here
  write.csv(df_result, paste(your_directory, file_name_part3, sep=""), row.names = FALSE)
}
