df_spurcluster_total$flag <- 1

df_test <- merge(df_weight_total[df_weight_total$model == "Model 2"
                                 & df_weight_total$case == "Syn",], df_spurcluster_total, all.x = TRUE)

df_test$flag[is.na(df_test$flag)] <- 0

for (i in 1:nrow(df_test)) {
  if (substr(df_test$path_name[i],3,3) == "x" & substr(df_test$path_name[i],10,10) == "y"
      | substr(df_test$path_name[i],3,3) == "y" & substr(df_test$path_name[i],10,10) == "x") {
    df_test$path_type[i] <- "X to Y"
  }
  else if (substr(df_test$path_name[i],3,3) == "x" & substr(df_test$path_name[i],10,10) == "z"
           | substr(df_test$path_name[i],3,3) == "z" & substr(df_test$path_name[i],10,10) == "x") {
    df_test$path_type[i] <- "X to Z"
  }
  else if (substr(df_test$path_name[i],3,3) == "y" & substr(df_test$path_name[i],10,10) == "z"
           | substr(df_test$path_name[i],3,3) == "z" & substr(df_test$path_name[i],10,10) == "y") {
    df_test$path_type[i] <- "Y to Z"
  }
  else if (substr(df_test$path_name[i],3,3) == "x" & substr(df_test$path_name[i],10,10) == "x") {
    df_test$path_type[i] <- "X to X"
  }
  else if (substr(df_test$path_name[i],3,3) == "y" & substr(df_test$path_name[i],10,10) == "y") {
    df_test$path_type[i] <- "Y to Y"
  }
  else if (substr(df_test$path_name[i],3,3) == "z" & substr(df_test$path_name[i],10,10) == "z") {
    df_test$path_type[i] <- "Z to Z"
  }
}

df_test$occur <- df_test$occur * 100

df_test$flag <- as.factor(df_test$flag)

df_test <- df_test[abs(df_test$avg_weight) < 0.5,]

library(ggplot2)
p1 <- plot(ggplot(df_test[df_test$path_type == "Y to Z"
                          & df_test$case == "Syn",], 
                  aes(x=occur, y=avg_weight, colour=flag)) + 
             geom_point() +
             expand_limits(x = c(0,0.6)) +
             expand_limits(y = c(-0.075, 0.075)) +
             geom_hline(yintercept=0, linetype="dashed", color = "grey") +
             scale_color_grey(start=0.6, end=0.2) +
             labs(x = "% of occurrence (0-100)", y = "Avg. Edge Weight") +
             theme(legend.position = "none") +
             facet_wrap(~ threshold, ncol=6))

your_directory <-"C:\\Users\\..." #edit file location here

ggsave(p1, filename = paste(your_directory, "ScatterPattern3.png", sep=""), width = 12, height = 5)
