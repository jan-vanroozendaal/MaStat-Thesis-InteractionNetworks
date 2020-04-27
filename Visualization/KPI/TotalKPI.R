#df_total <- data.frame(size=as.integer(), sensitivity=as.numeric(), specificity=as.numeric(), '%_programmed'=as.numeric(), 
                       #'%_nonprogrammed' = as.numeric(), '%_incluster'=as.numeric(),  '%_outcluster'=as.numeric(),
                       #case=as.character(), threshold=as.character(), model=as.character())
#load in workspace
load("SimModel_Ex7_V4_Threshold_Redun.RData")
df_temp <- list_results$`Iteration KPIs`

#prepare three additional columns with metadata of workspace scenario
case <- rep("Redun", nrow(df_temp))
threshold <- rep("True", nrow(df_temp))
model <- rep("Model 7", nrow(df_temp))

#if KPIs about clusters are not included
if(!'%_outcluster' %in% colnames(df_temp)) {
  outcluster <- rep(NA, nrow(df_temp))
  incluster <- rep(NA, nrow(df_temp))
  #add those to df_temp
  df_temp <- cbind(df_temp, incluster)
  df_temp <- cbind(df_temp, outcluster)
  colnames(df_temp)[6] <- '%_incluster'
  colnames(df_temp)[7] <- '%_outcluster'}

#add those to df_temp
df_temp <- cbind(df_temp, case)
df_temp <- cbind(df_temp, threshold)
df_temp <- cbind(df_temp, model)

#append rows to complete table
df_total <- rbind(df_total, df_temp)

#wipe workspace clean except complete table
rm(list=setdiff(ls(), "df_total"))


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