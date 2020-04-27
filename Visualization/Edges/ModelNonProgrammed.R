#subset on Model 1
df_nonprogrammed_model <- df_nonprogrammed_total[df_nonprogrammed_total$model == "Model 2"
                                                 & df_nonprogrammed_total$occur != 0,]
row.names(df_nonprogrammed_model) <- NULL

df_nonprogrammed_model$num[1] <- 1
j <- 2
for (i in 2:nrow(df_nonprogrammed_model)) {
  if (df_nonprogrammed_model$case[i] != df_nonprogrammed_model$case[i-1]
      | df_nonprogrammed_model$threshold[i] != df_nonprogrammed_model$threshold[i-1]){
    j <- 1
  }
  df_nonprogrammed_model$num[i] <- j
  
  j <- j + 1
}

library(ggplot2)
library(directlabels)
line_chart <- ggplot(data=df_nonprogrammed_model, aes(x=num, y=occur)) + geom_line() + geom_point() + 
  facet_grid(threshold ~ case)

area_chart <- ggplot(df_nonprogrammed_model, aes(x=num, y=occur))  +
  geom_area(alpha=0.4) +
  geom_line() +
  #geom_text(data = df_nonprogrammed_model[which.max(df_nonprogrammed_model[,"num"]),],aes(label=num)) +
  facet_grid(threshold ~ case)

area_chart