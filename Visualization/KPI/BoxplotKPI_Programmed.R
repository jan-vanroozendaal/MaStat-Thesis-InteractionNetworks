#=-=-=-=-=-=-=-=-=-=-=
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

library(ggplot2)
bp <- ggplot(df_total, aes(x=threshold, y=df_total$`%_programmed`, group=threshold)) + 
  geom_boxplot(aes(fill=threshold))

# Split in horizontal direction
bp + scale_y_continuous(breaks = seq(0, 1, 0.5), limits = c(0,1)) + 
  labs(x = "Threshold Level", y = "% programmed (scaled 0-1)", fill="Threshold Level") +
  #format axis labels and ticks
  theme(
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=8, angle=315),
    axis.text.y = element_text(size=10)) +
  
  #perform the facet grid    
  facet_grid(model ~ case) +
  
  #format facet labels  
  theme(
    strip.text.x = element_text(
      size = 14, face = "bold"
    ),
    strip.text.y = element_text(
      size = 14, face = "bold"
    )
  )






