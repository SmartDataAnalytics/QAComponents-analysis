#########################################################
### install and load R packages
#########################################################
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(ggplot2)

#########################################################
### functions
#########################################################

neddata <- read.csv("ned-unanswered.csv", sep = ',', row.names = 1)
neddf <- as.matrix(neddata)

# Outside bars
#ggplot(data=neddf, aes(x = dose, y=len)) +
#  geom_bar(stat="identity", fill="steelblue")+
#  geom_text(aes(label=len), vjust=-0.3, size=3.5)+
#  theme_minimal()