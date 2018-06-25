#########################################################
### install and load R packages
#########################################################

if (!require("gridExtra")) {
  install.packages("gridExtra")
}
if (!require("grid")) {
  install.packages("grid")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(gridExtra)
library(grid)
library(ggplot2)


#########################################################
### functions
#########################################################

plotbars <- function(data, ylab, title) {
  p <- ggplot(data = data,
              aes(x = feature, y = number)) +
    xlab(NULL) +
    ylab(ylab) +
    ggtitle(title) +
    geom_bar(stat = "identity",
             fill = "steelblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12))
  return(p)
}


#########################################################
### plot all barplots
#########################################################

# Unanswered questions per question type
neddata <- read.csv("data/ned-unanswered.csv", sep = ',')
plot1 <- plotbars(neddata, "Number of Questions", "NED") +
  scale_y_continuous(limits = c(0, 850))

reldata <- read.csv("data/rel-unanswered.csv", sep = ',')
plot2 <- plotbars(reldata, "Number of Questions", "RE") +
  scale_y_continuous(limits = c(0, 850))

classdata <- read.csv("data/class-unanswered.csv", sep = ',')
plot3 <- plotbars(classdata, "", "Class") +
  scale_y_continuous(limits = c(0, 850)) +
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 25, l = 0)))

qbdata <- read.csv("data/qb-unanswered.csv", sep = ',')
plot4 <- plotbars(qbdata, "", "QB") +
  scale_y_continuous(limits = c(0, 850))

pdf("unanswered-1.pdf")
lay <- rbind(c(1,1,1,1,1),
             c(2,2,3,4,4))
grid.arrange(plot1, plot2, plot3, plot4, layout_matrix = lay)
dev.off()

# All unanswered questions
allunanswered <- read.csv("data/unanswered.csv", sep = ',')
plot5 <- plotbars(allunanswered, "Number of Questions", "")

ggsave("unanswered-2.pdf", plot5 +
         xlab("QA Component Type") +
         theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 16),
               axis.text.y = element_text(size = 16),
               axis.title.y = element_text(size = 18)))
