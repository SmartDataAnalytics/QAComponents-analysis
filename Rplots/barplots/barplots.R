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

plotbars1 <- function(data, ylab, title, fill) {
  p <- ggplot(data = data,
              aes(x = feature, y = number)) +
    xlab(NULL) +
    ylab(ylab) +
    ggtitle(title) +
    geom_bar(stat = "identity",
             fill = fill) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 15))
  return(p)
}

plotbars2 <- function(data, xlab, ylab) {
  p <- ggplot(data = data,
              aes(x = feature, y = number)) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal() +
    geom_bar(stat = "identity",
             aes(fill = factor(feature))) +
    scale_fill_manual("legend", values = c("NED" = "#817f82", "RL" = "#b89685", "CL" = "#bcabae", "QB" = "#322214")) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size = 22))
  return(p)
}


#########################################################
### plot all barplots
#########################################################

# Unanswered questions per question type
neddata <- read.csv("data/ned-unanswered.csv", sep = ',')
plot1 <- plotbars1(neddata, "# of Questions", "NED", "#817f82") +
  scale_y_continuous(limits = c(0, 900),
                     breaks = c(50, 150, 300, 450, 600, 750))

reldata <- read.csv("data/rel-unanswered.csv", sep = ',')
plot2 <- plotbars1(reldata, "# of Questions", "RL", "#b89685") +
  scale_y_continuous(limits = c(0, 900),
                     breaks = c(250, 500, 750))

classdata <- read.csv("data/class-unanswered.csv", sep = ',')
plot3 <- plotbars1(classdata, "", "CL", "#bcabae") +
  scale_y_continuous(limits = c(0, 900),
                     breaks = c(250, 500, 750)) +
  theme(axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 25, l = 0)))

qbdata <- read.csv("data/qb-unanswered.csv", sep = ',')
plot4 <- plotbars1(qbdata, "", "QB", "#322214") +
  scale_y_continuous(limits = c(0, 900),
                     breaks = c(250, 500, 750))

pdf("unanswered-1.pdf")
lay <- rbind(c(1,1,1,1,1),
             c(2,2,3,4,4))
grid.arrange(plot1, plot2, plot3, plot4, layout_matrix = lay, heights=c(7,4.5))
dev.off()

# All unanswered questions
allunanswered <- read.csv("data/unanswered.csv", sep = ',')
allunanswered$feature <- factor(allunanswered$feature,
                         levels = c("NED", 
                                    "RL", 
                                    "CL", 
                                    "QB"),
                         ordered = TRUE)
plot5 <- plotbars2(allunanswered, "QA Component Type", "# of Questions")

ggsave("unanswered-2.pdf", plot5)

