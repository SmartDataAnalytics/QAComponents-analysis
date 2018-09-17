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

# single barplot for each QA task (unanswered questions)
plotUnansweredPerTask <- function(data, ylab, title, fill) {
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

# barplot for all QA tasks (unanswered questions)
plotAllUnanswered <- function(data, xlab, ylab) {
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

# barplot frequency of features
plotFrequencies <- function(data, xlab, ylab) {
  p <- ggplot(data = data,
              aes(x = feature, y = questions)) +
    xlab(xlab) +
    ylab(ylab) +
    theme_minimal() +
    geom_bar(stat = "identity",
             aes(fill = factor(task))) +
    scale_fill_manual("legend", values = c("NED" = "#817f82", "RL" = "#b89685", "CL" = "#bcabae", "QB" = "#322214")) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 22),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.y = element_text(size = 22))
  return(p)
}

#########################################################
### plot all barplots
#########################################################

# Unanswered questions per question type
neddata <- read.csv("data/ned-unanswered.csv", sep = ',')
plot1 <- plotUnansweredPerTask(neddata, "# of Questions", "NED", "#817f82")
  # scale_y_continuous(limits = c(0, 900),
  #                    breaks = c(50, 150, 300, 450, 600, 750))

reldata <- read.csv("data/rel-unanswered.csv", sep = ',')
plot2 <- plotUnansweredPerTask(reldata, "# of Questions", "RL", "#b89685")
  # scale_y_continuous(limits = c(0, 900),
  #                    breaks = c(250, 500, 750))

classdata <- read.csv("data/class-unanswered.csv", sep = ',')
plot3 <- plotUnansweredPerTask(classdata, "", "CL", "#bcabae") +
  # scale_y_continuous(limits = c(0, 900),
  #                    breaks = c(250, 500, 750)) +
  theme(axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 25, l = 0, unit = "pt")))

qbdata <- read.csv("data/qb-unanswered.csv", sep = ',')
plot4 <- plotUnansweredPerTask(qbdata, "", "QB", "#322214")
  # scale_y_continuous(limits = c(0, 900),
  #                    breaks = c(250, 500, 750))

pdf("unanswered-1.pdf")
lay <- rbind(c(1,1,1,1,1),
             c(2,2,3,4,4))
grid.arrange(plot1, plot2, plot3, plot4, layout_matrix = lay, heights = c(6.5,4.5))

# All unanswered questions
allunanswered <- read.csv("data/unanswered.csv", sep = ',')
allunanswered$feature <- factor(allunanswered$feature,
                         levels = c("NED", 
                                    "RL", 
                                    "CL", 
                                    "QB"),
                         ordered = TRUE)
plot5 <- plotAllUnanswered(allunanswered, "QA Component Type", "# of Questions")

ggsave("unanswered-2.pdf", plot5)

# Frequency of features along the questions
frequencies <- read.csv("data/question-features.csv", sep = ',')
frequencies$feature <- factor(frequencies$feature,
                              levels = c("e=1", "e=1, caps", "e=1, no caps", "e=1, explicit", "e=1, implicit",
                                         "e=1, special char", "e=1, no special char", "e=1, words=1 or 1.5",
                                         "e=1, words=2 or 2.5", "e=1, words=3 or 3.5", "e=1, words=4 or 4.5",
                                         "e=1, words=5 or more", "e=2", "e=2, no caps", "e=2, one in caps",
                                         "e=2, all in caps", "e=2, explicit", "e=2, one explicit",
                                         "e=2, implicit", "e=2, no special char", "e=2, one with special char",
                                         "e=2, special char", "e=2, words=1 or 1.5", "e=2, words=2 or 2.5",
                                         "e=2, words=3 or 3.5", "e=2, words=4 or 4.5", "rel=1", "rel=2",
                                         "rel=1, explicit", "rel=1, implicit", "rel=1, not hidden",
                                         "rel=1, hidden", "rel=1, wc=1", "rel=1, wc>=2",
                                         "rel=2, 0 hidden", "rel=2, 1 or 2 hidden",
                                         "rel=2, 0 explicit", "rel=2, 1 explicit", "rel=2, 2 explicit",
                                         "rel=2, av. wc=1", "rel=2, av. wc=1.5", "rel=2, av. wc=2",
                                         "implicit class", "explicit class", "wc=0 or 1", "wc=2", "wc=3",
                                         "triple size=2", "triple size=3", "triple size=4", "resources=1",
                                         "resources=2", "relations=1", "relations=2", "classes=0", "classes=1",
                                         "answer type=list", "answer type=count", "answer type=boolean"
                              ),
                              ordered = TRUE)
plot6 <- plotFrequencies(frequencies, "", "# of Questions")

ggsave("feature-frequencies.pdf", plot6, height = 10, width = 20)

dev.off()