#########################################################
### install and load R packages
#########################################################

if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
}

library(ggplot2)

#########################################################
### read data and transform it to data frame
#########################################################

loadData <- function(file) {
  input <- read.csv(file, sep = ',')
}

datn = loadData("components.csv")

#########################################################
### line plot
#########################################################
ggplot(data = datn,
       aes(x = Components, y = Questions, group = ComponentType, colour = ComponentType)) +
  geom_line(size = 2) +
  geom_point(size = 3) +
  labs(x = "Top-n QA Components", y= "Answered Questions", colour = "QA Component Type") +
  scale_color_brewer(type = "div") +
  theme_light() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        panel.grid.major = element_line(colour = "darkgrey"))

dev.print(pdf, file = "top-components.pdf")
dev.off()