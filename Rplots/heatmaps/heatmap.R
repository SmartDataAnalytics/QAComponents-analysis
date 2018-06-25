#########################################################
### install and load R packages
#########################################################

if (!require("pheatmap")) {
  install.packages("pheatmap", dependencies = TRUE)
}

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
}

library(pheatmap)
library(RColorBrewer)


#########################################################
### read data and transform it to matrix format
#########################################################

loadData <- function(file, from, to) {
  input <- read.csv(file, sep = ',')
  
  # first remember the names
  n <- input$Feature
  # transpose all but the first column (name)
  input <- as.data.frame(t(input[,-1]))
  colnames(input) <- n
  
  input <- input[from:to,]
  return(data.matrix(input))
}


#########################################################
### create and save heatmap plot
#########################################################

getHeatmap <- function(input_matrix, xlabels) {
  input_heatmap <- pheatmap(input_matrix,
                            color = brewer.pal(9,'Blues'),
                            cluster_rows = FALSE,
                            cluster_cols = FALSE,
                            drop_levels = T,
                            fontsize_row = 9,
                            labels_row = xlabels
                           )
  return(input_heatmap)
}

plotHeatmap <- function(folder, fname, from, to, xlabels, w, h) {
  precision <- loadData(paste0(folder, fname, "-precision.csv"), from, to)
  heatmap1 <- getHeatmap(precision, xlabels)
  dev.print(pdf, file = paste0(fname, "-precision.pdf"), width = w, height = h)
  
  recall <- loadData(paste0(folder, fname, "-recall.csv"), from, to)
  heatmap2 <- getHeatmap(recall, xlabels)
  dev.print(pdf, file = paste0(fname, "-recall.pdf"), width = w, height = h)
  
  fscore <- loadData(paste0(folder, fname, "-fscore.csv"), from, to)
  heatmap3 <- getHeatmap(fscore, xlabels)
  dev.print(pdf, file = paste0(fname, "-fscore.pdf"), width = w, height = h)
}


#########################################################
### print heatmaps
#########################################################

# NED
nedlabels = c("AylienNER + AGDISTIS","Tagme NED","DBpedia NER + AGDISTIS",
            "Ambiverse NED","MeaningCloud NER + AGDISTIS","DBpedia NED",
            "MeaningCloud NED","Textrazor NER + AGDISTIS",
            "Dandelion NER + AGDISTIS","Dandelion NED","Ontotext NED",
            "Babelfy NER + AGDISTIS","Ambiverse NER + AGDISTIS",
            "Tagme NER + AGDISTIS","EntityClassifier NER + AGDISTIS",
            "Babelfy NED","Stanford NER + AGDISTIS","Ontotext NER + AGDISTIS",
            "AmbiverseNED 2.0",	"AmbiverseNER 2.0 + AGDISTIS")
plotHeatmap("data/", "ned", 1, 20, nedlabels)

# Relation Linker
rellabels = c("RNLIWOD", "RelMatch", "AnnotationofSpotProperty",
              "RelationMatcher", "ReMatch")
plotHeatmap("data/", "rel", 1, 5, rellabels, 8, 3)

# Class Component
classlabels = c("OKBQA DM CLS", "CLSNLIWOD")
plotHeatmap("data/", "class", 1, 2, classlabels, 3, 2)

# Query Builder
qblabels = c("NLIWOD QB", "SINA")
plotHeatmap("data/", "qb", 1, 2, qblabels, 4, 2)

dev.off()