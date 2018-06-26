#########################################################
### install and load R packages
#########################################################

if (!require("factoextra")) {
  if(!require(devtools)) install.packages("devtools")
  devtools::install_github("kassambara/factoextra")
}

library(factoextra)


#########################################################
### functions
#########################################################

loadData <- function(file) {
  input <- read.csv(file, sep = ',', row.names = 1)
}

# Visualize k-means clusters
visualizeCluster <- function(d, df) {
  fviz_cluster(d,
               data = df,
               palette = "Set1",
               main = "",
               xlab = NULL,
               ylab = NULL,
               ellipse.type = "euclid",
               show.clust.cent = TRUE,
               star.plot = TRUE, # Add segments from centroids to items
               repel = TRUE, # Avoid label overplotting (slow)
               labelsize = 16,
               ggtheme = theme_minimal(base_size = 16)
  )
}

visualizePCA <- function(pca) {
  fviz_pca_var(pca,
               geom = c("arrow", "text"),
               repel = TRUE,
               col.var = "steelblue",
               select.var = list(contrib = 3),
               alpha.var="contrib",
               labelsize = 6,
               ggtheme = theme_minimal(base_size = 16)
  )
}

visualizeHclust <- function(df) {
  d <- dist(df)
  hc <- hclust(d) 
  plot(hc,
       main = NULL,
       xlab = NULL,
       ylab = NULL,
       axes = FALSE,
       sub = NULL)
}

#########################################################
### cluster components with respect to F-values
#########################################################

# Cluster NED components
#########################################################
neddata = loadData("data/ned-fscore.csv")
# Create matrix and transpose
neddf <- t(as.matrix(neddata))

# Find best value for k
# The location of a bend (knee) in the plot is generally considered
# as an indicator of the appropriate number of clusters.
fviz_nbclust(neddf, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
dev.print(pdf, file = "ned-1-cut.pdf")

# Compute k-means with k = 3
set.seed(123)
km1.res <- kmeans(neddf, 3, nstart = 25)

# Visualize k-means clusters
visualizeCluster(km1.res, neddf)
dev.print(pdf, file = "ned-clustering-1.pdf")

# Run Principal Component Analysis (PCA)
neddf.pca <- prcomp(neddf,  scale = TRUE)

# Hierarchical clustering
visualizeHclust(neddf)
dev.print(pdf, file = "ned-hierarchical-1.pdf")

# Visualize variables contributing most to PCA
visualizePCA(neddf.pca)
dev.print(pdf, file = "ned-pca-1.pdf")

# Cluster RL components
#########################################################
reldata = loadData("data/rel-fscore.csv")
# Remove rows with all values 0
reldata <- reldata[!!rowSums(abs(reldata[-c(1:2)])),]

reldf <- t(as.matrix(reldata))

# Find optimal k
fviz_nbclust(reldf, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)
dev.print(pdf, file = "rel-1-cut.pdf")

# Compute k-means with k = 2
set.seed(123)
km2.res <- kmeans(reldf, 2, nstart = 25)

visualizeCluster(km2.res, reldf)
dev.print(pdf, file = "rel-clustering-1.pdf")

# Hierarchical clustering
visualizeHclust(reldf)
dev.print(pdf, file = "rel-hierarchical-1.pdf")

reldf.pca <- prcomp(reldf,  scale = TRUE)
visualizePCA(reldf.pca)
dev.print(pdf, file = "rel-pca-1.pdf")



#########################################################
### cluster components with respect to questions
#########################################################

# Cluster NED components
#########################################################
neddata = loadData("data/ned-questions.csv")
# Replace NA values with 0
neddata[is.na(neddata)] <- 0
# Remove rows with all values 0
neddata <- neddata[!!rowSums(abs(neddata[-c(1:2)])),]

# Create matrix and transpose
neddf <- t(as.matrix(neddata))

# Find best value for k
# The location of a bend (knee) in the plot is generally considered
# as an indicator of the appropriate number of clusters.
fviz_nbclust(neddf, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)
dev.print(pdf, file = "ned-2-cut.pdf")

# Compute k-means with k = 2
set.seed(123)
km1.res <- kmeans(neddf, 2, nstart = 25)

# Remove constant rows
neddf <- neddf[ , apply(neddf, 2, var) != 0]
# Visualize k-means clusters
visualizeCluster(km1.res, neddf)
dev.print(pdf, file = "ned-clustering-2.pdf")

# Hierarchical clustering
visualizeHclust(neddf)
dev.print(pdf, file = "ned-hierarchical-2.pdf")

# Run Principal Component Analysis (PCA)
neddf.pca <- prcomp(neddf,  scale = TRUE)

# Cluster RL components
#########################################################
reldata = loadData("data/rel-questions.csv")
# Replace NA values with 0
reldata[is.na(reldata)] <- 0
# Remove rows with all values 0
reldata <- reldata[!!rowSums(abs(reldata[-c(1:2)])),]

reldf <- t(as.matrix(reldata))

# Find optimal k
fviz_nbclust(reldf, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)
dev.print(pdf, file = "rel-2-cut.pdf")

# Compute k-means with k = 2
set.seed(123)
km2.res <- kmeans(reldf, 2, nstart = 25)

# Remove constant rows
reldf <- reldf[ , apply(reldf, 2, var) != 0]

# Hierarchical clustering
visualizeHclust(reldf)
dev.print(pdf, file = "rel-hierarchical-2.pdf")

visualizeCluster(km2.res, reldf)
dev.print(pdf, file = "rel-clustering-2.pdf")

# Hierarchical clustering
#########################################################


dev.off()
