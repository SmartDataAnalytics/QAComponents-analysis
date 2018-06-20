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

#########################################################
### cluster components with respect to F-values
#########################################################

#########################################################
### cluster components with respect to questions
#########################################################

data = loadData("data/ned-fscore.csv")
df <- as.matrix(data)
df <- t(df)

# data("USArrests")      # Loading the data set
# df <- scale(USArrests) # Scaling the data
# View the firt 3 rows of the data
# head(df, n = 3)

fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)

# It’s possible to compute the mean of each variables by clusters using the original data:
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

# If you want to add the point classifications to the original data, use this:
dd <- cbind(USArrests, cluster = km.res$cluster)

# Cluster number for each of the observations
km.res$cluster

# Cluster size
km.res$size

# Cluster means
km.res$centers
head(dd)

# Visualize k-means clusters
fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

dev.print(pdf, file = "clustering-1.pdf")
dev.off()