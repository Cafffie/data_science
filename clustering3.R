library(ggplot2)
library(stats)
library(factoextra)
library(datasets)
library(cluster)
library(stats)
library(NbClust)
library(clustertend)


data("iris")
data <- iris
str(iris)

data <- iris[, 1:4]
data <- scale(df)
head(data, 3)

#Accessing clustering tendency
#Step 1. Visual inspection to check if the data contain meaningful clusters
#Since the data contain more than two variables, there is need to reduce 
#it's dimension(dimensionality) in order to plot a scatter plot

fviz_pca_ind(prcomp(data), title="PCA - Iris data", 
             habillage = iris$Species, palette="jco",
             geom= "point", ggtheme=theme_classic(),
             legend="bottom")

km <- kmeans(data, 3)
fviz_cluster(list(data=data, cluster=km$cluster), ellipse.type="norm", 
             geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic())

#Step 2
#Statistical method (Hopkins statistic)
#Visual method






#Determining the optimal number of cluster k
#Elbow method
fviz_nbclust(data, kmeans, method="wss")+
  geom_vline(xintercept = 4, linetype=2)+
  labs(subtitle = "Elbow method")

#Silhouette method
fviz_nbclust(data, kmeans, method="silhouette")+
  labs(subtitle = "Silhouette method")

#Gap statistic
#nboot = 50 to keep the function speedy
#recommended value: nboot= 500 for your analysis
#use verbose = FALSE to hide computing progression
set.seed(123)
fviz_nbclust(data, kmeans, nstart=25, method="gap_stat", nboot=50)+
  labs(subtitle="Gap statistic method")

#30 indices method
library(NbClust)
NbClust(data, distance="euclidean", min.nc=2,
        max.nc=15, method="kmeans")













