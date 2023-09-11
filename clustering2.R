library(ggplot2)
library(stats)
library(factoextra)
library(datasets)
library(cluster)
library(stats)
library(NbClust)

data("USArrests")
df <- USArrests
str(USArrests)

df <- scale(USArrests)
head(USArrests, 3)

#Accessing clustering tendency
#1. Visual inspection to check if the data contain meaningful clusters
#Since the data contain more than two variables, there is need to reduce 
#it's dimension(dimensionality) in order to plot a scatter plot

fviz_pca_ind(prcomp(data), title="PCA - Iris data", 
             habillage = iris$Species, palette="jco",
             geom= "point", ggtheme=theme_classic(),
             legend="bottom")


fviz_nbclust(x, FUNcluster, method= c("silhouette", "wss", "gap_statistics"))

#Elbow method
fviz_nbclust(df, kmeans, method="wss")+
  geom_vline(xintercept = 4, linetype=2)+
  labs(subtitle = "Elbow method")

#Silhouette method
fviz_nbclust(df, kmeans, method="silhouette")+
  labs(subtitle = "Silhouette method")

#Gap statistic
#nboot = 50 to keep the function speedy
#recommended value: nboot= 500 for your analysis
#use verbose = FALSE to hide computing progression
set.seed(123)
fviz_nbclust(df, kmeans, nstart=25, method="gap_stat", nboot=50)+
  labs(subtitle="Gap statistic method")

#30 indices method
library(NbClust)
NbClust(df, distance="euclidean", min.nc=2,
        max.nc=15, method="kmeans")


