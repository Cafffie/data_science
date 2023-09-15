library(factoextra)
library(cluster)
library(tidyverse)
library("FactoMineR")
library("factoextra")
library(cluster)


bank_customers <- read.csv("./data_Science/Bank customers.csv")
bankCustomers <- read.csv("./data_science2/Bank customers.csv")


table(bankCustomers$ Attrition_Flag)
length(bank_customers$ Attrition_Flag)

dim(bank_customers)
head(bank_customers)
colSums(is.na(bank_customers))
anyNA(bank_customers)
table(bank_customers$Card_Category)

summary(bank_customers$Card_Category)
str(bank_customers)
unique(bank_customers$Gender)

rep <- c("M","F")
code <- c(0,1)
bank_customers$Gender <- code[match(bank_customers$Gender, rep)]
bank_customers$Gender <- as.factor(bank_customers$Gender)

unique(bank_customers$Education_Level)
rep1 <- c( "Unknown","Uneducated", "High School","College", "Graduate", "Post-Graduate", "Doctorate")
code1 <- c(0,2,4,6,8,10,12)
bank_customers$Education_Level <- code1[match(bank_customers$Education_Level,rep1)]
bank_customers$Education_Level <- as.factor(bank_customers$Education_Level)

unique(bank_customers$Marital_Status)
rep2<- c("Unknown", "Single", "Married", "Divorced")
code2 <- c(0,5,10,15)
bank_customers$Marital_Status <- code2[match(bank_customers$Marital_Status,rep2)]
bank_customers$Marital_Status <- as.factor(bank_customers$Marital_Status)

unique(bank_customers$Income_Category)
rep3<- c("Unknown", "Less than $40K", "$40K - $60K","$60K - $80K", "$80K - $120K" ,  "$120K +")
code3<- c(0,10,20,30,40,50)
bank_customers$Income_Category <- code3[match(bank_customers$Income_Category,rep3)]
bank_customers$Income_Category <- as.factor(bank_customers$Income_Category)

unique(bank_customers$Card_Category)
rep4<- c( "Blue","Gold","Silver","Platinum")
code4<- c(1,2,3,4)
bank_customers$Card_Category <- code4[match(bank_customers$Card_Category,rep4)]
bank_customers$Card_Category <- as.factor(bank_customers$Card_Category)

bank_customers <- bank_customers[,-c(1,2)]

#Exploring relationship between variables
cor(bank_customers)
mean(cor(bank_customers))
cor(bank_customers)
bank_customers %>%
  plot_correlation(maxcat = 15)


dim(bank_customers)
head(bank_customers)
head(bankCustomers)
str(bank_customers)


#bank_customers$Total_Trans_Ct <- as.numeric(bank_customers$Total_Trans_Ct)
#bank_customers$Total_Trans_Amt <- as.numeric(bank_customers$Total_Trans_Amt)
#bank_customers$Total_Revolving_Bal <- as.numeric(bank_customers$Total_Revolving_Bal)
#bank_customers$Contacts_Count_12_mon <- as.numeric(bank_customers$Contacts_Count_12_mon)
#bank_customers$Months_Inactive_12_mon <- as.numeric(bank_customers$Months_Inactive_12_mon)
#bank_customers$Total_Relationship_Count <- as.numeric(bank_customers$Total_Relationship_Count)
#bank_customers$Months_on_book <- as.numeric(bank_customers$Months_on_book)
#bank_customers$Dependent_count <- as.numeric(bank_customers$Dependent_count)
#bank_customers$Customer_Age <- as.numeric(bank_customers$Customer_Age)
#bank_customers$CLIENTNUM <- as.numeric(bank_customers$CLIENTNUM)
#bank_customers$Gender <- as.numeric(bank_customers$Gender)
#bank_customers$Education_Level <- as.numeric(bank_customers$Education_Level)
#bank_customers$Marital_Status <- as.numeric(bank_customers$Marital_Status)
#bank_customers$Income_Category <- as.numeric(bank_customers$Income_Category)
#bank_customers$Card_Category <- as.numeric(bank_customers$Card_Category)

#colnames(bank_customers)[1] <- "Client_Number"
#scaled_data <- scale(bank_customers)
#bank_customers$label <- paste("C", 1:nrow(bank_customers), sep="_")


str(bank_customers)
head(bank_customers)

##Visual inspection to check if the data contain meaningful clusters
fviz_pca_ind(prcomp(sel_variables), title="PCA - bank_customer", 
             habillage = bankCustomers$Attrition_Flag, palette="jco",
             geom= "point", ggtheme=theme_classic(),
             legend="bottom")

#PCA
scaled_data <- scale(bank_customers)
bank_pca <- PCA(scaled_data)
bank_pca2 <- prcomp(scaled_data)
eigenvalue <- get_eigenvalue(bank_pca2)
fviz_eig(bank_pca2, addlabels = T)

#Loading scores
bank_pca2$rotation
variables <- get_pca_var(bank_pca2)
contributions <- variables$contrib

#Visualizing the principal components
fviz_contrib(bank_pca2, choice="var", axes=1)
fviz_contrib(bank_pca2, choice="var", axes=2)
fviz_contrib(bank_pca2, choice="var", axes=3)
fviz_contrib(bank_pca2, choice="var", axes=4)
fviz_contrib(bank_pca2, choice="var", axes=5)


#Selecting variables
sel_variables <- bank_customers[,-c(3,4,5,9,10, 11,15,18)]
dim(sel_variables)
head(sel_variables)

#Standardizing the data
scale_variables <- scale(sel_variables)

#Distance
dist <- get_dist(scale_variables, method="spearman")

#Building hierarchichal clustering model
hc$ <- hclust(d=dist, method= "ward.D2")

#Visualizing the clusters
plot(hc, cex=0.5)
fviz_dend(hc, cex=0.5)

fviz_cluster(list(data=data, cluster=cutree(hcl, k=3)), labelsize=8)
fviz_cluster(list(data=sel_variables, cluster=cutree(hc, k=2)), labelsize=8)
fviz_cluster(list(data=sel_variables, cluster=cutree(hc, k=3)), labelsize=8)
fviz_cluster(list(data=sel_variables, cluster=cutree(hc, k=7)), labelsize=8)



#Convert to csv file
#write.csv(sel_variables, "./data_Science/selected_bank_variables.csv")

#Using kmeans model
km <- kmeans(scale_variables, centers=2)
fviz_cluster(list(data=sel_variables, cluster=km$cluster))


fviz_nbclust(scale_variables, kmeans, method = "wss")
fviz_nbclust(scale_variables, kmeans, method = "silhouette")
fviz_nbclust(scale_variables, kmeans, method = "gap_stat")


sel_variables$cluster <-km$cluster 
km$centers
table(sel_variables$cluster, bankCustomers$ Attrition_Flag)
silhouette_score <- silhouette(sel_variables$cluster, dist(sel_variables))
print(silhouette_score)
mean_silhouette <- mean(silhouette_score[, "sil_width"])
mean_silhouette = 0.6210227
















#Accessing clustering tendency
#Step 1. Visual inspection to check if the data contain meaningful clusters
#Since the data contain more than two variables, there is need to reduce 
#it's dimension(dimensionality) in order to plot a scatter plot

fviz_pca_ind(prcomp(scaled_data), title="PCA - bank_customers", 
             habillage = bankCustomers$ Attrition_Flag, palette="jco",
             geom= "point", ggtheme=theme_classic(),
             legend="bottom")

km <- kmeans(data, 3)
fviz_cluster(list(data=data, cluster=km$cluster), ellipse.type="norm", 
             geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic())


#Step 2: Applying the Statistical method (Hopkins statistic) & Visual method
#1.Statistical method (Hopkins statistic): H

set.seed(123)
hopkins(scaled_data, n= nrow(scaled_data)-1)
#H= 0.1727501: this data set is highly clusterable because H is far below the threshold (0.5)
#if H is far below 0.5: It is highly clusterable
#If H is close to 0.5: It is not clusterable

#2. Visual method
fviz_dist(dist(df), show_labels= FALSE)+
  labs(title="Iris data")










#####
#Determining the optimal number of cluster k
#Elbow method
fviz_nbclust(scaled_data, kmeans, method="wss")+
  geom_vline(xintercept = 4, linetype=2)+
  labs(subtitle = "Elbow method")

#Silhouette method
fviz_nbclust(scaled_data, kmeans, method="silhouette")+
  labs(subtitle = "Silhouette method")

#Gap statistic
#nboot = 50 to keep the function speedy
#recommended value: nboot= 500 for your analysis
#use verbose = FALSE to hide computing progression
set.seed(123)
fviz_nbclust(scaled_data, kmeans, nstart=25, method="gap_stat", nboot=50)+
  labs(subtitle="Gap statistic method")

#30 indices method
library(NbClust)
NbClust(scaled_data, distance="euclidean", min.nc=2,
        max.nc=15, method="kmeans")
#######

#####
#Choosing the best clustering algorithm
#First method: Internal measures

library(clValid)
data <- scale(iris[,-5])

clmethods <- c("hierarchical", "kmeans", "pam")
intern <- clValid(scaled_data, nClust = 2:6, 
                  clMethods = clmethods, validation="internal")
summary(intern)


#Second method: Stability measures
clmethods <- c("hierarchical", "kmeans", "pam")
stab <- clValid(scaled_data, nClust = 2:6, clMethods= clmethods,
                validation = "stability")
#display only optimal scores
optimalScores(stab)

####





km_fit <- kmeans(bank_customers, centers = 5)
km_fit <- kmeans(bank_customers, centers = 5, iter=100, nstart = 100)

bank_customers$cluster <- km_fit$cluster
rownames(bank_customers) <- paste("C", 1:nrow(bank_customers), sep="_")
colnames(bank_customers)[,1] <- "Label"
fviz_cluster(km_fit, data=bank_customers)
head(bank_customers)
str(bank_customers)
bank_customers$label <- paste("C", 1:nrow(bank_customers), sep="_")
