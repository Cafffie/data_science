library(tidyverse)
library(learnr)
library(explore)
library(SmartEDA)
library(wordcloud2)
library(readxl)
library(DataExplorer)
library(tidytext)
library(wordcloud)
library(topicmodels)
library(tm)
library(visNetwork)
library(qdapRegex)
library(syuzhet)
library(ggraph)
library(quanteda)                                                        #For bigram wordcloud
library(stringr)                                                         #For str_replace_all
library(ggplot2)

#Data wrangling
covid_data <- read.csv("./data_science1/covid_19.csv")
covid_data %>% 
  subset(states == "Kaduna") %>% 
  group_by(states, month) %>% 
  summarize(count = n()) %>% 
  as.data.frame()

cluster_summary <- clustered_data %>%
  group_by(cluster) %>%
  summarise(avg_income=mean(Income),
            avg_spending_score=mean(SpendingScore),
            ncustomers=n())




#Logistic regression machine learning
library(caret)
library(kernlab)
mtcars <- mtcars
model <- glm(vs ~ wt + disp, data=mtcars, family = "binomial")
summary(model)

diabetes <- read.csv("./data_science2/diabetes.csv")
str(diabetes)
diabetes$Outcome <- as.factor(diabetes$Outcome)

log <- glm(Outcome ~., data = diabetes, family= "binomial")
summary(log)

set.seed(234)
intrain <- createDataPartition(y=diabetes$Outcome, p=0.8, list=FALSE)
train <- diabetes[intrain,]
test <- diabetes[-intrain,]
log_model <- glm(Outcome ~., data=diabetes, family = "binomial")
summary(log_model)
predictions <- predict(log_model, test, type= "response")
confusion_matrix <- table(test$Outcome, predictions>0.3)
(confusion_matrix[1,1] + confusion_matrix[2,2])/sum(confusion_matrix)


library(ROCR)
library(pROC)
ROC_prediction <- predict(log_model, train, type="response")
ROCRPred <- prediction(ROC_prediction, train$Outcome)
ROCRperf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

table(test$Outcome, predictions>0.3)
(70+43)/(70+30+10+43)= 0.73856

table(test$Outcome, predictions>0.4)
(82+43)/(82+34+18+19)= 0.81699

table(test$Outcome, predictions>0.5)
(90+31)/(90+10+22+31)= 0.79084



#Logistic regression model
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart_data <- read.csv(url, header = FALSE)
head(heart_data)
colnames(heart_data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)
head(heart_data)
str(heart_data)
heart_data[heart_data =="?"] <- NA
heart_data[heart_data$sex == 0,]$sex <- "F"
heart_data[heart_data$sex == 1,]$sex <- "M"

heart_data$sex <- as.factor(heart_data$sex)
heart_data$cp <- as.factor(heart_data$cp)
heart_data$fbs <- as.factor(heart_data$fbs)
heart_data$restecg <- as.factor(heart_data$restecg)
heart_data$exang <- as.factor(heart_data$exang)
heart_data$slope <- as.factor(heart_data$slope)

heart_data$ca <- as.integer(heart_data$ca)
heart_data$ca <- as.factor(heart_data$ca)

heart_data$thal <- as.integer(heart_data$thal)
heart_data$thal <- as.factor(heart_data$thal)

heart_data$hd <- ifelse(heart_data$hd == 0, yes="healthy", no="Unhealthy")
heart_data$hd <- as.factor(heart_data$hd)

#Checking columns containing NA
colSums(is.na(heart_data)) 

#Checking the number of rows containing NA
nrow(heart_data[is.na(heart_data$ca) | is.na(heart_data$thal),])
heart_data[is.na(heart_data$ca) | is.na(heart_data$thal),]

nrow(heart_data)
ncol(heart_data)

heart_data <- heart_data[!(is.na(heart_data$ca) | is.na(heart_data$thal)),]
nrow(heart_data)

#table(heart_data$hd, heart_data$sex)
xtabs(~ hd + sex, data=heart_data)
xtabs(~ hd + cp, data=heart_data)
xtabs(~ hd + fbs, data=heart_data)
xtabs(~ hd + restecg, data=heart_data)
xtabs(~hd + slope, data=heart_data)
xtabs(~hd + ca, data=heart_data)
xtabs(~hd + thal, data=heart_data)

logistic <- glm(hd ~ sex, data=heart_data, family="binomial")
summary(logistic)

logistic2 <- glm(hd ~., data=heart_data, family="binomial")
summary(logistic2)

#Calculating McFadden's Pseudo R^2
#ll.null(null deviance): log-likelihood of the null model
#ll.proposed(residual deviance): log-likelihood for the fancy model
ll.null2 <- logistic2$null.deviance/-2
ll.proposed2 <- logistic2$deviance/-2
(ll.null2- ll.proposed2)/ll.null2
#this is the overall effect size (R^2): 0.5533531

#Calculating a p-value for that R^2
1 - pchisq(2*(ll.proposed2 - ll.null2), df= (length(logistic2$coefficients) -1))
#p-value = 0 : Since the p-value is tiny, R^2 isn't due to dumb luck

#Plotting the logistic graph: creating the dataframe first
predicted_data <- data.frame(
  probability.of.hd= logistic2$fitted.values,
  hd= heart_data$hd
)
#Sorting from low probabilities to high
predicted_data <- predicted_data[order(predicted_data$probability.of.hd, decreasing = FALSE),]
head(predicted_data)

predicted_data$rank <- 1:nrow(predicted_data)

library(ggplot2)
library(cowplot)

ggplot(data=predicted_data, aes(x=rank, y= probability.of.hd))+
  geom_point()

ggplot(data=predicted_data, aes(x=rank, y= probability.of.hd))+
  geom_point(aes(color=hd), alpha=1, shape=4) +
  xlab("index")+
  ylab("Predicted probability of getting heart disease")

ggplot(data=predicted_data, aes(x=rank, y= probability.of.hd, label=hd))+
  geom_text(aes(label=hd, color=hd)) +
  xlab("index")+
  ylab("Predicted probability of getting heart disease")

#ROC curve
roc(heart_data$hd, logistic2$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage", ylab="False Positive Percentage", col="#377eb8",
    lwd=4, print.auc=TRUE, print.auc.x=45)




#AUC and ROC curve
library(pROC)
library(randomForest)
set.seed(420)

n <- 100
weight <- sort(rnorm(n=n, mean=172, sd=29))
obese <- ifelse(test= (runif(n) < (rank(weight)/100)), 1, 0)
plot(x=weight, y=obese)                

logistic <- glm(obese ~ weight, family="binomial")
lines(weight, logistic$fitted.values)

#Estimated probabilities that the samples are obese
roc(obese, logistic$fitted.values, plot=TRUE)

par(pty="s")
roc(obese, logistic$fitted.values, plot=TRUE, legacy.axes=TRUE)
roc(obese, logistic$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False positive percentage", ylab="True positive percentage", col="#377eb8",
    lwd=4)
roc_info <- roc(obese, logistic$fitted.values, plot=TRUE, legacy.axes=TRUE)
roc_df <- data.frame(
  tpp= roc_info$sensitivities*100,
  fpp=(1-roc_info$specificities)*100,
  thresholds= roc_info$thresholds
)

#Selecting the threshold
head(roc_df)
tail(roc_df)
roc_df[roc_df$tpp > 60 & roc_df$tpp < 80,]

roc(obese, logistic$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False positive percentage", ylab="True positive percentage", col="#377eb8",
    lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100, 90), auc.polygon=TRUE,
    auc.polygon.col="#377eb822")

#Comparing logit and randomF ROC curve
rf.model <- randomForest(factor(obese) ~ weight)

roc(obese, logistic$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False positive percentage", ylab="True positive percentage", col="#377eb8",
    lwd=4, print.auc=TRUE)

plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4,
         print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Logistic Regression", "Random Forest"), 
       col=c("#377eb8", "#4daf4a", lwd=4))
par(pty="m")





#Text analysis
text_df <- tibble(line=1:4, text=text)
text_df %>%
  unnest_tokens(word, text)
tokens <- tokens(text)
bigrams <- tokens_ngrams(tokens, n = 2)



#order(column, )
#count(column, sort, categorical)
#Text analysis: word frequency
ola <- ai_sample_papers$abstract %>% 
  str_replace_all("[^A-Za-z]"," ") 

ola_df <- tibble(line=1:500, text=ola)
ola_df <- data.frame(ola_df)
ola_df <- ola_df %>%
  unnest_tokens(tokens, text)
word_freq <- ola_df %>%
  count(tokens, sort = TRUE)
freq_subset <- subset(word_freq, word_freq$n >150)


#Generating a bigram wordcloud
ola <- ai_sample_papers$abstract %>% 
  str_replace_all("[^A-Za-z]"," ") 
tokens <- tokens(ola) %>%
  tokens_remove(pattern = stopwords("en"))
bigrams <- tokens_ngrams(tokens, n = 3) 
unlist_bigrams <- unlist(bigrams)
bigram_freq <- table(unlist_bigrams)



#Manually generating bigrams
text <- "The quick brown fox jumps over the lazy dog. The lazy dog barks loudly."
# Tokenize the text
tokens <- tokens(text)
# Create bigrams manually
bigrams <- lapply(tokens, function(tok) {
  n <- length(tok)
  if (n > 1) {
    paste(tok[1:(n-1)], tok[2:n], sep = " ")
  } else {
    character(0)
  }
})
# Combine bigrams from all documents
all_bigrams <- unlist(bigrams)
# Convert bigrams to a table of word frequencies
bigram_freq <- table(all_bigrams)
# Generate the word cloud using the bigram frequencies
wordcloud(words = names(bigram_freq), freq = bigram_freq, scale = c(3, 0.5))



#Clustering
fviz_nbclust(scaled_features, kmeans, method="wss")
fviz_nbclust(scaled_features, kmeans, method="silhouette")
fviz_nbclust(scaled_features, kmeans, method="gap_stat")
fviz_cluster(list(data=scaled_features, cluster=cluster_assignments))



#pca
data.matrix <- matrix(nrow=100, ncol=10)
colnames(data.matrix) <- c(paste("wt", 1:5, sep=""),
                           paste("ko", 1:5, sep=""))
rownames(data.matrix) <- paste("gene", 1:100, sep="")
head(data.matrix)
for (i in 1:100){
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  data.matrix[i,] <- c(wt.values, ko.values)
}
trans_data <- t(data.matrix)
pca <- prcomp(t(data.matrix), scale=TRUE)
#
#pca$x[,1], pca$x[,2]
pca$x
plot(pca$x[,1], pca$x[,2])
#
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="scree plot", xlab="principal components", 
        ylab="percent variation")
#
pca.data <- data.frame(sample=rownames(pca$x),
                       X= pca$x[,1],
                       Y=pca$x[,2])
ggplot(data=pca.data, aes(x=X, y=Y, label=sample))+
  geom_text()+
  xlab(paste("pc1-", pca.var.per[1], "%", sep=""))+
  ylab(paste("pc2-", pca.var.per[2], "%", sep=""))+
  theme_bw()+
  ggtitle("My PCA graph")

loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores)
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])
pca$rotation[top_10_genes, 1]




#Server and UI (OJO)
library(shiny)
shinyServer(
  function(input, output) {
    output$oid1 <- renderPrint({input$input1})
    output$oid2 <- renderPrint({input$input2})
    output$odate <- renderPrint({input$ojo})
  }
)

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Sample with Inputs"),
  sidebarPanel(
    numericInput('input1', 'Numeric input, labeled input1', 0, min = 1, max = 11, step = 0.5),
    checkboxGroupInput("input2", "Checkbox",
                       c("Value 1" = "1",
                         "Value 2" = "2",
                         "Value 3" = "3")),
    dateInput("ojo", "Date:")
  ),
  mainPanel(
    h3('Showing Outputs'),
    h4('You have entered'),
    verbatimTextOutput("oid1"),
    h4('You have entered'),
    verbatimTextOutput("oid2"),
    h4('You have entered'),
    verbatimTextOutput("odate")
  )
))


#Glucose
library(shiny)
diabetesRisk <- function(glucose)
  glucose/200

shinyServer(
  function(input, output){
    output$inputValue <- renderPrint({input$glucose})
    output$prediction <- renderPrint({diabetesRisk(input$glucose)})
  }
)

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Diabetics Prediction"), 
  
  sidebarPanel(
    numericInput('glucose', 'Glucose mg/dl', 90, min=50, max=200, 
                 step=5),
    submitButton('submit'), 
  ),
  mainPanel(
    h3('Results of Prediction'),
    h4('You entered'),
    verbatimTextOutput("inputValue"),
    h4('Which resulted in a prediction of '),
    verbatimTextOutput("prediction")
  )
))



library(shiny)
library(UsingR)
library(ggplot2)
data(galton)

shinyServer(
  function(input, output) {
    output$newHist <- renderPlot({
      hist(galton$child, xlab= 'Height of Children', col='lightblue',main='Histogram')
      mu <- input$mu
      lines(c(mu, mu), c(0, 200),col="red",lwd=5)
      mse <- mean((galton$child - mu)^2)
      text(63, 150, paste("mu = ", mu))
      text(63, 140, paste("MSE = ", round(mse, 2)))
    })
    
  }
)


library(shiny)
library(tidyverse)
shinyUI(pageWithSidebar(
  headerPanel("galton plot!"),
  
  sidebarPanel(
    sliderInput('mu', 'Guess at the mean', value=70, min=62, 
                max=74, step=0.05,)
  ),
  mainPanel(
    plotOutput('newHist')
  )
)
)

























