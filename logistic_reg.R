library(caret)
library(kernlab)

library(ROCR)
library(pROC)

mtcars <- mtcars
model <- glm(vs ~ wt + disp, data=mtcars, family = "binomial")
summary(model)

set.seed(234)
diabetes <- read.csv("./data_science2/diabetes.csv")
str(diabetes)
diabetes$Outcome <- as.factor(diabetes$Outcome)
head(diabetes)

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


#Using optimized model
log_model2 <- glm(Outcome ~.-SkinThickness, data=diabetes, family = "binomial")
summary(log_model2)
predictions2 <- predict(log_model2, test)
table(test$Outcome, predictions2>0.5)
(93+26)/(93+7+27+26)= 0.77777

ROC_prediction2 <- predict(log_model2, train)
ROCRpred2 <- prediction(ROC_prediction2, train$Outcome)
ROCRperf2 <- performance(ROCRpred2, "tpr", "fpr")
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))

table(test$Outcome, predictions2>0.5)
(93+26)/(93+7+27+26)= 0.77777
table(test$Outcome, predictions2>0.1)
(91+28)/(91+28+25+9)






#Logistic regression2
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart <- read.csv(url, header = FALSE)
head(heart)
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

#
logistic2$fitted.values

#

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

roc(heart_data$hd, logistic2$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
    xlab="False Positive Percentage", ylab="False Positive Percentage", col="#377eb8",
    lwd=4, print.auc=TRUE, print.auc.x=45)




library(ggplot2)

# Example data
data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 6, 8, 10),
  label = c("A", "B", "C", "D", "E"),
  color = c("red", "blue", "green", "purple", "orange")
)

# Create a basic scatter plot
p <- ggplot(data, aes(x, y)) +
  geom_point()

# Add text labels with color
p <- p + geom_text(aes(label = label, color = color))

# Customize the color scale
p <- p + scale_color_identity()

# Display the plot
print(p)