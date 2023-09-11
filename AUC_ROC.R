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
 