# ClassifyStudents.R
# Copyright ? 2017 by Ernst Henle.  All rights reserved

rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console

source("CollegeStudentsDataset.R") # Change CollegeStudentsDataset_template to CollegeStudentsDataset

# Set repeatable random seed
set.seed(4)

###################################################

# Partition data between training and testing sets

# Replace the following line with a function that partitions the data correctly
StudentsSplit <- PartitionExact(Students, fractionOfTest=0.4) 
TestStudents <- StudentsSplit$testingData
TrainStudents <-StudentsSplit$trainingData

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression
model.GLM <- glm(formula=formula, data=TrainStudents, family="binomial") 
# Predict the outcomes for the test data. (predict type="response")
predictions.GLM <- predict(model.GLM, newdata=TestStudents, type="response") 
###################################################

# Naive Bayes
# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf

# Get the algorithm
reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(e1071)

# Create Naive Bayes model
nb_model <- naiveBayes(formula, data=TrainStudents)
# Predict the outcomes for the test data. (predict type="raw")
nb_predictions <- predict(nb_model, newdata=TestStudents, type="raw")
###################################################

# Confusion Matrices

actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")
threshold <- 0.8

#Confusion Matrix for Logistic Regression
# convert the predicted probabilities to predictions using a threshold
predictedOutcome <- ifelse(predictions.GLM > threshold, "Attend", "NotAttend")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
print(table(predictedOutcome, actual))
#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions using a threshold
nb_predictedOutcome <- ifelse(nb_predictions[,2] > threshold, "Attend", "NotAttend")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")
# create a table to compare predicted values to actual values
print(table(nb_predictedOutcome, actual))
###################################################
# Wrong Partition; fractionOfTest=0.4; threshold = 0.5
# 
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#              Actual
# Predicted    Attend  NotAttend
# Attend        934        116
# NotAttend     759       1071
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (934 + 1071)/(934 + 759 + 116 + 1071) = 70%
# 
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      1154       228
# NotAttend    539       959
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (1154 + 959)/(1154 + 539 + 228 + 959) = 73%
###################################################

# Fill in the rest:

###################################################
# Assignment Item 8a
# Fast Partition; fractionOfTest=0.4; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend       691     227   
# NotAttend    262    1715   
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (691+1715)/(691+227+262+1715) =  83%  
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       713    273   
# NotAttend    240   1669   
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (713+1669)/(713+240+273+1669) = 82% 

###################################################
# Assignment Item 8b
# Exact Partition; fractionOfTest=0.4; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend       687     227   
# NotAttend    260    1706   
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (687+1706)/(687+1706+260+227) =  83%  
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       715     271   
# NotAttend    232     1662  
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (715+1662)/(715+1662+232+271) = 83%  

###################################################
# Assignment Item 8c
# Exact Partition;  fractionOfTest=0.4; threshold = 0.8
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend       338      27      
# NotAttend    609     1906   
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (338+1906)/(338+609+27+1906) = 78% 
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend       498      80      
# NotAttend    449     1853   
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   (498+1853)/(498+449+80+1853) = 82%  
###################################################
