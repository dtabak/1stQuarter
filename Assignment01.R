# Assignment 1 10/7/2017
# Duygu S Tabak
###################################################################

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Q3
# Assign the url to variable "url"
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
# Download the dataset
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)

# Q4
# Manually construct a vector of column headers 
headers <- c("Age", "Gender", "TB", "DB", "Alkphos", "Sgbt", "Sgot", "TP", "ALB", "AG", "Selector")
names(ILPD) <- headers

# Q5
# view the first six rows of the data
head(ILPD)

# Q6
# Determine the standard deviation, mean, and median of each vector
ILPD1 <- ILPD 
ILPD1$Gender <- NULL
sapply(ILPD1, sd, na.rm=TRUE) 
sapply(ILPD1, mean, na.rm=TRUE)
sapply(ILPD1, median, na.rm=TRUE) 

# Q7
# Convert Gender column to binary
ILPD$Gender <- as.numeric(ILPD$Gender == "Male")

#Create Histograms for each column
i <- 1
for (column in ILPD)
  { 
  dev.new()
  hist(column, col="blue", main=headers[i])
  i <- i+1
}

# Q8
# Present a general overview of the data
plot(ILPD)

# Q9
# How can you tell if a vector contains continuous numbers or binary data?
#Binary data appear as straight perpendicular lines. 
#Continuous data appear as cloud-like shape.

# Which two vectors are most strongly correlated?
#TB and DB are two vectors that most strongly correlated. The points on the scatter plot form a line.

# Give an example of two vectors that have little correlation
#There are more examples of vectors that have little corelation. 
#For example, Age and TP are two vectors that have little correlation.

# Q10
# Remove Outlier
vect <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
highLimit <- mean(vect) + 2*sd(vect)
lowLimit <- mean(vect) - 2*sd(vect)
goodFlag <- (vect < highLimit) & (vect > lowLimit)
vect <- vect[goodFlag]
vect

# Q11
# Relabeling
vect <- c("BS","MS","PhD","HS","Bachelors","Masters","High School","MS","BS","MS") 
vect[vect == "Bachelors"] <- "BS"
vect[vect == "Masters"] <- "MS" 
vect[vect == "High School"] <- "HS"
vect

# Q12
# Min-Max Normalization 
vect <- c(1,-1,-1,1,1,17,-3,1,1,3)
a <- min(vect)
b <- max(vect) - min(vect) 
normalized <- (vect - a) / b
normalized

# Q13
# z-Score Normalization 
vect <- c(1,-1,-1,1,1,17,-3,1,1,3)
a <- mean(vect) 
b <- sd(vect) 
normalized <- (vect - a) / b
normalized

# Q14
# Binarization 
vect <- c("Red","Green","Blue","Green","Blue","Blue","Red","Blue","Green","Blue")
isRed <- vect == "Red" 
isGreen <- vect == "Green"
isBlue <- vect == "Blue" 

isRed
isGreen
isBlue

# Q15
# Discretization
vect <- c(81,3,3,4,4,5,5,5,5,5,5,5,6,6,6,6,6,7,7,7,7,8,8,9,12,24,24,25)
range <- max(vect) - min(vect) 
binWidth <- range / 3
bin1Min <- -Inf
bin1Max <- min(vect) + binWidth 
bin2Max <- min(vect) + 2*binWidth 
bin3Max <- Inf 
vectDiscretized <- rep (NA, length(vect)) 
vectDiscretized[bin1Min < vect & vect <= bin1Max] <- "Low"
vectDiscretized[bin1Max < vect & vect <= bin2Max] <- "Medium" 
vectDiscretized[bin2Max < vect & vect <= bin3Max] <- "High" 
vectDiscretized 

# Q16 
# Manual Discretization
ages <- c(81,3,3,4,4,5,5,5,5,5,5,5,6,6,6,6,6,7,7,7,7,8,8,9,12,24,24,25)
length(ages)

# [1] 3 3 4 4 5 5 5 5 5 5 5  6 6 6 6 6 7 7 7 7  8 8 9 12 24 24 25 81
#    <----------------------|------------------|--------------------->
