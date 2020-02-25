#Part A

#Step 1: Exploring and preparing data
wbcd <- read.csv("C:\\Users\\shree\\OneDrive\\Documents\\Quarter 6\\Predictive Analytics\\wisc_bc_data.csv" , stringsAsFactors = FALSE) 
str(wbcd)
View(wbcd)

wbcd <- wbcd[-1]

table(wbcd$diagnosis)

wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Step 2: Transformation and normalizing numeric data
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}

# Checking normalize function on couple of vectors
normalize(c(1, 2, 3, 4, 5))

normalize(c(10, 20, 30, 40, 50))

#Automate normalization process for 30 different variables in the data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

#Check if the data is normalized or not
summary(wbcd_n$area_mean)

#Creating training and test dataset
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#Creating Training and test data for target variable "diagnosis"
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]


#Step 3: Training a model on data with k=21

#install.packages("class")
library("class")
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)

#Step 4: Evaluating model performance

#install.packages("gmodels")
library("gmodels")
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

#Training a model on data with k=5
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

#Training a model on data with k=15
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

#Training a model on data with k=27
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)




#Step 5: Improving model performance (Alternative to normalization is "scale function")
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

#Training a model on using z-score standardization
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#z-score standardized model using k=21
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq = FALSE)
 



#Part B

#install.packages("tcltk")
library(tcltk)
#abalone_data <- read.csv("C:\\Users\\shree\\OneDrive\\Documents\\Quarter 6\\Predictive Analytics\\Abalone_Data.csv" , stringsAsFactors = FALSE) 

#Choose data file using file explorer
abalone_data <- read.csv(tk_choose.files(caption = "Choose X"), stringsAsFactors = FALSE) 
str(abalone_data)
View(abalone_data)

#names(abalone_data)[1] <- "Sex"

abalone_data$age[abalone_data$Rings == 1 | abalone_data$Rings == 2 | abalone_data$Rings == 3 | abalone_data$Rings == 4 | abalone_data$Rings == 5] ="Young"
abalone_data$age[abalone_data$Rings == 6 | abalone_data$Rings == 7 | abalone_data$Rings == 8 | abalone_data$Rings == 9 | abalone_data$Rings == 10 |abalone_data$Rings == 11 |abalone_data$Rings == 13|abalone_data$Rings == 12] ="Adult"
abalone_data$age[abalone_data$Rings == 14 | abalone_data$Rings == 15 | abalone_data$Rings == 16 | abalone_data$Rings == 17 | abalone_data$Rings == 18 |abalone_data$Rings == 19 |abalone_data$Rings == 20|abalone_data$Rings == 21|abalone_data$Rings == 22|abalone_data$Rings == 23|abalone_data$Rings == 24|abalone_data$Rings == 25|abalone_data$Rings == 26|abalone_data$Rings == 27|abalone_data$Rings == 28|abalone_data$Rings == 29|abalone_data$Rings == 30] ="Old"

abalone_data <- abalone_data[-1]

table(abalone_data$age)


round(prop.table(table(abalone_data$age)) * 100, digits = 1)
summary(abalone_data)


#Transformation and normalizing numeric data
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}

# Checking normalize function on couple of vectors
normalize(c(1, 2, 3, 4, 5))

normalize(c(10, 20, 30, 40, 50))

#Automate normalization process for different variables in the data
abalone_n <- as.data.frame(lapply(abalone_data[1:8], normalize))

#Check if the data is normalized or not
summary(abalone_n)
summary(abalone_n$Height)

#Creating training and test dataset
abalone_train <- abalone_n[1:3800, ]
abalone_test <- abalone_n[3801:4177, ]

#Creating Training and test data for target variable "age"
abalone_train_data_lables <- abalone_data[1:3800, 9]
abalone_test_data_lables <- abalone_data[3801:4177, 9]

#Training a model on data with k=65
abalone_test_pred <- knn(train = abalone_train, test = abalone_test,cl = abalone_train_data_lables, k = 65)

#Evaluating model performance
CrossTable(x = abalone_test_data_lables, y = abalone_test_pred,prop.chisq=FALSE)

#Training a model on data with k=3
abalone_test_pred <- knn(train = abalone_train, test = abalone_test,cl = abalone_train_data_lables, k = 3)

#Evaluating model performance
CrossTable(x = abalone_test_data_lables, y = abalone_test_pred,prop.chisq=FALSE)

#Training a model on data with k=15
abalone_test_pred <- knn(train = abalone_train, test = abalone_test,cl = abalone_train_data_lables, k = 15)

#Evaluating model performance
CrossTable(x = abalone_test_data_lables, y = abalone_test_pred,prop.chisq=FALSE)

#Training a model on data with k=27
abalone_test_pred <- knn(train = abalone_train, test = abalone_test,cl = abalone_train_data_lables, k = 27)

#Evaluating model performance
CrossTable(x = abalone_test_data_lables, y = abalone_test_pred,prop.chisq=FALSE)


#Improving model performance (Alternative to normalization is "scale function")
abalone_z <- as.data.frame(scale(abalone_data[-9]))
summary(abalone_z)

#Training a model on using z-score standardization
abalone_train <- abalone_z[1:3800, ]
abalone_test <- abalone_z[3801:4177, ]
abalone_train_labels <- abalone_data[1:3800, 9]
abalone_test_labels <- abalone_data[3801:4177, 9]

#z-score standardized model using k=65
abalone_test_pred <- knn(train = abalone_train, test = abalone_test,cl = abalone_train_labels, k = 65)
CrossTable(x = abalone_test_labels, y = abalone_test_pred,prop.chisq = FALSE)

