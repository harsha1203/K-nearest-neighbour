glass<- read.csv(file.choose())
View(glass)

# table of diagnosis
table(glass$Type)

str(glass)
# recode as a factor
glass$Type[glass$Type==1]<-'A'
glass$Type[glass$Type==2]<-'B'
glass$Type[glass$Type==3]<-'C'
glass$Type[glass$Type==5]<-'E'
glass$Type[glass$Type==6]<-'F'
glass$Type[glass$Type==7]<-'G'
str(glass)
glass$Type <- factor(glass$Type)
str(glass)
View(glass)

# table or proportions with more informative labels
round(prop.table(table(glass$type)) * 100, digits = 1)

# summarize any three numeric features
summary(glass[c("Na", "Si", "K")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# normalize the glass data
glass_n <- as.data.frame(lapply(glass[1:9], normalize))
View(glass_n)

# confirm that normalization worked
summary(glass_n$K)

# create training and test data
set.seed(1234)
ind<-sample(2,nrow(glass_n), replace=T, prob = c(0.7,0.3))
glass_train<-glass_n[ind==1,]
glass_test<-glass_n[ind==2,]
#glass_traindf<-as.data.frame(glass_train)
#glass_testdf<-as.data.frame(glass_test)
dim(glass_train)
dim(glass_test)
# create labels for training and test data

glass_train_labels <- glass[1:67, 10]
#glass_train_labelsdf<-as.data.frame(glass_train_labels)
View(glass_train_labelsdf)
length(glass_train_labels)


glass_test_labels <- glass[70:214, 10]
View(glass_test_labels)


#---- Training a model on the data ----

# load the "class" library
install.packages("class")
install.packages("caret")
library(class)
library(caret)

glass_test_pred <- knn(train = glass_traindf, test = glass_testdf,
                      cl = glass_train_labelsdf[,2, drop = TRUE], k=7)
glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=7)
glass_test_pred <- knn(train = glass_train[,2, drop = FALSE], test = glass_test[,2, drop = FALSE],
                       cl = glass_train_labels, k=21)

cl = train_labels[,1, drop = TRUE]
knn(train_points, test_points, cl, k = 5)
View(glass_test_pred)



##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)


#evaluate by diagonal matrix
61+37
98/100








## Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]


# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

