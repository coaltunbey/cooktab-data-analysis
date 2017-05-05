# import libraries
library(data.table)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)


# read data in and store it in a variable called 'data'
data <- data.table(
  read.csv(file = "CookTab.csv"
    , sep = ";"
    , encoding = "UTF-8")
)

# get types of columns
str(data)

# get a summary of the data
summary(data)

data[, TOTCKBK := CKBK068 + CKBK082 + CKBK177 + CKBK211]

data[, TOTCOST := TOTCKBK*10]
data[, TOTINCOME := ORDER*50]
data[, REVENUE := TOTINCOME - TOTCOST]
profitableCustomers <- data[REVENUE == 50][TPAID > 3]




# change the type of categorical columns to factor 
data[, AGE50PL := as.factor(AGE50PL)]
data[, CKBK068 := as.factor(CKBK068)]
data[, CKBK082 := as.factor(CKBK082)]
data[, CKBK177 := as.factor(CKBK177)]
data[, CKBK211 := as.factor(CKBK211)]
data[, GENDER := as.factor(GENDER)]
data[, ORDER := as.factor(ORDER)]



# again look to data for statistics like mean, quartiles and min-max
str(data)
summary(data)

# since summary function doesn't give standard deviation, find them manually 
sd(data[, TSLBO])
sd(data[, TPAID])

# plot numerical datas with a histogram
hist(data[, TSLBO])
hist(data[, TPAID])

# plot numerical datas with a boxplot with buying status as a dimension
boxplot(data[, TSLBO] ~ data[, ORDER])
boxplot(data[, TPAID] ~ data[, ORDER])

# ---------------------------------------------------------

# 75% of data is the sample size
smp_size <- floor(0.75 * nrow(data))

# set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# create test and train datasets
train <- data[train_ind, ]
test <- data[-train_ind, ]

# ------------------------------------------------------

# rpart decision tree class method
tree <- rpart(ORDER ~ AGE50PL + TSLBO + GENDER + CKBK068 + CKBK082 + CKBK177 + CKBK211
              + TPAID,
             data=train,
             control = rpart.control(cp = 0.001, minbucket=3))

# plot decision tree 
fancyRpartPlot(tree, cex = 0.6, main = "Decision Tree Model")

# tree statistics
printcp(tree)

# predict according to generated tree and store it in prediction column
test$prediction <- predict(tree, test, type = "class")

# if prediction and order output is same, then prediction is correct
test[, isCorrect := 'not correct']
test[test$ORDER == test$prediction, isCorrect:= 'correct']

# since it is a 0-1 output, isCorrect is a categorical variable, so save it as a factor
test$isCorrect <- as.factor(test$isCorrect)

# find correctly predicted values / total values and call it as accuracy
accuracy <- nrow(test[isCorrect == 'correct',]) / nrow(test)
accuracy

# see what percentage of orders is correctly estimated in a pro
prop.table(table(test$ORDER, test$isCorrect), 1)

# see how gender variable causes some incorrect estimations
aggregate(test$ORDER ~ test$isCorrect + test$GENDER, FUN = length)

# -------------------------------------------------

# random forest tree
rftree <- randomForest(ORDER ~ AGE50PL + TSLBO + GENDER + CKBK068 + CKBK082 + CKBK177 + CKBK211
              + TPAID,
              data=train,
              importance=TRUE, 
              ntree=50)

# variable importance graphs
varImpPlot(rftree)

# after saw that TPAID is an important variable, i decided to look at TPAID variable more deeply
aggregate(test$ORDER ~ test$TPAID, FUN = length)

# if TPAID is more than 7 times then include these rows too.
test[TPAID > 7, `:=`(prediction = as.factor(1))]

# recalculate accuracies

# if prediction and order output is same, then prediction is correct
test[, isCorrect := 'not correct']
test[test$ORDER == test$prediction, isCorrect:= 'correct']

# since it is a 0-1 output, isCorrect is a categorical variable, so save it as a factor
test$isCorrect <- as.factor(test$isCorrect)

# find correctly predicted values / total values and call it as accuracy
accuracy <- nrow(test[isCorrect == 'correct',]) / nrow(test)
accuracy

# see what percentage of orders is correctly estimated in a pro
prop.table(table(test$ORDER, test$isCorrect), 1)

                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           