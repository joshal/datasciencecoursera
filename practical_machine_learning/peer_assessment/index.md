# Practical Machine Learning: Classifying physical acivity data




## Introduction

As a part of the quantified self movement, a lot of physiological data is collected by enthusiasts with an aim to improve their health or detect interesting patterns in their daily activities. A lot of devices are now available which can track this data time to time and present them in a readable format. As we collect such data over long term, it is now possible to quantify answers for some interesting questions such as whether we have performed an activity correctly, or how much improvement have we seen from the time we started.

In this project, 6 participants were asked to perform barbell lifts correctly and incorrectly in 5 different ways. The data was obtained from accelerometers on the belt, forearm, arm, and dumbell. Using this data, our goal is predict the manner in which they did the exercise. A detailed information is available at the [website of the project](http://groupware.les.inf.puc-rio.br/har).

## Data

The training and testing datasets have been generously made available by the [Groupware research group](http://groupware.les.inf.puc-rio.br/har).
The training data, which can be downloaded from [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv), consists of 19622 rows corresponding to different observations, and the 160 columns which correspond to different features. Similarly the testing data, which can be obtained [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv), consists of 20 rows and 160 columns.

## Analysis



### Preprocessing

#### Cleaning raw data
Having downloaded the data and having done a primary look at it, we find a some obvious errors which might spoil our analysis. To transform them while reading the data in `R`, the following steps are taken:

1. Remove excel division error strings **#DIV/0!** and replace with `NA` values.
2. Convert empty strings **""** to `NA` values.


```r
read_data <- function(file) {
    as.data.frame(fread(file, na.strings = c("#DIV/0!", "")))
}
```





#### Feature selection
Now that we have a clean dataset, we do reduce the dimensions by getting relevant columns and transforming some data. We approach this as follows:

1. Remove any columns that have `NA` data.
2. Ignore columns with metadata.
3. Transform `classe`, the variable to be predicted, to a factor as we know it takes only 5 values.


```r
training_cols_na <- as.vector(apply(raw_training, 2, function(x) any(is.na(x))))

# Ignore columns with metadata
ignore_columns <- function(data) {
    subset(data, select=-c(V1,
                           user_name,
                           raw_timestamp_part_1,
                           raw_timestamp_part_2,
                           cvtd_timestamp,
                           new_window,
                           num_window))
}

# Transform `classe` to factor
transform_features <- function(data) {
    data$classe <- factor(data$classe)
    data
}
```


We combine these two paradigms to obtain a training dataset:




```r
# Preprocess training data
training <- preprocess_training(raw_training)
dim(training)
```

```
## [1] 19622    53
```


As you can see, we've reduced the number of columns from 160 to 53!

### Cross Validation

Having obtained the preprocessed training data, we now obtain cross validation by splitting the training data into a train set and a test set. The data is partitioned by the `classe` variable so that both datasets contain examples of each class. Moreover, the train set consists of 60% training data, and the test set has 40%. This is achieved as follows:



```r
# Separate training and testing data for cross validation
idx_train <- createDataPartition(training$classe, p = 0.6, list = FALSE)
train <- training[idx_train[, 1], ]
test <- training[-idx_train[, 1], ]
```


### Prediction

Now that we have all the ingrediants to run our prediction algorithm, we construct a model using `random forest` training method from the `caret` library. This is achieved as follows:


```r
# Construct model using random forest
model <- train(classe ~ .,
               data=train,
               tuneGrid=data.frame(mtry=3),
               trControl=trainControl(method="none"),
               method="parRF")
```


With the model in hand, we now calculate the misclassification rate and the confusion matrix as follows:





```r
missClass(test$classe, predict(model, test))
```

```
## [1] 0.007137
```

```r
conf_mat <- confusionMatrix(predict(model, newdata = test), test$classe)
conf_mat$overall["Accuracy"]
```

```
## Accuracy 
##   0.9929
```

```r
conf_mat$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 2229   10    0    0    0
##          B    3 1503    6    0    0
##          C    0    5 1361   21    3
##          D    0    0    1 1261    3
##          E    0    0    0    4 1436
```


As you can see, the out-of-sample prediction accuracy is very high.

## Conclusion

We've developed a method to predict how correctly a subject has performed the activity given accelerometer data. We applied data cleaning and feature selection on raw data to reduce it's dimensionality. Then we split the data in training and test sets for cross validation whereby we applied the `random forest` algorithm. Then we measured the accuracy of our model by comparing the results with the test data. We can see that our model is pretty accurate in determining the class of the exercise given the accelerometer data.
