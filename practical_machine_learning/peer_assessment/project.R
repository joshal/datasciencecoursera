library(caret)
library(data.table)
library(doMC)

registerDoMC(cores = 7)
set.seed(12345)

# Download the data
download_data <- function() {
    # Training and test datasets
    training_data_url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    testing_data_url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    # Create a directory named data to save the files
    if (!file.exists("data")){
        dir.create("data")
    }
    # Download files if they do not exist
    if (!file.exists(training_data_path)){
        download.file(training_data_url, destfile=training_data_path, method="curl")
    }
    if (!file.exists(testing_data_path)){
        download.file(testing_data_url, destfile=testing_data_path, method="curl")
    }
}

# Read data using fread. Consider columns with "#DIV/0!" and "" as NA.
# Return a data frame
read_data <- function(file) {
    as.data.frame(fread(file, na.strings=c("#DIV/0!", "")))
}

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

## Pick only columns with values
preprocess_training <- function(data){
    ignore_columns(transform_features(data)[, !training_cols_na])
}

preprocess_testing <- function(data){
    # Drop the `problem_id` column
    ignore_columns(data[, !training_cols_na][-60])
}

# Get random forest data model
get_model <- function(raw_training) {
    # Preprocess training data
    training <- preprocess_training(raw_training)
    # Separate training and testing data for cross validation
    idx_train <- createDataPartition(training$classe, p=.60, list=FALSE)
    train <- training[idx_train[,1],]
    test <- training[-idx_train[,1],]
    # Construct model using random forest
    model <- train(classe ~ .,
                   data=train,
                   tuneGrid=data.frame(mtry=3),
                   trControl=trainControl(method="none"),
                   method="parRF")
    # Obtain the misclassification rate for test data
    print(missClass(test$classe, predict(model, test)))
    # Obtain confusion matrix
    print(confusionMatrix(predict(model, newdata=test), test$classe))
    model
}

# Calculate misclassification rate
missClass <- function(values,prediction){
    sum(prediction != values)/length(values)
}

# Submit the project
submit <- function(raw_testing, model, output_dir="output"){
    write_predictions <- function(x) {
        n = length(x)
        for(i in 1:n){
            if (!file.exists(output_dir)){
                dir.create(output_dir)
            }
            filename = paste0(output_dir, "/problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
    }
    testing <- preprocess_testing(raw_testing)
    write_predictions(predict(model, newdata=testing))
}

training_data_path <- "./data/pml-training.csv"
testing_data_path <- "./data/pml-testing.csv"
download_data()
raw_training <- read_data(training_data_path)
raw_testing <- read_data(testing_data_path)
training_cols_na <- as.vector(apply(raw_training, 2, function(x) any(is.na(x))))
model <- get_model(raw_training)
submit(raw_testing, model)