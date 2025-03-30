data <- read.csv("/home/nae/Téléchargements/projR/final_table.csv",sep=",")

labels <- data[, grepl("count", names(data))]
var <- data[, !grepl("count", names(data))]

set.seed(42)
library(caret)

train_index <- createDataPartition(labels[,1], p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

library(randomForest)

# Since we have 16 different labels each test will be done 16 times

# General body : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$general_body_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])
  
# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$general_body_count))

# acc : 0.6949

########################################################################

# endocrine : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$endocrine_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$endocrine_system_count))

# acc : 0.8136

########################################################################

# immune : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$immune_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$immune_system_count))

# acc : 0.5085

########################################################################

# digestive : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$digestive_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$digestive_system_count))

# acc : 0.6102 good on 1

########################################################################

# cardio : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$cardiovascular_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$cardiovascular_system_count))

# acc : 0.4576

########################################################################

# repro : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$reproductive_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$reproductive_system_count))

# acc : 0.5085

########################################################################

# nervous : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$nervous_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$nervous_system_count))

# acc : 0.5235


########################################################################

# integumentary : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$integumentary_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])

# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$integumentary_system_count))

# acc : 0.7288 on 0s


########################################################################

# respiratory : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$respiratory_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])

# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$respiratory_system_count))

# acc : 0.5932


########################################################################

# urinary : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$urinary_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$urinary_system_count))

# acc : 0.8475 On Os

########################################################################

# skeletal : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$skeletal_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$skeletal_system_count))

# acc : 0.6441

########################################################################

# sensory : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$sensory_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$sensory_system_count))

# acc : 0.7797 On 0s

########################################################################

# muscular : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$muscular_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$muscular_system_count))

# acc : 0.8644 On 0s

########################################################################

# anti_org : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$anti_organisms_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$anti_organisms_count))

# acc : 0.6441 Ok on 1

########################################################################

# anti_subs : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$anti_substances_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$anti_substances_count))

# acc : 0.8276 on 0s

########################################################################

# lymphatic : 

# Creating the model on the label general_body_count
rf_model <- randomForest(x = train_data[, names(var)], 
                         y = as.factor(train_data$lymphatic_system_count), ntree = 100)

# Predicting results
preds <- predict(rf_model, newdata = test_data[, names(var)])


# Evaluation of the model
confusionMatrix(preds, as.factor(test_data$lymphatic_system_count))

# acc : 0.9828 on 0s

############################################
# Here it is important to note that even if some acc are good it is only in cases where the model detect that nothing append so no utility in fact...
# Little good result for the general body tho, even with an accuracy of 0.3448 we can see that he can detect when there is effects but he isn't able to count the number of effects, if we convert to binary we could obtain a confusionMat :
#   0 1
# 0 5 12
# 1 9 32
# So an accuracy : 0.6379
############################################

# A Pca to visualize a bit the data
pca = prcomp(var)

plot(pca$x, main="PCA Plot")

# Trying to do some clustering on the pca, With the idea of 2 cluster -> W/ without action
clust <- kmeans(pca$x, 2)

plot(pca$x, col=clust$cluster)


# Done a dendrogram too but won't say it is usefull. 
hcl <- hclust(dist(var), "ave")
plot(hcl)


############################################

# Trying some SVM
library(e1071)

# On general body :

train_data$general_body_count <- as.factor(train_data$general_body_count)
test_data$general_body_count <- as.factor(test_data$general_body_count)

tune_result <- tune(svm, general_body_count ~ ., data = train_data,
                    ranges = list(cost = 10^(-1:2), gamma = c(0.1, 0.5, 1, 2)))

best_model <- tune_result$best.model
print(best_model)

preds <- predict(best_model, newdata = test_data)

confusionMatrix(preds, as.factor(test_data$general_body_count))

# Quite ok results on this base.

# On digestive sys : 

train_data$digestive_system_count <- as.factor(train_data$digestive_system_count)
test_data$digestive_system_count <- as.factor(test_data$digestive_system_count)

tune_result <- tune(svm, digestive_system_count ~ ., data = train_data,
                    ranges = list(cost = 10^(-1:2), gamma = c(0.1, 0.5, 1, 2)))

best_model <- tune_result$best.model
print(best_model)

preds <- predict(best_model, newdata = test_data)

confusionMatrix(preds, as.factor(test_data$digestive_system_count))

# Seeing a real up wrt Trees but not amazing tho





