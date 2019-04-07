 //smv-project
//data analysis on chatbot data using R
#install the caret package
installed.packages('caret')
installed.packages('e1071')
library('caret')
tap_csv1 <- read_csv("~/tap.csv1.csv")
View(tap_csv1)
attach(tap_csv1)
str(tap_csv1)
head(tap_csv1)
set.seed(3003)
intrain <- createDataPartition(y = tap_csv1$v5,p = 0.7,list = FALSE)
training <- tap_csv1[intrain,]
testing <- tap_csv1[-intrain,]
dim(training);
dim(testing);
anyNA(tap_csv1)
summary(tap_csv1)
training[["v5"]]=factor(training[["v5"]])

trctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
set.seed(3233)

svm_Linear <- train(v5 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)


svm_Linear
test_pred <- predict(svm_Linear,newdata = testing)
test_pred

confusionMatrix(
  factor(test_pred, levels = 1:148),
  factor(testing$v5, levels = 1:148)
)




grid <- expand.grid(C = c(0,1,3,4,6,0.2,0.3))
set.seed(3233)
svm_Linear_Grid <- train(v5 ~., data = training, method = "svmLinear", 
                         trControl=trctrl, 
                         preProcess = c("center", "scale"), 
                         tuneGrid = grid,
                         tuneLength = 10) 
svm_Linear_Grid
plot(svm_Linear_Grid)

test_pred_grid <- predict(svm_Linear_Grid,newdata = testing)
test_pred_grid
confusionMatrix(table(test_pred_grid,testing$v5 ))
