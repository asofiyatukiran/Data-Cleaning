#Name: Nur Ainin Sofiya Binti Tukiran
#Student ID: 22008550
#Lab 3: Data Cleaning
#Activity 4

library(titanic)

titanic_train$Fare
summary(titanic)
log_scale = log(as.data.frame(titanic$Fare))
print(log_scale)

library(caret)
process <- preProcess(as.data.frame(titanic$Fare), 
                      method=c("range"))
norm_scale <- predict(process, as.data.frame(titanic$Fare))


scale_data <- as.data.frame(scale(titanic$Fare))
