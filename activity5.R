#Name: Nur Ainin Sofiya Binti Tukiran
#Student ID: 22008550
#Lab 3: Data Cleaning
#Activity 5

gender_encode <- ifelse(titanic_train$Sex == "male",1,0)
table(gender_encode)

gender_encode <- ifelse(titanic_train$Survived == "survived",1,0)
table(gender_encode)

new_dat = 
  data.frame(titanic_train$Fare,titanic_train$Sex,titanic_train$Embarked)
summary(new_dat)

library(caret)
dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))
glimpse(dat_transformed)

summary(new_dat$titanic_train.Fare)
bins <- c(-Inf, 7.91, 31.00, Inf)
bin_names <- c("Low", "Mid50", "High")
new_dat$new_Fare <- cut(new_dat$titanic_train.Fare, breaks = 
                          bins, labels = bin_names)
summary(new_dat$titanic_train.Fare)
summary(new_dat$new_Fare)
