#Name: Nur Ainin Sofiya Binti Tukiran
#Student ID: 22008550
#Lab 3: Data Cleaning

#Activity 1
#Replace the column’s missing value with zero.
df<-data.frame(Product = c ('A', 'B', 'C', 'D', 'E'), Price = c (612, 447, NA, 374, 831))
df$Price[is.na(df$Price)]<- 0
print(df)

# Replace the column’s missing value with the mean.
df<-data.frame(Product = c ('A', 'B', 'C', 'D', 'E'), Price = c (612, 447, NA, 374, 831))
df$Price[is.na(df$Price)]<- mean(df$Price,na.rm = TRUE)
print(df)

# Replace the column’s missing value with the median.
df<-data.frame(Product = c ('A', 'B', 'C', 'D', 'E'), Price = c (612, 447, NA, 374, 831))
df$Price[is.na(df$Price)]<- median(df$Price,na.rm = TRUE)
print(df)
#########################################################################################
library(titanic)
summary(titanic)
titanic_train$Age

#visualize the plot
ggplot(titanic_train, aes(Age)) +
  geom_histogram(color = "pink", fill = "black") +
  ggtitle("variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

#Perform imputation value
value_imputed <- data.frame(
  original = titanic_train$Age,
  imputed_zero = replace(titanic_train$Age,is.na(titanic_train$Age), 0),
  imputed_mean = replace(titanic_train$Age, is.na(titanic_train$Age),  mean(titanic_train$Age, na.rm = TRUE)),
  imputed_median = replace(titanic_train$Age, is.na(titanic_train$Age), median(titanic_train$Age, 
                  na.rm=TRUE)))

value_imputed


#Create histogram
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = 
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = 
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)




#Activity 2(imputation with mice)

titanic_numeric <- titanic_train %>%
  select(Survived, Pclass, SibSp, Parch, Age)
md.pattern(titanic_numeric)

mice_imputed <- data.frame(original = titanic_train$Age,
                           imputed_pmm = complete(mice(titanic_numeric, method = "pmm"))$Age,
                           imputed_cart = complete(mice(titanic_numeric, method = "cart"))$Age,
                           imputed_lasso = complete(mice(titanic_numeric, method = "lasso.norm"))$Age
                           )
mice_imputed 

#histogram
h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = 
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = 
                   "identity") +
  ggtitle(" Predictive mean matching-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = 
                   "identity") +
  ggtitle("Classification and regression trees-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = 
                   "identity") +
  ggtitle(" Lasso linear regression-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)



#Activity 3 – Imputation with R missForest Package

library(missForest)
missForest_imputed <- data.frame(original = titanic_numeric$Age,
                                 imputed_missForest = missForest(titanic_numeric)$ximp$Age)
missForest_imputed

# Impute the missing values in the Age column using MICE.
mice_imputed <- mice(titanic_numeric, m = 5)$imputations[[1]]

# Impute the missing values in the Age column using missForest.
missForest_imputed <- data.frame(original = titanic_numeric$Age,
                                 imputed_missForest = missForest(titanic_numeric)$ximp$Age)

# Impute the missing values in the Age column using MICE.
mice_imputed <- data.frame(original = titanic_train$Age,
                           imputed_pmm = complete(mice(titanic_numeric, method = "pmm"))$Age,
                           imputed_cart = complete(mice(titanic_numeric, method = "cart"))$Age,
                           imputed_lasso = complete(mice(titanic_numeric, method = "lasso.norm"))$Age
)

# Create a histogram of the original, missForest imputed, and MICE imputed values.
ggplot(titanic_numeric, aes(x = Age)) +
  geom_histogram(fill = "lightblue", alpha = 0.5) +
  geom_histogram(data = missForest_imputed, aes(x = imputed_missForest), fill = "orange", alpha = 0.5) +
  geom_histogram(data = mice_imputed, aes(x = imputed_pmm), fill = "green", alpha = 0.5) +
  geom_histogram(data = mice_imputed, aes(x = imputed_cart), fill = "blue", alpha = 0.5) +
  geom_histogram(data = mice_imputed, aes(x = imputed_lasso), fill = "purple", alpha = 0.5) +
  labs(title = "Distribution of Age (original, missForest imputed, and MICE imputed)") +
  theme_classic()



#activity4 find distance in km and salary in rm
#Question 1

titanic_train$Fare
# Create a data frame
titanic <- data.frame(Fare = c(7.25, 447, NA, 374, 831))
log_scale = log(as.data.frame(titanic$Fare))
log_scale <- log(titanic$Fare)
print(log_scale)


library(caret)
process <- preProcess(as.data.frame(titanic$Fare), method=c("range"))
norm_scale <- predict(process, as.data.frame(titanic$Fare))

# Load the titanic data frame
titanic <- data.frame(Fare = c(7.25, 447, NA, 374, 831))

# Normalize the Fare column using min-max scaling
process <- preProcess(as.data.frame(titanic$Fare), method=c("range"))
norm_scale <- predict(process, as.data.frame(titanic$Fare))

# Print the normalized values
print(norm_scale)