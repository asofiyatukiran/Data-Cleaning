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


library(titanic)
summary(titanic)
titanic_train$Age


library(ggplot2)
library(dplyr)
library(cowplot)

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

