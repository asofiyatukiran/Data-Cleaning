#Name: Nur Ainin Sofiya Binti Tukiran
#Student ID: 22008550
#Lab 3: Data Cleaning
#Activity 2

library(mice)

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