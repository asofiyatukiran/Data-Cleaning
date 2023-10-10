#Name: Nur Ainin Sofiya Binti Tukiran
#Student ID: 22008550
#Lab 3: Data Cleaning
#Activity 3


library(missForest)
missForest_imputed <- data.frame(original = titanic_numeric$Age,
                                 imputed_missForest = missForest(titanic_numeric)$ximp$Age)
missForest_imputed

# Impute the missing values in the Age column using MICE.
mice_imputed <- mice(titanic_numeric, m = 5)$imputations[[1]]

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