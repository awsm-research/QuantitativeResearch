library(ggplot2)
library(dplyr)

df <- read.csv("train.csv", header=TRUE)

# remove outliers
df <- df[!df$SalePrice %in% boxplot.stats(df$SalePrice)$out,] 
df <- df[!df$GrLivArea %in% boxplot.stats(df$GrLivArea)$out,] 
df$has_fireplace <- df$FireplaceQu > 0 # define whether a house has fireplace

# boxplot w/o outliers
ggplot(df) +
  aes(x = "", y = SalePrice) +
  geom_boxplot(fill = "green") +
  theme_minimal()

# define smaller house as the first half, 
# and larger house is second half based on the living area
df <- df[order(df$GrLivArea),] 
smaller_houses <- df %>% slice(0:as.integer(nrow(df)/2)) 
larger_houses <- df %>% slice(as.integer(nrow(df)/2):nrow(df))


# Checking normality for SalePrice column
sale_price <- df$SalePrice

# 1. Histogram
hist(sale_price, breaks = 30, main = "Histogram of SalePrice", xlab = "SalePrice", col = "lightblue")

# 2. Q-Q Plot (Quantile-Quantile plot)
# It plots the quantiles of your data against the theoretical quantiles of a specified distribution (normal distribution in this case)
qqnorm(sale_price, main = "Q-Q Plot of SalePrice")
qqline(sale_price, col = "red")

# 3. Shapiro-Wilk Test
shapiro_test_saleprice <- shapiro.test(sale_price)
print(shapiro_test_saleprice)

# 4. Skewness and Kurtosis for SalePrice
skew_saleprice <- skewness(sale_price)
kurt_saleprice <- kurtosis(sale_price)
print(paste("Skewness of SalePrice: ", skew_saleprice))
print(paste("Kurtosis of SalePrice: ", kurt_saleprice))
