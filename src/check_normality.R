# Load necessary libraries
library(moments)

# Step 1: Generate some data
set.seed(123)  # For reproducibility
data <- rnorm(100, mean = 50, sd = 10)

# Step 2: Visualize data using histogram
hist(data, breaks = 20, main = "Histogram of Data", xlab = "Data Values")

# Step 3: Q-Q plot
qqnorm(data, main = "Q-Q Plot")
qqline(data, col = "red")

# Step 4: Perform Shapiro-Wilk test
shapiro_test <- shapiro.test(data)
print(shapiro_test)

# Step 5: Calculate skewness and kurtosis (requires 'moments' package)
skew <- skewness(data)
kurt <- kurtosis(data)
print(paste("Skewness: ", skew))
print(paste("Kurtosis: ", kurt))

# Interpretation:
# - p-value > 0.05 from Shapiro-Wilk indicates that the data follows normal distribution.
# - Skewness near 0 and Kurtosis near 3 also suggest normality.

####################################

# Load the dataset (adjust the path if necessary)
df <- read.csv("./data/house_price_data.csv")

# View the first few rows to ensure the data loaded correctly
head(df)

# Select the two columns we are interested in: "SalePrice" and "GrLivArea"
sale_price <- df$SalePrice
gr_liv_area <- df$GrLivArea

####################################

# Checking normality for SalePrice column

# 1. Histogram
hist(sale_price, breaks = 30, main = "Histogram of SalePrice", xlab = "SalePrice", col = "lightblue")

# 2. Q-Q Plot (Quantile-Quantile plot)
# It plots the quantiles of your data against the theoretical quantiles of a specified distribution (normal distribution in this case)
qqnorm(sale_price, main = "Q-Q Plot of SalePrice")
qqline(sale_price, col = "red")

# 3. Shapiro-Wilk Test

# The result of the Shapiro-Wilk normality test for the sale_price data shows a W value of 0.86967 and a p-value < 2.2e-16.
# This extremely small p-value (much less than 0.05) suggests that we reject the null hypothesis that the data is normally distributed.
# In other words, the sale_price data does not follow a normal distribution.
# The W value being significantly lower than 1 further indicates a departure from normality,
# meaning the distribution of sale_price is likely skewed or has heavy tails.

shapiro_test_saleprice <- shapiro.test(sale_price)
print(shapiro_test_saleprice)

# 4. Skewness and Kurtosis for SalePrice

# The skewness of SalePrice is 1.88, which indicates that the distribution is positively skewed (right-skewed),
# meaning there are more lower values with a tail extending to higher values.
# The kurtosis of SalePrice is 9.51, which is significantly higher than 3 (the kurtosis of a normal distribution).
# This suggests the distribution has heavy tails or is leptokurtic, indicating the presence of outliers or extreme values in the data.
# In summary, the SalePrice data is both right-skewed and has heavy tails.

skew_saleprice <- skewness(sale_price)
kurt_saleprice <- kurtosis(sale_price)
print(paste("Skewness of SalePrice: ", skew_saleprice))
print(paste("Kurtosis of SalePrice: ", kurt_saleprice))

### Plotting right-skewed data ###
# Simulate right-skewed data (log-normal distribution)
set.seed(123)  # For reproducibility
right_skewed_data <- rlnorm(1000, meanlog = 0, sdlog = 1)  # Log-normal distribution is right-skewed

# Plot the histogram
hist(right_skewed_data, breaks = 30, main = "Right-Skewed Distribution", xlab = "Value", col = "lightblue")

# Q-Q Plot for the data
qqnorm(right_skewed_data, main = "Q-Q Plot of Right-Skewed Data")
qqline(right_skewed_data, col = "red")

### Plotting left-skewed data ###
left_skewed_data <- -rlnorm(1000, meanlog = 0, sdlog = 1)  # Negative log-normal data

# Plot the histogram
hist(left_skewed_data, breaks = 30, main = "Left-Skewed Distribution", xlab = "Value", col = "lightcoral")

# Q-Q Plot for the data
qqnorm(left_skewed_data, main = "Q-Q Plot of Left-Skewed Data")
qqline(left_skewed_data, col = "blue")

####################################

# Checking normality for GrLivArea column

# 1. Histogram
hist(gr_liv_area, breaks = 30, main = "Histogram of GrLivArea", xlab = "GrLivArea", col = "lightgreen")

# 2. Q-Q Plot
qqnorm(gr_liv_area, main = "Q-Q Plot of GrLivArea")
qqline(gr_liv_area, col = "red")

# 3. Shapiro-Wilk Test
shapiro_test_grlivarea <- shapiro.test(gr_liv_area)
print(shapiro_test_grlivarea)

# 4. Skewness and Kurtosis for GrLivArea
skew_grlivarea <- skewness(gr_liv_area)
kurt_grlivarea <- kurtosis(gr_liv_area)
print(paste("Skewness of GrLivArea: ", skew_grlivarea))
print(paste("Kurtosis of GrLivArea: ", kurt_grlivarea))
