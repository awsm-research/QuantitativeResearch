# First, remove outliers
trainDf <- read.csv("../lecture2_src/data/house_price_data.csv", header=TRUE)
trainDf = trainDf[!trainDf$SalePrice %in% boxplot.stats(trainDf$SalePrice)$out,]
trainDf = trainDf[!trainDf$GrLivArea %in% boxplot.stats(trainDf$GrLivArea)$out,]
trainDf = trainDf[!trainDf$LotArea %in% boxplot.stats(trainDf$LotArea)$out,]

library(rms)
df <- read.csv("../lecture2_src/data/house_price_data.csv", header=TRUE) # 1460 rows
df = dplyr::select_if(df, is.numeric) # used only numerical column
df = df[, !(colnames(df) %in% c("Id"))] # remove ID
df[is.na(df)] <- 0
ind_vars = colnames(df[ , !(names(df) %in% 'SalePrice')]) # list the independent variables (remove y: SalePrice)

# RMS package requires a data distribution when building a model
dd <- datadist(df)
options(datadist='dd')

##################################################################
# perform correlation analysis
##################################################################

# Calculate spearman's correlation between independent variables
vc <- varclus(~ ., data=df[,ind_vars], trans="abs")
# Plot hierarchical clusters and the spearman's correlation threshold of 0.7
plot(vc)
threshold <- 0.7
abline(h=1-threshold, col = "red", lty = 2)

# Remove the highly correlated variable from the hierarchical clusters 
reject_vars <- c('GarageCars','X1stFlrSF','YearRemodAdd','GarageYrBlt',
                 'LotFrontage','TotRmsAbvGrd','X2ndFlrSF','BsmtFinSF1')

ind_vars <- ind_vars[!(ind_vars %in% reject_vars)]


# Re-calculate spearman's correlation between independent variables
vc <- varclus(~ ., data=df[,ind_vars], trans="abs")

# Re-plot hierarchical clusters and the spearman's correlation threshold of 0.7
plot(vc)
threshold <- 0.7
abline(h=1-threshold, col = "red", lty = 2)


#(Again) Remove the highly correlated variable from the hierarchical clusters 
reject_vars <- c('FullBath','OverallQual')
ind_vars <- ind_vars[!(ind_vars %in% reject_vars)]

##################################################################
# perform redundancy analysis
##################################################################
red <- redun(~., data=df[,ind_vars], nk=0) 
print(red)
reject_vars <- red$Out
ind_vars <- ind_vars[!(ind_vars %in% reject_vars)]

##################################################################
# Allocate degree of freedom for each independent variables
##################################################################
# This procedure dynamically builds a regression formula to model the relationship 
# between a dependent variable (SalePrice) and multiple independent variables (ind_vars).
# The formula adjusts the treatment of each independent variable based on its Spearman rank correlation 
# with the dependent variable (SalePrice).

# Strong correlations (ρ > 0.3) are modeled with restricted cubic splines (rcs) with 5 knots,
# allowing for a more flexible, non-linear relationship.
# Moderate correlations (0.15 < ρ ≤ 0.3) are modeled with rcs using 3 knots, 
# providing some flexibility but less complexity.
# Weak correlations (ρ ≤ 0.15) are included as linear terms, assuming a simple, linear relationship.

# Why use splines?
# Restricted cubic splines allow for flexible modeling of non-linear relationships 
# between the independent variables and the dependent variable, providing a better fit 
# when relationships are not purely linear.
# By adjusting the degree of flexibility (the number of knots) based on correlation strength, 
# the model captures complex patterns in highly correlated variables while keeping 
# the model simpler for weakly correlated variables.

# @param df A data frame containing the dependent variable `SalePrice` and independent variables `ind_vars`.
# @param ind_vars A character vector of independent variable names in the data frame `df`.

# @return A dynamically generated formula for use in regression modeling, with restricted cubic splines (rcs) applied 
# to independent variables based on their correlation strength with `SalePrice`.

# @details
# 1. The Spearman rank correlation between `SalePrice` and each independent variable is calculated.
# 2. The formula is built step by step, including restricted cubic splines for variables with a correlation strength 
#    above certain thresholds.
# 3. Variables with high correlation (`rho2 > 0.3`) are modeled with 5 degrees of freedom, using a restricted cubic spline 
#    with 5 knots. Variables with moderate correlation (`0.15 < rho2 <= 0.3`) are modeled with 3 degrees of freedom, 
#    using a restricted cubic spline with 3 knots. Variables with weak or no correlation (`rho2 <= 0.15`) are included linearly.

# 1. Calculate Spearman correlation between SalePrice and independent variables
sp <- spearman2(formula(paste("SalePrice", " ~ ", paste0(ind_vars, collapse=" + "))), data=df, p=2)

# 2. Plot the Spearman correlation results
plot(sp)

# 3. Initialize the formula with the dependent variable 'SalePrice'
formula <- paste0("SalePrice", " ~ ")

# 4. Loop through each independent variable to build the formula dynamically
for (i in 1:length(ind_vars)) {
  var <- rownames(sp)[i]    # Extract the name of the independent variable
  rho2 <- sp[i]             # Get the Spearman correlation coefficient for this variable
  
  # Apply restricted cubic splines based on the correlation strength
  if (rho2 > 0.3) {
    formula <- paste0(formula, "rcs(", var, ",", 5, ") + ")  # Strong correlation: 5 knots
  } else if (rho2 > 0.15) {
    formula <- paste0(formula, "rcs(", var, ",", 3, ") + ")  # Moderate correlation: 3 knots
  } else {
    formula <- paste0(formula, var, " + ")  # Weak correlation: linear term
  }
}

# 5. Remove the last " + " from the formula
formula <- substr(formula, 1, nchar(formula) - 3)

# 6. Print the dynamically generated formula
print(formula)

##################################################################
# Building the Non-Linear Regression model
##################################################################

# We do not assign degree of freedom to Fireplace since there are only 4 unique value, which are too few for 3 degree of freedoms, i.e., 3 knots
fit <- Glm(SalePrice ~ MSSubClass + rcs(LotArea, 3) + OverallCond + 
             rcs(YearBuilt, 5) + rcs(MasVnrArea, 3) + BsmtFinSF2 + BsmtUnfSF + 
             rcs(TotalBsmtSF, 5) + LowQualFinSF + rcs(GrLivArea, 5) + 
             BsmtFullBath + BsmtHalfBath + HalfBath + BedroomAbvGr + 
             KitchenAbvGr + Fireplaces + rcs(GarageArea, 5) + WoodDeckSF + 
             rcs(OpenPorchSF, 3) + EnclosedPorch + X3SsnPorch + ScreenPorch + 
             PoolArea + MiscVal + MoSold + YrSold, data = df)

##################################################################
# Measure the prediction performance
##################################################################

library(boot)
df$predicted = predict(fit, df)
library(Metrics)
rmse(df$SalePrice, df$predicted)
mae(df$SalePrice, df$predicted)
mse(df$SalePrice, df$predicted) 

##################################################################
# Examine the relationship between each dependent variables and the response variables (i.e., explanatory power)
##################################################################

explantory_power = data.frame(anova(fit,test='Chisq'))
explantory_power = explantory_power[order(explantory_power$Chi.Square, decreasing = TRUE),]
print(explantory_power)

predict <- Predict(fit,YearBuilt,fun=function(x)x)
plot(predict, ylab='Odds')

##################################################################
# Examine the partial effect of each variable
##################################################################

partialEffectDf = data.frame(summary(fit))
partialEffectDf = partialEffectDf[order(partialEffectDf$Effect, decreasing = TRUE),]

##################################################################
# Validating the performance (K-Fold)
##################################################################

library(rms)
df <- read.csv("../lecture2_src/data/house_price_data.csv", header=TRUE) # 1460 rows
df = dplyr::select_if(df, is.numeric) # used only numerical column
df = df[, !(colnames(df) %in% c("Id"))] # remove ID
df[is.na(df)] <- 0
ind_vars = colnames(df[ , !(names(df) %in% 'SalePrice')]) # list the independent variables (remove y: SalePrice)

# SINGLE SPLIT
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

# K-fold
library(caret)
# Define training control 
train.control <- trainControl(method = "cv", number = 10)
# Train the model 
model <- train(SalePrice ~., data = df, method = "glm", trControl = train.control) 
# Summarize the results 
print(model)

##################################################################
# Validating the performance (Bootstrap)
##################################################################

# Load necessary libraries
library(boot)

ratio <- function(d, w) sum(d$SalePrice * w) / sum(d$u * w)

# Define the bootstrap function
boot_function <- function(data, indices) {
  # Bootstrap sample: 'indices' is a vector of selected row indices for the bootstrap sample
  boot_sample <- data[indices, ]
  
  # Fit a GLM model to the bootstrap sample
  model <- glm(SalePrice ~ ., data = boot_sample)
  
  # Extract model coefficients or other statistics you are interested in
  # Here, we extract the coefficient for "OverallQual" as an example
  return(coef(model)["OverallQual"])  # Adjust depending on the coefficient you want to monitor
}

# Set number of bootstrap samples
n_bootstrap <- 1000

# Perform the bootstrap sampling
boot_results <- boot(data = train, statistic = boot_function, R = n_bootstrap)

# Summarize the bootstrap results
print(boot_results)

# Get the bootstrap estimate and confidence intervals
boot_estimate <- boot_results$t0  # The original estimate from the model (mean coefficient)
boot_ci <- boot.ci(boot_results, type = "perc")  # Percentile-based confidence intervals

# Print the results
print(boot_estimate)  # Original estimate
print(boot_ci)        # Confidence intervals