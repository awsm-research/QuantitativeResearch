df <- read.csv("./data/house_price_data.csv", header=TRUE)
df = df[!df$SalePrice %in% boxplot.stats(df$SalePrice)$out,] # remove outlier from SalePrice
df = df[!df$GrLivArea %in% boxplot.stats(df$GrLivArea)$out,] # remove outlier from GrLivArea


#################################

# Analysis of Variance Test (ANOVA)
score = c(5,6,7,8,9,5,6,7,8,5,6,7)
group = c("A","A","A","A","A","B","B","B","B","C","C","C")
df <- data.frame(score, group)

# test for homogeneity of variances
bartlett.test(score ~ group, df) 

# Bartlett's Test for Homogeneity of Variances:
# - Bartlett's K-squared = 0.45288
# - Degrees of freedom (df) = 2
# - p-value = 0.7974
# 
# Interpretation:
# - The p-value (0.7974) is greater than 0.05, so we fail to reject the null hypothesis of equal variances.
# - This suggests that the variances across the three groups (A, B, C) are not significantly different,
#   satisfying the ANOVA assumption of homogeneity of variances.
# - Bartlett's K-squared (0.45288): Low value with p-value (0.7974) > 0.05,
#   indicating variances across groups are similar, meeting ANOVA’s homogeneity requirement.

# ANOVA test
aov <- aov(score ~ group, df)
summary(aov)

# Analysis of Variance (ANOVA) Results:
# - Degrees of freedom (group) = 2
# - Degrees of freedom (residuals) = 9
# - F-statistic = 0.507
# - p-value = 0.618

# Interpretation:
# - The p-value (0.618) is greater than 0.05, so we fail to reject the null hypothesis.
# - This suggests that there is no statistically significant difference in mean scores between the three groups (A, B, C).
# - Therefore, we do not have enough evidence to conclude that the exam scores differ across groups.

# F-statistic: The F-statistic is the test statistic used in ANOVA to compare
# the variance between the group means relative to the variance within the groups.
# It’s calculated as the ratio of the mean square between groups to the mean square within groups.
# A higher F-statistic generally indicates that group means are more spread out,
# suggesting a potential difference between groups.
# The F-statistic of 0.507 is quite low, indicating that the variability between 
# group means is not large relative to the variability within groups.
# Combined with the high p-value (0.618), this suggests no statistically significant difference between group means.

# effect size
effSize = data.frame(unclass(summary(aov)), check.names = FALSE, stringsAsFactors = FALSE)
print(effSize$`Sum Sq`[1] / sum(effSize$`Sum Sq`))

# Interpretation:
# η² = 0.101 means that approximately 10.1% of the total variance
# in the dependent variable is explained by the factor we are studying.

# General guidelines for interpreting η²:
# - Small effect: η² ≈ 0.01 (1%)
# - Medium effect: η² ≈ 0.06 (6%)
# - Large effect: η² ≈ 0.14 (14%)

# In our case, η² = 0.101 indicates a medium to large effect size.
# This means the factor being studied has a noticeable impact on the outcome,
# explaining around 10% of the variance in the data.

# However, the remaining 90% of variance is due to other factors
# or random variation that isn't explained by the effect we're studying.

#################################

# Friedman Test 
data <- cbind(c(4,3,8,2,3), c(7,5,9,7,8), c(9,8,9,8,9))
friedman.test(data)

# Exam scores for 5 students across 3 different subjects
# 
#           Subject A   Subject B   Subject C
# Student 1      4           7           9
# Student 2      3           5           8
# Student 3      8           9           9
# Student 4      2           7           8
# Student 5      3           8           9
#
# Friedman Rank Sum Test Results:
# - Friedman chi-squared = 9.5789
# - Degrees of freedom (df) = 2
# - p-value = 0.008317
# 
# Interpretation:
# - The p-value (0.008317) is less than 0.05, so we reject the null hypothesis.
# - This suggests that there is a statistically significant difference in exam scores across the three subjects.
# - The Friedman test indicates that at least one subject's scores are significantly different from the others when accounting for the fact that scores are from the same students.

#################################

# Cochran's Q Test
passed <- c(1,0,1,0,0,1,0,1,0,0,1,1,0,0,1,1,1,1,0,0,1,0,1,1,0,1,1,0,1,1)
student <- factor(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10))
subject <- factor(rep(1:3, 10))
data <- data.frame(student, subject, passed)

library(coin)
symmetry_test(passed ~ factor(subject) | factor(student), data = data, teststat = "quad")

# Exam pass/fail data for 10 students across 3 subjects
# 
#           Subject A   Subject B   Subject C
# Student 1       1           0           1
# Student 2       0           0           1
# Student 3       0           1           0
# Student 4       0           1           1
# Student 5       0           0           1
# Student 6       1           1           1
# Student 7       0           0           1
# Student 8       0           1           1
# Student 9       0           1           1
# Student 10      0           1           1
#
# Cochran's Q Test Results:
# - Chi-squared = 8.2222 (A higher chi-squared value indicates greater differences)
# - Degrees of freedom (df) = 2
# - p-value = 0.01639
#
# Interpretation:
# - The p-value (0.01639) is less than 0.05, so we reject the null hypothesis.
# - This suggests that the odds of students passing the exams are not the same across the three subjects.
# - The Cochran’s Q test indicates a statistically significant difference in pass rates across the subjects.

#################################

# Exam scores for different times of day
scores <- c(4, 3, 8, 2, 3, 7, 5, 9, 7, 8, 9, 8, 9, 8)
time_of_day <- factor(c(rep("Morning", 5), rep("Afternoon", 5), rep("Evening", 4)))
# Kruskal-Wallis Test
kruskal.test(scores ~ time_of_day)

# Exam scores data:
#          Morning  Afternoon  Evening
# Student 1    4         7         9
# Student 2    3         5         8
# Student 3    8         9         9
# Student 4    2         7         8
# Student 5    3         8         NA

# Kruskal-Wallis Test Results:
# - Kruskal-Wallis chi-squared = 7.2759
# - Degrees of freedom (df) = 2
# - p-value = 0.02631

# Interpretation:
# - The p-value (0.02631) is less than 0.05, so we reject the null hypothesis (H0).
# - This indicates a statistically significant difference in exam scores
#   based on the time of day the exam was taken (Morning, Afternoon, or Evening).
# - The Kruskal-Wallis test suggests that exam score distributions vary by time of day

#################################

# Chi-square Test 
data <- matrix(c(16, 11, 3, 21, 8, 15), ncol=3, byrow=T)
chisq.test(data)

# Chi-square Test Data for Exam Pass Rates:
#               Morning Afternoon Evening
# Passed          16       11        3
# Not Passed      21        8       15

# Chi-square Test Results:
# - Pearson's Chi-squared = 6.742
# - Degrees of freedom (df) = 2
# - p-value = 0.03435
#
# Interpretation:
# - The p-value (0.03435) is less than 0.05, so we reject the null hypothesis (H0).
# - This result indicates a statistically significant difference in the odds of 
#   students passing the exam depending on the time of day (Morning, Afternoon, or Evening).
# - Specifically, the odds that students passed the exam are not equal across 
#   different exam times, suggesting time-of-day effects on passing rates.

# effect size
library(vcd)
assocstats(data)

# Effect Size Interpretation:
# - Likelihood Ratio Chi-square = 7.2218, p-value = 0.027
# - Cramer's V = 0.302
#
# Interpretation of Effect Size:
# - The Phi-Coefficient is not calculated because the table is not 2x2 (it’s 2x3).
#   Phi is typically used for 2x2 contingency tables and is not applicable in this case.
# - Cramer's V of 0.302 indicates a moderate association between the time of day 
#   and whether students passed the exam, with values between 0.3-0.5 often 
#   interpreted as moderate in social sciences.
# - The Contingency Coefficient of 0.289 also supports a moderate effect size, 
#   suggesting a meaningful but not strong relationship between exam timing and 
#   pass rates.

#################################

# Pearson Correlation
time <- c(10,14,12,20,15,13,18,11,10)
scores <- c(22,21,25,35,28,29,31,19,17)
cor.test(time,scores,method="pearson")

# Pearson's Correlation Test Interpretation:
# - t = 4.6855
#   - The t-statistic represents the test statistic for testing the correlation.
#   - A t-value of 4.6855 indicates a significant correlation between preparation time and scores.
# Small t-statistic: t ≤ 1.96 (commonly used threshold for significance at the 5% level)
# Medium t-statistic: 1.96 < t ≤ 3
# Large t-statistic: t > 3

# - df = 7
#   - Degrees of freedom used in the test (n-2, where n is the number of pairs of data points).

# - p-value = 0.002246
#   - The p-value is less than the common significance level of 0.05, so we reject the null hypothesis.
#   - This means there is sufficient evidence to conclude that there is a statistically significant correlation
#     between the exam preparation time and the test scores.

# - 95% Confidence Interval: [0.4900229, 0.9724978]
#   - The 95% confidence interval for the correlation coefficient suggests that the true correlation
#     is likely to fall within this range.
#   - The interval does not contain 0, reinforcing the conclusion that there is a statistically significant correlation.

# - Correlation Estimate: 0.8707668
#   - The correlation coefficient of 0.87 indicates a strong positive correlation between
#     the preparation time and the test scores.
#   - This suggests that as the preparation time increases, the test scores also tend to increase.

#################################

time <- c(10,14,12,20,15,13,18,11,10)
scores <- c(22,21,25,35,28,29,31,19,17)
cor.test(time,scores,method="spearman")

# Spearman's rank correlation rho
# rho = 0.8117
# - A strong positive monotonic relationship exists between exam preparation time and test scores.
# Spearman's rank correlation coefficient (rho) interpretation:
# - rho = 0 to 0.1: Very weak or no monotonic relationship.
# - rho = 0.1 to 0.3: Weak monotonic relationship.
# - rho = 0.3 to 0.5: Moderate monotonic relationship.
# - rho = 0.5 to 0.7: Strong monotonic relationship.
# - rho = 0.7 to 0.9: Very strong monotonic relationship.
# - rho = 1: Perfect positive monotonic relationship.
# Negative rho values indicate an inverse monotonic relationship:
# - rho = -0.8: Very strong negative monotonic relationship (as one variable increases, the other decreases).

# p-value = 0.007889
# - The result is statistically significant (p < 0.05), indicating that the correlation is unlikely to be due to random chance.

# S = 22.593
# - This is the test statistic for Spearman’s rank correlation, used to calculate the significance of the correlation.

#################################

# Cramer's V
data <- matrix(c(20, 10, 3, 27), ncol=2, byrow=T)
library(vcd)
assocstats(data)

# Case study data table
# Writing tool used by students in group A and group B
#               Pen    Pencil
# Group A        20        10
# Group B         3        27

# Cramer's V Range Interpretation:
# - 0.00 to 0.10: Negligible or very weak association
# - 0.10 to 0.30: Weak association
# - 0.30 to 0.50: Moderate association
# - 0.50 to 1.00: Strong association

# Since Cramer's V = 0.583, the association between the student groups and the writing tools used is
# considered to be strong. This means that the choice of writing tool is significantly influenced by the student group.

# Association Test Results:
# - Pearson's Chi-squared statistic = 20.376, p-value = 6.3622e-06
#   - The p-value is very small (p < 0.05), so we reject the null hypothesis (H0).
#   - There is a significant association between the student groups and the writing tools used.

# - Likelihood Ratio statistic = 22.185, p-value = 2.4762e-06
#   - The small p-value also supports the rejection of the null hypothesis, further suggesting
#     a strong association between the groups and their choice of writing tools.

# Cramer's V summary:
# - Phi-Coefficient = 0.583: A moderate effect size indicating the strength of association.
# - Contingency Coefficient = 0.503: This value is similar to Cramer's V, indicating a moderate relationship.
# - Cramer's V = 0.583: The association strength falls into the 'moderate' to 'strong' range for categorical variables.

#################################
