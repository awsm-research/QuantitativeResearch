df <- read.csv("./data/house_price_data.csv", header=TRUE)
df = df[!df$SalePrice %in% boxplot.stats(df$SalePrice)$out,] # remove outlier from SalePrice
df = df[!df$GrLivArea %in% boxplot.stats(df$GrLivArea)$out,] # remove outlier from GrLivArea


#################################

# Unpaired T-Test
morning = c(8,4,2,9,5)
afternoon = c(7,5,3)
t.test(x=morning, y=afternoon, alternative = "two.sided", 
       var.equal = FALSE, paired = FALSE)

# Welch Two Sample t-test Results:
# - t-statistic = 0.3468
# - Degrees of freedom (df) = 5.6789
# - p-value = 0.7412
# - 95% Confidence Interval: [-3.692178, 4.892178]
# - Mean of morning group = 5.6
# - Mean of afternoon group = 5.0

# Interpretation:
# - The p-value (0.7412) is greater than 0.05, so we fail to reject the null hypothesis.
# - This suggests there is no statistically significant difference in the means of the morning and afternoon exam scores.
# - The confidence interval includes 0, further supporting the lack of a significant difference between the groups.

# effect size
effectsize::cohens_d(x=morning, y=afternoon, var.equal = FALSE) 


# Effect Size (Cohen's d) = 0.24 (Medium)
# - The effect size calculation is performed using Cohen's d.
# - Typically, it would be interpreted as:
#   - Small effect size: d < 0.2
#   - Medium effect size: 0.2 ≤ d < 0.5
#   - Large effect size: d ≥ 0.5
# - The actual value of Cohen's d would help determine the magnitude of the difference between groups.

# Relationship between t-test and Cohen's d:
# - The unpaired t-test (Welch’s t-test in this case) tests if there is a statistically significant difference
#   between the means of the two groups (e.g., morning and afternoon exam scores).
#   - The output includes the t-statistic, p-value, and confidence interval for the difference in means.
#   - A significant p-value (p < 0.05) indicates a statistically significant difference in the means.
#   - A non-significant p-value (e.g., p > 0.05) suggests no significant difference in means.

# - Cohen's d measures the magnitude of the difference between the means in terms of standard deviations.
#   - It provides an effect size: a measure of how large or small the difference is.
#   - Small effect: d < 0.2, Medium effect: 0.2 ≤ d < 0.5, Large effect: d ≥ 0.5.
#   - Unlike the t-test, Cohen's d shows the practical significance of the difference, regardless of statistical significance.

# How they work together:
# - The t-test tells you whether the difference between groups is statistically significant (p-value).
# - Cohen’s d tells you the magnitude of that difference in practical terms.

#################################

# Mann-Whitney U test (also called the Wilcoxon rank-sum test)
morning = c(8,4,2,3,5)
afternoon = c(9,9,9)
examTime = factor(c(rep("morning", length(morning)), rep("afternoon", length(afternoon))))
scores = c(morning, afternoon)
wilcox.test(scores ~ examTime, distribution="exact", alternative="greater")

# Wilcoxon Rank Sum Test Results:
# - Test statistic (W) = 15
# - p-value = 0.01624
# - Alternative hypothesis: true location shift is greater than 0 (morning > afternoon)

# Interpretation:
# - The p-value (0.01624) is less than 0.05, so we reject the null hypothesis.
# - This suggests there is a statistically significant difference between the morning and afternoon exam scores.
# - The morning group has significantly higher scores than the afternoon group,
#   as indicated by the alternative hypothesis (greater).

#################################

# Fisher's Test 
m <- matrix(c(10,100,30,30), ncol=2, byrow=T)
fisher.test(m, alternative = "less")

# Fisher's Exact Test for Count Data
# Data table: 
#                | Morning Exam | Afternoon Exam
# Passed         |      10      |       100
# Not Passed     |      30      |       30

# Results:
# Fisher's Exact Test for Count Data
# data:  m
# p-value = 4.452e-09
# alternative hypothesis: true odds ratio is less than 1
# 95 percent confidence interval: 
#   0.000000 0.214895
# sample estimates:
#   odds ratio = 0.101681

# Interpretation:
# - **p-value = 4.452e-09**: The p-value is extremely small (very close to 0), 
#   which suggests that we reject the null hypothesis. This means there is strong evidence 
#   that the odds of passing are lower in the morning exam compared to the afternoon exam.

# - **Odds Ratio = 0.101681**: The odds of passing in the morning are about 0.1 times (or 10%) 
#   ->the odds of passing in the afternoon, indicating a significant disadvantage to taking the exam in the morning.
#   The odds ratio (OR) is a statistic used to measure the strength of association or non-independence
#   between two binary data outcomes
#   An odds ratio of 1 means there is no association between the two groups.

# - **95% Confidence Interval**: The interval (0.000000, 0.214895) for the odds ratio does not include 1, 
#   which further supports the conclusion that the morning exam has significantly lower odds of passing.
#   The confidence interval suggests that the odds ratio could be anywhere between 0 and about 0.21.

# Conclusion:
# There is very strong evidence that the odds of passing are significantly lower
# in the morning exam compared to the afternoon exam.

#################################
