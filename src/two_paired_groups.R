df <- read.csv("./data/house_price_data.csv", header=TRUE)
df = df[!df$SalePrice %in% boxplot.stats(df$SalePrice)$out,] # remove outlier from SalePrice
df = df[!df$GrLivArea %in% boxplot.stats(df$GrLivArea)$out,] # remove outlier from GrLivArea


#################################

# paired t-test
preExam = c(4,3,8,2,3)
postExam = c(7,5,9,7,8)
t.test(x=postExam, y=preExam, alternative = "greater", 
       var.equal = FALSE, paired = TRUE)

# effect size
library(effectsize)
effect_size = cohens_d(x=postExam, y=preExam, var.equal = FALSE)
interpret_cohens_d(effect_size)

# Interpretation:
# If the confidence interval includes 0, you would fail to reject the null hypothesis, indicating no significant effect.
# If the confidence interval does not include 0, you can reject the null hypothesis and conclude that there is a significant effect.

# Interpretation rule (cohen1988):
#   - Small effect size: d < 0.2
#   - Medium effect size: 0.2 ≤ d < 0.5
#   - Large effect size: d ≥ 0.5

#################################

# Wilcoxon (signed-rank) Test
preExam = c(4,3,8,2,3)
postExam = c(7,5,9,7,8)
library(coin)
wilcox.test(postExam, preExam, paired=T, alternative = "greater")

# V = 15: This is the test statistic (Wilcoxon signed-rank statistic),
# which measures the rank-based difference between the paired values in your two groups (post-exam and pre-exam scores).
# A larger value generally indicates a larger difference between the paired values.

# p-value = 0.02895: Since the p-value is less than 0.05,
# we reject the null hypothesis and conclude that there is a statistically significant difference,
# with post-exam scores being greater than pre-exam scores.

# The warning about ties indicates that there are pairs of values with no difference,
# which led to a continuity correction being applied to adjust the p-value.
# This is a common issue in paired data and does not invalidate the result,
# but it suggests that the test is using an approximation due to the ties in the data.

#################################

# McNemar's test
data <- matrix(c(5,5,25,65), ncol=2, byrow=T)
mcnemar.test(data)

# McNemar's Chi-squared Test Results:
# - Test statistic (chi-squared) = 12.033
# - Degrees of freedom (df) = 1
# - p-value = 0.0005226

# Interpretation:
# - Reject H0 (null hypothesis), accept H1 (alternative hypothesis)
# - There is a statistically significant difference in the number of students who passed
#   before and after the lecture (p-value < 0.05).
# - The test indicates that the number of students passing after the lecture is significantly
#   different from those passing before (significant change).

# Implication:
# - The passing rate significantly decreased post-lecture, suggesting the lecture might not have been effective.

# Conclusion:
# - Students did not learn well in the lecture, as indicated by the significant decrease in the passing rate.

# Contingency Table:
#                   | Pre-lecture Passed | Pre-lecture Not Passed
# ---------------------------------------------------------
# Post-lecture Passed |          5          |           5
# Post-lecture Not Passed |       25          |           65

#################################