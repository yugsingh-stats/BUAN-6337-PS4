##### Clear memory
rm(list=ls())

##### Set working directory
setwd("~/Dropbox/Teaching/UTD/predictive_analysis/2023/problem sets/pset4")

### Read required packages
library("data.table")
library("ggplot2")
#library("GGally")
#library("plm")
#library("stargazer")
library("psych")

################################################################################
### Preliminary operations                                                   ###
################################################################################
### Read data
BBB = fread(file = "BBB.csv",
               na.strings = c("NA", ""), 
               sep = "auto",
               stringsAsFactors = FALSE,
               data.table = TRUE,
               encoding = "UTF-8"
)

# Examine data
BBB


### Generate dummies for gender and buyer
# Gender
BBB = BBB[, gender_ind := 0]
BBB = BBB[gender == "M", gender_ind := 1]
# Buyer
BBB = BBB[, buyer_ind := 0]
BBB = BBB[buyer == "yes", buyer_ind := 1]


# Part I, Q.1
logistic_model1 <- glm(buyer_ind ~ last + total_ + gender_ind
                       + child + youth + cook + do_it + reference + art + geog, 
                       data=BBB, 
                       family="binomial"(link="logit"))
summary(logistic_model1) # ask for the regression output
# Add predicted probabily 
BBB = BBB[, purch_prob := predict(logistic_model1, type = "response")]


# Part I, Q.2
# Converting to Odds Ratio
exp(coef(logistic_model1))
# Odds ratios and 95% Confidence Interval
exp(cbind(OR = coef(logistic_model1), 
          confint.default(logistic_model1))
    )

# Part II, Q.1
# Assign to deciles
# number of observations
BBB_n = length(BBB$acctnum)
BBB = BBB[, purch_prob_percentile := frank(purch_prob, ties.method = "min") / BBB_n]
# The following two lines uses a bit of "trick" to find the purchase probability decile
BBB = BBB[, purch_prob_decile := purch_prob_percentile * 10]
BBB = BBB[, purch_prob_decile := ceiling(purch_prob_decile)]


# Part II, Q.2
ggplot(data = BBB, 
       mapping = aes(x = purch_prob_decile, y = buyer_ind)) + 
  stat_summary(fun=mean, geom="bar")


# Part II, Q.3
BBB = BBB[, count := 1] # Create a column of ones
BBB_P2Q3 = subset(BBB,
                  select = c("purch_prob_decile", "count", "buyer_ind"))
BBB_P2Q3 = BBB_P2Q3[, lapply(.SD, sum), by = "purch_prob_decile"]
BBB_P2Q3 = BBB_P2Q3[, response_rate := buyer_ind / count]
BBB_P2Q3 = BBB_P2Q3[order(purch_prob_decile)]
colnames(BBB_P2Q3) = c("purch_prob_decile", "no_cust", "sum_buyers", "response_rate")


# Part II, Q.4
BBB_subset = subset(BBB,
                    select = c("purch_prob_decile",
                               "total_",
                               "gender_ind",
                               "child",
                               "youth",
                               "cook",
                               "do_it",
                               "reference",
                               "geog"))

describeBy(BBB_subset,
           group = c("purch_prob_decile"))

            
# Part II, Q.5 (graphing a couple of them)
# Here, we expect downward slope if OR > 1, and upward slope if 0 < OR < 1.
# Generally the case, but not always the case. 
# Q1) For child, instead of upward sloping, it's U-shape. Why?
# A) In logit regression, you control for everything. In this graph, you don't control for other variables and look at one variable in isolation. 
# For instance, customers who buy children book are also likely to buy art book, hence the peak in the beginning.
# Beware of omitted variable bias when you look at these one variable graphs.
# Q2) How do we know it was art that affected children book sale?
# A) There could be others, but when you do correlation matrix, art and children are positively correlated (0.24)

ggplot(data = BBB, mapping = aes(x = purch_prob_decile, y = total_)) + 
  stat_summary(fun=mean, geom="bar")
# OR = 1.001 in logit

ggplot(data = BBB, mapping = aes(x = purch_prob_decile, y = last)) + 
  stat_summary(fun=mean, geom="bar")
# OR = 0.91 in logit

ggplot(data = BBB, mapping = aes(x = purch_prob_decile, y = art)) + 
  stat_summary(fun=mean, geom="bar")
# OR = 3.18 in logit

ggplot(data = BBB, mapping = aes(x = purch_prob_decile, y = child)) + 
  stat_summary(fun=mean, geom="bar")
# OR = 0.83 in logit

# Part III, Q.1
# Break-even = Cost to mail/net revenue per sale = .5/(18-9-3)= 0.083


# Part III, Q.2
# Generate mailto_logit
BBB = BBB[ ,mailto_logit := ifelse(purch_prob > 0.083, 1, 0)]

# Part III, Q.3
BBB_mailto = BBB[mailto_logit == 1]
describe(BBB_mailto)

# Focus on mean of "buyer_ind"
# 15,160 out of 50,000 should receive mail. Among 15,160, the response rate will be 21.3%
# Overall response rate is 9%:
summary(BBB$buyer_ind)
# Therefore, the difference is 21.3 - 9 = 12.3%.


# Part III, Q.4
BBB_mailto_buyer = BBB_mailto[buyer_ind == 1]
length(BBB_mailto_buyer$acctnum)

# 3,327 in this dataset. 
# Therefore, the expected number of buyers in the roll-out sample is 33,270 since the roll-out sample is 10X the test sample.




