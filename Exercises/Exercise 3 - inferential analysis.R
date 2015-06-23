########################################################################
#                                                                      #
# Statistics 101                                                       #
# Exercise 3: Inferential analyses                                     #
#                                                                      #  
########################################################################

# Load packages needed for analyses
library(ggplot2)
library(MASS)
library(dplyr)
library(car)
library(nlme) 

###################### Import Data #####################################


# We now use a new dataset, sampled from the same 
# experiment, to see whether we can confirm the 
# suspicions we have from the exploratory analyses

data_abinf <- read.csv("data_workshop_part2.csv")
data_abinf$users <- factor(data_abinf$users, levels=c(1,5,10,50))
data_abinf$system <- factor(data_abinf$system, levels = c("KittenInc", "DragonInc"))

summary(data_abinf)


###################### Bootstrapping ###################################


B <- 10000
resamples <- matrix(sample(data_abinf$process_A, length(data_abinf$process_A)*B, replace = TRUE), 
                    B, length(data_abinf$process_A))
medians <- apply(resamples, 1, median)

# You can use the quantile function to find the 95% CI of your bootstrapped 
# population distribution of the median. 
# 0.025 and 0.975 indicate the range of the interval.
# In this case 0.975 - 0.025 = 0.950 = 95% CI. You always want your confidence 
#interval to be centered between 0 and 1.
quantile(medians, c(0.025, 0.975))

# What does this 95% CI tell you about the probable population value of the 
# median duration of Process_A?

hist(medians, breaks = 20)

# What does the distribution of the median look like?


###################### T-test ##########################################

###################### Testing assumptions #############################


# Checking for outliers
ggplot(data_abinf, aes(x = factor(0), y = cycle)) + 
  geom_boxplot(outlier.size = 0) +
  stat_boxplot(geom ='errorbar') + 
  xlab("") +
  geom_point(position = position_jitter(width = .05, height = 0), size = 3, alpha = 0.7) +
  theme_classic()

# Are any of the dots (values) outside the whiskers of the boxplot? 

# Normality of outcome variable
# Visually
g1 <- data_abinf$cycle[which(data_abinf$system == "KittenInc")]
g2 <- data_abinf$cycle[which(data_abinf$system == "DragonInc")]

# KittenInc
qqnorm(g1)
qqline(g1)

# DragonInc
qqnorm(g2)
qqline(g2)

# With a formal Kolmogorov-Smirnov test
# Don't worry about the warning message!
# KittenInc
ks.test(g1, rnorm(length(g1), mean(g1), sd(g1)))

# DragonInc
ks.test(g2, rnorm(length(g2), mean(g2), sd(g2)))


###################### One sample t-test ###############################

# Let see whether the duration of cycle is significantly different from 0,
# in other words: whether cycle even takes up any time at all.

t.test(data_abinf$cycle, mu = 0)

#What would your conclusion be?

###################### Independent samples t-test ######################

# Testing final assumption: Homogeneity of variances
leveneTest(cycle~system, data=data_abinf)

# What is you conclusion? Are the variances the same for the two groups?

# The actual t-test
# Since levene's test was significant, we use var.equal = F to get 
# Welch's t-test, which is robust to heterogeneity of variances.

t.test(cycle ~ system, data=data_abinf, var.equal=F)

# Is the average cycle length significantly different between the two systems?

# To deal with the uneven variances, we can also estimate the t-value and 
# its distribution using bootstrapping. 

B <- 10000
t_values = numeric(B)  
n1 <- nrow(subset(data_abinf, system == "KittenInc"))
n2 <- nrow(subset(data_abinf, system == "DragonInc"))

for (i in 1:B) {
  group1 = sample(data_abinf$cycle[which(data_abinf$system=="KittenInc")], size=n1, replace=T)
  group2 = sample(data_abinf$cycle[which(data_abinf$system=="DragonInc")], size = n2, replace=T)
  t_values[i] = t.test(group1,group2, var.equal=F)$statistic
}

# Use the syntax below to view the mean of the bootstrapped distribution of 
# t-values, the 95% CI, and a histogram of the t-distribution.

mean(t_values)
quantile(t_values, c(0.025, 0.975))
hist(t_values, breaks = 20)

# Compare this mean t-value and confidence interval to the one we found 
# with a regular t-test. 
# Do they differ by much?
# We can also compare the regular t-value to the bootstrapped t-value 
# and compute the bias. The syntax below computes this for us. 
# In general, bias lower than +/-0.10 is acceptable.

bias <- as.numeric(t.test(cycle ~ system, data=data_abinf)$statistic - mean(t_values))
bias

# What would you conclude?

###################### ANOVA ###########################################

###################### Testing assumptions #############################

# Normality of outcome variable
# Visually

# 1 user
qqnorm(data_abinf$cycle[which(data_abinf$users=="1")])
qqline(data_abinf$cycle[which(data_abinf$users=="1")])

# 5 users
qqnorm(data_abinf$cycle[which(data_abinf$users=="5")])
qqline(data_abinf$cycle[which(data_abinf$users=="5")])

# 10 users
qqnorm(data_abinf$cycle[which(data_abinf$users=="10")])
qqline(data_abinf$cycle[which(data_abinf$users=="10")])

# 50 users
qqnorm(data_abinf$cycle[which(data_abinf$users=="50")])
qqline(data_abinf$cycle[which(data_abinf$users=="50")])


# With a formal Kolmogorov-Smirnov test
# Don't worry about the warnings!
# 1 user
ks.test(data_abinf$cycle[which(data_abinf$users=="1")], 
        rnorm(length(data_abinf$cycle[which(data_abinf$users=="1")]), mean(data_abinf$cycle[which(data_abinf$users=="1")]), sd(data_abinf$cycle[which(data_abinf$users=="1")])))
# 5 users
ks.test(data_abinf$cycle[which(data_abinf$users=="5")], 
        rnorm(length(data_abinf$cycle[which(data_abinf$users=="5")]), mean(data_abinf$cycle[which(data_abinf$users=="5")]), sd(data_abinf$cycle[which(data_abinf$users=="5")])))
# 10 users
ks.test(data_abinf$cycle[which(data_abinf$users=="10")], 
        rnorm(length(data_abinf$cycle[which(data_abinf$users=="10")]), mean(data_abinf$cycle[which(data_abinf$users=="10")]), sd(data_abinf$cycle[which(data_abinf$users=="10")])))
# 50 users
ks.test(data_abinf$cycle[which(data_abinf$users=="50")], 
        rnorm(length(data_abinf$cycle[which(data_abinf$users=="50")]), mean(data_abinf$cycle[which(data_abinf$users=="50")]), sd(data_abinf$cycle[which(data_abinf$users=="50")])))

# Homogeneity of variances
leveneTest(cycle~users, data=data_abinf)

# ANOVAs are quite robust to violation of the homogeneity of 
# variances, as long as group sizes are roughly equal. 
# Which is the case here.

###################### One Way ANOVA ###################################


# Use the syntax below to run a one way anova where number of users on
# the system predicts cycle length.

drop1(aov(cycle ~ users, data=data_abinf),~.,test="F")

# What do you conclude?
# Since there are more than two levels in the user-variable,
# we need post-hoc tests to explore which user-counts differ
# significantly from one another on cycle length.

# Post hoc comparison tests
TukeyHSD(aov(cycle ~ users, data=data_abinf))

# What do you conclude?

# Since the variances weren't homogeneous, we can also use a general least
# squares model, where we can correct for uneven variances. This was
# not part of the presentation, but I wanted to show it anyways
anova(gls(cycle ~ users, data=data_abinf,
  weights=varIdent(form= ~ 1 | users)))

# Does your conclusion change?

# We can't do TukeyHSD with this type of model, but we can compare
# the first level of the categorical variable to the other levels.
# You could rearrange the levels of users to see comparisons between
# all levels. For now, let's just look at the comparison of 1 user to
# all other numbers of users. Look for the values beneath Coefficients.
summary(gls(cycle ~ users, data=data_abinf,
            weights=varIdent(form= ~ 1 | users)))



###################### Factorial ANOVA #################################

# As an example, I want to show you what a factorial ANOVA can do.

# Test whether the average cycle length is different for different 
# numbers of users, different systems, and the interaction of users 
# and systems

# First we test for homogeneity of variances with the new variable 
# system added to the mix.
leveneTest(cycle~system*users, data=data_abinf)

# What does this test tell us now?

# Factorial ANOVA
drop1(aov(cycle ~ system*users, data=data_abinf),~.,test="F")

# Post-Hoc testing per significant main effect and for the interaction effect
TukeyHSD(aov(cycle ~ system*users, data=data_abinf), "users")
TukeyHSD(aov(cycle ~ system*users, data=data_abinf), "system:users")

# What do these post-hoc tests tell us?
# It is often easier to plot an interaction effect to understand
# what it means.

anova_sum <- data_abinf %>% group_by(system, users) %>% summarise(cycle = mean(cycle))
ggplot(anova_sum, aes(x=users, y = cycle, group = system, colour = system)) +
  geom_line(size = 1) + geom_point(size = 3, shape = 1) +
  ylab("Average Cycle Duration") +
  xlab("Number of users on the system at once") +
  theme_classic()

# What is your conclusion about the mean differences based on system and 
# users for cycle duration?

###################### Regression ######################################

# For regression analyses, it is often easier to test for assumptions
# after the model has been fit, since most of the assumptions are
# about the residual variance. I will show you what tests you can use
# for all the assumptions a little later.


###################### Regression with 1 dummy variable ################

# Lets first fit a regression predicting cycle duration
# from the system, using the command below
fit <- lm(cycle ~ system, data = data_abinf)
summary(fit)

# Does the type of system affect the cycle duration?
# What is the mean difference in cycle duration 
# between KittenInc and DragonInc?

###################### Regression with 2 categorical variables #########


# Let's include the categorical variable users to the model. R automatically
# creates dummy variables for categorical variables (as long as they are a
# factor variable) and uses the first category as the reference category (in 
# this case the 1 user condition).

fit2 <- update(fit, cycle ~ system + users)
summary(fit2)

# What can you say about the added effect of users?

# We can compare model fit using an ANOVA. A significant
# F-value indicates that the newer model explains more variance
# than the previous model
anova(fit, fit2)

# What is your conclusion?

###################### Regression with a continuous variable ###########

# Lets now include the duration of process_B to the model.
# Remember from our exploratory analyses that it seemed that
# process_B and cycle were strongly related?

fit3 <- update(fit, cycle ~ system + users + process_B)
summary(fit3)

# We use another ANOVA to test whether this third model fits
# better than the previous one.

anova(fit, fit2, fit3)

# What do you conclude?

###################### Regression with an interaction effect ###########

# We can now include some interaction effects. Lets first
# look whether the interaction of system and process_B
# is significant and whether it significantly improves
# model fit (using the ANOVA)

fit4 <- update(fit, cycle ~ system + users + process_B + system*process_B)
summary(fit4)

anova(fit, fit2, fit3, fit4)

# What is you conclusion? Should we include this interaction 
# term?

# Another possible interaction, is the interaction term between
# users and process_B. Lets see whether this adds significantly
# to the existing model.

fit4 <- update(fit, cycle ~ system + users + process_B + users*process_B)
summary(fit4)

anova(fit, fit2, fit3, fit4)

# What is your conclusion?

# Finally, let's try and see whether the three way interaction of
# system, users and process_B adds significantly to the model

fit5 <- update(fit, cycle ~ system + users + process_B + 
                 users*process_B + users*system*process_B)
summary(fit5)

anova(fit, fit2, fit3, fit4, fit5)

# What is your conclusion?

# Again, it is easier to interpret interaction effects with a plot

ggplot(data_abinf, aes(x = process_B, y = cycle, group = users, colour = users)) +
  geom_point() + geom_smooth(aes(group=users), method="lm") +
  facet_grid(users ~ system)


###################### Testing assumptions #############################

# To see whether the final model adheres to the
# assumptions of a linear regression, we use the 
# following diagnostics

# Looking at outliers, Bonferonni significant == there are some outliers
outlierTest(fit5)

#Influential observations with Cook's distance
cutoff <- 4/((nrow(data_abinf)-length(fit5$coefficients)-2)) 
plot(fit5, which=4, cook.levels=cutoff)

# Are there influential observations (these are the observations that don't
# fit the estimated model very well)? Only Cook's distances > 1 indicate
# worrisome observations.

# Leverage plot
# Look at the effect of a single variable on the residual variance.
# The bulk of the residual variances should center around 0.
# The slope of the red line is the same as the slope estimate found in the
# regression model
leveragePlots(fit5)

# QQ plot - test the normal distribution of residuals
# Are all the dots within the red dashed lines?
qqPlot(fit5, main="QQ Plot")

# Normal distribution of residuals with mean of 0
# Plot
sresid <- studres(fit5) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# The mean
mean(sresid)

# Constant error variance, homoscedasticity of variances
# NCV test  p > .05, yes homoscedasticity (what we want).
# Spread and level plot should show a approximately equal
# distribution of points above and below the red line.
ncvTest(fit5)
spreadLevelPlot(fit5)

# Multicollinearity
# If the sqrt of vif is greater than 2, then there exists multicollinearity
# between that predictor and some other predictor in the model
# Look at the third column.
# We look at the vif for the model without interaction terms,
# because interaction terms will always be correlated with 
# the variables that make up the interaction
vif(fit3)
sqrt(vif(fit3)) > 2

# sqrt(VIF) values higher than 2 can indicate problematic 
# correlations

# Linearity of relationship between predictors and outcome

# The green line should follow the red line.
# If the relationship is not linear, you need to think about
# tranforming the variable, leaving it out of your analysis,
# using non-linear modeling.

# This function cannot deal with interaction terms, so we use
# the last model without interaction terms

crPlots(fit3)

# Do the relations between the outcome variable and the predictors
# look linear?

# Indepdendence of errors between observations. 
# p > .5 = independent errors (what we want)
durbinWatsonTest(fit5)

# In conclusion, do you think model 5 is a good fit to the data? Are
# non of the assumptions strongly violated?

