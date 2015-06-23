########################################################################
#                                                                      #
# Statistics 101                                                       #
# Exercise 2: Exploratory analyses                                     #
#                                                                      #  
########################################################################

# Load packages needed for analyses
library(ggplot2)
library(dplyr)

###################### Categorical variables ###########################

# Creating bar charts of the variables users and system. 
# Are all user-counts represented in the data? How even
# is the representation of both systems?

ggplot(data_ab, aes(users)) + geom_bar() +
  theme_classic()

ggplot(data_ab, aes(system)) + geom_bar() +
  theme_classic()

###################### Continuous variables ############################

# Creating a boxplot of the variable Process A. What does this plot tell you?
# By replacing the 'process_A' in the syntax below, you can also 
# explore other continuous variables in the data.

ggplot(data_ab, aes(x = factor(0), y = process_A)) + 
  geom_boxplot(outlier.size = 0) +
  stat_boxplot(geom ='errorbar') + 
  xlab("") +
  geom_point(position = position_jitter(width = .05, height = 0), 
             size = 3, alpha = 0.7) +
  theme_classic()

###################### 2 Categorical variables #########################

# Here, we make a cross table of the variables users and system. 
# What does this table tell you?
tbl <- table(data_ab$users, data_ab$system)
tbl

# To test if there is dependence between these two variables (in this case, 
# we don't want that, because that would mean our experimental conditions 
# aren't independent), we  can compute the chi square.

chisq.test(tbl)

# What does the output tell you? Are users and system independent?

###################### 2 Continuous variables ##########################

# The syntax below creates a scatterplot of the variables cycle (y-axis) 
# and process_A (x-axis).
# What does the graph tell you? How strong do you think the relation 
# between cycle and process_A is?

ggplot(data_ab, aes(x = process_A, y = cycle)) + geom_point(size = 3) +
  stat_smooth(method = "lm")

# Use the syntax below to find out how strong the correlation between 
# process_A and cycle is.
correl <- cor.test(data_ab$process_A, data_ab$cycle)
correl

# What does the output tell you? Is there a positive or negative correlation? 
# Is the correlation between process_A and cycle significant?
# How large is the correlation (think effect sizes)?

# To compute the R-squared, we extract the correlation from our correl object,
# and square it.
correl$estimate^2

# How much of the variance in cycle can be accounted for by process_A?

###################### Continuous and categorical variables ############

# Below, we create a boxplot for cycle duraction per level of user-number

ggplot(data_ab, aes(x = users, y = cycle)) + 
  geom_boxplot(outlier.size = 0) +
  stat_boxplot(geom ='errorbar') + 
  xlab("Users") +
  geom_point(position = position_jitter(width = .05, height = 0), 
             size = 3, alpha = 0.7) +
  theme_classic()

# What do you see in this plot? Does cycle length appear to be 
# different for different numbers of users?
# You can replace users with system to look at the two ecommerce 
# platforms we are comparing, and you can replace cycle for any 
# of the other continuous variable to look at those.

# The plot below shows a scatterplot of cycle and process_B, 
# per level of users.
# What do you see in the plot? Is the association between cycle 
# and process_B different for different levels of users?

ggplot(data_ab, aes(x = cycle, y = process_B)) + geom_point(size = 3) +
  stat_smooth(method = "lm") +
  facet_grid(users ~ .)

# To get even more out of the plot, here, we've added system to the mix. 
# Now you see the association of cycle and process_B per level of users 
# and level of systems. What does this plot tell you?

ggplot(data_ab, aes(x = cycle, y = process_B)) + geom_point(size = 3) +
  stat_smooth(method = "lm") +
  facet_grid(users ~ system)

# You can replace process_C and cycle by any of the other continuous 
# variables in the dataset to explore some more.




