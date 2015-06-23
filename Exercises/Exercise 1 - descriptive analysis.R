########################################################################
#                                                                      #
# Statistics 101                                                       #
# Exercise 1: Descriptive analyses                                     #
#                                                                      #  
########################################################################

# Load packages needed for analyses
library(ggplot2)
library(dplyr)

###################### Import Data #####################################

data_ab <- read.csv("data_workshop_part1.csv")
data_ab$users <- factor(data_ab$users, levels=c(1,5,10,50))
data_ab$system <- factor(data_ab$system, levels = c("KittenInc", "DragonInc"))



###################### Descriptive statistics ##########################

# Let's describe some of the variables. Let's start with the 
# two systems we've compared to each other. The command below gives us 
# a basic summary (frequencies) of this varible.
summary(data_ab$system)

# What is the mode of the variable system? What kind of variable is system?

# Let's move on to the next variable, users. Again, the summary 
# function gives us the frequencies.
summary(data_ab$users)

# What is the mode of the variable users? What kind of variable is users?
# The command below returns the median of the variable users. 
# What does the median tell you?

median(as.numeric(as.character(data_ab$users)))

# The next variable is cycle; how long it took for a user to run 
# through the cycle you are testing. Let's look at the summary output. 
# This time, it looks a little different. What kind of variable
# do you think this is?

summary(data_ab$cycle)

# This time, it looks a little different. What kind of variable 
# do you think this is? What is the difference between the median 
# and the mean? Can you guess what the histogram of this variable 
# will look like based on the difference between the median and the mean?

# The hist() function creates a histogram of your variable.
hist(data_ab$cycle, breaks = 30, col = "grey")

# Did the histogram confirm your guess based on the median and mean? 
# What do you see in the histogram?
# What kind of distribution do you think fits this variable?

# The functions below find the maximum and minimum value of a variable.

max(data_ab$cycle)
min(data_ab$cycle)

# What is the range of the variable cycle? 
# (hint: you can use the max in min functions above)

# Let's look at some other values that tell us something about 
# the spread of the variable cycle. The two functions below 
# calculate the variance and the standard deviation of the variable.

var(data_ab$cycle)
sd(data_ab$cycle)

# The formmula below calculates the standard error of the mean of 
# the variable cycle and stores it in 'se'.
se <- var(data_ab$cycle)/sqrt(length(data_ab$cycle))

# To calculate the 95% CI of the mean, we use the formulas from the sheets
mean(data_ab$cycle) + 1.96 * se
mean(data_ab$cycle) - 1.96 * se

# What does the 95% CI of the mean tell you? Do you think the population 
# mean of this variable is likely to be very different from the sample 
# mean we found here?

# You can use the code below the create the histogram I showed in the presentation, 
# but then for the variable cycle. To run this syntax, select it all and then run.
# To view the plot in all its glory, please click Zoom.

ggplot(data_ab, aes(x = cycle)) + 
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(xintercept = mean(data_ab$cycle)) +
  geom_vline(xintercept = median(data_ab$cycle), color = "red") +
  geom_vline(xintercept = min(data_ab$cycle), linetype="dashed", color = "grey") +
  geom_vline(xintercept = max(data_ab$cycle), linetype="dashed", color = "grey") +
  geom_vline(xintercept = mean(data_ab$cycle)+sd(data_ab$cycle), linetype="dashed", color = "blue") +
  geom_vline(xintercept = mean(data_ab$cycle)-sd(data_ab$cycle), linetype="dashed", color = "blue") +
  geom_vline(xintercept = mean(data_ab$cycle)+se, linetype="dashed", color = "green") +
  geom_vline(xintercept = mean(data_ab$cycle)-se, linetype="dashed", color = "green") +
  geom_vline(xintercept = mean(data_ab$cycle) + 1.96 * se, linetype="dashed", color = "orange") +
  geom_vline(xintercept = mean(data_ab$cycle) - 1.96 * se, linetype="dashed", color = "orange") +
  xlab("") + ylab("") + theme(legend.position="none") +
  annotate("text", x = 1.2, y = 11, label = "Range", color="grey", size=8) +
  annotate("text", x = 3.5, y = 15, label = "95%\nCI", color="orange", size=8) +
  annotate("text", x = 2, y = 13, label = "-1 standard \n deviation", color="blue", size=8) +
  annotate("text", x = 6, y = 11, label = "-1 se", color="green", size=8) +
  annotate("text", x = 25, y = 11, label = "Range", color="grey", size=8) +
  annotate("text", x = 15.3, y = 15, label = "95%\nCI", color="orange", size=8) +
  annotate("text", x = 16.3, y = 11, label = "1 standard \n deviation", color="blue", size=8) +
  annotate("text", x = 12.5, y = 11, label = "1 se", color="green", size=8) +
  annotate("text", x = 10.3, y = 18, label = "Mean", color="black", size=8) +
  annotate("text", x = 6.5, y = 18, label = "Median", color="red", size=8) +
  theme_classic()

# Finally, let's look at the other variables in the data for a second.
# The function below gives us a summary of all the variables in the data frame
summary(data_ab)

# What type of variable are the remaining variables in the data frame? Can you say
# something about their distribution and spread from the summary statistics that you
# see here? If you want you can use the syntax above to find out more about all variables.







