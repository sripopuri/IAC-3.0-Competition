rm(list = ls())

# Set your Working directory

data = read.csv('Training Dataset.csv')

# Checking the correlation Matrix between the time variables
# From Sleeping to Volunteering
corrplot::corrplot(cor(data[,11:24]),
                   type = "lower",
                   diag = FALSE,
                   addCoef.col = "black")

# Observe in the picture that 
# there is good +ve correlation between
# 
# Caring for children - Parenting children - correlation = 0.69
# Socializing and Relaxing - Television - correlation = 0.71
# (Also observe no other variables has correlation more than 0.19)

# But when I explained you, I narrowed down to
# Housework - Food and Drink Prep also along with above pairs
# But surprisingly there is only 0.18 
# light correlation between these two variables

# So digging further deep.....

# The above 0.69 and 0.71 correlations between variables,
# are backed up by plotting these two pairs of variables

plot(data$Caring.for.Children,data$Playing.with.Children)
# Obviously there is some linear pattern, hence justified!

plot(data$Socializing...Relaxing,data$Television)
# Same here as well, correlation is justified!

# In both the above pictures, the slope between these pair
# of variables is clearly not 0, there is considerable slope,
# so we can trust the correlation of 0.69 and 0.71


# Lets see the last pair
plot(data$Housework,data$Food...Drink.Prep)
# Okay!,not a surprising plot for a 0.18 correlation

# May be the records with the 
# "Food...Drink.Prep > Housework" 
# are masking the correlation of Housework and Food...Drink.Prep
# lets check!

# Plotting the same above plot for those records
# which have "Housework > Food...Drink.Prep"
plot(data[data$Housework >= data$Food...Drink.Prep,'Housework'],
     data[data$Housework >= data$Food...Drink.Prep,'Food...Drink.Prep'])
# A linear pattern!

# Lets see the correlation between these variables now
cor(data[data$Housework >= data$Food...Drink.Prep,'Housework'],
    data[data$Housework >= data$Food...Drink.Prep,'Food...Drink.Prep'])
# 0.5349!!

# So these records which are having "Food...Drink.Prep > Housework" 
# are masking the correlation of otherwise related records
nrow(data[data$Housework >= data$Food...Drink.Prep,]) # --> 36619
# So the other records are 23005, and these are masking the correlation 
# of the other records.

############################## Conclusion ##############################
# My question with concept of correlation is,
# Even when there is no relationship between the variables,
# that is, when the slope of the linear relationship
# between two variables is 0, there is a good correlation 
# shown by the function.
# But if I can see a linear pattern between these pairs
# of variables as above plots, I think we can trust the
# correlation parameter here.
