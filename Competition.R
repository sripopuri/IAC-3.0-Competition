# cleaning my workspace
rm(list = ls())

library(data.table)
library(ggplot2)
library(gridExtra)

# Setting the required working directory
setwd("D:/New folder/Fwd__IAS-_Intelligence_Analytics_Challenge_3.0_-_Let's_Begin")

# Getting the data
data = read.csv('Training Dataset.csv')

# checking the ID column, to verify
# whether it is panel data
length(unique(data$Id))
# So not a panel data

sum(is.na(data))
# No NAs in the data

str(data)
summary(data)
# Checking different values in these fields
unique(data$Education.Level)
unique(data$Age.Range)
unique(data$Employment.Status)
unique(data$Year)

# coercing ID and Year to factors
data$Id = as.factor(data$Id)
data$Year = as.factor(data$Year)

nrow(data[data$Year == '2012',])
# All years data nearly 8001 rows
table(data$Year)

colnames(data)

# seperating the time variables
timeframe = data[,11:24]

# a1 = ggplot(data[,c(9,11)], aes(x=Year,y=Sleeping)) + geom_point()
# a2 = ggplot(data[,c(9,12)], aes(x=Year,y=Grooming)) + geom_point()
# 
# a3 = ggplot(data[,sum(data[,13]),by = Year], aes(x=Year,y=V1)) + geom_point()
# a4 = ggplot(data[,sum(data[,14]),by = Year], aes(x=Year,y=V1)) + geom_point()
# 
# grid.arrange(a1,a2, nrow = 2) # par(mfrow = c(7,2))
# 

p = matrix(0,nrow = 8,ncol = 14)
rownames(p) = c(levels(data$Year))
colnames(p) = colnames(timeframe)

for(j in 11:24){
  for(i in levels(data$Year)){
  p[i,j-10] = sum(data[data$Year == i,j])
  }
}

p = as.data.frame(p)

for(k in 1:14){
  plot(rownames(p),p[,k],xlab = "Year", ylab = "Total Time",main = colnames(p)[k])
}

############################# Observations in the trends #############################

########## DECREASING                                  INCREASING

# Volunteering - Decreasing                   Television - Perfect raise
# Shopping - Perfect dropping                 Running - Increasing
# Caring for children - Slowly dropping       Jobsearching - Perfect raising
#                                             Sleeping - Perfect raising
#                                             Socializing & Relaxing - Perfect raise
#                                             Playing with children - Slowly raising
# 
# 
# 
# Eating and Drinking - U shaped @ 2008
# Golfing - Not much
# Food Drink Prep - Not much
# House work - Not much
# Grooming - Not much
# 


emp_year = table(data$Employment.Status,data$Year)
emp_year
emp_year = as.data.frame(emp_year)


# there is clearly a decline in employment between 2008 and 2009
plot(emp_year[emp_year$Var1 == 'Employed',2],
     emp_year[emp_year$Var1 == 'Employed',3],
     type = "p",
     xlab = "Year",
     ylab = "Count",
     main = "Employment trend")


ggplot(data = emp_year[emp_year$Var1 == 'Employed',], aes(x = Var2)) +  
  geom_line(aes(y = Freq, colour = "Freq")) + 
  geom_point(aes(y = Freq))


###################################### Question1

year = '2012'
one = data[data$Year == year,]
nrow(one[one$Total > 24,])

# Working
ggplot(one, aes(Employment.Status, Sleeping)) +
  geom_boxplot(outlier.color = "red",outlier.size = 0.5)

ggplot(one, aes(Gender, Sleeping)) +
  geom_boxplot(outlier.color = "red",outlier.size = 0.5)

ggplot(one, aes(Age.Range, Sleeping)) +
  geom_boxplot(outlier.color = "red",outlier.size = 0.5)

ggplot(one, aes(Age.Range, Sleeping)) +
  geom_boxplot(outlier.color = "red",outlier.size = 0.5)

time_hr = one[,11:24]
x = apply(time_hr, 2, function(x){x/60})
x$Total = data[data$Year == year,25]

a = rowSums(x)

> head(a)
56006     56007     56008     56009     56010     56011 
32.516667 21.250000  7.683333 30.900000 13.000000 37.916667 
> View(one)
> 32.516667-24
[1] 8.516667
> 37.916667 - 24
[1] 13.91667
> 32.516667-8.8
[1] 23.71667
> 24-23.71667
[1] 0.28333