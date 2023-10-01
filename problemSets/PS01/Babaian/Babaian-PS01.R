#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)

###1. Find a 90% confidence interval for the average student IQ in the school.

t_score <- qt(0.95, df=length(y)-1)
lower_90_t <- mean(y)-(t_score)*(sd(y)/sqrt(length(y)))
upper_90_t <- mean(y)+(t_score)*(sd(y)/sqrt(length(y)))

# Confidence interval boundaries: 93.95993; 102.9201
lower_90_t
mean(y) #98.44
upper_90_t

#double checking:
t.test(y, conf.level = 0.9, alternative = "two.sided")
# 90 percent confidence interval: 93.95993; 102.92007

###2. Whether the average student IQ in her school is higher than
#the average IQ score (100) among all the schools in the country.

t.test(y, mu = 100, alternative = "greater")
# We cannot reject the null hypothesis that the average student IQ in the school is not higher than the average IQ score across all schools in the country (p-value = 0.7215)

##Using the same sample, conduct the appropriate hypothesis test with \alpha = 0.05.
t.test(y, mu = 100, conf.level = 0.95) # we cannot first reject the H0 that the averages are equal on 0.05 significance (p-value = 0.5569)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
str(expenditure)
#plot the relationships among Y, X1, X2, and X3.
#What are the correlations among them?

#install.packages("GGally)
library(GGally)
ggpairs(expenditure,
        columns = 2:5,
        columnLabels=c("Housing assistance in state",
       "Personal income in state",
       "Share of financially insecure in state",
       "Share of urban population in state"))
## According to the visualized relationships between all meaningful quantities in the data,
#the variables are positively related, but there are no strong correlations 
# between any of them

# plot the relationship between Y and Region.
#On average, which region has the highest per capita expenditure on housing assistance?

#transforming Region to a factor for meaningful visualization:
expenditure$Region <- factor(expenditure$Region, labels = c("Northeast", "North Central", "South", "West"))

library(ggplot2)
ggplot(data=expenditure, mapping=aes(x=Region, y=Y)) + 
  stat_summary(fun.data=mean_sdl, geom="bar")  + 
  labs(title="Per capita expenditure on shelters/housing assistance in state by region",
       x="", y="") 
#The highest per capita expenditure on housing assistance is in the Western states.

#plot the relationship between Y and X1:
plot(expenditure$X1, expenditure$Y,
     xlab="Per capita personal income in state",
     ylab="Housing assistance in state",
     main="The Relationship between state-level personal income and housing assistance")
# There is a positive relationship between per capita personal income in state
# and its housing assistance expenditures, however, there is no strong correlation
# as there is a significant amount on deviations on both sides of the distributions

#Reproduce the above graph including one more variable Region
#and display different regions with different types of symbols and colors.

plot(expenditure$X1, expenditure$Y,
    col= expenditure$Region, 
    pch=c(21,22,23,24)[as.numeric(expenditure$Region)],
    xlab="Per capita personal income in state",
    ylab="Housing assistance in state",
    main="The Relationship between state-level personal income and housing assistance by regions")

legend("topleft", cex=0.5, legend=levels(expenditure$Region), pch=unique(expenditure$Region), col=unique(expenditure$Region))

