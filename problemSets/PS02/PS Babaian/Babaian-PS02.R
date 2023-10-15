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

df <- data.frame(Not_Stopped = c(14,7), 
                 Bribe_requested= c(6, 7),
                 Stopped_or_given_warning = c(7, 1))

# R calculation
print(chisq.test(df)) #3.7912

#creating sums and adding to the table:
df2$Total <- c(sum(df2[1,1:3]), sum(df2[2,1:3]))
df2[nrow(df2) + 1,] <- c(sum(df2$Not_Stopped), sum(df2$Bribe_requested), sum(df2$Stopped_or_given_warning), sum(df2$Total))
rownames(df2) <- c("upper_class", "lower_class", "Total")

## manual calc ((row total/grand total)*column total),
# order by rows
f1e <- df2[1,4] / df2[3,4] * df2[3,1] #13.5
f2e <-df2[2,4] / df2[3,4] * df2[3,1] #7.5
f3e <-df2[1,4] / df2[3,4] * df2[3,2] # 8.36
f4e <-df2[2,4] / df2[3,4] * df2[3,2] # 4.64
f5e <- df2[1,4] / df2[3,4] * df2[3,3] # 5.14
f6e <-df2[2,4] / df2[3,4] * df2[3,3] # 2.86

### calculating the statistic: 
chisq <- sum((df2[1,1] - f1e)^2/f1e + (df2[2,1] - f2e)^2/f2e + (df2[1,2] - f3e)^2/f3e +(df2[2,2] - f4e)^2/f4e + (df2[1,3] - f5e)^2/f5e + (df2[2,3] - f6e)^2/f6e)
print(chisq) #3.791



## b: Now calculate the p-value from the test statistic you just created (in R). What do you conclude if \alpha = 0.1?
df_df <- (nrow(df2) - 1)*(ncol(df2)-1) 
print(df_df) # 6 degrees of freedom
p <- pchisq(3.79, df=6, lower.tail=F) 
print(p) # 0.705
## almost the same, as in the R's built-in function output
# cannot reject H0: The variables are statistically independent


## c: Calculate the standardized residuals for each cell and put them in the table below.
z11 <- (df2[1,1] - f1e) / sqrt(f1e*(1-(df2[1,4]/df2[3,4]))*(1-(df2[3,1]/df2[3,4])))
z21 <- (df2[2,1] - f2e) / sqrt(f2e*(1-(df2[2,4]/df2[3,4]))*(1-(df2[3,1]/df2[3,4])))
z12 <- (df2[1,2] - f3e) / sqrt(f3e*(1-(df2[1,4]/df2[3,4]))*(1-(df2[3,2]/df2[3,4])))
z22 <- (df2[2,2] - f4e) / sqrt(f4e*(1-(df2[2,4]/df2[3,4]))*(1-(df2[3,2]/df2[3,4])))
z13 <- (df2[1,3] - f5e) / sqrt(f5e*(1-(df2[1,4]/df2[3,4]))*(1-(df2[3,3]/df2[3,4])))
z23 <- (df2[2,3] - f6e) / sqrt(f6e*(1-(df2[2,4]/df2[3,4]))*(1-(df2[3,3]/df2[3,4])))

z11 <- (df2[1,1] - f1e) / sqrt(f1e)
z21 <- (df2[2,1] - f2e) / sqrt(f2e)
z12 <- (df2[1,2] - f3e) / sqrt(f3e)
z22 <- (df2[2,2] - f4e) / sqrt(f4e)
z13 <- (df2[1,3] - f5e) / sqrt(f5e)
z23 <- (df2[2,3] - f6e) / sqrt(f6e)

df_res <- data.frame(Not_Stopped <- c(z11, z21),
                     Bribe_requested <- c(z12, z22),
                     Stopped_or_given_warning <- c(z13, z23))

colnames(df_res) <- c("Not_Stopped", "Bribe_requested", "Stopped_or_given_warning")
rownames(df_res) <- c("upper_class", "lower_class")
library(xtable)
setwd("/Users/vv/Downloads/StatsI_Fall2023_oct4/problemSets/PS02/template")
print(xtable(df_res, type = "latex"), file = "residuals.tex")

#How far away is each observed value from "expectation". Worst predictions are where residuals are closer to abs(1.96) -- lower class Bribe requested Stopped/given warning

chisq.test(df)$residuals ## same results




##### 2 
#b 

data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
str(data)

model <- lm(water ~ reserved, data = data)
summary(model)

library(stargazer)
stargazer(model, title = "Reservation policies and improved drinking water facilities",
          out = "PS02_regression_Babaian.tex")
