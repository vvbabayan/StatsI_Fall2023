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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
str(inc.sub)

### 1
q1 <- lm(voteshare ~ difflog, data = inc.sub)

png("Babaian-plot1.png")
plot(voteshare ~ difflog, data = inc.sub,
     xlab="Incumbent-challenger campaign spending difference",
     ylab="Incumbent's vote share",
     main="Campaign spending difference and incumbent vote share") # plotting relationship
abline(q1) # adding the regression line
dev.off()

q1_res <- q1$residuals ## saving residuals
class(q1)

library(stargazer)
summary(q1)
stargazer(q1, out = "q1.tex")
#voteshare_hat = b_o_hat + b_1_hat * x = 0.579 + 0.042*difflog 




#### q2
q2 <- lm(presvote ~ difflog, data = inc.sub)

png("Babaian-plot2.png")
plot(presvote ~ difflog, data = inc.sub, 
     xlab="Incumbent-challenger campaign spending difference",
     ylab="Incumbent's party presidential candidate vote share",
     main="Campaign spending difference and incumbent party's presidential candidate vote share",
     cex.main = 0.8)# plotting relationship
abline(q2) # adding the regression line
dev.off()

q2_res <- q2$residuals ## saving residuals


summary(q2)
stargazer(q2, out = "q2.tex")
#presvote_hat = 0.508 + 0.024*difflog 


### q3
q3 <- lm(voteshare ~ presvote, data = inc.sub)

png("Babaian-plot3.png")
plot(voteshare ~ presvote, data = inc.sub,
     xlab="Incumbent's party presidential candidate vote share",
     ylab="Incumbent party's vote share",
     main="Incumbent party's presidential candidate vote share and party's electoral success",
     cex.main = 0.8) # plotting relationship
abline(q3)
dev.off()

summary(q3)
stargazer(q3, out = "q3.tex")
#voteshare_hat = 0.441 + 0.388*presvote 


### q4
q4 <- lm(q1_res~q2_res)

png("Babaian-plot4.png")
plot(q1_res~q2_res,
     xlab="Variation in incumbent party's electoral score unexplained by difference in spending",
     ylab="Variation in its presidential candidate's score unexplained by difference in spending",
     main="Relationship between residuals") # plotting relationship
abline(q4)
dev.off()
#voteshare_res = 0.441 + 0.388*presvote 


summary(q4)
stargazer(q4, out = "q4.tex")
#res1_hat = 4.86*10^(-18) + 0.257*res2 


###q5
q5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)

summary(q5)
stargazer(q5, out = "q5.tex")

#voteshare_hat = 0.449 + 0.036*difflog + 0.257*presvote
