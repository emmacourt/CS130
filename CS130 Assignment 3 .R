foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))
#install.packages("rgenoud")
#install.packages("date")

#Loading necessary libraries 
library(rgenoud)
library(Matching)
library(date)

#Initial regression 
model1 <- lm(nowtot ~ Dems + Repubs + Christian + age + srvlng + demvote + hasgirls, data = foo)
summary(model1)
confint(model1)

#See initial balance 
mb <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo)
set.seed(2324)

#Genetic Matching, M=1
genout <- GenMatch(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), pop.size = 20, nboots = 250)

mout <- Match(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = genout)
summary(mout)
mb_after <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo, match.out = mout)

#Genetic Matching, M=2
genout2 <- GenMatch(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), pop.size = 20, nboots = 250, M=2)

mout2 <- Match(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = genout2, M=2)

mb_after2 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo, match.out = mout2)

#Genetic Matching, M=3
genout3 <- GenMatch(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), pop.size = 20, nboots = 250, M=3)

mout3 <- Match(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = genout3, M=3)

mb_after3 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo, match.out = mout3)

#Rerun Match with Y included 
mout_final <- Match(Y = foo$nowtot, Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = genout)

mbY <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo, match.out = mout_final)

summary(mout_final)

#Confidence Interval 
mout_final$se

estimate <- mout_final$est
confidence_lower =mout_final$est - 1.96 * mout_final$se
confidence_upper = mout_final$est + 1.96 * mout_final$se


cat("The estimate for the treatment effect is [", estimate ,"], and confidence interval is [", confidence_lower, "," ,confidence_upper, "]") 

#QUESTION B 

#Load data
foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))

#Specify Treatment and Control 
treat <- subset(foo, foo$ngirls >= 2 & foo$nboys == 0)
control <- subset(foo, foo$nboys >= 2 & foo$ngirls == 0)

#Create new dataset with treatment and control observations
foo_subset <- rbind(treat, control)

dim(foo_subset)

#Intial model 
model2 <- lm(nowtot ~ Dems + Repubs + Christian + age + srvlng + demvote + hasgirls, data = foo_subset)
summary(model2)
confint(model2)

#Genetic Matching, M=1
genout_new1 <- GenMatch(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote), pop.size = 20, nboots = 250)

mout_new1 <- Match(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote), Weight.matrix = genout_new1)

mb_after_new1 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo_subset, match.out = mout_new1)

#Genetic Matching, M=2
genout_new2 <- GenMatch(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote), pop.size = 20, nboots = 250, M=2)

mout_new2 <- Match(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote), Weight.matrix = genout_new2, M=2)

mb_after_new2 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo_subset, match.out = mout_new2)

#Genetic Matching, M=3
genout_new3 <- GenMatch(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote), pop.size = 20, nboots = 250, M=3)

mout_new3 <- Match(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote), Weight.matrix = genout_new3, M=3)

mb_after_new3 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo_subset, match.out = mout_new3)

#Match with Y 
mout_final_new <- Match(Y = foo_subset$nowtot, Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote), Weight.matrix = genout_new1)

mb_final_new <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote, data = foo_subset, match.out = mout_final_new)


summary(mout_final_new)

#Confidence Intervals 
mout_final_new$se

estimate <- mout_final_new$est
confidence_lower =mout_final_new$est - 1.96 * mout_final_new$se
confidence_upper = mout_final_new$est + 1.96 * mout_final_new$se


cat("The estimate for the treatment effect is [", estimate ,"], and confidence interval is [", confidence_lower, "," ,confidence_upper, "]") 


#Bonus Code 

#Load data, specify treatment and control 
foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))

treat <- subset(foo, foo$ngirls >= 2 & foo$nboys == 0)
control <- subset(foo, foo$nboys >= 2 & foo$ngirls == 0)

foo_subset <- rbind(treat, control)
foo_subset <- na.omit(foo_subset)
dim(foo_subset)

#New model with additional variables 
model2 <- lm(nowtot ~ Dems + Repubs + Christian + age + srvlng + demvote + white + protgay + moredef + morecrimesp + hasgirls, data = foo_subset)
summary(model2)
confint(model2)

#Genetic Matching, M=1

genout_new1 <- GenMatch(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote, foo_subset$white, foo_subset$protgay, foo_subset$moredef, foo_subset$morecrimesp), pop.size = 20, nboots = 250)

mout_new1 <- Match(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote, foo_subset$white, foo_subset$protgay, foo_subset$moredef, foo_subset$morecrimesp), Weight.matrix = genout_new1)

mb_after_new1 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote + white + protgay + moredef + morecrimesp, data = foo_subset, match.out = mout_new1)

#Genetic Matching, M=2
genout_new2 <- GenMatch(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote, foo_subset$white, foo_subset$protgay, foo_subset$moredef, foo_subset$morecrimesp), pop.size = 20, nboots = 250, M=2)

mout_new2 <- Match(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote, foo_subset$white, foo_subset$protgay, foo_subset$moredef, foo_subset$morecrimesp), Weight.matrix = genout_new2, M=2)

mb_after_new2 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote + white + protgay + moredef + morecrimesp, data = foo_subset, match.out = mout_new2)

#Genetic Matching, M=3
genout_new3 <- GenMatch(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote, foo_subset$white, foo_subset$protgay, foo_subset$moredef, foo_subset$morecrimesp), pop.size = 20, nboots = 250, M=3)

mout_new3 <- Match(Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote, foo_subset$white, foo_subset$protgay, foo_subset$moredef, foo_subset$morecrimesp), Weight.matrix = genout_new3, M=3)

mb_after_new3 <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote + white + protgay + moredef + morecrimesp, data = foo_subset, match.out = mout_new3)

#Genetic Matching, with Y
mout_final_new <- Match(Y = foo_subset$nowtot, Tr = foo_subset$hasgirls, X = cbind(foo_subset$Dems, foo_subset$Repubs, foo_subset$Christian, foo_subset$age, foo_subset$srvlng, foo_subset$demvote, foo_subset$white, foo_subset$protgay, foo_subset$moredef, foo_subset$morecrimesp), Weight.matrix = genout_new1)

mb_final_new <- MatchBalance(hasgirls ~ Dems + Repubs + Christian + age + srvlng + demvote + white + protgay + moredef + morecrimesp, data = foo_subset, match.out = mout_final_new)


summary(mout_final_new)

#Confidence Intervals 
mout_final_new$se

estimate <- mout_final_new$est
confidence_lower =mout_final_new$est - 1.96 * mout_final_new$se
confidence_upper = mout_final_new$est + 1.96 * mout_final_new$se


cat("The estimate for the treatment effect is [", estimate ,"], and confidence interval is [", confidence_lower, "," ,confidence_upper, "]") 


#QUESTION 2 

## This code is available here:
#https://gist.githubusercontent.com/diamonaj/27eb90c53d29d193b990e8c2c3cc8adb/raw/21cb153a23c9eb672824eb38a0e671578f4d76ad/Memo%2520Code%2520that%2520works%2520on%2520platform

## This code deviates from the memo on line 157 below

rm(list=ls())

#install.packages("rgenoud")
#install.packages("date")
library(rgenoud)

foo <- read.csv("https://tinyurl.com/y2qv82ks")


# Only install the 2 packages below if not already installed
# install.packages("date")
# install.packages("Matching")

library(date)
library(Matching)

# dim(foo)
# 76772    14

names(foo)
#[1] "bank_code"      [2] "bank_name"      [3] "date_of_birth"  
#[4] "gender"         [5] "marital_status" [6]  "education"      
#[7] "occupation"     [8] "postal_code"    [9]  "district_code"  
#[10] "worker"        [11] "capital"       [12] "credit_proposal"
#[13] "status"        [14] "randomid"

# vars 1, 2, and 3 have 17,330, 68,029, and 1108 blanks (""), respectively
# vars 8 and 11 have 13,118 and 14,642 NAs, respectively

# create dummies indicating where the blanks and NAs are.
missing_bank_code <- rep(0, 76772)
missing_bank_name <- rep(0, 76772)
missing_date_of_birth <- rep(0, 76772)
NA_postal_code <- rep(0, 76772)
NA_capital <- rep(0, 76772)
NA_credit_proposal <- rep(0, 76772)

foo <- cbind(foo, missing_bank_code,
             missing_bank_name,
             missing_date_of_birth,
             NA_postal_code,
             NA_capital,
             NA_credit_proposal)

foo$missing_bank_code[which(foo$bank_code == "")] <- 1
foo$missing_bank_name[which(foo$bank_name == "")] <- 1
foo$missing_date_of_birth[which(foo$date_of_birth == "")] <- 1
foo$NA_capital[which(is.na(foo$capital) == TRUE)] <- 1
foo$NA_credit_proposal[which(is.na(foo$credit_proposal) == TRUE)] <- 1
foo$NA_postal_code[which(is.na(foo$postal_code) == TRUE)] <- 1

# change the dates to R-readable format
foo$R_date_of_birth <- as.character(foo[,3])
for(i in 1:length(foo[,3])) {foo$R_date_of_birth[i] <- as.date(foo$R_date_of_birth[i], order = 
                                                                 "dmy")}
foo$R_date_of_birth <- as.date(as.numeric(foo$R_date_of_birth))

oldest <- which(foo$R_date_of_birth < as.date("1-Jan-1910"))
youngest <- which(foo$R_date_of_birth > as.date("1 Jan 2001"))



foo$oldest <- rep(0, length(foo[,3]))
foo$youngest <- rep(0, length(foo[,3]))
foo$outlier_ages <- rep(0, length(foo[,3]))
foo$oldest[oldest] <- 1
foo$youngest[youngest] <- 1
foo$outlier_ages[c(oldest,youngest)] <- 1

foo$R_date_of_birth[which(is.na(foo$R_date_of_birth) == TRUE)] <- -9999999

# This obs with specific postal code makes no sense
foo <- foo[-which(foo$postal_code == 9151), ]

# To extract only the first digit of postal codes:
foo$postal_code1 <- foo$postal_code%/% 10000
foo$postal_code1[which(is.na(foo$postal_code1) == TRUE)] <- -9999999

# credit_proposal feature engineering
foo$credit_proposal[which(is.na(foo$credit_proposal) == TRUE)] <- 9999999

foo$credit_proposal_0 <- foo$credit_proposal == 0 & (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_0to5 <- foo$credit_proposal > 0 & foo$credit_proposal < 5000000 & 
  (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_5to10 <- foo$credit_proposal >= 5000000 & foo$credit_proposal < 10000000 & 
  (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_10to20 <- foo$credit_proposal >= 10000000 & foo$credit_proposal < 20000000 & 
  (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_20up <- foo$credit_proposal >= 20000000 & (is.na(foo$credit_proposal) == 
                                                                 FALSE)

foo$credit_proposal_transformed <-
  1*foo$credit_proposal_0 +
  2*foo$credit_proposal_0to5 +
  3*foo$credit_proposal_5to10 +
  4*foo$credit_proposal_10to20 +
  5*foo$credit_proposal_20up +
  6*foo$NA_credit_proposal

# NA capital
foo$capital[which(is.na(foo$capital) == TRUE)] <- 9999999

# capital feature engineering
foo$capital_0 <- foo$capital == 0 & (is.na(foo$capital) == FALSE)
foo$capital_0to2 <- foo$capital > 0 & foo$capital < 200000 & (is.na(foo$capital) == FALSE)
foo$capital_2to5 <- foo$capital >= 200000 & foo$capital < 500000 & (is.na(foo$capital) == FALSE)
foo$capital_5to10 <- foo$capital >= 500000 & foo$capital < 1000000 & (is.na(foo$capital) == 
                                                                        FALSE)
foo$capital_10to20 <- foo$capital >= 1000000 & foo$capital < 2000000 & (is.na(foo$capital) == 
                                                                          FALSE)
foo$capital_20to50 <- foo$capital >= 2000000 & foo$capital < 5000000 & (is.na(foo$capital) == 
                                                                          FALSE)
foo$capital_50up <- foo$capital >= 5000000 & (is.na(foo$capital) == FALSE)
foo$capital_transformed <-
  1*foo$capital_0 +
  2*foo$capital_0to2 +
  3*foo$capital_2to5 +
  4*foo$capital_5to10 +
  5*foo$capital_10to20 +
  6*foo$capital_20to50 +
  7*foo$capital_50up +
  8*foo$NA_capital

# worker feature engineering
# remove outlier in the control group (10 million workers)
foo <- foo[-which(foo$worker == max(foo$worker)),]

foo$worker_0 <- foo$worker == 0
foo$worker_1 <- foo$worker == 1
foo$worker_2 <- foo$worker == 2
foo$worker_3 <- foo$worker == 3
foo$worker_4 <- foo$worker == 4
foo$worker_5to9 <- foo$worker >=5 & foo$worker < 10
foo$worker_10to24 <- foo$worker >=10 & foo$worker < 25
foo$worker_25to99 <- foo$worker >=25 & foo$worker < 100
foo$worker_100up <- foo$worker >= 100

foo$worker_transformed <-
  1*foo$worker_0 +
  2*foo$worker_1 +
  3*foo$worker_2 +
  4*foo$worker_3 +
  5*foo$worker_4 +
  6*foo$worker_5to9 +
  7*foo$worker_10to24 +
  8*foo$worker_25to99 +
  9*foo$worker_100up


# Treatment Indicator
foo$treat <- foo$status == "Sudah"


foo_badan <- foo[which(foo$gender == "BADAN USAHA"), ]
foo_people <- foo[-which(foo$gender == "BADAN USAHA"), ]

######## CODE MODIFICATION SHOULD BEGIN HERE...

#Get characters as numerics for the model 
foo$gender <- as.factor(foo$gender)
foo$education <- as.factor(foo$education)
foo$marital_status <- as.factor(foo$marital_status)
foo$occupation <- as.factor(foo$occupation)
foo$status <- as.factor(foo$status)

foo$gender <- as.numeric(foo$gender)
foo$education <- as.numeric(foo$education)
foo$marital_status <- as.numeric(foo$marital_status)
foo$occupation <- as.numeric(foo$occupation)
foo$status <- as.numeric(foo$status)

#Change date of birth to number of days after Jan 1, 1900 (so that you can specify 365 days easily)

foo$date_of_birth <- as.Date(foo$date_of_birth, format = "%d-%m-%Y")

calc_date <- as.Date("01-01-1900", format = "%d-%m-%Y")

NumberOfDays = as.numeric(foo$date_of_birth - calc_date)

#Create new variable with first 2 numbers of district code
dist_code_2 <- as.numeric(substr(foo$district_code, 1, 2))

#Add new columns to dataframe
foo <- cbind(foo, NumberOfDays, dist_code_2)

#Remove NA's 
foo<- na.omit(foo)

#Subset data for easier processing 
foo_small <- foo[sample(nrow(foo), 1000), ]


#vector of independent variables 
X = cbind(foo_small$NumberOfDays, foo_small$gender, foo_small$marital_status,
          foo_small$education, foo_small$occupation, foo_small$district_code,
          foo_small$worker, foo_small$capital, foo_small$credit_proposal,
          foo_small$worker_transformed, foo_small$capital_transformed, foo_small$credit_proposal_transformed,
          foo_small$missing_date_of_birth,
          foo_small$NA_capital,
          foo_small$NA_credit_proposal)


#Specify treatment 
Tr <- foo_small$treat

BalanceMat <- X

#Genetic Matching 
genout <- GenMatch(Tr=Tr, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=5, wait.generations=3,
                   caliper = c(365, 0, 0,
                             0, 0, 0,
                             1, 1, 1,
                             0, 0, 0,
                             0,
                             0,
                             0))



mout <- Match(Tr=Tr, X=X, estimand="ATT", M=1,
              caliper = c(365, 0, 0,
                          0, 0, 0,
                          1, 1, 1,
                          0, 0, 0,
                          0,
                          0,
                          0), 
              Weight.matrix = diag(c(8.229663e+02, 3.248991e+02,
                                     1.582134e+02, 6.569852e+02, 9.990704e+01, 2.693163e+02, 
                                     3.513421e+02, 7.512798e+00, 1.233057e+02,
                                     7.083123e+02, 9.801175e+02, 5.627142e+02,
                                     6.751423e+02, 4.900635e+02, 1.064447e+02,
                                     3.989456e+02)))
summary(mout)

mb <- MatchBalance(foo_small$treat~
                     foo_small$NumberOfDays + foo_small$gender + foo_small$marital_status +
                     foo_small$education + foo_small$occupation + foo_small$district_code +
                     foo_small$worker + foo_small$capital + foo_small$credit_proposal +
                     foo_small$worker_transformed + foo_small$capital_transformed + 
                     foo_small$credit_proposal_transformed +
                     foo_small$missing_date_of_birth +
                     foo_small$NA_capital +
                     foo_small$NA_credit_proposal,
                   match.out=mout, nboots=500)


#Question 3 

#install.packages("sensemakr")
library(sensemakr)

#For the Indonesia Case Study 
t  = 0.0037439 	 #t-statistic from the output of mb
dof = 999 - 15 #degrees of freedom, nrows - ncolumns - 1
robustness_value(t_statistic = t, dof = dof)
#0.0001193441

#For the Daughters Case Study, Part A 
t  = 0.48519 #t-statistic from the output of mb
dof = 429 - 5 #degrees of freedom, nrows - ncolumns - 1
robustness_value(t_statistic = t, dof = dof)
#0.02328694


#For the Daughters Case Study, Part B
t  = 3.0869 	 #t-statistic from the output of mb
dof = 83 - 5 #degrees of freedom, nrows - ncolumns - 1
robustness_value(t_statistic = t, dof = dof)
#0.2937369

#For the Daughters Case Study, Bonus 
t  =2.3924 	 #t-statistic from the output of mb
dof = 83 - 9 #degrees of freedom, nrows - ncolumns - 1
robustness_value(t_statistic = t, dof = dof)
#0.08378609








