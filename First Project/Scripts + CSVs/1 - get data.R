## Proyecto 1 MD

# URL:   https://www.kaggle.com/faviovaz/bank-term-deposit

#===============================================================================
# Read data
setwd("C:/Users/adria/Downloads")  # uncomment it if wanna set a path
data <- read.table('bank_term_deposit.csv', header = TRUE, sep = ';')

#===============================================================================
# Data rephrasing

## Jobs
for (i in 1:length(data$job)) {
  if (data$job[i] == "admin.") {
    data$job[i] = "adm"
  }
  
  else if (data$job[i] == "blue-collar") {
    data$job[i] = "bcol"
  }
  
  else if (data$job[i] == "entrepreneur") {
    data$job[i] = "ent"
  }
  
  else if (data$job[i] == "housemaid") {
    data$job[i] = "hous"
  }
  
  else if (data$job[i] == "management") {
    data$job[i] = "mana"
  }
  
  else if (data$job[i] == "retired") {
    data$job[i] = "ret"
  }
  
  else if (data$job[i] == "self-employed") {
    data$job[i] = "semp"
  }
  
  else if (data$job[i] == "services") {
    data$job[i] = "serv"
  }
  
  else if (data$job[i] == "student") {
    data$job[i] = "stu"
  }
  
  else if (data$job[i] == "technician") {
    data$job[i] = "tech"
  }
  
  else if (data$job[i] == "unemployed") {
    data$job[i] = "unem"
  }
  
  else if (data$job[i] == "unknown") {
    data$job[i] = "unk"
  }
}

## Marital
for (i in 1:length(data$marital)) {
  if (data$marital[i] == "divorced") {
    data$marital[i] = "div"
  }
  
  else if (data$marital[i] == "married") {
    data$marital[i] = "marr"
  }
  
  else if (data$marital[i] == "single") {
    data$marital[i] = "sing"
  }
}

## Education
for (i in 1:length(data$education)) {
  if (data$education[i] == "primary") {
    data$education[i] = "prim"
  }
  
  else if (data$education[i] == "secondary") {
    data$education[i] = "sec"
  }
  
  else if (data$education[i] == "tertiary") {
    data$education[i] = "ter"
  }
  
  else if (data$education[i] == "unknown") {
    data$education[i] = "unk"
  }
}

## Contact
for (i in 1:length(data$contact)) {
  if (data$contact[i] == "cellular") {
    data$contact[i] = "cell"
  }
  
  else if (data$contact[i] == "telephone") {
    data$contact[i] = "tel"
  }
  
  else if (data$contact[i] == "unknown") {
    data$contact[i] = "unk"
  }
}

## poutcome
for (i in 1:length(data$poutcome)) {
  if (data$poutcome[i] == "failure") {
    data$poutcome[i] = "fail"
  }
  
  else if (data$poutcome[i] == "other") {
    data$poutcome[i] = "oth"
  }
  
  else if (data$poutcome[i] == "success") {
    data$poutcome[i] = "suc"
  }
  
  else if (data$poutcome[i] == "unknown") {
    data$poutcome[i] = "unk"
  }
}

#===============================================================================

class(data)        # Get type of database
dim(data)          # Database dimensions

# Numerical variables:
#   Discrete:   age, balance, duration, campaign, pdays, previous
#   Continuous: 
#
# Categorical:
#   Ordinal: education
#   Binary: default, housing, loan, y
#   Nominal: job, martial, contact, poutcome
#
# Date: day, month
#===============================================================================

# Get grouped values by column

table(data$age, useNA = "always")             # Numerical     Discrete
table(data$job, useNA = "always")             # Categorical   Nominal
table(data$marital, useNA = "always")         # Categorical   Nominal
table(data$education, useNA = "always")       # Categorical   Ordinal
table(data$default, useNA = "always")         # Categorical   Binary
table(data$balance, useNA = "always")         # Numerical     Discrete
table(data$housing, useNA = "always")         # Categorical   Binary
table(data$loan, useNA = "always")            # Categorical   Binary
table(data$contact, useNA = "always")         # Categorical   Nominal
table(data$day, useNA = "always")             # Date
table(data$month, useNA = "always")           # Date
table(data$duration, useNA = "always")        # Numerical     Discrete
table(data$campaign, useNA = "always")        # Numerical     Discrete
table(data$pdays, useNA = "always")           # Numerical     Discrete
table(data$previous, useNA = "always")        # Numerical     Discrete
table(data$poutcome, useNA = "always")        # Categorical   Nominal
table(data$y, useNA = "always")               # Categorical   Binary

#===============================================================================

# Get missings of each variables

missings_age = sum(is.na(data$age))
missings_job = length(which(data$job == 'unknown'))
missings_marital = 0
missings_education = length(which(data$education == 'unknown'))
missings_default = 0
missings_balance = sum(is.na(data$balance))
missings_housing = 0
missings_loan = 0
missings_contact = length(which(data$contact == 'unknown'))
missings_day = 0
missings_month = 0
missings_duration = 0
missings_campaign = 0
missings_pdays = 0
missings_previous = 0
missings_poutcome = 0
missings_y = 0

# Get % missings of each variables

missings_perc_age = missings_age / nrow(data)
missings_perc_job = missings_job / nrow(data)
missings_perc_marital = missings_marital / nrow(data)
missings_perc_education = missings_education / nrow(data)
missings_perc_default = missings_default / nrow(data)
missings_perc_balance = missings_balance / nrow(data)
missings_perc_housing = missings_housing / nrow(data)
missings_perc_loan = missings_loan / nrow(data)
missings_perc_contact = lmissings_contact / nrow(data)
missings_perc_day = missings_day / nrow(data)
missings_perc_month = missings_month / nrow(data)
missings_perc_duration = missings_duration / nrow(data)
missings_perc_campaign = missings_campaign / nrow(data)
missings_perc_pdays = missings_pdays / nrow(data)
missings_perc_previous = missings_previous / nrow(data)
missings_perc_poutcome = missings_poutcome / nrow(data)
missings_perc_y = missings_y / nrow(data)

# Get % matrix missings

missings_perc_matrix = (missings_age + missings_job + missings_marital + missings_education + missings_default + missings_balance +
                        missings_housing + missings_loan + missings_contact + missings_day + missings_month + missings_duration + 
                        missings_campaign + missings_pdays + missings_previous +  missings_poutcome + missings_y) / (nrow(data) * ncol(data))

#===============================================================================
# Obtain the csv file with the modified data
write.table(data, file="1 - get data.csv", sep=";", dec = " ")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Get an histogram of numerical variables
hist(data$age)
hist(data$balance)
hist(data$duration)
hist(data$campaign)
hist(data$pdays)
hist(data$previous)
hist(data$day)

# Obtain the median and the mean
summary(data)

# Get a bar plot of non-numerical variables
barplot(table(data$job))
barplot(table(data$marital))
barplot(table(data$education))
barplot(table(data$default))
barplot(table(data$housing))
barplot(table(data$loan))
barplot(table(data$contact))
barplot(table(data$poutcome))
barplot(table(data$y))
barplot(table(data$month))

# Get a 
pie(table(data$job))
pie(table(data$marital))
pie(table(data$education))
pie(table(data$default))
pie(table(data$housing))
pie(table(data$loan))
pie(table(data$contact))
pie(table(data$poutcome))
pie(table(data$y))
pie(table(data$month))

