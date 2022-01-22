#  READING CREDSCO.CSV. NOTE: Change the path of the file for the proper one in your computer

#Note: Take care to use "/" fo the directory file. "\" provides errors


setwd("C:/Users/adria/Downloads")  # uncomment it if wanna set a path

# get from the current script path
dd <- read.table("1 - get data.csv",header=T, sep=";");
          
#start substituting the structural missing values.
#with remaining, impute: Knn, MIMMI, MICE (multiple imputation, only if you know well)

# IMPUTATION By THE 1NN

library(class)

# FOR EVERY INDIVIDUAL WITH MISSING Incomes LOOK FOR THE MOST SIMILAR INDIVIDUAL 
# wrt REMAINING VARIABLES AND COPY THE VALUE OF INGRESSOS ON THE FIRST 
#For more robustness average the values of k-NN in general (with small k)

# DELETE THE ROWS WITH NO VALUE FOR AGE AND BALANCE

dd2<-dd
for (i in 1:length(dd2$age)) {
  if (is.na(dd2$age[i]) & is.na(dd2$balance[i])) {
    dd2<-dd2[-c(i),] 
  }
}
# WE HAVE DIVIDED dd2 IN A DATAFRAME WITH NA IN balance AND WITH NO NA IN balance 
ddnmb <- dd2[!is.na(dd2$balance),]
ddmb  <- dd2[is.na(dd2$balance),]

# For a single variable:
# Build an artificial matrix with the full numerical variables

fullVariables<-c(6,12,13,14,15)
aux<-ddnmb[,fullVariables]
dim(aux)
names(aux)

# divide in rows that had missing age or not on the target variable to be imputed
aux1 <- aux[!is.na(ddnmb$age),]
dim(aux1)
aux2 <- aux[is.na(ddnmb$age),]
dim(aux2)

sum(is.na(aux2['balance']))

#Find nns for aux2
knn.ing = knn(aux1,aux2,ddnmb$age[!is.na(ddnmb$age)])   

#CARE: neither aux1 nor aux2 can contain NAs


#CARE: knn.ing is generated as a factor. 
#Be sure to retrieve the correct values

ageO<-ddnmb$age
ddnmb$age[is.na(ddnmb$age)] <- as.numeric(as.character(knn.ing))


hist(ageO)
summary(ageO)

hist(ddnmb$age)
summary(ddnmb$age)

# CONCATENATE THE NA's OF BALANCE THAT WE DIVIDED BEFORE
dd3 <- rbind(ddnmb, ddmb)

# NOW WE REPEAT THE SAME PROCESS WITH BALANCE

fullVariables<-c(1,12,13,14,15)
aux<-dd3[,fullVariables]
dim(aux)
names(aux)

# divide in rows that had missing balance or not on the target variable to be imputed
aux1 <- aux[!is.na(dd3$balance),]
dim(aux1)
aux2 <- aux[is.na(dd3$balance),]
dim(aux2)

#Find nns for aux2
knn.ing = knn(aux1,aux2,dd3$balance[!is.na(dd3$balance)])   

#CARE: neither aux1 nor aux2 can contain NAs


#CARE: knn.ing is generated as a factor. 
#Be sure to retrieve the correct values

balO<-dd3$balance
dd3$balance[is.na(dd3$balance)] <- as.numeric(as.character(knn.ing))

hist(balO)
summary(balO)

hist(dd3$balance)
summary(dd3$balance)

#we remove the variable contact because there's no way to fix the missings
dd4 <- subset(dd3, select = -(contact))

summary(dd3$age)
##############################################################################

write.table(dd4, file="2 - kNN.csv", sep=";", dec = " ")

