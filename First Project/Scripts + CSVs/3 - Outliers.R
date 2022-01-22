setwd("C:/Users/adria/Downloads")  # uncomment it if wanna set a path

# get from the current script path
dd <- read.table("2 - kNN.csv",header=T, sep=";");

# Get numerical variables
fullVariables<-c(1, 6, 11, 12, 13, 14)
aux<-dd[,fullVariables]

#===============================================================================
# Delete outliers
#===============================================================================
hist(aux$age)
plot(aux$age, aux$age)
summary(aux$age)
# Comprovamos que la edad mínima son los 19 años y la máxima los 87, por lo que
# los rangos son totalmente normales
#-------------------------------------------------------------------------------
hist(aux$balance)
plot(aux$balance, aux$balance)
# Vemos que hay un dato muy alejado del resto
summary(aux$balance)
# Obtenemos el número de la fila del valor máximo
numRows = which(aux$balance == 71188)
# eliminamos la fila con ese valor
aux <- aux[-c(numRows), ]
#-------------------------------------------------------------------------------
hist(aux$duration)
plot(aux$duration, aux$duration)
summary(aux$duration)
outDuration = aux$duration[aux$duration > 2400]
# Pasamos los valores a minutos en vez de segundos
outDuration = outDuration / 60
meanDuration = mean(aux$duration) / 60
numRows = which(aux$duration > 2400)
#-------------------------------------------------------------------------------
hist(aux$campaign)
plot(aux$campaign, aux$campaign)
summary(aux$campaign)
aux$campaign[aux$campaign > 40]
numRows = which(aux$campaign > 40)
#-------------------------------------------------------------------------------
hist(aux$pdays)
plot(aux$pdays, aux$pdays)
summary(aux$pdays)
# años que ha durado la máxima campaña
max(aux$pdays)/365
#-------------------------------------------------------------------------------
hist(aux$previous)
plot(aux$previous, aux$previous)
summary(aux$previous)
#-------------------------------------------------------------------------------

#===============================================================================
write.table(data, file="3 - Outliers.csv", sep=";", dec = " ")
