#Sekvenci 10 – 20 v celých èíslech a uložte ji jako sek1
(sek1 <- 10:20)
#Sekvenci 10 – 20 po pùl èíslech (10, 10.5, 11,…) 
#a uložte ji jako sek2
(sek2 <- seq(10, 20, 0.5))

#Urèete délku tìchto sekvencí.
length(sek1)
length(sek2)

#Najdìte 5. – 10. èíslo v každé sekvenci.
sek1[5:10]
sek2[5:10]
#Najdìte hodnotu maxima v každé sekvenci.
max(sek1)
max(sek2)

#Urèete poøadí hodnoty maxima v každé sekvenci.
which(sek1 == max(sek1))
which(sek2 == max(sek2))
#Urèete poøadí hodnoty 15 v každé sekvenci.
which(sek1 == 15)
which(sek2 == 15)
#Urèete, která èísla v sekvenci jsou vyšší než 18
which(sek1 > 18)
which(sek2 > 18)

#Urèete hodnoty pøirozených a dekadických logaritmù v obou sekvencích.
log(sek1)
log(sek2)
log(sek1, 10)
log(sek2, 10)
#Urèete hodnoty druhých mocnin v obou sekvencích.
sek1^2
sek2^2
#Seètìte hodnoty v obou sekvencích.
sum(sek1)
sum(sek2)
#Seètìte hodnoty prvních pìti èísel v obou sekvencích.
sum(sek1[1:5])
sum(sek2[1:5])
#Seètìte hodnoty èísel vyšších než 18 v obou sekvencích.
sum(sek1[which(sek1 > 18)])
sum(sek2[which(sek2 > 18)])

#Vytvoøte následující matici
matice <- matrix(c(5,8,3,10,4,2,-5,-3,-2), nrow = 3, ncol = 3, 
          byrow = TRUE)
#Seètìte hodnoty v matici.
sum(matice)
#Vyberte druhý sloupec matice.
matice[,2]
#Vyberte tøetí øádek matice.
matice[3,]

#Pøidejte k matici øádek s hodnotami 5, 4, 2
matice <- rbind(matice, c(5, 4, 2))
#Pøidejte k matici sloupec s hodnotami 8, 4, -5, 3
(matice <- cbind(matice, c(8, 4, -5, 3)))

getwd()
setwd()
data_studenti <- read.csv("C:/Users/edcab/Downloads/data_studenti.csv", sep=";")
View(data_studenti)

data <- data_studenti
rm(data_studenti)

#Zobrazte si zaèátek souboru data.
head(data)
#Vyberte šestý øádek souboru data.
data[6,]
#Vyberte tøetí sloupec souboru data.
data[,3]

#Použijte hodnoty promìnné Výška a spoèítejte v R (k tomu vytvoøte tøi funkce):

#1) Souèet hodnot: 7717
sum(data$Výška)
sum(data[,3])

#2) Souèet druhých mocnin (ètvercù): 1 359 489
sum(data$Výška^2)

#3) Druhou mocninu (ètverec) souètu: 59 552 089
sum(data$Výška)^2

#4) Násobek hodnot: 4.926811e+98
prod(data$Výška)

#5) Souèet hodnot dìlený jejich poètem: (aritmetický prùmìr):
length(data$Výška)
nrow(data)  #ncol()

sum(data$Výška)/length(data$Výška)
mean(data$Výška)
#175.38

#6) Souèet ètvercových hodnot dìlený jejich poètem (prùmìr ètvercù):
sum(data$Výška^2)/length(data$Výška)
mean(data$Výška^2)
#30 897.48

#7) Souèet odchylek od prùmìru:

sum(data$Výška - mean(data$Výška))
# 4.547474e-13 (= 0)

#8) Souèet ètvercových odchylek od prùmìru:
Squared <- (data$Výška - mean(data$Výška))^2
sum(Squared)
sum((data$Výška - mean(data$Výška))^2)
#6032.432

#9) Souèet ètvercových odchylek od prùmìru dìlený jejich poètem (rozptyl):
sum(Squared)/length(Squared)
#137.1

#10) Souèet násobkù hodnot Výška a Délka Chodidla: 197 051
sum(data$Výška*data$Délka.chodidla)

#11) Souèet násobkù hodnot Výška a Délka Chodidla dìlený jejich poètem (prùmìr násobkù):
sum(data$Výška*data$Délka.chodidla)/length(data$Výška)
mean(data$Výška*data$Délka.chodidla)
#4478.4

#Funkce populaèního rozptylu

var.p <- function (hod)
{
  sum((hod - mean(hod))^2)/length(hod)
}

var.p(data$Výška)

#Funkce pro prùmìr násobkù
MeanProd <- function(hod1, hod2)
{
  sum(hod1*hod2)/length(hod1)  
}
