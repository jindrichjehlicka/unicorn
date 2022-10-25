################################## CVICENI 3 ############################
#primkova regrece
#hyperbolicka regrese
#logaritmicka regrese
# parabolicka regrese
# polynomicka regrese

#testu vyznamnosti
#test o modelu = celkovy f-test
#test  o parametrech = individualni t-testy

# teoreticky soucet ctvercu vetsi nez rezidualni
install.packages("MASS") # balicek pro analyzu jackknife rezidui
install.packages("ggplot2")

##### PRIKLAD 1 #####

# Import datoveho souboru Firmy.csv:
#File -> Import Datasets -> From text (base) -> vybrat soubor.csv (!!! nutno mit kratkou cestu k datum)
getwd()
Firmy <- read.csv("./MMDA/03/Firmy.csv", sep = ";", dec = ".")
Cars <- read.csv("./MMDA/03/Cars.csv", sep = ";", dec = ",")
# XY bodovy diagram
plot(Firmy$Investice, Firmy$Produkce, xlab = "Investice", ylab = "Produkce")

# regresni funkce - primka y=b0+b1*x
#pokud parabola -> x²
reg1 <- lm(Produkce ~ Investice, data = Firmy) #pokud vice dat da se tam +
summary(reg1)   # vystup
print(reg1)     # parametry

# analyza rezidui
library(MASS)
summary(reg1$residuals) # miry urovne rezidui
plot(reg1$residuals)  # graf rezidui
hist(reg1$residuals)  # histogram rezidui -> mel by vypadat jako gausovka
shapiro.test(reg1$residuals)  # test normality rezidui -> nezamitame hypotezu
print(studres(reg1))  # Jacknife rezidua

# odhad konkretni hodnoty
new1 <- data.frame(Investice = 18)
new1
predict(reg1, newdata = new1) # bodovy odhad
#intervalovy odhad
predict(reg1, newdata = new1, interval = "confidence", level = 0.95) # IS pro podminenou stredni hodnotu
predict(reg1, newdata = new1, interval = "prediction", level = 0.95) # IS pro konkretni hodnotu

# graf predpovedi
library("ggplot2")
pred.int <- predict(reg1, interval = "prediction")
mydata <- cbind(Firmy, pred.int)
ggplot(mydata, aes(Investice, Produkce)) +
  geom_point() +
  stat_smooth(method = lm) +
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed")


##### PRIKLAD 2 #####
cor(Firmy$Investice, Firmy$Produkce) # korelacni koeficient
cor.test(Firmy$Investice, Firmy$Produkce) # test vyznamnosti korelacniho koeficientu


##### PRIKLAD 3 #####
# XY bodovy diagram
plot(Cons$Speed, Cons$Consumption, xlab = "Speed", ylab = "Consumption")

# regresni funkce - primka y=b0+b1*x
reg2 <- lm(Consumption ~ Speed, data = Cons)
summary(reg2)

# analyza rezidui
summary(reg2$residuals)
hist(reg2$residuals)
plot(reg2$residuals)
shapiro.test(reg2$residuals)

# regresni funkce - parabola  y=b0+b1*x+b2*x^2
reg3 <- lm(Consumption ~ Speed + I(Speed^2), data = Cons)
print(reg3)
summary(reg3)

# analyza rezidui
summary(reg3$residuals)
hist(reg3$residuals)
plot(reg3$residuals)
shapiro.test(reg3$residuals)

# odhad konkretni hodnoty
auto <- data.frame(Speed = 115)
predict(reg3, newdata = auto, interval = "confidence", level = 0.95)
predict(reg3, newdata = auto, interval = "prediction", level = 0.95)


##### PRIKLAD 4 #####

# Import datasets: Beer.csv

# korelacni matice
library(corrplot)
cor_beer <- cor(Beer)
print(cor_beer)
corrplot(cor_beer, "number")

# XY bodovy diagram
x <- Beer$PRICE.18PK
y <- Beer$CASES.18PK
plot(x, y, xlab = "Cena", ylab = "Prodane_kusy")

# regresni funkce 
reg4_zk <- lm(y ~ x, data = Beer) # nejdrive pro ukazku regresni primka
summary(reg4_zk)
plot(reg4_zk$residuals)
shapiro.test(reg4_zk$residuals)

reg4 <- lm(log(y) ~ log(x), data = Beer) # mocninna fce y=b0*x^b1 -> nutna logaritmizace pro ziskani modelu linearniho v parametrech
print(reg4)
summary(reg4)
summary(reg4$residuals)
plot(reg4$residuals)
hist(reg4$residuals)
shapiro.test(reg4$residuals)

# odhad konkretni hodnoty
new4 <- data.frame(x = 15)
predict(reg4, newdata = new4, interval = "confidence", level = 0.95)
pred.i <- predict(reg4, newdata = new4, interval = "confidence", level = 0.95)
pred.i2 <- predict(reg4, newdata = new4, interval = "prediction", level = 0.95)

# odlogaritmovani vysledne y
exp(pred.i)
exp(pred.i2)


##### PRIKLAD 6 #####
# Import datasets: Cars.csv

summary(Cars)  # prehled popisna statistika
colSums(is.na(Cars)) # prehled chybejicich pozorovani
str(Cars)  # vypis typu promennych
Cars_data <- Cars[1:428, 7:15] # vylouceni kvalitativnich promennych

# vicerozmerny regresni model Y=b0+b1X1+b2X2+b3X3
reg6 <- lm(Cena_prodej ~ Motor + Horsepower + Dalnice_MPG, data = Cars_data)
summary(reg6)
reg6$coefficients

# analyza rezidui  
summary(reg6$residuals)
plot(reg6$residuals)
plot(studres(reg6))
hist(reg6$residuals)
shapiro.test(reg6$residuals)

#detekce vlivnych pozorovani
# 1. Mahalanobisova vzdalenost
Cars_data2 <- Cars_data[, -c(4)] # vylouceni promenne Valce, nebot neni kvantitativni spojita
oneC <- as.matrix(rep(1, dim(Cars_data2)[1])) # jednotkov? vektor 
nC <- dim(Cars_data2)[1] # rozsah v?b?ru
xbarC <- 1 / nC * t(Cars_data2) %*% oneC # vektor prumeru
print(xbarC)
cov_xC <- cov(Cars_data2)
mahal <- mahalanobis(Cars_data2, xbarC, cov_xC, inverted = FALSE)
plot(density(mahal, bw = 0.3), main = "Squared Mahalanobis distancec"); rug(mahal)
which(mahal > 12)
write.table(mahal, "c:/R/Data/mahal6.txt", sep = "\t", dec = ",")

# 2. Jacknife rezidua
jack6 <- studres(reg6)
ggplot(reg6, aes(x = .fitted, y = jack6)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red")

which(jack6 > 3 | jack6 < (-3))

# vylouceni odlehlych pozorovani
Cars_data3 <- Cars_data[-c(7, 220, 252, 262, 269, 270, 335),]
reg6_1 <- lm(Cena_prodej ~ Motor + Horsepower + Dalnice_MPG, data = Cars_data3)
summary(reg6_1)
plot(studres(reg6_1))
hist(reg6_1$residuals)
shapiro.test(reg6_1$residuals)

# srovnani s prvnim modelem se vsemi pozorovanimi
summary(reg6_1)
summary(reg6)

# predikce
new6 <- data.frame(Motor = 4, Horsepower = 250, Dalnice_MPG = 30)
predict(reg6_1, newdata = new6, interval = "confidence", level = 0.95)
predict(reg6, newdata = new6, interval = "confidence", level = 0.95) # srovnani s predikci podle puvodniho modelu se vsemi pozorovanimi

# ukazka - vicenasobny regresni model se vsemi promennymi
reg6_2 <- lm(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka, data = Cars_data)
summary(reg6_2)
reg6_3 <- lm(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Dalnice_MPG +
  Vaha +
  Delka, data = Cars_data)
summary(reg6_3)

# korelacni matice z vysvetlujicich promennych
X6_3 = model.matrix(reg6_3) # matice vysvetlujicich promennych
View(X6_3)
X6_3a <- X6_3[, 2:7] # bez b0
cor(X6_3a)
corrplot(cor(X6_3a), "number")

reg6_4 <- lm(Cena_prodej ~ Motor + Horsepower + Dalnice_MPG + Delka, data = Cars_data) # vyloucena promenna Valce, nebot je hodne korelovana
summary(reg6_4)
X6_4a <- X6_3[, -c(1, 3, 6)]
cor(X6_4a)


##### PRIKLAD 7 #####
# projekcni matice H
X7 <- model.matrix(reg6_1) # definice matice vysvetlujicich promennych
hat_matrix7 <- X7 %*% (solve(t(X7) %*% X7) %*% t(X7)) # Vypocet matice H
print(hat_matrix7)
write.table(hat_matrix7, "c:/R/Data/H7.txt", sep = "\t", dec = ",")
d7 <- diag(hat_matrix7) # leverages = diagonalni prvky matice H
max(d7) # maximalni hodnota
print(d7)
write.table(d7, "c:/R/Data/d7.txt", sep = "\t", dec = ",")

# leverages - jinym zpusobem
lev7 <- lm.influence(reg6_1)$hat
p <- sum(lev7) # soucet leverages = stopa matice = pocet parametru
p
plot(lev7)
max(lev7)
print(lev7)
h <- 3 * p / (nC)
h
which(lev7 > h) # identifikace vlivnych pozorovani


#poznamky
# multikoranelita - 3 a vice zavislych promennych na sobe

#--------------------
# Ukol3
Cars <- read.csv("Cars.csv", sep = ";", dec = ",")
Cars_data <- Cars[1:428, 7:15, , -4] # vylouceni kvalitativnich promennych
Cars_data2 <- Cars_data[, -4] # vylouceni promenne Valce, nebot neni kvantitativni spojita

?cor
cor(Cars_data2$Cena_prodej, Cars_data2$Horsepower) #jsou visoce zavisle primo umerne
# cor.test(Cars$Cena_prodej, Cars$Horsepower) #jsou zavisle
plot(Cars_data2$Cena_prodej, Cars_data2$Horsepower, xlab = "Cena_prodej", ylab = "Horsepower")


# 1. Mahalanobisova vzdalenost
oneC <- as.matrix(rep(1, dim(Cars_data2)[1]))
nC <- dim(Cars_data2)[1]
xbarC <- 1 / nC * t(Cars_data2) %*% oneC
print(xbarC)
cov_xC <- cov(Cars_data2)
mahal <- mahalanobis(Cars_data2, xbarC, cov_xC, inverted = FALSE)
plot(density(mahal, bw = 0.3), main = "Squared Mahalanobis distancec"); rug(mahal)
which(mahal > 12)


# 2. Jacknife rezidua
reg <- lm(Cena_prodej ~ Horsepower, data = Cars_data2)
jack <- studres(reg)
ggplot(reg, aes(x = .fitted, y = jack)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red")
which(jack > 3 | jack < (-3))

# vylouceni extremnich pozorovani
intersect(which(jack > 3 | jack < (-3)), which(mahal > 12))

Cars_data3 <- Cars_data2[-intersect(which(jack > 3 | jack < (-3)), which(mahal > 12)),] # vylouceni promenne Valce, nebot neni kvantitativni spojita

plot(Cars_data3$Cena_prodej, Cars_data3$Horsepower, xlab = "Cena_prodej", ylab = "Horsepower")

reg2 <- lm(Cena_prodej ~ Horsepower, data = Cars_data3)


# viz.priklad 3
#rezidua nemusi vychazet v shapiro-wiklove testu -> pokud vypada dobre histogram
# regresni funkce - primka y=b0+b1*x
summary(reg2)

# analyza rezidui
summary(reg2$residuals)
# checkni r square
hist(reg2$residuals)
plot(reg2$residuals)
shapiro.test(reg2$residuals)

# regresni funkce - parabola  y=b0+b1*x+b2*x^2
reg3 <- lm(Cena_prodej ~ Horsepower + I(Horsepower^2), data = Cars_data3)

print(reg3)
summary(reg3)
# analyza rezidui
summary(reg3$residuals)
hist(reg3$residuals)
plot(reg3$residuals)
shapiro.test(reg3$residuals)
?shapiro.test
reg4 <- lm(Cena_prodej ~ log(Horsepower), data = Cars_data3)
print(reg4)
summary(reg4)
# analyza rezidui
summary(reg4$residuals)
hist(reg4$residuals)
plot(reg4$residuals)
shapiro.test(reg4$residuals)

reg5 <- lm(log(Cena_prodej) ~ log(Horsepower), data = Cars_data3)
print(reg5)
summary(reg5)
# analyza rezidui
summary(reg5$residuals)
hist(reg5$residuals)
plot(reg5$residuals)
shapiro.test(reg5$residuals)

jack2 <- studres(reg5)
ggplot(reg5, aes(x = .fitted, y = jack2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red")
which(jack2 > 3 | jack2 < (-3))

Cars_data4 <- Cars_data3[-c(150, 151),]
reg6 <- lm(log(Cena_prodej) ~ log(Horsepower), data = Cars_data4)
print(reg6)
summary(reg6)
# analyza rezidui
summary(reg6$residuals)
hist(reg6$residuals, probability = T)
lines(density(reg6$residuals), col = "red")

plot(reg6$residuals)
shapiro.test(reg6$residuals)
# odhad konkretni hodnoty
auto <- data.frame(Horsepower = 400)
predict_price <- predict(reg6, newdata = auto, interval = "confidence", level = 0.95)
exp(predict_price)

# predict(reg2, newdata = auto, interval = "prediction", level = 0.95)