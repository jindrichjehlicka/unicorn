####################################################### CVICENI 4 ############################################################

# Instalace balicku
install.packages("matlib")
install.packages("mctest")
install.packages("Hmisc")
install.packages("lmridge")
install.packages("MLmetrics")
install.packages("olsrr")
install.packages("lmtest")
install.packages("MASS")


##### PRIKLAD 1 #####
# Import datasets: usacities.csv
# prvotni regresni model
usacities <- read.csv("./04/usacities.csv", sep = ";", dec = ".")
reg4_1 <- lm(Mortality ~ Rainfall +
  Education +
  Popden +
  Nonwhite +
  NOX +
  SO2, data = usacities)
summary(reg4_1)

# analyza rezidui
plot(reg4_1$residuals)
hist(reg4_1$residuals)
shapiro.test(reg4_1$residuals)
library(MASS)
jack4_1 <- studres(reg4_1)
library(ggplot2)
ggplot(reg4_1, aes(x = .fitted, y = jack4_1)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red")
which(jack4_1 > 3 | jack4_1 < (-3))

# projekcni matice H + leverages
lev4_1 <- lm.influence(reg4_1)$hat
p4_1 <- sum(lev4_1) # soucet leverages = pocet parametru
which(lev4_1 > 2 * p4_1 / 60) # identifikace vlivnych pozorovani

# vypusteni vlivnych pozorovani
usa2 <- usacities[-c(12, 29, 32, 40, 59),]
reg4_1a <- lm(Mortality ~ Rainfall +
  Education +
  Popden +
  Nonwhite +
  NOX +
  SO2, data = usa2)
summary(reg4_1a)

# korelacni matice X  - multikolinearita + VIF faktory
library(Hmisc)
X4_1 = model.matrix(reg4_1a)    # matice vysvetlujicich promennych (vcetne konstanty)
X4_1b <- X4_1[, 2:7]    # matice vysvetlujicich promennych (bez konstanty)
R4_1 <- cor(X4_1b) # korelacni matice
print(R4_1)
library(corrplot)
corrplot(R4_1, "number")

# detekce multikolinearity
# 1. moznost - determinant matice R
det(R4_1)

# 2. moznost - VIF faktory
library(matlib)
R4_1I <- inv(R4_1) # inverzni matice R-1
print(R4_1I)
VIF4_1 <- diag(R4_1I)
VIF4_1 > 5 # kdyz neni multikolinearita -> u vsech promennych musi byt FALSE
which(VIF4_1 > 5) # kdyz neni multikolinearita -> musi vyjit prazdna mnozina

# 3. moznost - testy multikolinearity
library(mctest)
omcdiag(reg4_1a) # optimalni vysledek same nuly u vsech testu 


##### PRIKLAD 2 #####
# Import datasets: Cars.csv
Cars_data <- Cars[1:428, 7:15] # vylouceni kvalitativnich promennych
Cars_data2 <- Cars_data[-c(335),] # odstraneni extremniho pozorovani - uz o nem vime z minula

# prvotni regresni model
reg4_2 <- lm(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka,
             data = Cars_data2)
summary(reg4_2)

# korelacni matice X  - multikolinearita + VIF faktory
X4_2 = model.matrix(reg4_2)    # matice vysvetlujicich promennych (vcetne konstanty)
X4_2b <- X4_2[, 2:8]    # matice vysvetlujicich promennych (bez konstanty)
R4_2 <- cor(X4_2b) # korelacni matice
print(R4_2)
det(R4_2) # determinant matice R
R4_2I <- inv(R4_2) # inverzni matice R-1
VIF4_2 <- diag(R4_2I) # VIF faktory
VIF4_2 > 5
which(VIF4_2 > 5) # tyto promenne jsou korelovane 

# druha moznost korelacni analyzy
rcorr(as.matrix(Cars_data2))
R4_2b <- rcorr(as.matrix(X4_2b))
print(R4_2b$r) #korelacni koeficienty

# testy multikolinearity
omcdiag(reg4_2)

# (1) Hrebenova regrese

# hrebenova regrese obecne pri neznamem K
library(lmridge)
reg4_2a <- lmridge(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka,
                   data = Cars_data2, K = seq(0, 0.05, 0.001))
summary(reg4_2a)
coef(reg4_2a)
plot(reg4_2a, type = "ridge")
plot(reg4_2a, type = "vif")

# hrebenova regrese obecne pri znamem nejlepsim K
reg4_2b <- lmridge(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka,
                   data = Cars_data2, K = 0.004)
summary(reg4_2b)

# rozdeleni dat na testovaci a predikcni mnozinu
Cars_data_test <- Cars_data2[1:400,]
Cars_data_pred <- Cars_data2[401:427,]

# hledani K
reg4_2_obyc <- lm(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka,
                  data = Cars_data_test)
reg4_2_hreb_obec <- lmridge(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka,
                            data = Cars_data_test, K = seq(0, 0.05, 0.001)) # 0.05 nizsi kvuli vetsi rychlosti vypoctu, obecne by melo byt vyssi cislo
plot(reg4_2_hreb_obec, type = "ridge")
reg4_2_hreb <- lmridge(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka,
                       data = Cars_data_test, K = 0.004)
summary(reg4_2_obyc)
summary(reg4_2_hreb) # pouze pro zvoleny model s k

# test predikcniho modelu
reg4_2_pred <- lm(Cena_prodej ~ Motor +
  Valce +
  Horsepower +
  Mesto_MPG +
  Dalnice_MPG +
  Vaha +
  Delka,
                  data = Cars_data_pred)
X4_2p = model.matrix(reg4_2_pred)
X4_2pred <- (X4_2p[, 2:8])
new4_2 <- as.data.frame(X4_2pred) # dalsi fce predict neumi pocitat s matici, proto nutno prevest na data.frame
predikce_lm <- predict(reg4_2_obyc, newdata = new4_2, se.fit = TRUE, interval = "predict", level = 0.95)
predikce_lmridge <- predict(reg4_2_hreb, newdata = new4_2, se.fit = TRUE, interval = "predict", level = 0.95)
predikce_lm$fit[, 1] # fit = pouze bodovy odhad
predikce_lmridge
Cars_data_pred$Cena_prodej

# odhad stredni ctvercove chyby MSE
library(MLmetrics)
MSE(predikce_lm$fit[, 1], Cars_data_pred$Cena_prodej)  # MSE pro klasicky linearmi model
MSE(predikce_lmridge, Cars_data_pred$Cena_prodej) # MSE pro hrebenovou regresi

# (2) vylouceni korelovanych vysvetlujicich promennych 
# odzkouseno pro kazdou kombinaci promennych, tato jedina vyjde stat.vyznamna
reg4_2a_obyc <- lm(Cena_prodej ~ Horsepower + Dalnice_MPG, data = Cars_data_test)
summary(reg4_2a_obyc)

# test predikcniho modelu
reg4_2a_pred <- lm(Cena_prodej ~ Horsepower + Dalnice_MPG, data = Cars_data_pred)
X4_2ap = model.matrix(reg4_2a_pred)
X4_2a_pred <- (X4_2ap[, 2:3])
new4_2a <- as.data.frame(X4_2a_pred) # dalsi fce predict neumi pocitat s matici, proto nutno prevest na data.frame
predikce_lm_a <- predict(reg4_2a_obyc, newdata = new4_2a, se.fit = TRUE, interval = "predict", level = 0.95)

# srovnani predikci
Cars_data_pred$Cena_prodej #  puvodni promenna Y pozorovane hodnoty
predikce_lm$fit[, 1]  # puvodni obyc model pro vsechna X
predikce_lmridge  # ridge model pro vsechna X
predikce_lm_a$fit[, 1] # uzky obyc model pro X = horsepower, dalnice_mpg

# stredni ctvercove chyby MSE
# (a) teoreticky by to mel byt nejhorsi model, puvodni s korelovanymi promennymi
MSE(predikce_lm$fit[, 1], Cars_data_pred$Cena_prodej)

# (b) hrebenova regrese se vsemi korelovanymi promennymi
MSE(predikce_lmridge, Cars_data_pred$Cena_prodej)

# (c) model pouze se dvema vysvetlujicimi promennymi - jiz bez multikolinearity
MSE(predikce_lm_a$fit[, 1], Cars_data_pred$Cena_prodej)

# data do Excelu pro srovnavaci graf predikce lm a lmridge
write.table(predikce_lmridge, "c:/R/Data/lm/predikce_hreb.txt", sep = "\t", dec = ",")
write.table(predikce_lm$fit[, 1], "c:/R/Data/lm/predikce_puv_mult.txt", sep = "\t", dec = ",")
write.table(predikce_lm_a$fit[, 1], "c:/R/Data/lm/predikce_2X.txt", sep = "\t", dec = ",")
write.table(Cars_data_pred$Cena_prodej, "c:/R/Data/lm/Cena_prodej.txt", sep = "\t", dec = ",")


##### PRIKLAD 3 #####
library(olsrr)

# pro test pouzit vysledny model z Prikladu 1: reg4_1a (viz radek 39)
# testy heteroskedasticity
ols_test_breusch_pagan(reg4_1a) # H0 nema byt zamitnuta
ols_test_f(reg4_1a)  # H0 nema byt zamitnuta
ols_test_score(reg4_1a) # H0 nema byt zamitnuta


##### PRIKLAD 4 #####
library(olsrr)

# vybran model bez korelovanych vysvetlujicich promennych
reg4_4 <- lm(Cena_prodej ~ Horsepower + Dalnice_MPG,
             data = Cars_data2)

# testy heteroskedasticity
ols_test_breusch_pagan(reg4_4) # H0 nema byt zamitnuta
ols_test_f(reg4_4)  # H0 nema byt zamitnuta
ols_test_score(reg4_4)  # H0 nema byt zamitnuta

# vazena MNC pro heteroskedasticitu (odhady rozptylu nahodne slozky pres zavislost rezidui na vyrovnanych hodnotach) - vaha se vzdy zadava od modelu jako odhad W = 1/odhad rozptylu
# postupne vytvoreni vah 
vahovy.model <- lm(abs(reg4_4$residuals) ~ reg4_4$fitted.values) #regresni model zavislost absolutni hodnoty rezidui na vyrovnanych hodnotach
vaha <- 1 / (vahovy.model$fitted.values^2)
vaha

# to same lze udelat v jednom kroku
weights4_4 <- 1 / lm(abs(reg4_4$residuals) ~ reg4_4$fitted.values)$fitted.values^2
weights4_4

# regresni model - vazena MNC
reg4_4w <- lm(Cena_prodej ~ Horsepower + Dalnice_MPG,
              data = Cars_data2, weights = weights4_4) #nebo vaha (weights a vaha vyjdou stejne)
summary(reg4_4w) # vazena MNC
summary(reg4_4)  # puvodni heteroskedasticky model

# grafy rezidui
plot(reg4_4$residuals)
plot(reg4_4w$residuals)

#graficke srovnani - ale pouze jednorozmerne - Horsepower
library(ggplot2)
ggplot(data = Cars_data2, aes(y = Cena_prodej, x = Horsepower)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE,
              color = "black",
              size = 0.5,
              linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE,
              aes(weight = vaha),
              color = "red",
              size = 0.5,
              linetype = "dashed") +
  labs(title = "Scatterplot of Cena_prodej ~ Horsepower")

