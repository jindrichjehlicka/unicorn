################################################## CVICENI 2 ############################################################

# Instalace balicku na dnesni cviceni

install.packages("pastecs")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("robustHD")
install.packages("MVN")
install.packages("mvnormtest")


## PRIKLAD 1

# Import dat: File -> Import Datasets -> From text (base) -> vybrat soubor.csv (!!! nutno mit kratkou cestu k datum)

# Deskriptivni analyza
#install.packages("pastecs")
library(pastecs)

MMDA_02_data <- read.csv("MMDA_02_data.csv", sep = ";", dec = ".")
MMDA_02_data
stat.desc(MMDA_02_data) # tabulka popisne statistiky

mean(MMDA_02_data$Rainfall) #prumer jedne promenne

# Vektor vyberovych strednich hodnot
x <- as.matrix(MMDA_02_data[, 2:8]) # definovani matice z datove sady
x
y <- as.matrix(MMDA_02_data[, 6:7])
y2 <- as.matrix(MMDA_02_data[c("Nonwhite", "NOX")])
y2
nox <- as.matrix(MMDA_02_data[, 6])
nox
nonwhite <- as.matrix(MMDA_02_data[, 7])
nonwhite


one <- as.matrix(rep(1, dim(x)[1])) # jednotkovy vektor 
one
oney <- as.matrix(rep(1, dim(y)[1]))
oney
n <- dim(x)[1] # rozsah vyberu
n
ny <- dim(y)[1]
ny
xbar <- 1 / n * t(x) %*% one # vektor prumeru
print(xbar) # zobrazeni vektoru prumeru
ybar <- 1 / ny * t(y) %*% oney
ybar


# Kovariancni matice
cov_x <- cov(x)
print(cov_x)
cov_y <- cov(y)
print(cov_y)
# Korelacni matice
cor_x <- cor(x)
print(cor_x)


## PRIKLAD 3

# graficka analyza promennych
plot(MMDA_02_data$Education)
plot(MMDA_02_data$Popden)
plot(MMDA_02_data$NOX)
plot(nox)
plot(nonwhite)
# normovani promennych
library(robustHD)
standardize(x, centerFun = mean, scaleFun = sd)
#find more than 1.960
# Mahalanobisova vzdalenost
require(graphics)
vzdalenost_mh <- mahalanobis(x, xbar, cov_x, inverted = FALSE)
vzdalenost_2 <- mahalanobis(y, ybar, cov_y, inverted = FALSE)
print(vzdalenost_2)
#vetsi nez cca 12

plot(density(vzdalenost_mh, bw = 0.3), main = "Squared Mahalanobis distances"); rug(vzdalenost_mh)
plot(density(vzdalenost_2, bw = 0.3), main = "Squared Mahalanobis distances"); rug(vzdalenost_2)
# ulozeni do csv

write.table(vzdalenost_mh, "/Users/jindrichjehlicka/unicorn/vzdalenost_mh.txt", sep = "\t", dec = ",")


## PRIKLAD 4

which(vzdalenost_mh > 12)


## PRIKLAD 5

# Testy jednorozmerne normality

# 1.graficke nastroje

# Q-Q graf pro normalitu      
#install.packages("ggpubr")    
#library(ggpubr)
#library(ggplot2)
qqnorm(MMDA_02_data$Mortality, pch = 1, frame = TRUE) #
qqline(MMDA_02_data$Mortality, col = "steelblue", lwd = 2) #

qqnorm(MMDA_02_data$Rainfall, pch = 1, frame = TRUE) #
qqline(MMDA_02_data$Rainfall, col = "steelblue", lwd = 2) #

qqnorm(MMDA_02_data$Education, pch = 1, frame = TRUE) #
qqline(MMDA_02_data$Education, col = "steelblue", lwd = 2) #

# Hustota pravdepodobnosti
hist(MMDA_02_data$Mortality, probability = TRUE)
lines(density(MMDA_02_data$Mortality), col = "red")


hist(MMDA_02_data$Rainfall, probability = TRUE)
lines(density(MMDA_02_data$Rainfall), col = "red")

hist(MMDA_02_data$Mortality, probability = TRUE)
lines(density(MMDA_02_data$Mortality), col = "red")

# 2. statisticke nastroje

# Shapiro-Wilkuv test
shapiro.test(MMDA_02_data$Mortality)
#p-value -> <0,1> -> kdyz je p hodnota vyssi nez alfa(0.01,0.05,0.1), nezamitame hypotezu; pokud nizka tak zamitame 

# Kolmogorovuv-Smirnovuv test
ks.test(MMDA_02_data$Mortality, "pnorm", mean = mean(MMDA_02_data$Mortality), sd = sd(MMDA_02_data$Mortality))


# Testy vicerozmerne normality
#install.packages("mvnormtest")
install.packages("MVN")
library(mvnormtest)
library(MVN)

# Shapiro-Wilkuv test
#x je matice vsech promennych
#p value porovnavame s alfou Ⲁ
mshapiro.test(t(x))

# Q-Q graf chisq-mahalanobis
qqplot(qchisq(ppoints(60), df = 6), vzdalenost_mh)
abline(0, 1, col = 'gray')




