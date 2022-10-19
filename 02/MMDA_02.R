
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
stat.desc(MMDA_02_data) # tabulka popisne statistiky

mean(MMDA_02_data$Rainfall) #prumer jedne promenne

# Vektor vyberovych strednich hodnot
x <- as.matrix(MMDA_02_data[,2:8]) # definovani matice z datove sady
one <- as.matrix(rep(1, dim(x)[1])) # jednotkovy vektor 
n <- dim(x)[1] # rozsah vyberu
xbar <- 1/n*t(x)%*%one # vektor prumeru
print(xbar) # zobrazeni vektoru prumeru

# Kovariancni matice
cov_x <- cov(x)
print(cov_x)

# Korelacni matice
cor_x <- cor(x)
print(cor_x)


## PRIKLAD 3

# graficka analyza promennych
plot(MMDA_02_data$Education)
plot(MMDA_02_data$Popden)
plot(MMDA_02_data$NOX)

# normovani promennych
library(robustHD)
standardize(x,centerFun = mean, scaleFun = sd)

# Mahalanobisova vzdalenost
require(graphics)
vzdalenost_mh <- mahalanobis(x, xbar, cov_x, inverted = FALSE)
print(vzdalenost_mh) 
plot(density(vzdalenost_mh, bw = 0.3), main="Squared Mahalanobis distances, n=428, p=9"); rug(vzdalenost_mh)

# ulozeni do csv

write.table(vzdalenost_mh, "c:/R/Data/vzdalenost_mh.txt", sep="\t",dec = ",")


## PRIKLAD 4

which(vzdalenost_mh>12) 


## PRIKLAD 5

# Testy jednorozmerne normality

# 1.graficke nastroje

# Q-Q graf pro normalitu      
#install.packages("ggpubr")    
#library(ggpubr)
#library(ggplot2)
qqnorm(MMDA_02_data$Mortality, pch = 1, frame = TRUE)
qqline(MMDA_02_data$Mortality, col = "steelblue", lwd = 2)

# Hustota pravdepodobnosti
hist(MMDA_02_data$Mortality,probability=TRUE)
lines(density(MMDA_02_data$Mortality),col="red")

# 2. statisticke nastroje

# Shapiro-Wilkuv test
shapiro.test(MMDA_02_data$Mortality)

# Kolmogorovuv-Smirnovuv test
ks.test(MMDA_02_data$Mortality, "pnorm", mean=mean(MMDA_02_data$Mortality), sd=sd(MMDA_02_data$Mortality))


# Testy vicerozmerne normality
#install.packages("mvnormtest")
#install.packages("MVN")
library(mvnormtest)
library(MVN)

# Shapiro-Wilkuv test
mshapiro.test(t(x))

# Q-Q graf chisq-mahalanobis
qqplot(qchisq(ppoints(60), df = 6), vzdalenost_mh)
abline(0, 1, col = 'gray')




