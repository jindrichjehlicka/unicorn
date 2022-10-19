#4. Pøíklad
#Pracovníci dopravního podniku pøedpokládají, že prùmìrná 
#rychlost autobusové linky è. 135 MHD v centru mìsta v dopravní
#špièce je 8 km/hod. Bylo provedeno 31 mìøení a byla 
#zaznamenána prùmìrná rychlost 7,6 km/hod a smìrodatná 
#odchylka 1,2. Testujte na 5% hladinì významnosti hypotézu, 
#že autobus jezdí menší rychlostí, než s jakou pøedpokládají 
#pracovníci DP.

#H0: E(X) = 8
#H1: E(X) < 8

Mu0 <- 8
Prumer <- 7.6
S <- 1.2
n <- 31

U <- (Prumer-Mu0)/S*sqrt(n) #U = -1.856

#W(alfa) = {u; u <= - u(1-alfa)}
#alfa = 0.05 -> u(0.95)
qnorm(0.95) #1.645
#W(0.05) = {u; u<= - 1.645}

#Závìr OBECNÌ:
#hodnota testového kritéria patøí do kritického oboru 
#-> zamítám H0 ve prospìch H1 na alfa% hladinì významnosti.

#hodnota testového kritéria NEpatøí do kritického oboru 
#-> NEzamítám H0 ve prospìch H1 na alfa% hladinì významnosti.

#Zde: hodnota testového kritéria (-1.856) patøí do kritického
#oboru {u; u<= - 1.645} -> Zamítám H0 (E(X)=8) ve prospìch
#H1 (E(X)<8) na 5% hladinì významnosti.

#6. Pøíklad
#Na základì údajù o výšce studentù otestujte, že støední výška 
#studentù VŠE ochotných odpovídat na anketu je:
#„vìtší než 170 cm“, nebo
#„menší než 175 cm“
# a to na 5% a 1% hladinì významnosti. 
#Pøedpokládejte nejprve, že z pøedchozích mìøení víme, 
#že smìrodatná odchylka je 8 cm, poté pøedpokládejte, 
#že ji neznáme a použijte odhadnutou. 
#(Celkem tedy 2x2x2 = 8 rùzných zadání)

#(pøedpokládáme normalitu)
#H0: Mu = 170
#H1: Mu > 170
data <- read.csv("data_studenti.csv", sep = ";")

Sigma <- 8
Prumer <- mean(data$Výška)
n <- length(data$Výška)
Mu0 <- 170

U <- (Prumer-Mu0)/Sigma*sqrt(n) #U = 4.466

#W(alfa) = {u; u => u(1-alfa)}
#alfa1 = 0.05 a alfa2 = 0.01
qnorm(0.95) #1.645
qnorm(0.99) #2.326
#W(0.05) = {u; u => 1.645}
#W(0.01) = {u; u => 2.326}

#Hodnota testového kritéria patøí do 5% i do 1% kritického oboru 
#Zamítám H0 (Mu = 170) ve prospìch H1 (Mu > 170) 
#na 5% i na 1% hladinì významnosti.

#H0: Mu = 170
#H1: Mu > 170
S <- sd(data$Výška) #11.84
Prumer <- mean(data$Výška)
n <- length(data$Výška)
Mu0 <- 170

T <- (Prumer-Mu0)/S*sqrt(n) #T = 3.017

#W(alfa) = {t; u => t(1-alfa)}
#alfa1 = 0.05 a alfa2 = 0.01
qt(0.95, n-1) #1.681
qt(0.99, n-1) #2.416
#W(0.05) = {t; t => 1.681}
#W(0.01) = {t; t => 2.416}

#Hodnota testového kritéria patøí do 5% i do 1% kritického oboru 
#Zamítám H0 (Mu = 170) ve prospìch H1 (Mu > 170) 
#na 5% i na 1% hladinì významnosti.

#H0: Mu = 175
#H1: Mu < 175

Sigma <- 8
Prumer <- mean(data$Výška)
n <- length(data$Výška)
Mu0 <- 175

U <- (Prumer-Mu0)/Sigma*sqrt(n) #U = 0.320

#W(alfa) = {u; u <= - u(1-alfa)}
#alfa1 = 0.05 a alfa2 = 0.01
qnorm(0.95) #1.645
qnorm(0.99) #2.326
#W(0.05) = {u; u <= - 1.645}
#W(0.01) = {u; u <= - 2.326}

#Hodnota testového kritéria NEpatøí do kritického oboru
#NEzamítám H0 (Mu = 175) ve prospìch H1 (Mu < 175) 
#na 5% ani na 1% hladinì významnosti.

#H0: Mu = 175
#H1: Mu < 175

S <- sd(data$Výška) #11.84
Prumer <- mean(data$Výška)
n <- length(data$Výška)
Mu0 <- 175

T <- (Prumer-Mu0)/S*sqrt(n)  #T = 0.216

#W(alfa) = {t; t <= - t(1-alfa)}
#alfa1 = 0.05 a alfa2 = 0.01
qt(0.95, n-1) #1.681
qt(0.99, n-1) #2.416
#W(0.05) = {t; t <= - 1.681}
#W(0.01) = {t; t <= - 2.416}

#Hodnota testového kritéria nepatøí do kritického oboru
#Nezamítám H0 (Mu = 175) ve prospìch H1(Mu < 175)
#na 5% ani na 1% hladinì významnosti.

#7. Pøíklad
#Na základì vyplnìných dotazníkù otestujte hypotézu, že 
#ženy tvoøí 50 % studentù ochotných zodpovìdìt dotazník 
#proti alternativní hypotéze, že tvoøí ménì než 50 % 
#studentù ochotných zodpovìdìt dotazník.

#H0: Pi = 0.5
#H1: Pi < 0.5

m <- sum(data$Pohlaví=="Žena") #16
n <- length(data$Pohlaví) #44 
p <- m/n  #0.364
Pi0 <- 0.5

U <- (p-Pi0)/sqrt(Pi0*(1-Pi0)/n) #U = -1.809

#Alfa = 0.05
#W(alfa) = {u; u <= - u(1-alfa)}
qnorm(0.95) #1.645
#W(0.05) = {u; u <= - 1.645}

#Hodnota testového kritéria patøí do kritického oboru
#Zamítáme H0 (Pi = 0.5) ve prospìch H1 (Pi < 0.5) 
#na 5% hladinì významnosti.

#9. Pøíklad
#Následují údaje o deseti náhodnì vybraných studentech z 
#mých loòských cvièení a bodové hodnoty 1. a 2. testu. 
#Pøedpokládejte normální rozdìlení rozdílù. Získali studenti 
#stejné množství bodù v prvním i ve druhém testu nebo 
#získali více bodù v prvním testu? Hladina významnosti je 5 %.

#H0: Mu1 = Mu2
#H1: Mu1 > Mu2

x1 <- c(15, 19, 15, 17, 14, 20, 14, 13, 20, 20)
x2 <- c( 6, 19, 11, 15, 20, 20, 18, 14, 19, 13)

d <- x1 - x2

prumer <- mean(d) #1.2
S <- sd(d) #4.59
n <- length(d) #10

T <- sqrt(n)*prumer/S #T = 0.827

#W(alfa) = {t; t => t(1-alfa)}; alfa = 0.05
qt(0.95, n-1) #1.833
#W(0.05) = {t; t => 1.833}

#Hodnota testového kritéria NEpatøí do kritického oboru
#NEzamítám H0 (Mu1 = Mu2) ve prospìch H1 (Mu1>Mu2)
#na 5% hladinì významnosti.

#10. Pøíklad
#Rozdìlte soubor s vyplnìnými dotazníky podle pohlaví a 
#otestujte hypotézu, že støední výška studentek je stejná 
#jako støední výška studentù proti hypotéze, že je nižší.

data <- read.csv("data_studenti.csv", sep = ";")
muzi <- data$Výška[data$Pohlaví == "Muž"]
zeny <-data$Výška[data$Pohlaví == "Žena"]

#H0: E(X1) = E(X2)
#H1: E(X1) > E(X2)

prm <- mean(muzi)   #182.3
prz <- mean(zeny)   #163.3
rozm <- var(muzi)   #55.5
rozz <- var(zeny)   #58.0
nm <- length(muzi)  #28u; 
nz <- length(zeny)  #16

U <- (prm-prz)/sqrt(rozm/nm+rozz/nz) #8.015

#W(alfa) = {u; u => u(1-alfa)}
#alfa = 0.05 -> u(0.95)
qnorm(0.95) #1.645
#W(0.05) = {u; u => 1.645}

#Hodnota testového kritéria patøí do kritického oboru
#H0 (E(X1)=E(X2)) zamítáme ve prospìch H1 (E(X1)>E(X2))
#na 5% hladinì významnosti.