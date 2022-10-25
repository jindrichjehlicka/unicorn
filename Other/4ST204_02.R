#Pro promìnné Sourozenci a Fakulta vytvoøte tabulku 
#rozdìlení absolutních a relativních èetností. 
#Tam, kde to dává smysl, urèete i kumulativní èetnosti. 
#Vytvoøte vhodný typ grafu.

data <- read.csv("data_studenti.csv", sep = ";")

data$Sourozenci
(T1 <- table(data$Sourozenci)) #Absolutní èetnosti
sum(T1) #Poèet pozorování
T1/sum(T1) #Relativní èetnosti
sum(T1/sum(T1)) #Souèet relativních èetností = 1
(rf <- round(T1/sum(T1), 3)) #Zaokrouhlení relativních èetností

cumsum(T1) #Kumulativní absolutní èetnost
cumsum(rf) #Kumulativní relativní èetnost

data.frame(T1, RelFreq = rf, CumAbsFreq = cumsum(T1),
           CumRelFreq = cumsum(rf))
#Plní funkci tabulky èetností, ale nevypadá úplnì dobøe

data.frame(T1) #pøevod z table na data.frame
T2 <- data.frame(T1)[,2] #vypíchnutí jen èetností
data.frame(rf) #dtto pro relativní
rf2 <- data.frame(rf)[,2] #dtto pro relativní

Finalni <- data.frame(AbsFreq = T2, RelFreq = rf2, CumAbsFreq = cumsum(T1),
           CumRelFreq = cumsum(rf))
#Už vypadá lépe

Finalni
#Vhodný typ grafu? Kvantitativní diskrétní -> sloupcový graf
barplot(Finalni$AbsFreq, ylim = c(0, 30),
        ylab = "Èetnost", xlab = "Poèet sourozencù",
        names.arg = row.names(Finalni), 
        main = "Sloupcový graf poètu sourozencù",
        col = c("red", "orange", "yellow", "green", "darkgreen"))

data$Fakulta
(T1 <- table(data$Fakulta)) #Absolutní èetnosti
sum(T1) #Poèet pozorování
T1/sum(T1) #Relativní èetnosti
sum(T1/sum(T1)) #Souèet relativních èetností = 1
(rf <- round(T1/sum(T1), 3)) #Zaokrouhlení relativních èetností

data.frame(T1, RelFreq = rf)
#Plní funkci tabulky èetností, ale nevypadá úplnì dobøe

data.frame(T1) #pøevod z table na data.frame
T2 <- data.frame(T1)[,2] #vypíchnutí jen èetností
data.frame(rf) #dtto pro relativní 
rf2 <- data.frame(rf)[,2] #dtto pro relativní

data.frame(AbsFreq = T2, RelFreq = rf2) #Nejsou vidìt hodnoty (Fakulty)
data.frame(AbsFreq = T2, RelFreq = rf2, row.names = c("FIS", "FMV", "NF")) #Lepší, ale manuálnì

(l1 <- levels(as.factor(data$Fakulta))) #Vypíchnutí hodnot promìnné Fakulta (nutné, aby se jednalo o faktor)
Finalni <- data.frame(AbsFreq = T2, RelFreq = rf2, row.names = l1) #Automatizováno
Finalni

#Vhodný typ grafu? Kategoriální nominální -> Koláèový graf
pie(Finalni$RelFreq, labels = row.names(Finalni),
    radius = 1, col = rainbow(3), density = 20,
    main = "Koláèový graf")

#Pøíklad 2
#Pro promìnnou Délka Chodidla vytvoøte 
#tabulku intervalových èetností a vytvoøte histogram.

#Snaha objektivizovat postup:
#1/ Poèet intervalù k ~ 1+3.3*log(n) Sturgessovo pravidlo
1+3.3*log10(44) #6.423 ~ 6 = k
#2/ Intervaly stejné délky
#   Intervaly obsahují všechna pozorování
# minimální délka intervalu: (max - min)/k
max(data$Délka.chodidla)-min(data$Délka.chodidla)
10/6
#Tvorba intervalù a poèet pozorování v nich
#[21, 22.7]; (22.7; 24.3], (24.3,26]....
data$Intervaly <- cut(data$Délka.chodidla, breaks = 6)
table(data$Intervaly)

#Pro krásu -> mùžu zmìnit délku intervalù (nahoru!)
#Použiju délku 2 -> mùžu posunout poèátek prvního intervalu
#Tøeba: [20; 22]; (22;24]; (24, 26];....

data$Intervaly2 <- cut(data$Délka.chodidla, breaks = c(20, 22, 24, 26, 28, 30, 32))
table(data$Intervaly2)

#Histogram
hist(data$Délka.chodidla, breaks = 6,
     col = "orange", density = 20, xlab = "Délka chodidla",
     main = "Histogram")

#Pøíklad 3
#Vypoèítejte medián a aritmetický prùmìr 
#pro následující hodnoty:
mean(c(12, 15, 13, 14, 16, 18, 18, 17, 15, 16))
median(c(12, 15, 13, 14, 16, 18, 18, 17, 15, 16))

mean(c(12, 15, 13, 14, 16, 18, 18, 17, 15, 56))
median(c(12, 15, 13, 14, 16, 18, 18, 17, 15, 56))

#Do hloubky kvantily:
#Medián -> P = 0.5; n = 10 -> nP = 10*0.5 = 5
#5 je celoèíselné -> nP = z = 5
#(x(5)+x(6))/2
s1 <- sort(c(12, 15, 13, 14, 16, 18, 18, 17, 15, 16))
(s1[5]+s1[6])/2

s2 <- sort(c(12, 15, 13, 14, 16, 18, 18, 17, 15, 56))
sort
(s2[5]+s2[6])/2

#Pøíklad 4
#Na základì tabulky èetností v Pøíkladu 2 
#urèete prùmìr, modus a medián poètu sourozencù.
Finalni
Finalni$Hodnoty <- as.numeric(row.names(Finalni))
sum(Finalni$Hodnoty*Finalni$AbsFreq)
sum(Finalni$AbsFreq)
sum(Finalni$Hodnoty*Finalni$AbsFreq)/sum(Finalni$AbsFreq) #1.091...

sum(Finalni$Hodnoty*Finalni$RelFreq) #1.092 kvùli zaokrouhlení
mean(data$Sourozenci) #1.091....

#Modus - nejèetnìjší hodnota -> 1

#Medián -> P = 0.5; n = 44 -> nP = 44*0.5 = 22
#22 je celoèíselné -> nP = z = 22
#(x(22)+x(23))/2
#Kumulativní èetnosti:
#Do (10) pozice jsou 0; pak do (36) pozice jsou 1;
#pak (40) jsou 2;...
#x(22) = 1 + x(23) = 1   -> (1+1)/2 = 1

median(data$Sourozenci) #kontrola

#Pøíklad 5
#Máme následující tabulku èetností:
#    Hodnota 35 42 28 x4 40
#    Èetnost n1 15 12 8 10
#Urèete chybìjící hodnoty, pokud víte, 
#že celkový poèet pozorování je 50 a prùmìr 40.

#n = sum(ni)
#50 = n1 + 15 + 12 + 8 + 10
#n1 = 50 - 15 - 12 - 8 - 10
#n1 = 5
#prùmìr = sum(xi*ni)/n
#40 = (35*5+42*15+28*12+x4*8+40*10)/50
#(40*50-35*5-42*15-28*12-40*10)/8 = x4
#x4 = 57.375

#Pøíklad 6
#Øidiè autobusu pražské MHD si mìøil svoji 
#prùmìrnou rychlost na své pravidelné 10 km 
#dlouhé trase a získal následující výsledky:
#47; 53; 58; 50; 49

#A) Jaká je jeho celková prùmìrná rychlost na 
#   základì získaných údajù?

#xi - rychlost = dráha/èas  (podíl)
#dráhy - ni 
#èas = dráha/rychlost (ni/xi)
#prùmìr x = sum(ni)/sum(ni/xi) harmonický prùmìr

n <- c(10, 10, 10, 10, 10)
x <- c(47, 53, 58, 50, 49)
sum(n)
n/x
sum(n/x)
sum(n)/sum(n/x) # 51.12785

#B 
#Jaký vliv na výsledek bude mít, pokud délka trasy bude 15 km?
n <- c(15,15,15,15,15)
sum(n)
n/x
sum(n)/sum(n/x) #51.12785

#C
#Jak se tato hodnota zmìní, pokud by mìøil rychlost
#na rùzných trasách, které mají délku 12, 10, 8, 9 a 6 km?
n <- c(12, 10, 8, 9, 6)
sum(n)
sum(n)/sum(n/x) #50.8832

#xi -> je podíl dvou hodnot      (km/h)
#ni -> èitatel tohoto podílu      (km)
#-> harmonický prùmìr