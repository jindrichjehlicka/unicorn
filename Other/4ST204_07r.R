#1. Pøíklad
#Budete sestrojovat intervaly spolehlivosti pro oèekávanou 
#výšku všech studentù. Nejprve urèete poèet pozorování, 
#výbìrový prùmìr a výbìrovou smìrodatnou odchylku.

data <- read.csv("data_studenti.csv", sep = ";")

prumer <- mean(data$Výška) #prùmìr = 175.4
n <- length(data$Výška) #poèet pozorování n = 44
S <- sd(data$Výška) #výbìrová sm. odchylka S = 11.84

#Sestrojte dvoustranný 95% interval spolehlivosti za 
#pøedpokladu, že výška studentù má normální rozdìlení.

#Výška má normální rozdìlení
#Spolehlivost 1-alfa = 0.95
#-> alfa = 0.05 -> 1-alfa/2 = 1-0.05/2 = 0.975
kvantil <- qt(0.975, n-1) #2.017

prumer-kvantil*S/sqrt(n) #171.79
prumer+kvantil*S/sqrt(n) #178.99

#95% interval spolehlivosti pro støední výšku je (171.79, 178.99)

#Jak by se zmìnil výsledek, kdyby vypoèítaná výbìrová 
#smìrodatná odchylka byla skuteèná populaèní smìrodatná 
#odchylka?

Sigma <- S  #11.844
kvantil <- qnorm(0.975) #1.960

prumer-kvantil*Sigma/sqrt(n) #171.89
prumer+kvantil*Sigma/sqrt(n) #178.89

#95% interval spolehlivosti pro støední výšku je (171.89, 178.89)

#Sestrojte dvoustranný 95% interval spolehlivosti bez 
#pøedpokladu normality.

prumer-kvantil*S/sqrt(n) #171.89
prumer+kvantil*S/sqrt(n) #178.89

#95% interval spolehlivosti pro støední výšku je (171.89, 178.89)

#Jak by se zmìnil výsledek ve tøetím bodu, pokud by byl 
#zkoumán, se stejnými výsledky, ètyønásobný poèet studentù.

n2 <- 4*n

prumer-kvantil*S/sqrt(n2) #173.64
prumer+kvantil*S/sqrt(n2) #177.14

#95% interval spolehlivosti pro støední výšku je (173.64, 177.14)

#Jak by se zmìnil výsledek ve tøetím bodu, kdyby byla 
#použita 99% spolehlivost.
#1-alfa = 0.99 -> alfa = 0.01 -> 1-alfa/2 = 0.995
kvantil <- qnorm(0.995) #2.576

prumer-kvantil*S/sqrt(n) #170.79
prumer+kvantil*S/sqrt(n) #179.99

#99% interval spolehlivosti pro støední výšku je (170.79, 179.99)


#2. Pøíklad
#Pøi sledování životnosti nových baterií bylo ze souboru 
#50 000 baterií vybráno 30 a u nich byl vypoèten prùmìr 
#195 dnù, smìrodatná odchylka v základním souboru je známá 
#a její hodnota je 20. Pøedpokládáme, že životnost baterií 
#se øídí normálním rozdìlením.

n <- 30
prumer <- 195
Sigma <- 20

#Sestrojte 95% jednostranný interval spolehlivosti pro 
#støední životnost baterií omezený zdola, tzn. levostranný 
#interval spolehlivosti.

kvantil <- qnorm(0.95) #1.645

prumer-kvantil*Sigma/sqrt(30)

#Interval spolehlivhosti pro støední životnost baterií 
#je (188.99; Inf)

#Sestrojte 95% dvoustranný interval spolehlivosti.
#1-alfa = 0.95 -> alfa = 0.05 -> 1-alfa/2 = 0.975

kvantil <- qnorm(0.975) #1.960

prumer-kvantil*Sigma/sqrt(30) #187.84
prumer+kvantil*Sigma/sqrt(30) #202.16

#Interval spolehlivhosti pro støední životnost baterií 
#je (187.84; 202.16)

#Sestrojte 95% dvoustranný interval spolehlivosti, pokud 
#smìrodatná odchylka v základním souboru nebyla známa, ale 
#zjistili jsme výbìrovou hodnotu smìrodatné odchylky 30.

S <- 30
kvantil <- qt(0.975, n-1) #2.045

prumer-kvantil*S/sqrt(30) #183.80
prumer+kvantil*S/sqrt(30) #206.20

#Interval spolehlivhosti pro støední životnost baterií 
#je (183.80; 206.20)


#Jak se zmìní interval spolehlivosti z druhého bodu, pokud 
#zvýšíme rozsah výbìru napø. na 100 baterií?

kvantil <- qnorm(0.975) #1.960

prumer-kvantil*Sigma/sqrt(100) #191.08
prumer+kvantil*Sigma/sqrt(100) #198.92

#Interval spolehlivhosti pro støední životnost baterií 
#je (191.08; 198.92)


#Jak se zmìní interval spolehlivosti z druhého bodu, pokud 
#nebudeme požadovat spolehlivost 95%, ale 99%
#1-alfa = 0.99 -> alfa = 0.01 -> 1-alfa/2 = 0.995

kvantil <- qnorm(0.995)

prumer-kvantil*Sigma/sqrt(30) #185.59
prumer+kvantil*Sigma/sqrt(30) #204.41

#3. Pøíklad
#Bìhem jednoho dne byla u 60 náhodnì vybraných zákazníku 
#supermarketu spoèítána cena jejich nákupu. Z údajù byly 
#vypoèteny výbìrové charakteristiky ????¯=245 a S = 111. 
#V jakých mezích je možné se spolehlivostí 95 % oèekávat 
#celkovou tržbu v Kè, navštívilo-li supermarket v tomto 
#dni 2 400 zákazníkù

n <- 60
prumer <- 245
S <- 111
#1-alfa = 0.95 -> 1-alfa/2 = 0.975
kvantil <- qnorm(0.975) #1.960

(dolni <- prumer-kvantil*S/sqrt(n)) #216.9
(horni <- prumer+kvantil*S/sqrt(n)) #273.1

#95% interval spolehlivosti pro støední útratu je (216.9, 273.1)

N <- 2400
N*dolni #520 593
N*horni #655 407

#95% interval spolehlivosti pro celkovou tržbu je (520 593; 655 407)

#4. Pøíklad
#Urèete 95% interval spolehlivosti pro podíl studentù 
#ženského pohlaví na základì dotazníku, pøedpokládaje, 
#že se jedná o náhodný výbìr všech studentù.

n <- length(data$Pohlaví) #44
m <- sum(data$Pohlaví == "Žena") #16
p <- m/n  # 0.364 (36.4 %)

#1-alfa = 0.95 -> 1-alfa/2 = 0.975
kvantil <- qnorm(0.975) #1.960

p-kvantil*sqrt(p*(1-p)/n) #0.221 (22.1 %)
p+kvantil*sqrt(p*(1-p)/n) #0.506 (50.6 %)

#95% interval spolehlivosti pro podíl žen je (0.221, 0.506)

#Na základì údajù v tomto èlánku:
#https://www.median.eu/cs/wp-content/uploads/2020/02/MEDIAN_volebni_model_2020_01.pdf
#dle údajù ze slidu 6 (snìmovní volební model)
#a.) Urèete s 95% spolehlivostí pøípustnou chybu odhadu pro 
#strany ANO 2011 a TOP09.

Pano <- 0.3
Ptop <- 0.045
n <- 695
#1-alfa = 0.95 -> 1-alfa/2 = 0.975
kvantil <- qnorm(0.975) #1.960

kvantil*sqrt(Pano*(1-Pano)/n) #0.034
kvantil*sqrt(Ptop*(1-Ptop)/n) #0.015

#Pøípustná chyba odhadu pro ANO je +- 3.4 p.b.
#Pøípustná chyba odhadu pro TOP09 je +- 1.5 p.b.

#b.) Urèete s 95% spolehlivostí minimální volební zisk 
#tìchto stran podle tohoto odhadu a pøepoètìte ho na 
#volièské hlasy, jestliže by v následujících volbách 
#bylo odevzdáno 5 000 000 platných hlasù.

kvantil <- qnorm(0.95) #1.645

Spodni <- Pano-kvantil*sqrt(Pano*(1-Pano)/n) #0.271
Spodni2 <- Ptop-kvantil*sqrt(Ptop*(1-Ptop)/n) #0.032

#95% interval spolehlivosti relativní èetnosti volièù 
#ANO je (0.271; 1) a pro TOP09 je (0,032; 1)

N <- 5000000
Spodni*N   #1 357 040
Spodni2*N  #  160 329

#95% interval spolehlivosti poètu volièù ANO je 
#(1 357 040; 5 000 000) a pro TOP09 je (160 329; 5 000 000)