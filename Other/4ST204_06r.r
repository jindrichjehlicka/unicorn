#1. Pøíklad
#Zasadíme 10 semen urèité rostliny a pøedpokládáme, že z 
#každého semena je možno vypìstovat zdravou rostlinu s 
#pravdìpodobností 0,6. Za náhodnou velièinu X budeme 
#považovat poèet zdravých rostlin vypìstovaných z tìchto semen.

#X - poèet náhodných jevù v "n" náhodných pokusech s 
#    konstantní pravdìpodobností "úspìchu" (Pí)
#X ~ Bi(n, Pí)

#X ~ Bi(10, 0.6)
n <- 10
Pi <- 0.6

#a.) Jaká je støední hodnota a rozptyl této náhodné velièiny?
(EX = n*Pi) #6
(DX = n*Pi*(1-Pi)) #2.4
#b.) Jaká je pravdìpodobnost, že vyrostou 4 rostliny?
choose(n, 4)*Pi^4*(1-Pi)^6 #0.111
#F(x) = pbinom
dbinom(4, n, Pi) #0.111


#3. Pøíklad
#Pøi kontrole jakosti zásilky 1 000 fotbalových míèù 
#vyrobených je proveden náhodný výbìr 80 kusù. Pøi 
#nejvýše šesti nevyhovujících míèích je zásilka pøijata. 
#Jaká je pravdìpodobnost, že bude pøijata zásilka tohoto 
#rozsahu obsahující 60 nevyhovujících míèù?

#X - Poèet náhodných jevù ve výbìru bez vracení z koneèné 
#    množiny (základní soubor).
#(s vracením -> Binomické r.)

# X ~ Hypergeometrické r.
# M - Poèet jevù v ZS
# N - Velikost ZS
# n - velikost výbìru

#X ~ Hg(60 ,1000, 80)
M <- 60
N <- 1000
n <- 80
  
#P (X <= 6)

dhyper(0:6, M, N-M, n)
sum(dhyper(0:6, M, N-M, n)) #0.804 (souèet pravdìpodobností)

phyper(6, M, N-M, n) #0.804 (distribuèní funkce)

#5. Pøíklad
#Èerpací stanice obslouží v prùmìru 54 automobilù za hodinu. 
#Jaká je pravdìpodobnost, že bìhem pøíštích pìti minut obslouží 
#èerpací stanice aspoò tøi automobily?

#X - poèet náhdoných jevù za interval (èasu, vzdálenosti..)
# - pokud se náhodné jevy vyskytují náhodnì v èase
#X ~ Po (Lambda)
#Lambda - støední (oèekávaný) poèet náhodných jevù za daný interval
#Lambda = 54/60*5 = 4.5 (za pìt minut) 
Lambda <- 4.5

#P(X => 3) = P(3) + P(4) + .... = 1 - P(X < 3) = 1 - P(X <= 2)
(4.5^2/factorial(2)*exp(-4.5)) #Vzorce pro 2
dpois(0:2, 4.5)  #Funkce pro 0, 1, 2
1 - sum(dpois(0:2, Lambda)) #0.826
# 1 - P(X <= 2) = 1 - F(2)
1 - ppois(2, 4.5) #0.826 pres distribucni

#7. Pøíklad
#Náhodnou velièinou X je výška mužù v urèité populaci. 
#Tato náhodná velièina má normální rozdìlení se støední 
#hodnotou 180 cm a smìrodatnou odchylkou 7 cm. 

#X ~ N (180, 7)
Mu <- 180 #E(X)
Sigma <- 7 #Smìrodatná odchylka

#Urèete pravdìpodobnost, že náhodnì vybraný muž z této populace 
#bude mít výšku vìtší než 200 cm. 

#F(x) = P(X <= x)

# P(X > 200) = 1 - P(X <= 200) = 1 - F(200) 
1 - pnorm(200, Mu, Sigma) #0.00214
#Urèete výšku 5 % nejmenších mužù dané populace.

#P(X < x) = 0.05

#P(X <= x) = P   -> 100P% kvantil

#P(X < x) = 0.05 -> 5% kvantil 

qnorm(0.05, Mu, Sigma) #168.486

pnorm(168.486, Mu, Sigma) #Vrátí 5 % (kontrola)

#8. Pøíklad
#Poèet bodù získaných ze zkoušky má normální rozdìlení 
#N(µ = 35; ??2 = 100). Zkoušející se rozhodl využít znalosti 
#poètu bodù v jednotlivých testech v minulosti a stanovil 
#následující hranice pro jednotlivé známky:

Mu <- 35
Sigma <- sqrt(100) #10 

#1: poèet bodù > 45 
#P(X > 45) = 1 - F(45)
1 - pnorm(45, Mu, Sigma) #0.1587

#2: 35 < poèet bodù < 45
#P(35 < X < 45) = P(X < 45) - P(X <= 35) = F(45) - F(35)
pnorm(45, Mu, Sigma) - pnorm(35, Mu, Sigma) #0.3413

#3: 25 < poèet bodù < 35
#P(25 < X < 35) = F(35) - F(25)
pnorm(35, Mu, Sigma) - pnorm(25, Mu, Sigma) #0.3413

#4+: 15 < poèet bodù < 25
#P(15 < X < 25) = F(25) - F(15)
pnorm(25, Mu, Sigma) - pnorm(15, Mu, Sigma) #0.1359

#4: poèet bodù < 15
#P(X <= 15) = F(15)
pnorm(15, Mu, Sigma) #0.0228

#10. Pøíklad
#Doba vypracování testu studenty má normální rozdìlení se 
#støední hodnotou 30 minut a smìrodatnou odchylkou 3 minut. 
#Limit pro vypracování testu je 35 minut.
Mu <- 30
Sigma <- 3

#a) Kolik procent studentù stihne test vypracovat do 29 minut?
#P(X < 29) = F(29)
pnorm(29, Mu, Sigma) #0.369

#b) Kolik procent studentù test nestihne?
#P(X > 35) = 1 - F(35)
1 - pnorm(35, Mu, Sigma) #0.0478

#c) Kolik procent studentù vypracuje test bìhem 
#posledních 5 minut pøed limitem?
#P(30 < X < 35) = F(35) - F(30)
pnorm(35, Mu, Sigma) - pnorm(30, Mu, Sigma) #0.452


#d) V jaké dobì stihne test vypracovat 99 % studentù?
#P(X <= x) = P   -> 100P% kvantil
#P(X < x) = 0.99 -> 99% kvantil
qnorm(0.99, Mu, Sigma) #36.979 minut

#e.) V jaké dobì nestihne test vypracovat 5 % studentù?
#P(X > x) = 0.05
#P(X <= x) = 1 - 0.05 = 0.95 -> 95% kvantil
qnorm(0.95, Mu, Sigma) #34.93

