#1. P��klad
#Zasad�me 10 semen ur�it� rostliny a p�edpokl�d�me, �e z 
#ka�d�ho semena je mo�no vyp�stovat zdravou rostlinu s 
#pravd�podobnost� 0,6. Za n�hodnou veli�inu X budeme 
#pova�ovat po�et zdrav�ch rostlin vyp�stovan�ch z t�chto semen.

#X - po�et n�hodn�ch jev� v "n" n�hodn�ch pokusech s 
#    konstantn� pravd�podobnost� "�sp�chu" (P�)
#X ~ Bi(n, P�)

#X ~ Bi(10, 0.6)
n <- 10
Pi <- 0.6

#a.) Jak� je st�edn� hodnota a rozptyl t�to n�hodn� veli�iny?
(EX = n*Pi) #6
(DX = n*Pi*(1-Pi)) #2.4
#b.) Jak� je pravd�podobnost, �e vyrostou 4 rostliny?
choose(n, 4)*Pi^4*(1-Pi)^6 #0.111
#F(x) = pbinom
dbinom(4, n, Pi) #0.111


#3. P��klad
#P�i kontrole jakosti z�silky 1 000 fotbalov�ch m��� 
#vyroben�ch je proveden n�hodn� v�b�r 80 kus�. P�i 
#nejv��e �esti nevyhovuj�c�ch m���ch je z�silka p�ijata. 
#Jak� je pravd�podobnost, �e bude p�ijata z�silka tohoto 
#rozsahu obsahuj�c� 60 nevyhovuj�c�ch m���?

#X - Po�et n�hodn�ch jev� ve v�b�ru bez vracen� z kone�n� 
#    mno�iny (z�kladn� soubor).
#(s vracen�m -> Binomick� r.)

# X ~ Hypergeometrick� r.
# M - Po�et jev� v ZS
# N - Velikost ZS
# n - velikost v�b�ru

#X ~ Hg(60 ,1000, 80)
M <- 60
N <- 1000
n <- 80
  
#P (X <= 6)

dhyper(0:6, M, N-M, n)
sum(dhyper(0:6, M, N-M, n)) #0.804 (sou�et pravd�podobnost�)

phyper(6, M, N-M, n) #0.804 (distribu�n� funkce)

#5. P��klad
#�erpac� stanice obslou�� v pr�m�ru 54 automobil� za hodinu. 
#Jak� je pravd�podobnost, �e b�hem p��t�ch p�ti minut obslou�� 
#�erpac� stanice aspo� t�i automobily?

#X - po�et n�hdon�ch jev� za interval (�asu, vzd�lenosti..)
# - pokud se n�hodn� jevy vyskytuj� n�hodn� v �ase
#X ~ Po (Lambda)
#Lambda - st�edn� (o�ek�van�) po�et n�hodn�ch jev� za dan� interval
#Lambda = 54/60*5 = 4.5 (za p�t minut) 
Lambda <- 4.5

#P(X => 3) = P(3) + P(4) + .... = 1 - P(X < 3) = 1 - P(X <= 2)
(4.5^2/factorial(2)*exp(-4.5)) #Vzorce pro 2
dpois(0:2, 4.5)  #Funkce pro 0, 1, 2
1 - sum(dpois(0:2, Lambda)) #0.826
# 1 - P(X <= 2) = 1 - F(2)
1 - ppois(2, 4.5) #0.826 pres distribucni

#7. P��klad
#N�hodnou veli�inou X je v��ka mu�� v ur�it� populaci. 
#Tato n�hodn� veli�ina m� norm�ln� rozd�len� se st�edn� 
#hodnotou 180 cm a sm�rodatnou odchylkou 7 cm. 

#X ~ N (180, 7)
Mu <- 180 #E(X)
Sigma <- 7 #Sm�rodatn� odchylka

#Ur�ete pravd�podobnost, �e n�hodn� vybran� mu� z t�to populace 
#bude m�t v��ku v�t�� ne� 200 cm. 

#F(x) = P(X <= x)

# P(X > 200) = 1 - P(X <= 200) = 1 - F(200) 
1 - pnorm(200, Mu, Sigma) #0.00214
#Ur�ete v��ku 5 % nejmen��ch mu�� dan� populace.

#P(X < x) = 0.05

#P(X <= x) = P   -> 100P% kvantil

#P(X < x) = 0.05 -> 5% kvantil 

qnorm(0.05, Mu, Sigma) #168.486

pnorm(168.486, Mu, Sigma) #Vr�t� 5 % (kontrola)

#8. P��klad
#Po�et bod� z�skan�ch ze zkou�ky m� norm�ln� rozd�len� 
#N(� = 35; ??2 = 100). Zkou�ej�c� se rozhodl vyu��t znalosti 
#po�tu bod� v jednotliv�ch testech v minulosti a stanovil 
#n�sleduj�c� hranice pro jednotliv� zn�mky:

Mu <- 35
Sigma <- sqrt(100) #10 

#1: po�et bod� > 45 
#P(X > 45) = 1 - F(45)
1 - pnorm(45, Mu, Sigma) #0.1587

#2: 35 < po�et bod� < 45
#P(35 < X < 45) = P(X < 45) - P(X <= 35) = F(45) - F(35)
pnorm(45, Mu, Sigma) - pnorm(35, Mu, Sigma) #0.3413

#3: 25 < po�et bod� < 35
#P(25 < X < 35) = F(35) - F(25)
pnorm(35, Mu, Sigma) - pnorm(25, Mu, Sigma) #0.3413

#4+: 15 < po�et bod� < 25
#P(15 < X < 25) = F(25) - F(15)
pnorm(25, Mu, Sigma) - pnorm(15, Mu, Sigma) #0.1359

#4: po�et bod� < 15
#P(X <= 15) = F(15)
pnorm(15, Mu, Sigma) #0.0228

#10. P��klad
#Doba vypracov�n� testu studenty m� norm�ln� rozd�len� se 
#st�edn� hodnotou 30 minut a sm�rodatnou odchylkou 3 minut. 
#Limit pro vypracov�n� testu je 35 minut.
Mu <- 30
Sigma <- 3

#a) Kolik procent student� stihne test vypracovat do 29 minut?
#P(X < 29) = F(29)
pnorm(29, Mu, Sigma) #0.369

#b) Kolik procent student� test nestihne?
#P(X > 35) = 1 - F(35)
1 - pnorm(35, Mu, Sigma) #0.0478

#c) Kolik procent student� vypracuje test b�hem 
#posledn�ch 5 minut p�ed limitem?
#P(30 < X < 35) = F(35) - F(30)
pnorm(35, Mu, Sigma) - pnorm(30, Mu, Sigma) #0.452


#d) V jak� dob� stihne test vypracovat 99 % student�?
#P(X <= x) = P   -> 100P% kvantil
#P(X < x) = 0.99 -> 99% kvantil
qnorm(0.99, Mu, Sigma) #36.979 minut

#e.) V jak� dob� nestihne test vypracovat 5 % student�?
#P(X > x) = 0.05
#P(X <= x) = 1 - 0.05 = 0.95 -> 95% kvantil
qnorm(0.95, Mu, Sigma) #34.93

