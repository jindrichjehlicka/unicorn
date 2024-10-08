#1. P��klad
#Budete sestrojovat intervaly spolehlivosti pro o�ek�vanou 
#v��ku v�ech student�. Nejprve ur�ete po�et pozorov�n�, 
#v�b�rov� pr�m�r a v�b�rovou sm�rodatnou odchylku.

data <- read.csv("data_studenti.csv", sep = ";")

prumer <- mean(data$V��ka) #pr�m�r = 175.4
n <- length(data$V��ka) #po�et pozorov�n� n = 44
S <- sd(data$V��ka) #v�b�rov� sm. odchylka S = 11.84

#Sestrojte dvoustrann� 95% interval spolehlivosti za 
#p�edpokladu, �e v��ka student� m� norm�ln� rozd�len�.

#V��ka m� norm�ln� rozd�len�
#Spolehlivost 1-alfa = 0.95
#-> alfa = 0.05 -> 1-alfa/2 = 1-0.05/2 = 0.975
kvantil <- qt(0.975, n-1) #2.017

prumer-kvantil*S/sqrt(n) #171.79
prumer+kvantil*S/sqrt(n) #178.99

#95% interval spolehlivosti pro st�edn� v��ku je (171.79, 178.99)

#Jak by se zm�nil v�sledek, kdyby vypo��tan� v�b�rov� 
#sm�rodatn� odchylka byla skute�n� popula�n� sm�rodatn� 
#odchylka?

Sigma <- S  #11.844
kvantil <- qnorm(0.975) #1.960

prumer-kvantil*Sigma/sqrt(n) #171.89
prumer+kvantil*Sigma/sqrt(n) #178.89

#95% interval spolehlivosti pro st�edn� v��ku je (171.89, 178.89)

#Sestrojte dvoustrann� 95% interval spolehlivosti bez 
#p�edpokladu normality.

prumer-kvantil*S/sqrt(n) #171.89
prumer+kvantil*S/sqrt(n) #178.89

#95% interval spolehlivosti pro st�edn� v��ku je (171.89, 178.89)

#Jak by se zm�nil v�sledek ve t�et�m bodu, pokud by byl 
#zkoum�n, se stejn�mi v�sledky, �ty�n�sobn� po�et student�.

n2 <- 4*n

prumer-kvantil*S/sqrt(n2) #173.64
prumer+kvantil*S/sqrt(n2) #177.14

#95% interval spolehlivosti pro st�edn� v��ku je (173.64, 177.14)

#Jak by se zm�nil v�sledek ve t�et�m bodu, kdyby byla 
#pou�ita 99% spolehlivost.
#1-alfa = 0.99 -> alfa = 0.01 -> 1-alfa/2 = 0.995
kvantil <- qnorm(0.995) #2.576

prumer-kvantil*S/sqrt(n) #170.79
prumer+kvantil*S/sqrt(n) #179.99

#99% interval spolehlivosti pro st�edn� v��ku je (170.79, 179.99)


#2. P��klad
#P�i sledov�n� �ivotnosti nov�ch bateri� bylo ze souboru 
#50 000 bateri� vybr�no 30 a u nich byl vypo�ten pr�m�r 
#195 dn�, sm�rodatn� odchylka v z�kladn�m souboru je zn�m� 
#a jej� hodnota je 20. P�edpokl�d�me, �e �ivotnost bateri� 
#se ��d� norm�ln�m rozd�len�m.

n <- 30
prumer <- 195
Sigma <- 20

#Sestrojte 95% jednostrann� interval spolehlivosti pro 
#st�edn� �ivotnost bateri� omezen� zdola, tzn. levostrann� 
#interval spolehlivosti.

kvantil <- qnorm(0.95) #1.645

prumer-kvantil*Sigma/sqrt(30)

#Interval spolehlivhosti pro st�edn� �ivotnost bateri� 
#je (188.99; Inf)

#Sestrojte 95% dvoustrann� interval spolehlivosti.
#1-alfa = 0.95 -> alfa = 0.05 -> 1-alfa/2 = 0.975

kvantil <- qnorm(0.975) #1.960

prumer-kvantil*Sigma/sqrt(30) #187.84
prumer+kvantil*Sigma/sqrt(30) #202.16

#Interval spolehlivhosti pro st�edn� �ivotnost bateri� 
#je (187.84; 202.16)

#Sestrojte 95% dvoustrann� interval spolehlivosti, pokud 
#sm�rodatn� odchylka v z�kladn�m souboru nebyla zn�ma, ale 
#zjistili jsme v�b�rovou hodnotu sm�rodatn� odchylky 30.

S <- 30
kvantil <- qt(0.975, n-1) #2.045

prumer-kvantil*S/sqrt(30) #183.80
prumer+kvantil*S/sqrt(30) #206.20

#Interval spolehlivhosti pro st�edn� �ivotnost bateri� 
#je (183.80; 206.20)


#Jak se zm�n� interval spolehlivosti z druh�ho bodu, pokud 
#zv���me rozsah v�b�ru nap�. na 100 bateri�?

kvantil <- qnorm(0.975) #1.960

prumer-kvantil*Sigma/sqrt(100) #191.08
prumer+kvantil*Sigma/sqrt(100) #198.92

#Interval spolehlivhosti pro st�edn� �ivotnost bateri� 
#je (191.08; 198.92)


#Jak se zm�n� interval spolehlivosti z druh�ho bodu, pokud 
#nebudeme po�adovat spolehlivost 95%, ale 99%
#1-alfa = 0.99 -> alfa = 0.01 -> 1-alfa/2 = 0.995

kvantil <- qnorm(0.995)

prumer-kvantil*Sigma/sqrt(30) #185.59
prumer+kvantil*Sigma/sqrt(30) #204.41

#3. P��klad
#B�hem jednoho dne byla u 60 n�hodn� vybran�ch z�kazn�ku 
#supermarketu spo��t�na cena jejich n�kupu. Z �daj� byly 
#vypo�teny v�b�rov� charakteristiky ????�=245 a S = 111. 
#V jak�ch mez�ch je mo�n� se spolehlivost� 95 % o�ek�vat 
#celkovou tr�bu v K�, nav�t�vilo-li supermarket v tomto 
#dni 2 400 z�kazn�k�

n <- 60
prumer <- 245
S <- 111
#1-alfa = 0.95 -> 1-alfa/2 = 0.975
kvantil <- qnorm(0.975) #1.960

(dolni <- prumer-kvantil*S/sqrt(n)) #216.9
(horni <- prumer+kvantil*S/sqrt(n)) #273.1

#95% interval spolehlivosti pro st�edn� �tratu je (216.9, 273.1)

N <- 2400
N*dolni #520 593
N*horni #655 407

#95% interval spolehlivosti pro celkovou tr�bu je (520 593; 655 407)

#4. P��klad
#Ur�ete 95% interval spolehlivosti pro pod�l student� 
#�ensk�ho pohlav� na z�klad� dotazn�ku, p�edpokl�daje, 
#�e se jedn� o n�hodn� v�b�r v�ech student�.

n <- length(data$Pohlav�) #44
m <- sum(data$Pohlav� == "�ena") #16
p <- m/n  # 0.364 (36.4 %)

#1-alfa = 0.95 -> 1-alfa/2 = 0.975
kvantil <- qnorm(0.975) #1.960

p-kvantil*sqrt(p*(1-p)/n) #0.221 (22.1 %)
p+kvantil*sqrt(p*(1-p)/n) #0.506 (50.6 %)

#95% interval spolehlivosti pro pod�l �en je (0.221, 0.506)

#Na z�klad� �daj� v tomto �l�nku:
#https://www.median.eu/cs/wp-content/uploads/2020/02/MEDIAN_volebni_model_2020_01.pdf
#dle �daj� ze slidu 6 (sn�movn� volebn� model)
#a.) Ur�ete s 95% spolehlivost� p��pustnou chybu odhadu pro 
#strany ANO 2011 a TOP09.

Pano <- 0.3
Ptop <- 0.045
n <- 695
#1-alfa = 0.95 -> 1-alfa/2 = 0.975
kvantil <- qnorm(0.975) #1.960

kvantil*sqrt(Pano*(1-Pano)/n) #0.034
kvantil*sqrt(Ptop*(1-Ptop)/n) #0.015

#P��pustn� chyba odhadu pro ANO je +- 3.4 p.b.
#P��pustn� chyba odhadu pro TOP09 je +- 1.5 p.b.

#b.) Ur�ete s 95% spolehlivost� minim�ln� volebn� zisk 
#t�chto stran podle tohoto odhadu a p�epo�t�te ho na 
#voli�sk� hlasy, jestli�e by v n�sleduj�c�ch volb�ch 
#bylo odevzd�no 5 000 000 platn�ch hlas�.

kvantil <- qnorm(0.95) #1.645

Spodni <- Pano-kvantil*sqrt(Pano*(1-Pano)/n) #0.271
Spodni2 <- Ptop-kvantil*sqrt(Ptop*(1-Ptop)/n) #0.032

#95% interval spolehlivosti relativn� �etnosti voli�� 
#ANO je (0.271; 1) a pro TOP09 je (0,032; 1)

N <- 5000000
Spodni*N   #1 357 040
Spodni2*N  #  160 329

#95% interval spolehlivosti po�tu voli�� ANO je 
#(1 357 040; 5 000 000) a pro TOP09 je (160 329; 5 000 000)