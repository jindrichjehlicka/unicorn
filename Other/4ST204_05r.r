#1. P��klad
#H�z�me �estist�nnou hrac� kostkou, o kter� p�edpokl�d�me, �e 
#h�z� v�echna ��sla se stejnou pravd�podobnost�. 
#Jak� je pravd�podobnost, �e:

#a.) p�i jednom hodu padne �estka;

#A - p�i prvn�m hodu padne 6
#P(A) = 1/6

#b.) p�i jednom hodu padne sud� ��slo;

#P(S) = 3/6 = 1/2

#c.) p�i dvou hodech padne dvakr�t �estka;

#B - p�i druh�m hodu padne 6
#P(A"pr"B) = P(A)P(B) = 1/6*1/6 = 1/36

#d.) p�i dvou hodech nepadne ani jednou �estka;

#dA - jev, kter� nastane tehdy, kdy� nenastane A (dopln�k A)
#dA - v prvn�m hodu nepadne 6
#P(dA) = 1 - P(A) = 1 - 1/6 = 5/6
#P(dB) = 5/6
#P(dB"pr"dA) = 5/6*5/6 = 25/36


#e.) p�i dvou hodech padne pr�v� jednou �estka;

#P(A"pr"dB) + P(dA"pr"B) = 5/36 + 5/36 = 10/36


#f.) p�i dvou hodech padne alespo� jednou �estka;

# P(A"pr"dB) + P(dA"pr"B) + P(A"pr"B) = 10/36+1/36 = 11/36
# 1 - P(dA"pr"dB) = 1 - 25/36 = 11/36
# P(A"sj"B) = P(A) + P(B) - P(A"pr"B) = 1/6 + 1/6 - 1/36 = 11/36

#2. P��klad
#T�i absolventi st�edn� �koly - pan Nov�k, pan Svoboda a 
#pan Ma��k skl�daj� p�ij�mac� zkou�ky na t�i r�zn� vysok� 
#�koly. Rodi�e t�chto student� odhaduj� jejich �ance na 
#�sp�ch na 70 % pro studenta Nov�ka, 40 % pro studenta 
#Svobodu a na 60 % pro studenta Ma��ka. Jak� je 
#pravd�podobnost �e:

#P(N) = 0.7; P(S) = 0.4; P(M) = 0.6

#a.) v�ichni t�i usp�j�;

#P(N"pr"S"pr"M) = 0.7*0.4*0.6 = 0.168

#b.) ani jeden z nich neusp�je;

#P(dN"pr"dS"pr"dM) = 0.3*0.6*0.4 = 0.072

#c.) usp�je jen student Nov�k?;

#P(N"pr"dS"pr"dM) = 0.7*0.6*0.4 = 0.168

#d.) usp�je pr�v� jeden z nich?

#0.7*0.6*0.4 + 0.3*0.4*0.4 + 0.3*0.6*0.6 = 0.324

#e.) neusp�je jen student Svoboda?

#P(N"pr"dS"pr"M) = 0.7*0.6*0.6 = 0.252

#f.) usp�j� pr�v� dva z nich?

#0.3*0.4*0.6 + 0.7*0.6*0.6 + 0.7*0.4*0.4 = 0.436

#g.) usp�je alespo� jeden?

# 0.324 + 0.436 + 0.168 = 0.928
# 1 - 0.072 = 0.928


#6. P��klad
#Uva�ujme ter�, kter� je tvo�en st�edem a mezikru��m. 
#Z�sah do st�edu je hodnocen 20 body, z�sah do mezikru�� 
#10 body. Ur�it� st�elec st��l� tak, �e zas�hne st�ed s 
#pravd�podobnost� 0,6, mezikru�� s pravd�podobnost� 0,3 
#a netref� se v�bec do ter�e s pravd�podobnost� 0,1. 
#N�hodn� veli�ina X je sou�et bod� z�skan�ch t�mto 
#st�elcem po dvou st�el�ch. Ur�ete hodnoty jej� 
#pravd�podobnostn� a distribu�n� funkce.

#Hodnoty
x <- c(0, 10, 20, 30, 40)

#Pravd�podobnostn� funkce (X - n�hodn� veli�ina; x - hodnota)
#P(x) = P(X = x)
#P(0) = P(X = 0) = 0.1*0.1 = 0.01
#P(10) = 0.1*0.3 + 0.3*0.1 = 0.06
#P(20) = 0.1*0.6 + 0.6*0.1 + 0.3*0.3 = 0.21
#P(30) = 0.3*0.6 + 0.6*0.3 = 0.36
#P(40) = 0.6*0.6 = 0.36

Px <- c(0.01, 0.06, 0.21, 0.36, 0.36)
sum(Px)  #kontrola - sum(Px) = 1

#Distribu�n� funkce F(x) = P(X <= x)
#F(0) = P(X <= 0) = P(0) = 0.01
#F(10) = P(X <= 10) = P(0) + P(10) = 0.01 + 0.06 = 0.07

Fx <- cumsum(Px)

data.frame(x, Px, Fx)

#7. P��klad
#Pravd�podobnostn� rozd�len� n�hodn� veli�iny X je d�no 
#n�sleduj�c� tabulkou:
x <- c(1, 2, 3, 4, 5)
Px <- c(0.15, 0.25, 0.5, 0.09, 0.01)

#a) Spo��tejte st�edn� hodnotu, rozptyl a sm�rodatnou odchylku.
(EX <- sum(x*Px) ) #E(X) = 2.56
(EX2 <- sum(x^2*Px)) #E(X^2) = 7.34 
(DX <- EX2 - EX^2) #D(X) = 0.7864
(Sigma <- sqrt(DX)) #Sigma(X) = 0.887

#b) Spo�t�te pravd�podobnosti: 
#P(X = 4) = 0.09
Px[4]

#P(1 < X ??? 3 ) = 0.75
Px[2]+Px[3]#pres pravd. fci
Fx[3]-Fx[1]#pres distribucni fci

#P(X > 1) = 0.85
sum(Px[2:5]) 
1 - Px[1]

#c) Ur�ete distribu�n� funkci n�hodn� veli�iny X.
(Fx <- cumsum(Px))
data.frame(x, Px, Fx)

#9. P��klad
#Zn�me distribu�n� funkci n�hodn� veli�iny X , 
#Ur�ete pravd�podobnostn� funkci P(x) n�hodn� veli�iny X, 
#st�edn� hodnotu a rozptyl.

x <- c(10, 20, 30, 1000)
Px <- c(0.15, 0.3, 0.53, 0.02)
cumsum(Px) #kontrola

(EX <- sum(x*Px))  #E(X) = 43.4
(EX2 <- sum(x^2*Px)) #E(X^2) = 20 612
(DX <- EX2 - EX^2) #D(X) = 18 728.44

