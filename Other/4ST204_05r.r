#1. Pøíklad
#Házíme šestistìnnou hrací kostkou, o které pøedpokládáme, že 
#hází všechna èísla se stejnou pravdìpodobností. 
#Jaká je pravdìpodobnost, že:

#a.) pøi jednom hodu padne šestka;

#A - pøi prvním hodu padne 6
#P(A) = 1/6

#b.) pøi jednom hodu padne sudé èíslo;

#P(S) = 3/6 = 1/2

#c.) pøi dvou hodech padne dvakrát šestka;

#B - pøi druhém hodu padne 6
#P(A"pr"B) = P(A)P(B) = 1/6*1/6 = 1/36

#d.) pøi dvou hodech nepadne ani jednou šestka;

#dA - jev, který nastane tehdy, když nenastane A (doplnìk A)
#dA - v prvním hodu nepadne 6
#P(dA) = 1 - P(A) = 1 - 1/6 = 5/6
#P(dB) = 5/6
#P(dB"pr"dA) = 5/6*5/6 = 25/36


#e.) pøi dvou hodech padne právì jednou šestka;

#P(A"pr"dB) + P(dA"pr"B) = 5/36 + 5/36 = 10/36


#f.) pøi dvou hodech padne alespoò jednou šestka;

# P(A"pr"dB) + P(dA"pr"B) + P(A"pr"B) = 10/36+1/36 = 11/36
# 1 - P(dA"pr"dB) = 1 - 25/36 = 11/36
# P(A"sj"B) = P(A) + P(B) - P(A"pr"B) = 1/6 + 1/6 - 1/36 = 11/36

#2. Pøíklad
#Tøi absolventi støední školy - pan Novák, pan Svoboda a 
#pan Maøík skládají pøijímací zkoušky na tøi rùzné vysoké 
#školy. Rodièe tìchto studentù odhadují jejich šance na 
#úspìch na 70 % pro studenta Nováka, 40 % pro studenta 
#Svobodu a na 60 % pro studenta Maøíka. Jaká je 
#pravdìpodobnost že:

#P(N) = 0.7; P(S) = 0.4; P(M) = 0.6

#a.) všichni tøi uspìjí;

#P(N"pr"S"pr"M) = 0.7*0.4*0.6 = 0.168

#b.) ani jeden z nich neuspìje;

#P(dN"pr"dS"pr"dM) = 0.3*0.6*0.4 = 0.072

#c.) uspìje jen student Novák?;

#P(N"pr"dS"pr"dM) = 0.7*0.6*0.4 = 0.168

#d.) uspìje právì jeden z nich?

#0.7*0.6*0.4 + 0.3*0.4*0.4 + 0.3*0.6*0.6 = 0.324

#e.) neuspìje jen student Svoboda?

#P(N"pr"dS"pr"M) = 0.7*0.6*0.6 = 0.252

#f.) uspìjí právì dva z nich?

#0.3*0.4*0.6 + 0.7*0.6*0.6 + 0.7*0.4*0.4 = 0.436

#g.) uspìje alespoò jeden?

# 0.324 + 0.436 + 0.168 = 0.928
# 1 - 0.072 = 0.928


#6. Pøíklad
#Uvažujme terè, který je tvoøen støedem a mezikružím. 
#Zásah do støedu je hodnocen 20 body, zásah do mezikruží 
#10 body. Urèitý støelec støílí tak, že zasáhne støed s 
#pravdìpodobností 0,6, mezikruží s pravdìpodobností 0,3 
#a netrefí se vùbec do terèe s pravdìpodobností 0,1. 
#Náhodná velièina X je souèet bodù získaných tímto 
#støelcem po dvou støelách. Urèete hodnoty její 
#pravdìpodobnostní a distribuèní funkce.

#Hodnoty
x <- c(0, 10, 20, 30, 40)

#Pravdìpodobnostní funkce (X - náhodná velièina; x - hodnota)
#P(x) = P(X = x)
#P(0) = P(X = 0) = 0.1*0.1 = 0.01
#P(10) = 0.1*0.3 + 0.3*0.1 = 0.06
#P(20) = 0.1*0.6 + 0.6*0.1 + 0.3*0.3 = 0.21
#P(30) = 0.3*0.6 + 0.6*0.3 = 0.36
#P(40) = 0.6*0.6 = 0.36

Px <- c(0.01, 0.06, 0.21, 0.36, 0.36)
sum(Px)  #kontrola - sum(Px) = 1

#Distribuèní funkce F(x) = P(X <= x)
#F(0) = P(X <= 0) = P(0) = 0.01
#F(10) = P(X <= 10) = P(0) + P(10) = 0.01 + 0.06 = 0.07

Fx <- cumsum(Px)

data.frame(x, Px, Fx)

#7. Pøíklad
#Pravdìpodobnostní rozdìlení náhodné velièiny X je dáno 
#následující tabulkou:
x <- c(1, 2, 3, 4, 5)
Px <- c(0.15, 0.25, 0.5, 0.09, 0.01)

#a) Spoèítejte støední hodnotu, rozptyl a smìrodatnou odchylku.
(EX <- sum(x*Px) ) #E(X) = 2.56
(EX2 <- sum(x^2*Px)) #E(X^2) = 7.34 
(DX <- EX2 - EX^2) #D(X) = 0.7864
(Sigma <- sqrt(DX)) #Sigma(X) = 0.887

#b) Spoètìte pravdìpodobnosti: 
#P(X = 4) = 0.09
Px[4]

#P(1 < X ??? 3 ) = 0.75
Px[2]+Px[3]#pres pravd. fci
Fx[3]-Fx[1]#pres distribucni fci

#P(X > 1) = 0.85
sum(Px[2:5]) 
1 - Px[1]

#c) Urèete distribuèní funkci náhodné velièiny X.
(Fx <- cumsum(Px))
data.frame(x, Px, Fx)

#9. Pøíklad
#Známe distribuèní funkci náhodné velièiny X , 
#Urèete pravdìpodobnostní funkci P(x) náhodné velièiny X, 
#støední hodnotu a rozptyl.

x <- c(10, 20, 30, 1000)
Px <- c(0.15, 0.3, 0.53, 0.02)
cumsum(Px) #kontrola

(EX <- sum(x*Px))  #E(X) = 43.4
(EX2 <- sum(x^2*Px)) #E(X^2) = 20 612
(DX <- EX2 - EX^2) #D(X) = 18 728.44

