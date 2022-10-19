#1. Pøíklad
#Použijte tabulku èetností poètu sourozencù vytvoøenou døíve. 
#Vypoèítejte rozptyl, smìrodatnou odchylku a variaèní koeficient. Použijte nejdøíve absolutní èetnosti a poté relativní èetnosti.

Finalni
hodnota <- as.numeric(row.names(Finalni))

n <- sum(Finalni$AbsFreq)

prumer <- sum(hodnota*Finalni$AbsFreq)/n
prumer #1.091

#Z absolutních èetností:
#Definièní tvar
(hodnota - prumer)^2 #Ètvercové odchylky
(hodnota - prumer)^2*Finalni$AbsFreq #Ukázka mezivýpoètù
rozptyl <- sum((hodnota - prumer)^2*Finalni$AbsFreq)/n #0.9463

#Výpoètový tvar
prumerctv <- sum(hodnota^2*Finalni$AbsFreq)/n #Prùmìr ètvercù 2.1364
prumerctv - prumer^2 #0.9463

sqrt(rozptyl) #Smodch = 0.9728
sqrt(rozptyl)/prumer #Variacni koeficient 0.8917

#Z relativních èetností:
#Definièní tvar:
(hodnota-prumer)^2*Finalni$RelFreq #Ukázka mezivýpoètu
sum((hodnota-prumer)^2*Finalni$RelFreq) #0.9495 Rozdíl je daný zaokrouhlením relativních èetností

#Výpoètový tvar:
sum(hodnota^2*Finalni$RelFreq) #Prùmìr ètvercù 2.142
2.142 - prumer^2 #0.9519 (Zase rozdíl zaokrouhlením)

#2. Pøíklad
#Byly zjištìny následující hodnoty:
#Urèete míry absolutní i relativní variability mezd.

prumery <-c(26, 35)
varkoef <- c(0.15, 0.3)
ni <- c(20,10)

#Variaèní koeficient = smodch/prùmìr
#-> smoch = VarKoef * prùmìr
#-> roztpyl = smodch^2 = (VarKoef*prùmìr)^2

rozptyly <- (varkoef*prumery)^2

(vnitrosk <- sum(rozptyly*ni)/sum(ni) ) #Vnitroskupinový r. = 46.89

(prumer <- sum(prumery*ni)/sum(ni)) #Prùmìr 29

(mezisk <- sum((prumery-prumer)^2*ni)/sum(ni)) #Meziskupinový r. = 18

rozptyl = vnitrosk + mezisk #64.89

sqrt(rozptyl) #smodch = 8.055
sqrt(rozptyl)/prumer #VarKoef = 0.278

#6. Pøíklad
#V tabulce je uvedena èasová øada hodnoty ukazatele v 
#letech 1991 až 1998. Charakterizujte vývoj hodnoty 
#ukazatele pomocí øetìzových a bazických indexù (1995 = 1)

Ukazatel <- c(282996, 345008, 398505, 365265, 368624, 387374, 397845)
Ukazatel

#Øetìzové indexy ruènì:
Ukazatel[2]/Ukazatel[1] #1.219
Ukazatel[3]/Ukazatel[2] #1.155

#Øetìzové indexy automatizovanì:
Retezove <- 0
for (i in 2:length(Ukazatel))
{
 Retezove[i] <-  Ukazatel[i]/Ukazatel[i-1]
}
Retezove

#Bazické indexy:
Ukazatel/Ukazatel[5]

#7. Pøíklad
#V tabulce jsou bazické indexy poètu dokonèených bytù v ÈR 
#v letech 1997 – 2000 se základem v roce 1997, a dále 
#bazické indexy poètu dokonèených bytù v letech 2000 až 2003 
#se základem v roce 2000. Dopoèítejte chybìjící bazické indexy 
#v obou øadách.

Baz97 <- c(100, 132.4, 141.6, 150.4, NA, NA, NA)
Baz00 <- c(NA, NA, NA, 100, 98.2, 108.3, 107.6)
data.frame(Baz97, Baz00)

#øetìzové I(t/t-1) = I(t/1)/I(t-1/1)
Ret <- c(NA, Baz97[2]/Baz97[1], Baz97[3]/Baz97[2], Baz97[4]/Baz97[3],
         Baz00[5]/Baz00[4], Baz00[6]/Baz00[5], Baz00[7]/Baz00[6])
Ret
#Bazický "dopøed" I(t/1)=I(t-1/1)*I(t/t-1)
data.frame(Baz97, Baz00, Ret)
Baz97[5] <- Baz97[4]*Ret[5]
Baz97[6] <- Baz97[5]*Ret[6]
Baz97[7] <- Baz97[6]*Ret[7]

data.frame(Baz97, Baz00, Ret)

#Bazický dozadu I(t-1/1) = I(t/1)/I(t/t-1)
Baz00[3] <- Baz00[4]/Ret[4]
Baz00[2] <- Baz00[3]/Ret[3]
Baz00[1] <- Baz00[2]/Ret[2]

data.frame(Baz97, Baz00, Ret)

#8. Pøíklad
#Pan Bakala objevil na zahrádce uhlí a rozhodl se ho prodávat. 
#V prvním roce prodal 200 tun uhlí za cenu 2000,- Kè/t. 
#Ve druhém roce se rozhodl zvýšit cenu na 2200,-Kè/t a 
#prodal takto 180 tun. Porovnejte zmìnu cen, prodaného 
#množství a tržeb ve druhém roce oproti prvnímu.

#Znaèení: 0/1     základní/bìžné       obvyle: starší/novìjší
#                 s èím   / co

p0 <- 2000
p1 <- 2200
q0 <- 200
q1 <- 180

#Cena
p1 - p0   #(jednoduchá) diference ceny  + 200 
p1/p0     #(jednoduchý) index ceny      *1.1 / + 10 %

#Množství
q1 - q0   #(jednoduchá) diference množství  - 20
q1/q0     #(jednoduchý) index množství      *0.9 / - 10 %

#Tržby Q = p*q
(Q0 <- p0*q0)  #400000
(Q1 <- p1*q1)  #396000
Q1 - Q0   #(jednoduchá) diference tržeb   - 4 000
Q1/Q0     #(jednoduchý) index tržeb      *0.99 / - 1 %

p1/p0*q1/q0   #(1.1*0.9 = 0.99)    Index tržeb 0.99

#9. Pøíklad
#Panu Bakalovi však uhlí z jeho zahrádky nestaèilo, 
#a tak zaèal tìžit a prodávat i na druhé poboèce, 
#øeknìme OKD. Na základì údajù v tabulce posuïte, 
#jak se meziroènì zmìnily tržby, prodané množství 
#a prùmìrná cena uhlí v celé jeho firmì.

q0 <- c(210,1000)
q1 <- c(260, 1200)
p0 <- c(2200, 1800)
p1 <- c(1800, 1900)

data.frame(q0,p0,q1,p1)

#Prodané množství -> souèet jednotlivých poboèek
sum(q1)-sum(q0)  #diference množství složená   + 250
sum(q1)/sum(q0)  #Index množství složený       1.207 (+20.7 %)

#Tržby -> tržby jednotlivých poboèek + souèet pøes poboèky
(Q0 <- p0*q0)
(Q1 <- p1*q1)
sum(Q1)-sum(Q0)  #diference tržeb složená   + 486 000
sum(Q1)/sum(Q0)  #Index tržeb složený       1.215 (+21.5 %)

#Prùmìrná cena -> celková tržba / celkové množství
(p0 <- sum(Q0)/sum(q0))
(p1 <- sum(Q1)/sum(q1))

p1-p0  #diference prùmìrné ceny složená   + 12.8
p1/p0  #Index prùmìrné ceny složený       1.007 (+0.7 %)

#10. Pøíklad
#Pan Bakala se jako správný magnát rozhodl diverzifikovat 
#své portfolio a kromì prodeje uhlí zaèal prodávat ruskou 
#ropu. Pomocí všech možných indexù posuïte na základì údajù 
#z tabulky, jak se zmìnily ceny a jak se zmìnil objem 
#prodaného množství celé firmy ve druhém roce oproti prvnímu.

p0 <- c(1900,1000)
p1 <- c(2000, 2000)
q0 <- c(1600,300)
q1 <- c(1800, 1000)

data.frame(p0, q0, p1, q1)

#Indexy ceny
#Laspeyersùv:   sum(q0*p1)/sum(q0*p0)   1.138 (+13.8 %)
#Paascheho:     sum(q1*p1)/sum(q1*p0)   1.268 (+26.8 %)
IpL <- sum(q0*p1)/sum(q0*p0)
IpP <- sum(q1*p1)/sum(q1*p0)
(IpF <- sqrt(IpL*IpP))        #1.201  (+20.1 %) 

#Indexy množství
#Laspeyersùv:        q1*p0/q0*p0
#Paascheho:          q1*p1/q0*p1
(IqL <- sum(q1*p0)/sum(q0*p0))       #1.323 (+32.3 %)
(IqP <- sum(q1*p1)/sum(q0*p1))       #1.476 (+47.4 %)
(IqF <- sqrt(IqL*IqP))               #1.396 (+39.6 %)
