#1. Pøíklad
#Následující tabulka udává (zaokrouhlenì) hustotu obyvatel, 
#poèet obyvatel a rozlohu ètyø zemí Visegrádské ètyøky.
#Urèete (tøemi rùznými zpùsoby) prùmìrnou hustotu obyvatel 
#v zemích Visegrádské ètyøky dohromady.

xi <- c(132, 110, 122, 108)
ni1 <- c(10400000, 5400000, 38100000, 10000000)
ni2 <- c(79000, 49000, 313000, 93000)
data.frame(Hustota = xi, obyvatel = ni1, rozloha = ni2)

#Logický -> hustota obyvatel = obyvatel/rozloha
sum(ni1)/sum(ni2) #119.663

#xi - hustota (pomìr)
#ni1 - poèet obyvatel (èitatel) -> harmonický prùmìr
sum(ni1)
ni1/xi
sum(ni1/xi) #dopoèítaná rozloha
sum(ni1)/sum(ni1/xi) #119.94

#ni2 - rozloha (jmenovatel) -> aritmetický
xi*ni2
sum(xi*ni2) #dopoèítaný poèet obyvatel
sum(xi*ni2)/sum(ni2) #119.94

#2. Pøíklad
#Na základì uvedené tabulky vypoètìte s pøesností 
#na dvì desetinná místa prùmìrný vìk zamìstnancù dané firmy.

x <- c(30, 31, 32, 33, 34, 35, 36)
P <- c(0, 0.2, 0.55, 0.75, 0.8, 0.95, 1)
p <- c(0, 0.2, 0.35, 0.2, 0.05, 0.15, 0.05)
cumsum(p) #kontrola
sum(x*p) #32.75

#3.Pøíklad
#Malý podnik má 20 zamìstnancù, kteøí pobírají prùmìrný plat 
#ve výši 22 000,-Kè. V podniku došlo v rámci úsporných 
#opatøení ke dvìma zmìnám – zaprvé byli propuštìni dva 
#zamìstnanci s platem ve výši 10 000,-Kè a 12 000,-Kè. 
#Zadruhé byl všem zbývajícím zamìstnancùm snížen plat o 5 %. 
#Kolik je nyní prùmìrný plat?

#prùmìr = suma/n
#22000 = suma1/20
#suma1 = 22000*20 = 440 000
#suma2 = 440 000 - 22 000 = 418 000
#suma3 = 418000*(1-0.05) = 397 100
#n2 = 20 - 2 = 18
#prùmìr2 = suma3/n2 = 397100/18 = 22 061,11,- 

#4. Pøíklad
#Na støední škole je prùmìrný plat 25 780,-Kè, prùmìrný 
#plat pedagogù je zde 29 198,-Kè a tvoøí 85 % zamìstnancù )
#školy. Jaký je prùmìrný mìsíèní plat nepedagogických 
#povolání na této škole?

#prùmìr = suma(xi*pi)
#25 780 = 29 198*0,85 + x2*0,15
#x2 = (25780-29198*0.85)/0.15 = 6 411,33,-


#Rozdìlte pozorování do dvou skupin podle pohlaví.
#Vypoèítejte prùmìr, (populaèní) rozptyl, smìrodatnou 
#odchylku a variaèní koeficient výšky pro každé pohlaví 
#zvláš a pro obì dohromady.

data<- read.csv("data_studenti.csv", sep = ";")

data$Výška

prumer <- mean(data$Výška) #Prùmìr = 175.3864

mean((data$Výška - mean(data$Výška))^2) #Rozptyl = 137.101
roz <- var(data$Výška)*43/44 #Rozptyl = 137.101 (cm^2)
sqrt(roz) #sm. odchylka = 11.709 (cm)
sqrt(roz)/mean(data$Výška) #variaèní koef. = 0.0668 (%)

#Rozdìlení
Muzi <- data[data$Pohlaví == "Muž",]
head(Muzi)
Zeny <- data[data$Pohlaví == "Žena",]
head(Zeny)

(prmuzi <- mean(Muzi$Výška)) #182.3
(przeny <- mean(Zeny$Výška)) #163.3

length(Muzi$Výška) #28 mùžu
(rozmuzi <- var(Muzi$Výška)*27/28) #53.4898
length(Zeny$Výška) #16 žen
(rozzeny <- var(Zeny$Výška)*15/16) #54.3398

sqrt(rozmuzi) #7.3 cm
sqrt(rozzeny) #7.4 cm

sqrt(rozmuzi)/prmuzi #0.0401
sqrt(rozzeny)/przeny #0.0451 

# Vytvoøte tabulku obsahující jenom tyto informace: 
#poèet pozorování, prùmìr a rozptyl výšky pro muže a 
#pro ženy zvláš

(Tabulka <- data.frame(pozorovani = c(28,16), prumery = c(prmuzi, przeny),
           rozptyly = c(rozmuzi, rozzeny)))

#Na základì této tabulky vypoèítejte prùmìr výšky všech
#studentù. Zkontrolujte výsledek.
Tabulka$prumery*Tabulka$pozorovani
sum(Tabulka$prumery*Tabulka$pozorovani)/sum(Tabulka$pozorovani) #175.3864

#Na základì této tabulky vypoèítejte vnitroskupinový rozptyl, 
#meziskupinový rozptyl a souètem celkový rozptyl. 
#Zkontrolujte výsledek.

Tabulka$rozptyly*Tabulka$pozorovani

Vnitrosk <- sum(Tabulka$rozptyly*Tabulka$pozorovani)/sum(Tabulka$pozorovani)
#Vnitroskupinový rozptyl = 53.7989

Tabulka$prumery - prumer
(Tabulka$prumery - prumer)^2
(Tabulka$prumery - prumer)^2*Tabulka$pozorovani
Mezisk <- sum((Tabulka$prumery - prumer)^2*Tabulka$pozorovani)/sum(Tabulka$pozorovani)
#Meziskupinový rozptyl = 83.30182

Vnitrosk+Mezisk #Celkový rozptyl = 137.1