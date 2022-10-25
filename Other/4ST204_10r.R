#1. Pøíklad
#U náhodnì vybraných studentù jsme sledovali poèet dosažených 
#bodù na závìreèném testu (od 0 do 60). Vždy 4 z tìchto studentù 
#chodili k jednomu ze 3 cvièících – panu Kubovi, panu Kubinovi, 
#nebo panu Kubinèákovi. Proveïte test k posouzení otázky, zda 
#má osoba cvièícího vliv na poèet dosažených bodù u závìreèného 
#testu? Jaká je síla vztahu (interpretujte význam použitého 
#ukazatele)?

#H0: Mu1 = Mu2 = Mu3 (nezávislost)
#H1: non H0


priklad1 <- data.frame(body = c(53, 47, 55, 50, 42, 48, 
                  48, 44, 50, 39, 46, 39, 43, 44, 38),
                  cvicici = c(rep("Kuba", 4), rep("Kubina", 5),
                              rep("Kubincak", 6)))
priklad1

summary(aov(body ~ cvicici, data = priklad1))

#F = 10.41

#OBECNÌ:
#p-hodnota <= alfa -> H0 zamítám ve prospìch H1
#p-hodnota > alfa -> H0 NEzamítám ve prospìch H1

#V pøíkladu:
#p-hodnota = 0.00239  <  alfa = 0.05
#Zamítám H0 (nezávislost) ve prospìch H1 na 5% hladinì významnosti.

#P^2 = Sy.m/Sy = 231.5/(231.5 + 133.4) = 0.634   
#P^2 <0; 1> 
#63.4 % variability poètu bodù, které je vysvìtlitelné 
#osobou cvièícího.

boxplot(body ~ cvicici, data = priklad1)

#2. Pøíklad
#Na základì údajù v souboru data_studenti.csv posuïte za 
#pomoci testu, zda se liší støední délka chodidla u studentù 
#rùzných fakult. Jak silný je vztah mezi tìmito dvìma 
#promìnnými?

data <- read.csv("data_studenti.csv", sep = ";")

summary(aov(Délka.chodidla ~ Fakulta, data = data))

#H0: Mu1 = Mu2 = Mu3
#H1: non H0

#F = 1.815

#p-hodnota =  0.176 > alfa = 0.05
#Nezamítám H0 ve prospìch H1


#P^2 = 20.22/(20.22 + 228.41) = 0.081
#8.1 % variability délky chodidla je vysvìtlitelné fakultou studenta.

boxplot(Délka.chodidla ~ Fakulta, data = data)

#3. Pøíklad
#V následující tabulce jsou vybrané údaje z provedené analýzy 
#rozptylu. Jednalo se o test, zda-li se liší spotøeba 
#automobilu pøi použití rùzných typù benzínu. Doplòte 
#tabulku a zjistìte z ní následující údaje:

#OBECNÌ:
#           Df   Sum   Mean Sq    F value     Pr(>F)
#Palivo     k-1  Sy.m  Sy.m/(k-1) MS(1)/MS(2) p-hodnota
#Residual   n-k  Sy.v  Sy.v/(n-k) 

#V pøíkladu
#           Df   Sum   Mean Sq   F value Pr(>F)
#Palivo         0.164  0.05462          0.01164
#Residual   12         0.0096

#MS = Sum/Df -> Df = Sum/MS = 0.164/0.05462 = 3
#Sum = MS*Df = 12*0.0096 = 0.1152
#F = 0.05462/0.0096 = 5.690

#a.) Poèet celkových pokusù (testovacích jízd)
#n = 16

#b.) Poèet testovaných typù benzínu
#k = 4

#c.) Množství variability vysvìtlené modelem
#P^2 =  0.164/(0.164+0.1152) = 0.587; 58.7 % 

#d.) Liší se statisticky významnì (na hladinì významnosti 0,05) 
#spotøeba automobilu v závislosti na typu benzínu?

#H0: Mu1 = Mu2 = Mu3 = Mu4
#H1: non H0

#F = 5.690

#p-hodnota = 0.012  <   alfa = 0.05
#Zamítám H0 ve prospìch H1 na 5% hladinì významnosti

#5. Pøíklad
#Na základì tabulky odhadnìte parametry lineární regresní 
#funkce (pøímky) popisující závislost y na x a zapište pro 
#dané hodnoty x vyrovnané hodnoty y. Udìlejte graf vèetnì 
#sestrojené pøímky.

priklad5 <- data.frame(x = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
            y = c(3, 5, 8, 10, 12, 9, 15, 14, 17, 19, 25, 24))

fit <- lm(y ~ x, data = priklad5)
summary(fit)
#y = 0.06667 + 3.81429*x  

cbind(priklad5$x, fit$fitted.values)  #Vyrovnané hodnoty

plot(priklad5$x, priklad5$y)
abline(fit, col = "red")

#6. Pøíklad
#Na základì údajù v souboru data_studenti.csv sestrojte 
#regresní pøímku popisující závislost výšky na délce chodidla 
#(a délky na výšce chodidla).

fit <- lm(Výška~Délka.chodidla, data = data)

# Y - výška; x - délka chodidla
# Y = 76.356 + 3.897*x

#a.) Otestujte model jako celek a jeho jednotlivé parametry (?? = 0,05).
#H0: Beta0 = c A Beta1 = 0  
#H1: non H0

anova(fit)

#F = 70.3

#p-hodnota = 1.629e-10 <  alfa = 0.05
#H0 zamítám ve prospìch H1

#H0: Beta0 = 0    H1: Beta0 =! 0
summary(fit)
#t = 6.438
#p-hodnota = 9.31e-08 < alfa = 0.05
#H0 zamítám ve prospìch H1

#H0: Beta1 = 0    H1: Beta1 =! 0
#t = 8.386
#p-hodnota = 1.63e-10 < alfa = 0.05
#H0 zamítám ve prospìch H1

#b.) Zhodnote kvalitu modelu pomocí koeficientu determinace (interpretujte význam).
#R^2 = 3776.8/(3776.8+2255.6) = 0.6261 (poèítáno z anova(fit))
#Též multiple R-squared v summary(fit)
#62.6 % variability výšky je vysvìtlitelné modelovanou 
#závislostí na délce chodidla

#c.) Interpretujte vìcnì hodnotu odhadu parametrù b0 a b1.
#b0 = 76.4  - odhadnutá výška (Y) pro délku chodidla (X) 0
#b1 = 3.897 - odhadnutá jednotková zmìna výšky (Y) pøi
#             jednotkové zmìnì délky chodidla (x)

plot(data$Délka.chodidla, data$Výška)
abline(fit, col = "red")

#d.) Odhadnìte (støední délku chodidla u výšky 180 cm) a støední výšku u délky chodidla 22 cm.
#76.3559 + 3.8974*22 = 162.1
fit$coefficients[1] + fit$coefficients[2]*22 #162.1
predict(fit, newdata = list(Délka.chodidla = 22)) #162.1



# 4
priklad4 <- data.frame(naklady = c(835,63,240,1005,184,213,313,658,195,545),
                       cena = c(136,24,52,143,42,43,67,106,61,99))
fit <- lm (naklady~cena,priklad4)
fit
# otestovani jako celek : H0: B0 je konstanta a B1 = 0, H1: non H0
anova(fit) #p-value je mensi nez alfa 0,05 -> zamitame H0 coz je pro nas dobre

# otestovani B0: H0: B0 =0, H1: B0 != 0
summary(fit) #Intercept t value neni 0, zamitam H0
 
# otestovani B1: H0: B1 =0, H1: B1 != 0
summary(fit) #y t value neni 0, zamitam H0

# Pokud vsechny H0 zamitnu, je model uspesne otestovan

# Zhodnoceni kvality pomoci koeficientu
summary(fit) # Multiple R-squared = 0.97 tzn 97% variability nakladu na udrzbu je vysvetlitelne modelovanou zavislosti na cene domu

# Interpretace odhadu parametru
fit
# b0 = -160 = odhadnute naklady pri cene domu 0
# b1 = 7.5 = odhadnuta jednotkova zmena nakladu pri jednotkove zmene ceny domu

# Odhad stredni hodnoty
predict(fit, newdata = list(cena = 80)) # Odhadnute naklady budou 445.6




#8. Pøíklad
#Použijte data v R jménem cars. Odhadnìte model regresní 
#paraboly závislosti vzdálenosti do zastavení (dist) na 
#rychlosti (speed). Srovnejte výsledky dílèích t-testù s 
#celkovým F-testem. Odhadnìte modely pøímky a paraboly bez 
#lineárního èlenu, porovnejte všechny tøi modely. Následnì 
#pro „nejlepší“ model odhadnìte ujetou vzdálenost pro 
#rychlost 16 mph.

cars

fit1 <- lm(dist ~ speed + I(speed^2), data = cars)
summary(fit1)
#H0: Beta0 = c A Beta1 = 0 A Beta2 = 0
#F-test zamítá H0 
#Všechny t-testy nezamítají H0
#-> rozpor mezi testy (zpùsobeno multikolinearitou)

fit2 <- lm(dist ~ speed, data = cars)
fit3 <- lm(dist ~ I(speed^2), data = cars)
summary(fit2)
summary(fit3)
#Srovnání modelù - R^2(adj) 
#Parabola - R^2(adj) = 0.6532
#Pøímka - R^2(adj) = 0.6438
#Parabola bez lin. èlena - 0.6589 NEJVYŠSÍ! 
#8.86 + 0.12897*16^2 = 41.9
predict(fit3, newdata = list(speed = 16))

