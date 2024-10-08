#1. P��klad
#U n�hodn� vybran�ch student� jsme sledovali po�et dosa�en�ch 
#bod� na z�v�re�n�m testu (od 0 do 60). V�dy 4 z t�chto student� 
#chodili k jednomu ze 3 cvi��c�ch � panu Kubovi, panu Kubinovi, 
#nebo panu Kubin��kovi. Prove�te test k posouzen� ot�zky, zda 
#m� osoba cvi��c�ho vliv na po�et dosa�en�ch bod� u z�v�re�n�ho 
#testu? Jak� je s�la vztahu (interpretujte v�znam pou�it�ho 
#ukazatele)?

#H0: Mu1 = Mu2 = Mu3 (nez�vislost)
#H1: non H0


priklad1 <- data.frame(body = c(53, 47, 55, 50, 42, 48, 
                  48, 44, 50, 39, 46, 39, 43, 44, 38),
                  cvicici = c(rep("Kuba", 4), rep("Kubina", 5),
                              rep("Kubincak", 6)))
priklad1

summary(aov(body ~ cvicici, data = priklad1))

#F = 10.41

#OBECN�:
#p-hodnota <= alfa -> H0 zam�t�m ve prosp�ch H1
#p-hodnota > alfa -> H0 NEzam�t�m ve prosp�ch H1

#V p��kladu:
#p-hodnota = 0.00239  <  alfa = 0.05
#Zam�t�m H0 (nez�vislost) ve prosp�ch H1 na 5% hladin� v�znamnosti.

#P^2 = Sy.m/Sy = 231.5/(231.5 + 133.4) = 0.634   
#P^2 <0; 1> 
#63.4 % variability po�tu bod�, kter� je vysv�tliteln� 
#osobou cvi��c�ho.

boxplot(body ~ cvicici, data = priklad1)

#2. P��klad
#Na z�klad� �daj� v souboru data_studenti.csv posu�te za 
#pomoci testu, zda se li�� st�edn� d�lka chodidla u student� 
#r�zn�ch fakult. Jak siln� je vztah mezi t�mito dv�ma 
#prom�nn�mi?

data <- read.csv("data_studenti.csv", sep = ";")

summary(aov(D�lka.chodidla ~ Fakulta, data = data))

#H0: Mu1 = Mu2 = Mu3
#H1: non H0

#F = 1.815

#p-hodnota =  0.176 > alfa = 0.05
#Nezam�t�m H0 ve prosp�ch H1


#P^2 = 20.22/(20.22 + 228.41) = 0.081
#8.1 % variability d�lky chodidla je vysv�tliteln� fakultou studenta.

boxplot(D�lka.chodidla ~ Fakulta, data = data)

#3. P��klad
#V n�sleduj�c� tabulce jsou vybran� �daje z proveden� anal�zy 
#rozptylu. Jednalo se o test, zda-li se li�� spot�eba 
#automobilu p�i pou�it� r�zn�ch typ� benz�nu. Dopl�te 
#tabulku a zjist�te z n� n�sleduj�c� �daje:

#OBECN�:
#           Df   Sum   Mean Sq    F value     Pr(>F)
#Palivo     k-1  Sy.m  Sy.m/(k-1) MS(1)/MS(2) p-hodnota
#Residual   n-k  Sy.v  Sy.v/(n-k) 

#V p��kladu
#           Df   Sum   Mean Sq   F value Pr(>F)
#Palivo         0.164  0.05462          0.01164
#Residual   12         0.0096

#MS = Sum/Df -> Df = Sum/MS = 0.164/0.05462 = 3
#Sum = MS*Df = 12*0.0096 = 0.1152
#F = 0.05462/0.0096 = 5.690

#a.) Po�et celkov�ch pokus� (testovac�ch j�zd)
#n = 16

#b.) Po�et testovan�ch typ� benz�nu
#k = 4

#c.) Mno�stv� variability vysv�tlen� modelem
#P^2 =  0.164/(0.164+0.1152) = 0.587; 58.7 % 

#d.) Li�� se statisticky v�znamn� (na hladin� v�znamnosti 0,05) 
#spot�eba automobilu v z�vislosti na typu benz�nu?

#H0: Mu1 = Mu2 = Mu3 = Mu4
#H1: non H0

#F = 5.690

#p-hodnota = 0.012  <   alfa = 0.05
#Zam�t�m H0 ve prosp�ch H1 na 5% hladin� v�znamnosti

#5. P��klad
#Na z�klad� tabulky odhadn�te parametry line�rn� regresn� 
#funkce (p��mky) popisuj�c� z�vislost y na x a zapi�te pro 
#dan� hodnoty x vyrovnan� hodnoty y. Ud�lejte graf v�etn� 
#sestrojen� p��mky.

priklad5 <- data.frame(x = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
            y = c(3, 5, 8, 10, 12, 9, 15, 14, 17, 19, 25, 24))

fit <- lm(y ~ x, data = priklad5)
summary(fit)
#y = 0.06667 + 3.81429*x  

cbind(priklad5$x, fit$fitted.values)  #Vyrovnan� hodnoty

plot(priklad5$x, priklad5$y)
abline(fit, col = "red")

#6. P��klad
#Na z�klad� �daj� v souboru data_studenti.csv sestrojte 
#regresn� p��mku popisuj�c� z�vislost v��ky na d�lce chodidla 
#(a d�lky na v��ce chodidla).

fit <- lm(V��ka~D�lka.chodidla, data = data)

# Y - v��ka; x - d�lka chodidla
# Y = 76.356 + 3.897*x

#a.) Otestujte model jako celek a jeho jednotliv� parametry (?? = 0,05).
#H0: Beta0 = c A Beta1 = 0  
#H1: non H0

anova(fit)

#F = 70.3

#p-hodnota = 1.629e-10 <  alfa = 0.05
#H0 zam�t�m ve prosp�ch H1

#H0: Beta0 = 0    H1: Beta0 =! 0
summary(fit)
#t = 6.438
#p-hodnota = 9.31e-08 < alfa = 0.05
#H0 zam�t�m ve prosp�ch H1

#H0: Beta1 = 0    H1: Beta1 =! 0
#t = 8.386
#p-hodnota = 1.63e-10 < alfa = 0.05
#H0 zam�t�m ve prosp�ch H1

#b.) Zhodno�te kvalitu modelu pomoc� koeficientu determinace (interpretujte v�znam).
#R^2 = 3776.8/(3776.8+2255.6) = 0.6261 (po��t�no z anova(fit))
#T� multiple R-squared v summary(fit)
#62.6 % variability v��ky je vysv�tliteln� modelovanou 
#z�vislost� na d�lce chodidla

#c.) Interpretujte v�cn� hodnotu odhadu parametr� b0 a b1.
#b0 = 76.4  - odhadnut� v��ka (Y) pro d�lku chodidla (X) 0
#b1 = 3.897 - odhadnut� jednotkov� zm�na v��ky (Y) p�i
#             jednotkov� zm�n� d�lky chodidla (x)

plot(data$D�lka.chodidla, data$V��ka)
abline(fit, col = "red")

#d.) Odhadn�te (st�edn� d�lku chodidla u v��ky 180 cm) a st�edn� v��ku u d�lky chodidla 22 cm.
#76.3559 + 3.8974*22 = 162.1
fit$coefficients[1] + fit$coefficients[2]*22 #162.1
predict(fit, newdata = list(D�lka.chodidla = 22)) #162.1



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




#8. P��klad
#Pou�ijte data v R jm�nem cars. Odhadn�te model regresn� 
#paraboly z�vislosti vzd�lenosti do zastaven� (dist) na 
#rychlosti (speed). Srovnejte v�sledky d�l��ch t-test� s 
#celkov�m F-testem. Odhadn�te modely p��mky a paraboly bez 
#line�rn�ho �lenu, porovnejte v�echny t�i modely. N�sledn� 
#pro �nejlep�� model odhadn�te ujetou vzd�lenost pro 
#rychlost 16 mph.

cars

fit1 <- lm(dist ~ speed + I(speed^2), data = cars)
summary(fit1)
#H0: Beta0 = c A Beta1 = 0 A Beta2 = 0
#F-test zam�t� H0 
#V�echny t-testy nezam�taj� H0
#-> rozpor mezi testy (zp�sobeno multikolinearitou)

fit2 <- lm(dist ~ speed, data = cars)
fit3 <- lm(dist ~ I(speed^2), data = cars)
summary(fit2)
summary(fit3)
#Srovn�n� model� - R^2(adj) 
#Parabola - R^2(adj) = 0.6532
#P��mka - R^2(adj) = 0.6438
#Parabola bez lin. �lena - 0.6589 NEJVY�S�! 
#8.86 + 0.12897*16^2 = 41.9
predict(fit3, newdata = list(speed = 16))

