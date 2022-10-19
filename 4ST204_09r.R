#H0: Pi1 = 1/6; Pi2 = 1/6; Pi3 = 1/6; Pi4 = 1/6; Pi5 = 1/6; Pi6 = 1/6
#H1: non H0

nj <- c(7, 9, 10, 6, 15, 13) #Pozorovan? ?etnosti - O
n <- sum(nj) #Po?et pozorov?n? - 60
Pi0j <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6) #Nulov? hypot?za
E <- n*Pi0j #O?ek?van? ?etnosti

G <- sum((nj-E)^2/E) #G = 6 

#W(alfa) = {g; g => chikv(1-alfa)}
#alfa = 5, k = 6
qchisq(0.95, 5) #11.07
#W(0.05) = {g; g => 11.07}

#Hodnota testov?ho krit?ria nepat?? do kritick?ho oboru
#H0 nezam?t?m ve prosp?ch H1 na 5% hladin? v?znamnosti

#FUNKC?
chisq.test(nj, p = Pi0j)

#G = 6, df = 5 (stupn? volnosti), p-value = 0.3062
#p-hodnota <= alfa   ->     H0 zam?t?m
#p-hodnota > alfa    ->     H0 nezam?t?m

#p-hodnota = 0.3062 > alfa = 0.05   
#H0 nezam?t?m ve prosp?ch H1 na 5% hladin? v?znamnosti

#2. P??klad
#V p?edlo?sk?m semestru se z??astnilo zkou?ky z p?edm?tu 
#4ST201 ? Statistika 1491 student?, z nich? 7,5 % dostalo 
#jedni?ku, 24,5 % dvojku, 37 % trojku a 31 % ?ty?ku nebo 
#bylo omluveno.Z m?ch cvi?en? dostalo jedni?ku 6 student?,
#dvojku 8 student?, trojku 16 student? a ?ty?ku nebo omluveno 
#10 student?.
#Otestujte na 5% hladin? v?znamnosti tvrzen?, ?e rozlo?en? 
#zn?mek student? z m?ch cvi?en? odpov?d? celo?kolsk?mu 
#rozlo?en? zn?mek.

#H0: Pi1 = 0.075, Pi2 = 0.245, Pi3 = 0.37, Pi4 = 0.31
#H1: non H0

Pi0j <- c(0.075, 0.245, 0.37, 0.31) #H0
nj <- c(6, 8, 16, 10) #Pozorovan?
n <- sum(nj) #40
E <- n*Pi0j  #O?ek?van?

#1. o?ek?van? ?etnost je pod 5 -> slou?it

chisq.test(nj, p = Pi0j) #Chybov? hl?ka (approx. may be incorrect)

#H0: Pi1 = 0.32, Pi2 = 0.37, Pi3 = 0.31
Pi0j <- c(0.32, 0.37, 0.31) 
nj <- c(14, 16, 10)
chisq.test(nj, p = Pi0j)

#G = 0.674, df = 2, p-value = 0.7138
#p-hodnota = 0.7138 > alfa = 0.05
#H0 nezam?t?m ve prosp?ch H1 na 5% hladin? v?znamnosti.

#3. P??klad
#Vytvo?te kontingen?n? tabulku popisuj?c? sdru?en? rozd?len? 
#?etnost? prom?nn?ch Zamet?n? a ?upl?k. Otestujte na 5% 
#hladin? v?znamnosti hypot?zu o nez?vislosti t?chto dvou 
#prom?nn?ch. Dodr?te p?edpoklady dan?ho testu. Ur?ete hodnotu 
#Pearsonova kontingen?n?ho koeficientu C i Cram?rova V. 
#Interpretujte v?sledek.

t <- table(data$Zaamet?n?, data$?upl?k) #Pozorovan? ?etnosti nij

#H0: znaky jsou nez?visl?
#H1: non H0


ni. <- rowSums(t) #Sou?ty ??dk?
n.j <- colSums(t) #Sou?ty sloupc?
n <- sum(t)       #Po?et pozorov?n?

E <- ni.%*%t(n.j)/n #O?ek?van? ?etnosti (nij?); jsou < 5
chisq.test(t) #chybov? hl?ka

#Je pot?eba slou?it sloupce a ??dk? L+O
data$Zamet?n?2 <- data$Zaamet?n?
data$Zamet?n?2[data$Zamet?n?2 == "L"] <- "O"

data$?upl?k2 <- data$?upl?k
data$?upl?k2[data$?upl?k2 == "L"] <- "O"

t <- table(data$Zamet?n?2, data$?upl?k2) #Pozorovan? ?etnosti nij

#H0: znaky jsou nez?visl?
#H1: non H0

ni. <- rowSums(t) #Sou?ty ??dk?
n.j <- colSums(t) #Sou?ty sloupc?
n <- sum(t)       #Po?et pozorov?n?
E <- ni.%*%t(n.j)/n #O?ek?van? ?etnosti (nij?)

G <- sum((t-E)^2/E)  #G = 10.749

#W(alfa) = {g; g => chikv(1-alfa)}
#alfa = 0.05, r = 2, s = 2 -> (r-1)(s-1) = 1 stupe? volnosti
qchisq(0.95, 1) #3.841
#W(0.05) = {g; g => 3.841}
#Hodnota testov?ho krit?ria pat?? do kritick?ho oboru
#H0 zam?t?m ve prosp?ch H1 na 5% hladin? v?znamnosti

chisq.test(t, correct = FALSE)
#G = 10.749, df = 1, p-value = 0.001
#p-hodnota = 0.001 <- alfa = 0.05 
##H0 zam?t?m ve prosp?ch H1 na 5% hladin? v?znamnosti

C <- sqrt(G/(G+n)) #C = 0.443 C -> <0; Cmax>
#m = min(r,s) = min(2, 2) = 2
V <- sqrt(G/(n*(2-1))) #V = 0.494 V -> <0; 1>

#Interpretace -> porovn?n? pozorovan?ch a o?ek?van?ch ?estnost?
t
E

#4. P??klad
#Byly sledov?ny rodinn? stavy nev?st a ?enich? p?i uzav?r?n? 
#s?atk? a byla vytvo?ena n?sleduj?c? tabulka ?etnost?. 
#Zjist?te na 1% hladin? v?znamnosti, zda existuje statistick? 
#z?vislost mezi rodinn?m stavem ?enicha a nev?sty. 
#Vypo??tejte m?ru t?snosti t?to z?vislosti. Interpretujte 
#pozorovan? vztah.

t <- matrix(c(37, 10, 6, 
              8, 12, 8, 
              5, 8, 6), ncol = 3)
t <- as.table(t)
chisq.test(t) #Chyba v o?ek?van?ch ?etnostech

ni. <- rowSums(t) #??rky
n.j <- colSums(t) #sloupce
n <- sum(t) #po?et pozorov?n?
E <- ni.%*%t(n.j)/n  #O?ek?van? ?etnosti
#Jedin? bu?ka, relativn? bl?zko 5 -> nebudeme m?nit tabulku

chisq.test(t)
#G = 17.822, df = 4, p-value = 0.0013
#p-hodnota = 0.0013 < alfa = 0.01
#H0 zam?t?m ve prosp?ch H1 na 1% hladin? v?znamosti

C <- sqrt(17.822/(17.822+n)) #C = 0.389
#r = 3, s = 3 -> m = min(r,s) = 3
V <- sqrt(17.822/(n*(3-1))) #V = 0.299 
  
t
E