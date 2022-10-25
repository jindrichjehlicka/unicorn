# Video2

# Atomicke vektory a matice
c(1, 3, 3)

c(TRUE, FALSE, TRUE)

c("Karel", "Jana", "Pavla")

c(2+2i, 1+3i)

x = c(1, 2, 10, 11, 20, 22)
x
str(x)
length(x)
summary(x)
head(x,3)
tail(x,2)
x[1]
x[1:3]
x[3:1]

names(x) = c("jedna", "dva", "deset", "jedenact", "dvacet", "dvacetdva")
x

x["dva"]
x[c("dva","jedna")]
x[c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)]


y = c("a", "b", "c", "d")
u = c("e", "f", "g")
v = c(y, u)
v
append(y,"jedna")


z = c(1, "a", TRUE)
z
class(z)
str(z)

integer(6)
numeric(5)
logical(4)
complex(3)
character(2)
raw(2)

1:10
c(1:10)
x = seq(from = 1, to = 10, by = 1)
seq(from = 1, to = 10, length.out = 4)
rep(x = 1, times = 10)
rep(x = c(1, 2), times = 4)
rep(x = c(1, 2), times = c(2, 4))


set.seed(1)
z = sample(x = 1:100, size = 10)
z
sort(z)
sort(z, decreasing = TRUE)
order(z)
order(z, decreasing = TRUE)

x = vector(mode = "character", length = 3)
print(x)
x[2] = "dva"

M <- matrix(c(1:20), nrow = 5, ncol = 4)
M
colnames(M) <- c("jedna","dva", "tri", "ctyri")
M
rownames(M) <- paste0("radka_", 1:5)
M


M[3, 2]
M[2:3,c(1,3)]
M[2:3,]
M[,3]
M[1]
M[6]
c(M)[1]

class(M)
M[,2]
class(M[,2])
is.vector(M)
is.vector(M[,2])
M["radka_2",]
M["radka_2", "dva"]

M
M[,2]
M[,2]["radka_1"]
M["radka_1",2]

dim(M)
nrow(M)
ncol(M)
attributes(M)

cbind(rep(1,3),rep(2,3))
rbind(c(1,2),c(3,4))


# Zakladni matematicke operace
1 + 3
2 - 3
2 / 3
3 * 2
2 ^ 3
4 ^ (1/2)

log(2)
exp(2)
sqrt(4)

factorial(4)
choose(3,2)

7 %/% 2
7 %% 2

X = matrix(sample(1:100,9), ncol = 3)
X
t(X)
sum(X)
colSums(X)
rowSums(X)
colMeans(X)
rowMeans(X)
summary(X)

solve(X)
solve(X,c(1,2,3))
solve(X) %*% c(1:3)

X + 1000
X + c(1,100, 1000)
X - c(1,100, 1000)
X * c(1,100, 1000)
X / c(1,100, 1000)

t(t(X)  / c(1, 100, 1000))

x = 1.12345
round(x, digits = 2)
floor(x)
ceiling(x)

x = c(1, 3, 2)
y = c(2, 1, 4)

sd(x)
var(y)
cor(x,y)
cov(x,y)
mean(x)
median(x)
min(x)
max(x)

z = c(1, 1, 0, 1, 1, 0, 1, 1, 0, 0)
unique(z)
table(z)

head(mtcars)
table(mtcars$cyl, mtcars$am)

## Logicke operace
TRUE
FALSE
!TRUE
!FALSE

1 == 2
2  < 1
2 >= 2
3 <= 5
4 != 4

TRUE & TRUE
TRUE & FALSE
TRUE | FALSE
FALSE | FALSE

x = 1:10
x
x < 5
x >= 3

x[ x < 5]
x[ x > 3 & x < 8]

head(mtcars)
mtcars[mtcars$disp > 300, ]

x %in% c(2, 4, 5)
colnames(mtcars)

mtcars[mtcars$disp > 300 & mtcars$mpg < 15 , c("mpg","disp","am")]

x = 0.3
y = 0.1 *3
x == y
abs(x - y)
abs(x - y) <= 0.000000001
all.equal(x, y)


## Datove struktury
x = data.frame(id = c(1:4),jmeno = c("karel", "pavel", "jan", "aneta"), 
               val = c(10, 6, 13, 2), ucast = c(TRUE, FALSE, FALSE, TRUE))
x
class(x)
str(x)
colnames(x)
rownames(x)
summary(x)
ncol(x)
nrow(x)
dim(x)
is.data.frame(x)
as.matrix(x)

U = matrix(1:9, 3)
as.data.frame(U)
x[1,]
x$id

muj_list = list(jedna = c(1, 3), dva = c(TRUE, FALSE, FALSE), 
                tri = matrix(0, 5, 2))
muj_list
class(muj_list)
muj_list$dva
class(muj_list$dva)
muj_list[2]
class(muj_list[2])
muj_list[[2]]
class(muj_list[[2]])
names(muj_list)
str(muj_list)
z = append(muj_list, 10)
z
as.list(c(1,2))
is.list(muj_list)
unlist(muj_list)

muj_faktor = factor(x = c(0, 1, 1, 0, 0), levels = c(1, 0), 
                    labels = c("Zena", "Muz"))
muj_faktor
as.character(muj_faktor)
as.numeric(muj_faktor)
x = as.factor(c("Karel", "Jana", "Jana", "Jana", "Karel"))
unique(c("Karel", "Jana", "Jana", "Jana", "Karel"))
unique(x)
as.numeric(x)


## Co je funkce
x = c(1,0,3)
y = c(3:1)

cor(x = x, y = y)
cor(y = y, x = x)
cor(x, y)
cor(x = x, y)
cor(x = x, y = y, method = c("kendall"))
args(cor)

y = mtcars$mpg
X = cbind(mtcars$wt,mtcars$am)

args(lm)
fit <- lm(y ~ X)
print(fit)
str(fit)
Xc = cbind(1,X)
solve(t(Xc) %*% Xc) %*% t(Xc) %*% y

# Uzitecne funkce:
ls()
setwd("//Users//karelsafr//Desktop//Uvod_do_R//Skripty")
getwd()
setwd("..//")
getwd()
dir()
x = 1:10
rev(x)
rm(x)
rm(list=ls())
rm(list= ls(all.names = TRUE))
# ukladani a nacitani:
# flat:
nas_dataframe = data.frame(id = 1:3, jmeno = c("Aneta","Pavla", "Jan"),
                           ucast = c(TRUE, TRUE, FALSE), 
                           znamka = c(1,2,NA))
nas_dataframe
write.table(nas_dataframe, file = "data_saved.csv", sep = ";", dec = ",")
rm(list=ls())
read.table(file = "data_saved.csv")
readLines("data_saved.csv",2)
read.table(file = "data_saved.csv", sep = ";", dec = ",")
nas_dataframe = read.csv2("data_saved.csv") # opak k funkci write.csv2
nas_dataframe
?read.csv
?write.csv

install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)
write_xlsx(nas_dataframe, path = "nas_excel.xlsx")
rm(list=ls())
getwd()
setwd("//Users//jindrichjehlicka//dev//unicorn")
nas_dataframe=read_excel("jesterky.xlsx")
nas_dataframe
nas_dataframe = as.data.frame(nas_dataframe)
nas_dataframe


save(nas_dataframe, file= "saved.RData")
rm(list=ls())
load("saved.RData")
nas_dataframe
x = 1
y = 10
save.image("saved_all.RData")
rm(list=ls())
load("saved_all.RData")


# Chybejici promenne
NA
NaN
Inf
-Inf
NULL

x = c(1,2,NA, 2, NaN, 7, 10, NULL)
class(x)
x
na.omit(x)
complete.cases(x)
x[complete.cases(x)]

mean(x)
mean(x, na.rm = TRUE)

y = c(x, Inf, 10, -Inf)
y

y[is.na(y)]
y[is.nan(y)]
y[is.infinite(y)]
y[is.finite(y)]

nas_dataframe = data.frame(id = 1:3, jmeno = c("Aneta","Pavla", "Jan"), 
                           ucast = c(TRUE, TRUE, FALSE), znamka = c(1,2,NA))
nas_dataframe
summary(nas_dataframe)
complete.cases(nas_dataframe)
sum(complete.cases(nas_dataframe))

nas_dataframe[complete.cases(nas_dataframe),]
is.finite(nas_dataframe[,4])

nas_dataframe[is.na(nas_dataframe[,4]),4] = mean(nas_dataframe$znamka, 
                                                 na.rm = TRUE)

nas_dataframe

Inf
-Inf
1.797693e+308
1.797694e+308
-1.797693e+308
-1.797694e+308


# Grafy:
pressure
class(pressure)
str(pressure)
summary(pressure)

plot(pressure)

plot(x = pressure$temperature, y = pressure$pressure)


plot(x = pressure$temperature, y = pressure$pressure, type = "l")
plot(x = pressure$temperature, y = pressure$pressure, type = "p")
plot(x = pressure$temperature, y = pressure$pressure, type = "b")
plot(x = pressure$temperature, y = pressure$pressure, type = "c")
plot(x = pressure$temperature, y = pressure$pressure, type = "n")

plot(x = pressure$temperature, y = pressure$pressure, type = "l", 
     lwd = 2)
plot(x = pressure$temperature, y = pressure$pressure, type = "l", 
     col = "red")

plot(x = pressure$temperature, y = pressure$pressure, type = "b",
     main = "nadpis"
     )

plot(x = pressure$temperature, y = pressure$pressure, type = "b",
     main = "nadpis", cex.main = 2
)

plot(x = pressure$temperature, y = pressure$pressure, type = "b",
     main = "nadpis", col.main = "red"
)

plot(x = pressure$temperature, y = pressure$pressure, type = "b",
     main = "Nadpis", sub = "Podnadpis", col.sub = "red", cex.sub = 1.2
)

plot(x = pressure$temperature, y = pressure$pressure, type = "l", 
     xlab = "popis osy x", ylab = "text na ose y")

plot(x = pressure$temperature, y = pressure$pressure, type = "b", 
     col = "darkblue",
     cex = 1.5, lwd = 2, main = "Pressure vs Temperature", 
     xlab = "Temperature in C", ylab = "Pressure",
     sub = "Source: Weast, R. C., ed. (1973)", cex.sub = 0.75
     )

head(mtcars)

boxplot(mtcars$qsec,
        main="Krabickovi graf rychlosti aut pri vzdalenosti ctvrt mile",
        sub="Zdroj: Motor Trend US magazine, 1974",
        ylab="sekundy"
)

hist(mtcars$qsec, breaks=8, col="green",
     main="Histogram rychlosti aut pri vzdalenosti ctvrt mile",
     sub="Datovi soubor: mtcars - 1974 Motor Trend magazine",
     ylab="Cetnost", xlab="tridy")

alkohol = matrix(c(1,2,2,3), nrow = 2)
colnames(alkohol) = c("zeny", "muzi")
rownames(alkohol) = c("vino", "pivo")

barplot(alkohol,beside=TRUE,
        legend.text=rownames(alkohol),
        main="Mnozstvi vypiteho alkoholu",
        xlab="Pohlavi osoby",ylab="Spotreba")

