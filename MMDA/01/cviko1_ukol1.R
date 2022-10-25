##### SAMOSTATNÁ PRÁCE
## 1. zjistěte jaký aktuálně používáte adresář
# vytvořte ve svém počítači složku "cviceni_r"
# nastavte jako výchozí adresář tuto složku
# vytvořte v této složce "novy_soubor.txt"
# napište do tohoto souboru větu
# vymažte tuto složku

getwd()
dir.create("./cviceni_r")
setwd("./cviceni_r")
file.create("../../Other/novy_soubor.txt")
line <- "Hello world!"
write(line, file = "../../Other/novy_soubor.txt", append = TRUE)
setwd("./unicorn")
getwd()
unlink("./cviceni_r", recursive = TRUE)


## 2. ověřte, že je číslo a menší než číslo b
# a je zbytek po dělení 4 z druhé odmocnina ze 729
# b je třetí odmocnina z 923

a <- sqrt(729) %% 4
?sqrt
b <- exp(log(923) / 3) #treti odmocnina
b <- 923^(1 / 3) #treti odmocnina

a < b

class(a)

is.numeric(a)

b <- "2"
is.numeric(b)
class(b)
as.numeric(b)

g <- numeric()
g

# integer
ab <- 5L
is.integer(ab)

ad <- 5i
class(ad)

z <- 2
v <- c(1, 3)
z * v
z^v

y <- 1:10

y[y %% 2 == 0]

tail(y, n = 1)
head(y, n = 2)


seq(from = 1, to = 10, by = 1)
seq(from = 1, to = 10, length.out = 3)

rep(2, times = 10)

rep(c(1, 2, 3), times = 3)
rep(c(1, 2, 3), each = 3)
rep(c(1, 2, 3), length.out = 9)


letters
LETTERS
paste0("", letters)

M <- matrix(1:9, nrow = 3, ncol = 3)
# M <-t(M)
dim(M)
class(M)
is.matrix(M)

M[1,]
M[, 1]

vec <- 1:9

class(vec)
attributes(vec)
attributes(M)

attributes(vec)$dim <- c(3, 3)

X <- matrix(1:4, ncol = 2)
X
colnames(X) <- c("C1", "C2")
rownames(X) <- c("R1", "R2")

Y <- matrix(1:4, ncol = 2)

X * Y
X %*% Y

vec_a <- 1:6
vec_b <- 6:1

mat1 <- rbind(vec_a, vec_b)
mat1
mat2 <- cbind(vec_a, vec_b)
mat2

X <- matrix(data = rnorm(27), nrow = 3)
colnames(X) <- paste0(c("a", "b"), 1:9)
X[, c("a1", "a3")]
rowSums(X)
colSums(X)
mean(X)
rowMeans(X)


M <- matrix(c(NA, 1, NA, 1, NA, 2, NA, NA, NA, 1, 2, 3), ncol = 4)
M
which(is.na(M), arr.ind = T)

?Titanic
titanicDataFrame <- as.data.frame(Titanic)
Titanic[, , "Child", "Yes"]
sum(Titanic[, , "Adult",])
sum(Titanic[, , "Adult", "No"])

died <- sum(Titanic[, , "Adult", "No"]) / sum(Titanic[, , "Adult",]) * 100
died

# příklad 1

X <- matrix(1:10, ncol = 10, nrow = 5)

colnames(X) <- paste0(c("a", "b", "c", "d", "e"), 1:10)
# vyberte sloupce,  které mají v názvu písmeno "a"
X[, substring(colnames(X), 1, 1) == "a"]
colnames(X)[grepl('a', colnames(X))]

# vyberte sloupce, které mají v názvu písmena "a" a nebo "c"

colnames(X)[grepl("a", colnames(X)) |
              grepl("b", colnames(X)) |
              grepl("c", colnames(X))]

# vyberte sloupce, které mají v názvu číslo 2 nebo 3
colnames(X)[grepl(c("2"), colnames(X)) | grepl(c("3"), colnames(X))]


# příklad 2

# vytvořte sekvenci čísel od 10 do 100 po 2.5
seq(from = 10, to = 100, by = 2.5)

# vytvořte sekvenci čísel od 10 do 100 o 14 hodnotách
seq(from = 10, to = 100, length.out = 14)

### příklad 3

# matice 0

M <- matrix(0, ncol = 2, nrow = 2)
M
a <- 10

b <- 14

c <- 22

d <- 11


# vložte do matice M hodnoty následovně

# a d

# c b
M[1, 1] <- a
M[1, 2] <- d
M[2, 1] <- c
M[2, 2] <- b
M[1,] = c(a, d)
M[2,] = c(c, b)

# příklad 4
matrix(0, ncol = 2, nrow = 2)
# vytvořte čtvercovou matici 9x9, ve které se opakují čísla 3, 6 a 9
P <- matrix(data = c(3, 6, 9), nrow = 9, ncol = 9)
# pojmenujte sloupce čísly od 1 do 9
colnames(P) <- 1:9
# pojmenujte řádky písmeny od a po i
rownames(P) <- paste0("", letters[1:9])
rownames(P) <- letters[1:9]
P
# vypiště hodnotu, která je v poli e5
P["e", "5"]
# ověřte, jestli má hodnota v poli e5 menší hodnotu než ta v poli i3
P["e", "5"] < P["i", "3"]


# příklad 5
?Titanic
# kolik bylo na Titaniku dospělých mužů?
sum(Titanic[, "Male", "Adult",])
# kolik bylo na Titaniku mužů a chlapců?
sum(Titanic[, "Male", ,])
# kolik bylo na Titaniku žen ve 3. třídě, které přežily
sum(Titanic["3rd", "Female", "Adult", "Yes"])

rm(list = ls())

#------------------------------------------------------------
?mtcars
head(mtcars)
tail(mtcars)

mtcarsDataFrame <- as.data.frame(mtcars)
mtcarsDataFrame

mtcarsDataFrame$name <- rownames(mtcarsDataFrame)
mtcarsDataFrame
view(mtcarsDataFrame)


plot(mpg ~ hp, data = mtcars)
mtcars$carb

#---------------------------
# myDataFrame
name <- c("John", "Paul", "George", "Ringo")
instrument <- c("guitar", "bass", "guitar", "drums")
birth <- c(1940, 1942, 1943, 1940)
death <- c(1980, NA, 2001, NA)
alive <- c(F, T, F, T)

beatles <- data.frame(name, instrument, birth, death, alive)
rownames(beatles) <- beatles$name
beatles

beatles$deathAge = beatles$death - beatles$birth
beatles

# ----------------------------------
# vyberte všechna auta s 6 cylindry
?mtcars
mtcars[mtcars$cyl == 6,]
#všechna auta, která mají 6 cylindrů a hp menší než 110
mtcars[mtcars$cyl == 6 & mtcars$hp < 110,]

# jaká je váha auta, které má 6 cylindrů a hp menší než 110?
mtcars[mtcars$cyl == 6 & mtcars$hp < 110,]$wt
#vyberte všechna auta, která mají více než 6 cylindrů a manuální převodovku (am = 1)
mtcars[mtcars$cyl > 6 & mtcars$am == 1,]
# nebo méně než 6 cylindrů a automatickou převodovku
mtcars[mtcars$cyl < 6 & mtcars$am == 0 | mtcars$cyl > 6 & mtcars$am == 1,]

# vypište pouze názvy předchozích vybraných aut
# rownames(mtcars[mtcars$cyl < 6 & mtcars$am == 0,])
rownames(mtcars[mtcars$cyl < 6 & mtcars$am == 0 | mtcars$cyl > 6 & mtcars$am == 1,])