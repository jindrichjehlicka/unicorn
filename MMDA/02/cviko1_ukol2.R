a <- 1:10
b <- paste0("a", 1:10)
c <- rep(c(T, F), 5)
data.frame(a = a, b = b, c = c)

head(iris)
?iris
IrisDT <- iris[iris$Species == "setosa",]
IrisDT


?merge
Y <- data.frame(id = 1:4, names = c("John", "Ringo", "George", "Paul"))
X <- data.frame(id = 4:1, instruments = c("drums", "guitar", "bass", "guitar"))

merge(x = X, y = Y)
intersect(names(X), names(Y))

X <- data.frame(id = 1:5, names = c("John", "Ringo", "George", "Paul", "Karel"))
Y <- data.frame(ID = c(1:4, 6), instruments = c("drums", "guitar", "bass", "guitar", "basson"))

merge(x = X, y = Y) # can't merge

?merge
merge(x = X, y = Y, by.x = "id", by.y = "ID")
merge(x = X, y = Y, by.x = "id", by.y = "ID", all = TRUE)
merge(x = X, y = Y, by.x = "id", by.y = "ID", all.x = TRUE, all.y = FALSE)
merge(x = X, y = Y, by.x = "id", by.y = "ID", all.y = TRUE, all.x = FALSE)


Y <- data.frame(id = 1:4, names = c("John", "Ringo", "George", "Paul"))
X <- data.frame(id = 4:1, names = c("drums", "guitar", "bass", "guitar"))
merged <- merge(x = X, y = Y, by = "id", suffixes = c(".jmeno", ".nastroj"))

colnames(merged)[colnames(merged) == "names.jmeno"] <- "jmeno"
colnames(merged)[3] <- "instrument"
merged

library(tidyverse)


rename(merged, instrument = names.nastroj)

#-------------------------------------------------------------

muj_list <- list(
  mtcars = mtcars,
  vektor = 1:10,
  titanic = Titanic
)
str(muj_list)

muj_list$vektor
class(muj_list$mtcars) # returns dataframe
muj_list$mtcars[1,]

dim(muj_list) # NULL
length(muj_list)

mini_list <- list(
  x = 1,
  xx = 2:3,
  y = 3:4
)
mini_list

class(mini_list[1]) # returns list
class(mini_list[[1]]) # returns numeric

mini_list[[2]][2]
mini_list$xx[2]

#------------------------------------------------------

# příklad: co může být v listu?
#porad: Too Hot to Handle
# soutezici: Francesca Farago, Harry Jowsey, Chloe Veitch,
## Sharron Townsend, Rhonda Paul, David Birtwistle, Kelz Dyke, Nicole O'Brien
# hodnoceni: 10, 5, 1, 2, 5
# reviews: "Maybe the best love reality show there is!",
## "I cannot believe I watched the whole thing...", "A new cultural low",
## "Reality Show Stupidity At It's Finest.",
## "For All The People Questioning Why This Exists"


name <- "Too Hot to Handle"
cast <- c("Francesca Farago", "Harry Jowsey", "Chloe Veitch",
          "Sharron Townsend", "Rhonda Paul", "David Birtwistle",
          "Kelz Dyke", "Nicole O'Brien")
score <- c(10, 5, 1, 2, 5)
comments <- c("Maybe the best love reality show there is!",
              "I cannot believe I watched the whole thing...",
              "A new cultural low",
              "Reality Show Stupidity At It's Finest.",
              "For All The People Questioning Why This Exists")


# vytvořte list, který obsahuje nazev show (show), cast,
# hodnoceni jako DF "reviews" složený ze score a comments
# a průměrné hodnocení (avg_score)

too_hot_to_handle_list <- list(
  show = name,
  cast = cast,
  reviews = data.frame(score = score, comments = comments),
  avg_score = mean(score)
)

too_hot_to_handle_list


x <- factor(
  x = c(1, 3, 2, 1, 3, 2, 1, 3),
  labels = c("jedna", "dva", "tri")
)
faktor <- factor(
  x = c(1, 2, 4, 3, 2, 2, 1),
  levels = c(1:4),
  labels = c("prvni", "druhy", "treti", "ctvrty"),
  order = TRUE
)

faktor

#casova rada - time-series objects
?AirPassengers
?ts
class(AirPassengers)
plot(AirPassengers)

d <- decompose(AirPassengers)
d$seasonal
plot(d$seasonal)

plot(d$trend)
hist(d$figure)
plot(d$random)

install.packages("forecast")
library(forecast)

fit <- auto.arima(AirPassengers)
my_fc <- forecast(fit, 10)
plot(my_fc)


#mesicni casova rada
set.seed(10)
hodnoty <- rnorm(100)
mesic <- ts(hodnoty, start = 2000, frequency = 12)
mesic

ctvrtletni <- ts(hodnoty, start = 2000, frequency = 4)
ctvrtletni
plot(ctvrtletni)

mesic1 <- ts(hodnoty, start = c(2000, 6), frequency = 12)
mesic1

frequency(mesic1)
start(mesic1)
summary(mesic1)
end(mesic1)
cycle(mesic1)


getwd()
# setwd("")
carka <- read.table("../../Other/carka.csv", sep = ",", header = T)
carka

strednik <- read.table("../../Other/strednik.csv", sep = ";", header = T, dec = ",")
strednik

carka2 <- read.csv("../../Other/carka.csv")
carka2
strednik2 <- read.csv2("../../Other/strednik.csv")
strednik2

rozdily <- read.table("../../Other/rozdily.txt", sep = "\t")
rozdily

odkazy <- read.table("../../Other/odkazy.txt")
odkazy

url <- "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.csv"

nakaza <- read.csv(url, header = T)
nakaza

class(nakaza$datum)
nakaza$datum <- as.Date(nakaza$datum, format = "%Y-%m-%d")
class(nakaza$datum)
nakaza$datum

plot(x = nakaza$datum, y = nakaza$kumulativni_pocet_nakazenych, type = "l")

hoste <- read.csv("https://www.czso.cz/documents/62353418/171453756/020064-22data090822.csv/9b850356-996d-4b7c-9884-10b6c383ca7b?version=1.1", header = T)

hoste
install.packages("rvest")
library(rvest)
html_encoding_guess(hoste$stapro_txt)

library(readxl)
excel <- read_xls("../../Other/excel.xls")
excel

excel_data_frame <- as.data.frame(excel)
getwd()
setwd("/")
excel2 <- read_xlsx("../../Other/excel2.xlsx")
excel2 <- read_excel("../../Other/excel2.xlsx", skip = 2)
excel2


my_data_frame <- as.data.frame(excel2)
my_data_frame
soubor <- my_data_frame[my_data_frame$Year >= 2000,]

soubor

write.csv(soubor, file = "../../Other/soubor.csv", row.names = F)
write.table(soubor, file = "../../Other/soubor.txt", sep = ";", dec = "")


dim(mtcars)
?apply
apply(X = mtcars, MARGIN = 2, FUN = mean)
apply(X = mtcars, MARGIN = 2, FUN = function(X) {
  mean(X, na.rm = T)
})

apply(X = mtcars, MARGIN = 2, FUN = sum)

apply(X = mtcars, MARGIN = 2, FUN = function(X) {
  max(X) - min(X)
})

M <- matrix(1:9, 3, 3)
apply(M, 1:2, function(X)X + 10)

#LAPPLY - vraci list
L <- list(a = 1:10, b = 2:5, c = -5:5, d = matrix(1:9, 3, 3))
unlist(lapply(L, sum)) #unlist-> vektor


L2 <- list(A = matrix(1:9, 3, 3), b = matrix(10:18, 3, 3), c = matrix(11:19, 3, 3))
unlist(lapply(L2, FUN = apply, 2, sum))


#SAPPLY - zjednodusena fuknce lapply
sapply(L2, sum)

#VAPPLY - definuje se typ objektu a delka
vapply(L2, sum, FUN.VALUE = numeric(1))
vapply(L2, sum, FUN.VALUE = character(1))
vapply(L2, sum, FUN.VALUE = numeric(2)) #ne

#REPLICATE
set.seed(1)
replicate(n = 5, expr = sample(x = 1:100, size = 3))

#MAPPLY
Q <- matrix(data = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)), nrow = 4, ncol = 4)
mapply(FUN = rep, x = 1:4, times = 4)
mapply(function(x, y) x^y, x = c(1:10), y = 2)

#TAPPLY
iris
#nejmensi listek
tapply(X = iris$Sepal.Length, INDEX = iris$Species, FUN = min)

tapply(X = mtcars$mpg, INDEX = list(mtcars$cyl, mtcars$am), FUN = mean)

#BY - prumer delky okvetnich listku
by(data = iris$Sepal.Length, INDICES = iris$Species, FUN = mean)
by(data = iris$Sepal.Length, INDICES = list(iris$Species, iris$Petal.Length), FUN = mean)

#prumerna spotreba aut pro auta s nad (>) a podprumernou housepower
?mtcars
# my
tapply(mtcars$mpg, INDEX = mtcars$hp > mean(mtcars$hp), FUN = mean)
# prumerna spotreba aut podle poctu cylindru pro auta s nad (>) a podprumernou housepower
tapply(mtcars$mpg, INDEX = list(mtcars$hp > mean(mtcars$hp), mtcars$cyl), FUN = mean)


# uloha

# načtěte dataset uloha.csv
ukol <- read.csv2("../../Other/uloha.csv", header = T)
ukol
# prozkoumejte soubor

class(ukol)
# výška je uvedená v palcích
# vytvořte novou proměnnou, která bude uvádět výšku v cm
inch <- 2.54
class(ukol$Height)
as.numeric(ukol$Height)
ukol$HeightCm <- as.numeric(ukol$Height) * 2.54
ukol$HeightCm
# jaká je průměrně strávená doba u televize
class(ukol$TV)
meanTV <- mean(as.numeric(ukol$TV))
meanTV
# 8.881503
# kolik je v souboru abstinentů?
class(ukol$alcohol)
abstinentsCount <- length(ukol$alcohol[ukol$alcohol == "0"])
#80
# kolik průměrně času stráví muži abstinenti cvičením?
class(ukol$exercise)
tapply(X = as.numeric(ukol$exercise), INDEX = list(ukol$alcohol == "0", ukol$Sex == "Male"), FUN = mean)
tapply(X = as.numeric(ukol$exercise), INDEX = list(ukol$alcohol == "0", ukol$Sex), FUN = mean)
# 4.833333

# a kolik  času stráví u televize?

tapply(X = as.numeric(ukol$TV), INDEX = list(ukol$alcohol == "0", ukol$Sex == "Male"), FUN = mean)
tapply(X = as.numeric(ukol$TV), INDEX = list(ukol$alcohol == "0", ukol$Sex), FUN = mean)
# 8.863636

# vytvořte novou proměnnou, která je TRUE pokud osoba cvičí
# více než 3 hodiny týdně a FALSE, pokud ne
ukol$exercises <- as.numeric(ukol$exercise) > 3
ukol$exercises <- ifelse(uloha$exercise > 3, T, F)

# kolik lidí cvičí více než 3 hodiny týdně?

length(ukol$exercises[ukol$exercises == TRUE])
