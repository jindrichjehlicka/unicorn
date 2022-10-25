#ukol jesterky
setwd("//Users//jindrichjehlicka//dev//unicorn")
library(readxl)
nas_dataframe <- read_excel("jesterky.xlsx")

nas_dataframe <- as.data.frame(nas_dataframe)
print(nas_dataframe)
lizardsWeight <- nas_dataframe$hmotnost
lizardsLength <- nas_dataframe$delka
print(lizardsWeight)
mean(lizardsWeight) #prumer

var(lizardsWeight) #rozptyl

sd(lizardsWeight) #smerodatna odchytlka

cvWeight <- sd(lizardsWeight) / mean(lizardsWeight) * 100  # variacni koeficient v procentech * 100
cvWeight
median(lizardsWeight)

quantile(lizardsWeight)

#----------
lengthWithoutNA = lizardsLength[is.finite(lizardsLength)]
print(lengthWithoutNA)
mean(lizardsLength) #prumer

var(lizardsLength) #rozptyl


sd(lizardsLength) #smerodatna odchytlka

cvLength <- sd(lizardsLength) / mean(lizardsLength) * 100  # variacni koeficient v procentech * 100
cvLength

median(lizardsLength)

cor(lizardsWeight, lizardsLength) 

# pomocí korelačního koeficientu charakterizujte vzájemný vztah hmotnosti a délky ještěrek
hist(lizardsWeight)

install.packages("moments")
library(moments)

skewness(lizardsWeight)

install.packages("fungible")
library(fungible)
kurt(lizardsWeight)

length(nas_dataframe$hmotnost)
mean(nas_dataframe$hmotnost)
sd(nas_dataframe$hmotnost)

testHypo <- (13.83928  - 13 ) / (11.01432/ sqrt(40))
testHypo

firstSide <- qnorm(1 - (0.05 / 2))

firstSide
secondSide <- qnorm(0.05 / 2)
secondSide
boxplot(lizardsWeight)

test1 <- 76.475-(1.959964*sqrt(271.4737)/sqrt(40))
test1

test2 <- 76.475+(1.959964*sqrt(271.4737)/sqrt(40))
test2
