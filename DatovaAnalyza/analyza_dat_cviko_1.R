#instalace balíčků
install.packages("tidyverse")
library(tidyverse)

print("hello world")

help(rlm, "MASS")
help.search("mtcars")

args(lm)
args(mean)
example(mean)

RSiteSearch("mean")
args(print)

print(x = "hello", quote = FALSE)

a <- 9
a = 9


getwd()
# setwd("DIRECTORY")
dir()

dir.create("../test")
dir()
getwd()
setwd("//Users//jindrichjehlicka//dev//unicorn")
file.create("../test/test.txt")


2 * 3
2^3

9 %/% 4


all.equal(0.3, 0.1 + 0.2)
9 %% 4 #modulus

sqrt(9)
factorial(4)

exp(5)

sin(4)
cos(4)
rnorm(1)
rnorm(10, mean = 10, sd = 1)
set.seed(10)
hist(rnorm(30, mean = 10, sd = 1))

1 == 1

1 == 2 / 2

all.equal(0.3, 0.1 + 0.2)

T & T
F | T

#%in%
c("Jana", "Petr") %in% c("Jana", "Karel")
"Jana" %in% c("Jana", "Karel")



