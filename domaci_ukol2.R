# Deskriptivni analyza
#install.packages("pastecs")
library(pastecs)
MMDA_02_data <- read.csv("MMDA_02_data.csv", sep = ";", dec = ".")
MMDA_02_data

nox_nonwhite_matrix <- as.matrix(MMDA_02_data[c("Nonwhite", "NOX")])
nox_nonwhite_matrix
nonwhite <- MMDA_02_data$Nonwhite
nox <- MMDA_02_data$NOX

mean(nox) #22.65
mean(nonwhite)# 11.87
sd(nox) #46.33329
sd(nonwhite)#8.921148

one <- as.matrix(rep(1, dim(nox_nonwhite_matrix)[1])) #jednotkovy vektor
n <- dim(nox_nonwhite_matrix)[1]
nox_nonwhite_bar <- 1 / n * t(nox_nonwhite_matrix) %*% one ## vektor prumeru
nox_nonwhite_bar

cov_x <- cov(nox_nonwhite_matrix)
cor_x <- cor(nox_nonwhite_matrix)

hist(nox)
hist(nonwhite)
require(graphics)
vzdalenost <- mahalanobis(nox_nonwhite_matrix, nox_nonwhite_bar, cov_x, inverted = FALSE)
vzdalenost

which(vzdalenost > 12)
MMDA_02_data[29,]
# City Rainfall Education Popden Nonwhite NOX SO2 Mortality
# 29 losangCA       11      12.1   4700      7.8 319 130     861.8

MMDA_02_data[48,]
# City Rainfall Education Popden Nonwhite NOX SO2 Mortality
# 48 sanfrnCA       18      12.2   4253     13.7 171  86     911.7

plot(density(vzdalenost, bw = 0.3), main = "Squared Mahalanobis distances"); rug(vzdalenost)

length(rownames(MMDA_02_data))
filtered_nox_nonwhite_matrix <- MMDA_02_data[-29,]
filtered_nox_nonwhite_matrix

mean(filtered_nox_nonwhite_matrix$NOX) #17.62712
mean(filtered_nox_nonwhite_matrix$Nonwhite) #11.93898
sd(filtered_nox_nonwhite_matrix$NOX) #25.37573
sd(filtered_nox_nonwhite_matrix$Nonwhite) #8.981571



education_popden_matrix <- as.matrix(MMDA_02_data[c("Education", "Popden")])
education_popden_matrix
shapiro.test(education_popden_matrix)

mshapiro.test(t(education_popden_matrix))
?mshapiro.test
?shapiro.test

one <- as.matrix(rep(1, dim(education_popden_matrix)[1])) #jednotkovy vektor
n <- dim(education_popden_matrix)[1]
education_popden_bar <- 1 / n * t(education_popden_matrix) %*% one ## vektor prumeru
education_popden_bar

cov_x <- cov(education_popden_matrix)
cor_x <- cor(education_popden_matrix)
vzdalenost_mh <- mahalanobis(education_popden_matrix, education_popden_bar, cov_x, inverted = FALSE)
qqplot(qchisq(ppoints(60), df = 6), vzdalenost_mh)
?qchisq
abline(0, vzdalenost_mh, col = 'gray')

hist(MMDA_02_data$Popden, probability = TRUE)
lines(density(MMDA_02_data$Popden), col = "red")

# Jak se změní výběrový
# průměr a výběrová směrodatná odchylka, pokud u těchto proměnných odlehlé
# pozorování vyloučíte?