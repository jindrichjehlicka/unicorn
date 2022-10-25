#https://github.com/Rdelrio164/R_Practice/blob/master/USArrests_Regression_2.28.17.R
# https://rpubs.com/elizabrikmane/secondreport


arrests <- as.data.frame(USArrests)
attach(arrests)
scatterplot(Murder, UrbanPop)
scatterplot(Assault, UrbanPop)

pairs(arrests, panel = panel.smooth, main = "USArrests data") #

reg <- lm(UrbanPop ~ Murder +
  Assault +
  Rape
  , data = arrests)

summary(reg)
test <- lm(formula = Murder ~ Assault + UrbanPop, data = USArrests)
# normalita je shapiro wilkuv test vetsi nez 0.05 a kdyz delas to prvni tak mensi nez 0.05
summary(test)
# analyza rezidui
plot(reg$residuals)
hist(reg$residuals)
shapiro.test(reg$residuals) #vic jak 0.05 aby byla normalne rozdelena

#jacknife rezidua
jack <- studres(reg)
ggplot(reg, aes(x = .fitted, y = jack)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red")
which(jack > 3 | jack < (-3)) # Alaska(2) -> asi nechame

# projekcni matice H + leverages
lev <- lm.influence(reg)$hat
p <- sum(lev) # soucet leverages = pocet parametru
which(lev > 2 * p / 60) # identifikace vlivnych pozorovani

# korelacni matice X  - multikolinearita + VIF faktory
X <- model.matrix(reg)    # matice vysvetlujicich promennych (vcetne konstanty)
X
Xb <- X[, 2:4]    # matice vysvetlujicich promennych (bez konstanty)
R <- cor(Xb) # korelacni matice
print(R)
library(corrplot)
corrplot(R, "number")

# detekce multikolinearity
# 1. moznost - determinant matice R
det(R)

# 2. moznost - VIF faktory
Sys.setenv(RGL_USE_NULL = TRUE)
library(matlib) #????
R_I <- inv(R) # inverzni matice R-1
print(R_I)
VIF4_1 <- diag(R_I)
VIF4_1 > 5 # kdyz neni multikolinearita -> u vsech promennych musi byt FALSE
which(VIF4_1 > 5) # kdyz neni multikolinearita -> musi vyjit prazdna mnozina

# 3. moznost - testy multikolinearity
library(mctest)
omcdiag(reg) # optimalni vysledek same nuly u vsech testu

plot(reg$residuals)
# testy heteroskedasticity -> vetsi nez hladina zavislosti (1%,5%,10%)
ols_test_breusch_pagan(reg) # H0 nema byt zamitnuta
ols_test_f(reg)  # H0 nema byt zamitnuta
ols_test_score(reg)  # H0 nema byt zamitnuta