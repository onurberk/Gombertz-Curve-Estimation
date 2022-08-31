library(readxl)
library(dplyr)
a_knw <- read_excel("C:/Users/Onur/OneDrive/Masaüstü/a.xlsx")
a_knw$lnCO <- log(a_knw$CO,base = exp(1))
a_knw$lnS <- log(500,base = exp(1))
a_knw$ystar <- a_knw$lnS-a_knw$lnCO#lnS - LnCo#
a_knw$yhat <- log(a_knw$ystar,base = exp(1))

lpm <- lm(yhat~GNPPC,data=a_knw)
summary(lpm)

A <- exp(1.010e+00)
B <- 2.961e-05
S <- 500

CO_estimatefunc <- function(x){S*exp(-A*exp(-B*x))}
a_knw$CO_est <- CO_estimatefunc(a$GNPPC)
a
library("writexl")
write_xlsx(a_knw,"C:/Users/Onur/OneDrive/Masaüstü/a_gce.xlsx")