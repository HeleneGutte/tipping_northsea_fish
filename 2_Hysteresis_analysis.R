#2_Hysteresis_analysis ----

library(tidyverse)
library(ggrepel)
library(readxl)

#load data
#setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data/Stock_Assessment_Data_2021")
plaice <- read.csv("SA_plaice_2021.csv",
                   sep = ",")
df_hake <- read.csv("SA_hake_2021.csv", sep = ",")
herring <- read.csv("SA_herring_2021.csv",
                    sep = ",")
haddock <- read.csv("SA_haddock_2021.csv",
                    sep = ",")
df_saithe<-read.csv("SA_saithe_2021.csv", sep = ",")
cod <- read.csv("SA_cod_2021.csv",
                sep = ",")

#test for time lags (Ft-n) ####
plaice <- plaice[-65,]
ccf(plaice$SSB, plaice$F_2_6, type = "correlation") #3 years

df_hake <- df_hake[-44,]
ccf(df_hake$SSB, df_hake$F, type = "correlation") #No lag

herring <- herring[-75,]
ccf(herring$SSB, herring$F_2_6, type = "correlation") #No lag

haddock <- haddock[-50,]
ccf(haddock$SSB, haddock$F_2_4, type = "correlation") #No lag

df_saithe <- df_saithe[-55,]
ccf(df_saithe$SSB, df_saithe$F_4_7, type = "correlation") #4 years

cod <- cod[-59,]
ccf(cod$SSB, cod$F_2_6, type = "correlation") #5 years



#Break point analysis ####
opt_bpts <- function(x) {
  #x = bpts_sum$RSS["BIC",]
  n <- length(x)
  lowest <- vector("logical", length = n-1)
  lowest[1] <- FALSE
  for (i in 2:n) {
    lowest[i] <- x[i] < x[i-1] & x[i] < x[i+1]
  }
  out <- as.integer(names(x)[lowest])
  return(out)
}



# 1.Plaice #####
plaice <- read.csv("SA_plaice_2021.csv",
                   sep = ",")


plaice$SSB_lag <- lag(plaice$SSB, 3)

## 1.1.breakpoint analysis#####
bpts <- strucchange::breakpoints(SSB_lag/1000 ~  F_2_6, data = plaice)

plot(bpts)
summary(bpts)

bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #2
bpts2 <- strucchange:: breakpoints(bpts, breaks = opt_brks)
best_brk <- plaice$F_2_6[bpts2$breakpoints]

best_brk # 0.63 0.22

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(SSB_lag/1000 ~ F_2_6, data = plaice, type = "p")
for (i in 1: opt_brks) {
  abline(v = plaice$F_2_6[ci_mod$confint[i,2]], col = "blue")
  abline(v = plaice$F_2_6[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = plaice$F_2_6[ci_mod$confint[i,3]], col = "red", lty = 3)
}
#get corresponding years:
best_brk_years <- plaice$Year[bpts2$breakpoints]
best_brk_years
#1992, 2008


# 2.Hake ####
df_hake <- read.csv("SA_hake_2021.csv", sep = ",")


df_hake<-df_hake %>% mutate(
  R = R_0,
  Fishing = F,
  SSB = SSB/1000)


## 2.1.breakpoint analysis#####
bpts <- strucchange :: breakpoints(SSB ~ Fishing, data = df_hake)


plot(bpts)
summary(bpts)

bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #1 3

bpts2 <- strucchange:: breakpoints(bpts, breaks = opt_brks[1]) #with 1
best_brk <- df_hake$Fishing[bpts2$breakpoints]
best_brk #0.37

bpts2 <- strucchange:: breakpoints(bpts, breaks = opt_brks[2]) #with 3
best_brk <- df_hake$Fishing[bpts2$breakpoints]
best_brk #1.10 0.60 0.25

#Don´t use fishing values but years to derive the breakpoints, as we want to know the corresponding year
best_brk <- df_hake$Year[bpts2$breakpoints]
best_brk # 1993 2008 2014

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks[1]) #with 1
plot(SSB ~ Fishing, data = df_hake, type = "p")
for (i in 1: opt_brks[1]) {
  abline(v = df_hake$Fishing[ci_mod$confint[i,2]], col = "blue")
  abline(v = df_hake$Fishing[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = df_hake$Fishing[ci_mod$confint[i,3]], col = "red", lty = 3)
}


par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks[2]) #with 3
plot(SSB ~ Fishing, data = df_hake, type = "p")
for (i in 1: opt_brks[2]) {
  abline(v = df_hake$Fishing[ci_mod$confint[i,2]], col = "blue")
  abline(v = df_hake$Fishing[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = df_hake$Fishing[ci_mod$confint[i,3]], col = "red", lty = 3)
}


# 3.Herring####
herring <- read.csv("SA_herring_2021.csv",
                    sep = ",")


## 3.1.breakpoint analysis#####
bpts <- strucchange :: breakpoints(SSB/1000 ~ F_2_6, data = herring)


plot(bpts)
summary(bpts)

bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #4
bpts2 <- strucchange:: breakpoints(bpts, breaks = opt_brks)
best_brk <- herring$F_2_6[bpts2$breakpoints]

best_brk #0.27 1.08 0.27 0.22 ---> 2x 0.27

best_brk_years <- herring$Year[bpts2$breakpoints]
best_brk_years
#1957 1968 1983 2000

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(SSB/1000 ~ F_2_6, data = herring, type = "p")
for (i in 1: opt_brks) {
  abline(v = herring$F_2_6[ci_mod$confint[i,2]], col = "blue")
  abline(v = herring$F_2_6[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = herring$F_2_6[ci_mod$confint[i,3]], col = "red", lty = 3)
}


# 4.Haddock#####----
haddock <- read.csv("SA_haddock_2021.csv",
                    sep = ",")
haddock <- haddock[-50,]

## 4.1.breakpoint analysis#####
bpts <- strucchange :: breakpoints(SSB/1000 ~ F_2_4, data = haddock)


plot(bpts)
summary(bpts)

bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #2
bpts2 <- strucchange:: breakpoints(bpts, breaks = opt_brks)
best_brk <- haddock$F_2_4[bpts2$breakpoints]

best_brk #0.91 0.21

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(SSB/1000 ~ F_2_4, data = haddock, type = "p")
for (i in 1: opt_brks) {
  abline(v = haddock$F_2_4[ci_mod$confint[i,2]], col = "blue")
  abline(v = haddock$F_2_4[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = haddock$F_2_4[ci_mod$confint[i,3]], col = "red", lty = 3)
}

best_brk_years <- best_brk <- haddock$Year[bpts2$breakpoints]
best_brk_years
# 1989 2003


# 5.Saithe ####

df_saithe<-read.csv("SA_saithe_2021.csv", sep = ",")

df_saithe <- df_saithe[-55,]

df_saithe<-df_saithe %>% mutate(
  catches = Landings+Discards,
  exploit_rate = catches/SSB,
  SSB = SSB/1000,
  R = R_3/1000,
  R_low = R_low/1000,
  R_high = R_high/1000)

df_saithe$SSB_lag <- lag(df_saithe$SSB, 4)

## 5.1.breakpoint analysis#####
bpts <- strucchange :: breakpoints(SSB_lag ~ F_4_7, data = df_saithe)

plot(bpts)
summary(bpts)

bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #3
bpts2 <- strucchange:: breakpoints(bpts, breaks = opt_brks)
best_brk <- df_saithe$F_4_7[bpts2$breakpoints]

best_brk #0.43 0.65 0.51

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(SSB_lag ~ F_4_7, data = df_saithe, type = "p")
for (i in 1: opt_brks) {
  abline(v = df_saithe$F_4_7[ci_mod$confint[i,2]], col = "blue")
  abline(v = df_saithe$F_4_7[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = df_saithe$F_4_7[ci_mod$confint[i,3]], col = "red", lty = 3)
}

best_brk_years <- df_saithe$Year[bpts2$breakpoints]

best_brk_years #1973 1983 1996



# 6.Cod ####
cod <- read.csv("SA_cod_2021.csv",
                sep = ",")
cod$SSB_lag <- lag(cod$SSB, 5)

## 6.1.breakpoint analysis #####
bpts <- strucchange :: breakpoints(SSB_lag/1000 ~ F_2_6, data = cod)


plot(bpts)
summary(bpts)

bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #5, allerdings ist der BIC Unterschied von 4 zu 5 nur 0.1, daher nehmen wir nur 4 breakpoints
opt_brks <- 4
bpts2 <- strucchange:: breakpoints(bpts, breaks = opt_brks)
best_brk <- cod$F_2_6[bpts2$breakpoints]

best_brk #0.81 1.03 1.14 0.66

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(SSB/1000 ~ F_2_6, data = cod, type = "p")
for (i in 1: opt_brks) {
  abline(v = cod$F_2_6[ci_mod$confint[i,2]], col = "blue")
  abline(v = cod$F_2_6[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = cod$F_2_6[ci_mod$confint[i,3]], col = "red", lty = 3)
}

best_brk_years <- cod$Year[bpts2$breakpoints]

best_brk_years #1972 1983 1999, 2007
