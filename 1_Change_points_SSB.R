#Changepoint analysis SSB

#setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data")
library("bcp")
library("changepoint")
library("tidyverse")

#1. Plaice ----
plaice <- read.csv("SA_plaice_2021.csv",
                   sep = ",")
ssbcpts_plaice <- cpt.mean(data = plaice$SSB, method = "BinSeg", Q = 6) #Beginning Year 1957
plot(ssbcpts_plaice, type = "l", cpt.col = "blue", xlab = "Index", pt.widt = 4)
cpts(ssbcpts_plaice)
#identifies 13 28 35 51 53 56 as changepoint
# Year      1969, 1974,1991,2007,2009,2012
bcp.ssb_plaice <- bcp(plaice$SSB)
plot(bcp.ssb_plaice, main = "SSB")
View(bcp.ssb_plaice$posterior.prob) #Point  51 , 53,56 probability higher than 0.7 -> but in my
#opinion 53 and 56 shows only higher steepness, both represent same regime
#Year:  2007, 2009,2012

#Möllis Vorschlag: Do analysis without huge increase phase
newdata <- plaice%>%
  filter(Year<2009)

ssb2cpts_plaice <- cpt.mean(data = newdata$SSB, method = "BinSeg")
plot(ssb2cpts_plaice, type = "l", cpt.col = "blue", xlab = "Index 1957-2009", pt.width = 4)
cpts(ssb2cpts_plaice)#5, 13, 28, 35, 49

bcpssb2 <- bcp(newdata$SSB)
plot(bcpssb2)
View(bcpssb2$posterior.prob)
# 51


#2. Hake ----
df_hake <- read.csv("SA_hake_2021.csv", sep = ",")
View(df_hake)

ssbcpts_hake <- cpt.mean(data = df_hake$SSB, method = "BinSeg") #Beginning Year 1978
plot(ssbcpts_hake, type = "l", cpt.col = "blue", xlab = "Index", pt.widt = 4)
cpts(ssbcpts_hake)
#cpt:   8,     32,   33,   37    , 40
#Year: 1985,   2009, 2010, 2014,  2017

bcp.ssb_hake <- bcp(df_hake$SSB)
plot(bcp.ssb_hake, main = "SSB")
View(bcp.ssb_hake$posterior.prob)
#cpt:  32,  33,    37   , 40
#Year: 2009, 2010, 2014,  2017


#3. Herring ----
## 1. Total
herring <- read.csv("SA_herring_2021.csv", sep = ",")
View(herring)

### cpt
m.cptSSB_herring <- cpt.mean(herring$SSB,penalty = "MBIC", pen.value = 0, method = "BinSeg") # Beginning year 1947
plot(m.cptSSB_herring,  xlab="Years - starting 1947", ylab ="SSB",
     main = "SSB_cpt_herring")
cpts(m.cptSSB_herring)
#cpt:   4     18      20   40    54
#Year: 1950   1964  1966   1968  2000

### bcp
m.bcpSSB_herring<- bcp(y=herring$SSB)
plot(m.bcpSSB_herring, main = "SSB_bcp_herring")
summary(m.bcpSSB_herring)
View(m.bcpSSB_herring$posterior.prob)
#cpt:  20     67 (0.5)
#Year: 1966   1983

#Take 1966 and 1983??? --> from earlier result

## 2. Test if there are more regimes in the original second regime (1964 - now) of Herring

herring_new <- herring %>%
  filter(Year>1963)
#View(Herring)

### cpt
m.cptSSB2_herring<- cpt.mean(herring_new$SSB,penalty = "MBIC", pen.value = 0, method = "BinSeg")
plot(m.cptSSB2_herring,  xlab="Years - starting 1964", ylab ="SSB",
     main = "SSB_cpt_herring")
cpts(m.cptSSB2_herring)
# 3     20    34     37    55
#       1966  1980  1983  2001

m.bcpSSB2_herring<- bcp(y=herring_new$SSB)
plot(m.bcpSSB2_herring,  main = "SSB_bcp_herring")
summary(m.bcpSSB2_herring)
View(m.bcpSSB_herring$posterior.prob)
#cpt: 20     37
#Year:1966   2000


#4. Haddock ----
haddock <- read.csv("SA_haddock_2021.csv", sep = ",")
View(haddock)
#no lag needed. Age_0
ssb_cpt_haddock <- cpt.mean(haddock$SSB,penalty = "MBIC", pen.value = 0, method = "BinSeg") #Beginning 1972
plot(ssb_cpt_haddock,
     xlab="Years - starting 1963",
     ylab ="SSB",
     main = "SSB_cpt",
     axis(x, at = seq(1963, 2018, by = 10), labels = F))

ssb_cpt_haddock@cpts
#points: 6    10   15    30    32     50
#years:  1977 1981 1986  2001  2003   2021

#bcp
bcp_ssb_haddock <- bcp(haddock$SSB)
plot(bcp_ssb_haddock, main = "SSB_bcp")
summary(bcp_ssb_haddock)
#points: 30  32 (0.5)
#years : 01  03

#5. Saithe ----
df_saithe<-read.csv("SA_saithe_2021.csv", sep = ",")
View(df_saithe)

ssbcpts_saithe <- cpt.mean(data = df_saithe$SSB, method = "BinSeg", Q=10) #Beginning Year 1967
plot(ssbcpts_saithe, type = "l", cpt.col = "blue", xlab = "Index", pt.widt = 4)
cpts(ssbcpts_saithe)
#cpt:   2,    3,     5,    9,    12,   17,    22,   30,   37,   44
#Year: 1968, 1969, 1971, 1975, 1978, 1983,  1988, 1997, 2003, 2010


bcp.ssb_saithe <- bcp(df_saithe$SSB)
plot(bcp.ssb_saithe, main = "SSB")
View(bcp.ssb_saithe$posterior.prob)
#cpt:   2,     4,    9,    44 (0.6)
#Year: 1968, 1970, 1975, 2010

#First identified cpts would result in very short regimes -> take 9 as first cpt

#6. Cod ----
cod <-  read.csv("SA_cod_2021.csv", sep =",")
View(cod)

ssb_cpt_cod <- cpt.mean(cod$SSB,penalty = "MBIC", pen.value = 0, method = "BinSeg") #Beginning Year 1963

plot(ssb_cpt_cod,
     xlab="Years - starting 1963",
     ylab ="SSB",
     main = "SSB_cpt",
     axis(x, at = seq(1963, 2018, by = 10), labels = F))
ssb_cpt_cod@cpts
#points:   3    10    14    23    37     59
#years :1965, 1972, 1976, 1985 ,1999 , 2021


#bcp
bcp_ssb_cod <- bcp(cod$SSB)
plot(bcp_ssb_cod, main = "SSB_bcp")
summary(bcp_ssb_cod)
View(bcp_ssb_cod$posterior.prob)
#points: 4      10    37    45    56
#years :1966 ,1972, 1999, 2006, 2018
