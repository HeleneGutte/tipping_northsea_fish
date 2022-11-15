setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data")
library(readr)
library(tidyverse)
library(ggrepel)
library(readxl)
library(patchwork)
library(cowplot)
library(segmented)
library(strucchange)

#colors
# "green4"
# "steelblue3"
# "darkorange"
# "purple"

#Plaice ----
#Stock assessment:
#SSB in billions
#Recruitment in billions 

plaice <- read.csv("SA_plaice_2021.csv", 
                   sep = ",")

plaice <- arrange(plaice, SSB)
color_regimes_plaice <- NULL
color_regimes_plaice[plaice$Year %in% c(1957:1969)] <- "green4"
color_regimes_plaice[plaice$Year %in% c(1970:1991)] <- "steelblue3"
color_regimes_plaice[plaice$Year %in% c(1992:2007)] <- "darkorange"
color_regimes_plaice[plaice$Year %in% c(2008:2021)] <- "purple"
plaice$color_regimes_plaice <- color_regimes_plaice

plaice$SSB_lag <- lag(plaice$SSB)
#remove first year, is now NA
plaice <- plaice[-1,]

#6.4. glm with negative binomial
library(MASS)
plaice_negbi <- glm.nb(R_1 ~ SSB_lag, data = plaice)
summary(plaice_negbi)

mean(plaice$SSB_lag, na.rm = TRUE) 
seg_plaice_negbi <- segmented::segmented(glm.nb(R_1 ~ SSB_lag, data = plaice), seg.Z =  ~SSB_lag, psi = mean(plaice$SSB_lag, na.rm = TRUE))
summary(seg_plaice_negbi)
summary(seg_plaice_negbi)$psi 

coef_plaice_negbi <-c(seg_plaice_negbi$fitted.values)
brpt_plaice_negbi <- seg_plaice_negbi$psi[2]
brpt_ste_plaice_negbi <- seg_plaice_negbi$psi[3]

#plot
SRR_plaice_1 <- ggplot(data = plaice, aes(x = SSB_lag/1000, y = R_1/1000))+
  geom_point(data = plaice[plaice$Year < 2008,], col = plaice[plaice$Year < 2008,]$color_regimes_plaice)+
  geom_point(data = plaice[plaice$Year >= 2008,],aes(x = SSB_lag/1000, y = R_1/1000), shape = 3, col ="purple", size =3, stroke = 1)+
  geom_vline(aes(col = "segmented",xintercept = brpt_plaice_negbi/1000), linetype = 2, col ="black")+
  geom_line(aes(y = coef_plaice_negbi/1000, col = "segmented"))+
  scale_color_manual(values  = "black")+
  xlab("")+ylab("")+
  theme_test()+
  theme(
    legend.position = c(0.99,0.999),
    legend.justification = c("right", "top"),
    legend.margin = margin(2,2,2,2), 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text = element_text(size = 12)
  )
SRR_plaice_1



### Hake ----
#Stock assessment:
#SSB in 1000 t
#Recruitment in millions 
hake <- read.csv("SA_hake_2021.csv", sep = ",")

hake <- arrange(hake, SSB)
color_regimes_hake <- NULL
color_regimes_hake[hake$Year %in% c(1978:1985)] <- "steelblue3"
color_regimes_hake[hake$Year %in% c(1986:2010)] <- "darkorange"
color_regimes_hake[hake$Year %in% c(2011:2021)] <- "purple"
hake$color_regimes_hake <- color_regimes_hake

#6.4. glm with negative binomial
library(MASS)
hake_negbi <- glm.nb(R_0 ~ SSB, data = hake)
summary(hake_negbi)

mean(hake$SSB, na.rm = TRUE)
seg_hake_negbi <- segmented::segmented(glm.nb(R_0 ~ SSB, data = hake), seg.Z =  ~SSB, psi = mean(hake$SSB, na.rm = TRUE))
summary(seg_hake_negbi)
summary(seg_hake_negbi)$psi #43908

coef_hake_negbi <-c(seg_hake_negbi$fitted.values)
brpt_hake_negbi <- seg_hake_negbi$psi[2]
brpt_ste_hake_negbi <- seg_hake_negbi$psi[3]

#plot
SRR_hake_1 <- ggplot(data = hake, aes(x = SSB/1000, y = R_0/1000))+
  geom_point(data = hake[hake$Year < 2011,],col = hake[hake$Year < 2011,]$color_regimes_hake)+
  geom_point(data = hake[hake$Year >= 2011,],aes(x = SSB/1000, y = R_0/1000), shape = 3, col ="purple", size =3, stroke = 1)+
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_hake_negbi/1000), linetype = 2, col ="black")+
  geom_line(aes(y = coef_hake_negbi/1000, col = "seg. neg bi"))+
  scale_color_manual(values  = "black")+
  xlab("")+ylab("")+
  theme_test()+
  theme(
    legend.position = c(0.99,0.999),
    legend.justification = c("right", "top"),
    legend.margin = margin(2,2,2,2), 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text = element_text(size = 12)
  )
SRR_hake_1


#Herring ----

#Stock assessment:
#SSB in million t
#Recruitment in billions 
herring <-read.csv("SA_herring_2021.csv", sep = ",")
herring <- arrange(herring,SSB )

#delete unneeded columns
herring <- herring[,-c(8:11)]


color_regimes_herring <- NULL
color_regimes_herring[herring$Year %in% c(1947:1966)] <- "green4"
color_regimes_herring[herring$Year %in% c(1967:1983)] <- "steelblue3"
color_regimes_herring[herring$Year %in% c(1984:2000)] <- "darkorange"
color_regimes_herring[herring$Year %in% c(2001:2021)] <- "purple"
herring$color_regimes_herring <- color_regimes_herring


#6.4. glm with negative binomial
library(MASS)
herring_negbi <- glm.nb(R_0 ~ SSB, data = herring)
summary(herring_negbi)

mean(herring$SSB, na.rm = TRUE)
seg_herring_negbi <- segmented::segmented(glm.nb(R_0 ~ SSB, data = herring), seg.Z =  ~SSB, psi = mean(herring$SSB, na.rm = TRUE))
summary(seg_herring_negbi)
summary(seg_herring_negbi)$psi #43908

coef_herring_negbi <-c(seg_herring_negbi$fitted.values)
brpt_herring_negbi <- seg_herring_negbi$psi[2]
brpt_ste_herring_negbi <- seg_herring_negbi$psi[3]

#plot
SRR_herring_1 <- ggplot(data = herring, aes(x = SSB/1000, y = R_0/1000000))+
  geom_point(data = herring[herring$Year < 2001,],col = herring[herring$Year < 2001,]$color_regimes_herring)+
  geom_point(data = herring[herring$Year >= 2001,],aes(x = SSB/1000, y = R_0/1000000), shape = 3, col ="purple", size =3, stroke = 1)+
  geom_vline(aes(col = "segmented",xintercept = brpt_herring_negbi/1000), linetype = 2, col ="black")+
  geom_line(aes(y = coef_herring_negbi/1000000, col = "segmented"))+
  scale_color_manual(values  = "black")+
  xlab("")+ylab("")+
  theme_test()+
  theme(
    legend.position = c(0.99,0.999),
    legend.justification = c("right", "top"),
    legend.margin = margin(2,2,2,2), 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text = element_text(size = 12)
  )
SRR_herring_1

#Haddock ----
#Stock assessment:
#SSB in in 1000 t
#Recruitment in billions
haddock <- read.csv("SA_haddock_2021.csv", sep = ",")

color_regimes_haddock <- NULL
color_regimes_haddock[haddock$Year %in% c(1972:2001)] <- "darkorange"
color_regimes_haddock[haddock$Year %in% c(2002:2021)] <- "purple"
haddock$color_regimes_haddock <- color_regimes_haddock

#6.4. glm with negative binomial
library(MASS)
haddock_negbi <- glm.nb(R_0 ~ SSB, data = haddock)
summary(haddock_negbi)

mean(haddock$SSB, na.rm = TRUE)
seg_haddock_negbi <- segmented::segmented(glm.nb(R_0 ~ SSB, data = haddock), seg.Z =  ~SSB, psi = mean(haddock$SSB, na.rm = TRUE))
summary(seg_haddock_negbi)
summary(seg_haddock_negbi)$psi #43908

coef_haddock_negbi <-c(seg_haddock_negbi$fitted.values)
brpt_haddock_negbi <- seg_haddock_negbi$psi[2]
brpt_ste_haddock_negbi <- seg_haddock_negbi$psi[3]

#plot
SRR_haddock_1 <- ggplot(data = haddock, aes(x = SSB/1000, y = R_0/1000000))+
  geom_point(data = haddock[haddock$Year < 2002,],col = haddock[haddock$Year < 2002,]$color_regimes_haddock)+
  geom_point(data = haddock[haddock$Year >= 2002,],aes(x = SSB/1000, y = R_0/1000000), shape = 3, col ="purple", size =3, stroke = 1)+
  geom_vline(aes(col = "strucchange",xintercept = brpt_haddock_negbi/1000), linetype = 2, col ="black")+
  geom_line(aes(y = coef_haddock_negbi/1000000, col = "strucchange"))+
  scale_color_manual(values  = "black")+
  xlab("")+ylab("")+
  theme_test()+
  theme(
    legend.position = c(0.99,0.999),
    legend.justification = c("right", "top"),
    legend.margin = margin(2,2,2,2), 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text = element_text(size = 12)
  )
SRR_haddock_1

### Saithe ----
#Stock assessment:
#SSB in in 1000 t
#Recruitment in millions
saithe<- read.csv("SA_saithe_2021.csv", sep = ",")

#lag SSB 3 times 
saithe$SSB_lag <- lag(saithe$SSB)
saithe$SSB_lag <- lag(saithe$SSB_lag)
saithe$SSB_lag <- lag(saithe$SSB_lag)

saithe <- saithe[-c(1:3),]

saithe<-arrange(saithe, SSB_lag)

color_regimes_saithe <- NULL
color_regimes_saithe[saithe$Year %in% c(1967:1975)] <- "steelblue3"
color_regimes_saithe[saithe$Year %in% c(1976:2010)] <- "darkorange"
color_regimes_saithe[saithe$Year %in% c(2011:2021)] <- "purple"
saithe$color_regimes_saithe <- color_regimes_saithe
#6.4. glm with negative binomial
library(MASS)
saithe_negbi <- glm.nb(R_3 ~ SSB_lag, data = saithe)
summary(saithe_negbi)

mean(saithe$SSB_lag, na.rm = TRUE) #436260.5
seg_saithe_negbi <- segmented::segmented(glm.nb(R_3 ~ SSB_lag, data = saithe), seg.Z =  ~SSB_lag, psi = mean(saithe$SSB_lag, na.rm = TRUE))
summary(seg_saithe_negbi)
summary(seg_saithe_negbi)$psi #226854.4 breakpoint

coef_saithe_negbi <-c(seg_saithe_negbi$fitted.values)
brpt_saithe_negbi <- seg_saithe_negbi$psi[2]
brpt_ste_saithe_negbi <- seg_saithe_negbi$psi[3]

#plot
SRR_saithe_1 <- ggplot(data = saithe, aes(x = SSB_lag/1000, y = R_3/1000))+
  geom_point(data = saithe[saithe$Year < 2011,], col = saithe[saithe$Year < 2011,]$color_regimes_saithe)+
  geom_point(data = saithe[saithe$Year >= 2011,],aes(x = SSB_lag/1000, y = R_3/1000), shape = 3, col ="purple", size =3, stroke = 1)+
  geom_vline(aes(col = "segmented",xintercept = brpt_saithe_negbi/1000), linetype = 2, col ="black")+
  geom_line(aes(y = coef_saithe_negbi/1000, col = "segmented"))+
  scale_color_manual(values  = "black")+
  xlab("")+ylab("")+
  theme_test()+
  theme(
    legend.position = c(0.99,0.999),
    legend.justification = c("right", "top"),
    legend.margin = margin(2,2,2,2), 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text = element_text(size = 12)
  )
SRR_saithe_1

#Cod ----
#Stock assessment:
#SSB in in 1000 t
#Recruitment in billions

cod <- read.csv("SA_cod_2021.csv", sep = ",")

cod <- arrange(cod,SSB )

cod <- cod %>% 
  mutate( SSB_lag = lag(SSB))
#remove NA year
cod <- cod[-1,]

color_regimes_cod <- NULL
color_regimes_cod[cod$Year %in% c(1964:1972)] <- "steelblue3"
color_regimes_cod[cod$Year %in% c(1973:1999)] <- "darkorange"
color_regimes_cod[cod$Year %in% c(2000:2021)] <- "purple"
cod$color_regimes_cod <- color_regimes_cod


#6.4. glm with negative binomial
library(MASS)
cod_negbi <- glm.nb(R_1 ~ SSB, data = cod)
summary(cod_negbi)

mean(cod$SSB, na.rm = TRUE)
seg_cod_negbi <- segmented::segmented(glm.nb(R_1 ~ SSB, data = cod), seg.Z =  ~SSB, psi = mean(cod$SSB, na.rm = TRUE))
summary(seg_cod_negbi)
summary(seg_cod_negbi)$psi #43908

coef_cod_negbi <-c(seg_cod_negbi$fitted.values)
brpt_cod_negbi <- seg_cod_negbi$psi[2]
brpt_ste_cod_negbi <- seg_cod_negbi$psi[3]

#plot
SRR_cod_1 <- ggplot(data = cod, aes(x = SSB/1000, y = R_1/1000000))+
  geom_point(data = cod[cod$Year < 2000,],col = cod[cod$Year < 2000,]$color_regimes_cod)+
  geom_point(data = cod[cod$Year >= 2000,],aes(x = SSB/1000, y = R_1/1000000), shape = 3, col ="purple", size =3, stroke = 1)+
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_cod_negbi/1000), linetype = 2, col ="black")+
  geom_line(aes(y = coef_cod_negbi/1000000, col = "seg. neg bi"))+
  scale_color_manual(values  = "black")+
  xlab("")+ylab("")+
  theme_test()+
  theme(
    legend.position = c(0.99,0.999),
    legend.justification = c("right", "top"),
    legend.margin = margin(2,2,2,2), 
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.background = element_blank(),
    legend.key = element_rect(colour = NA, fill = NA),
    axis.text = element_text(size = 12)
  )
SRR_cod_1




#Figure3_new ----
# without sprat and pout 
SRR_plaice_1 + SRR_hake_1 + SRR_herring_1 + SRR_haddock_1 + SRR_saithe_1  + SRR_cod_1 + plot_layout(ncol =2)




