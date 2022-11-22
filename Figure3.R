setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data/Stock_Assessment_Data_2021")

library(readr)
library(tidyverse)
library(ggrepel)
library(readxl)
library(patchwork)
library(cowplot)
library(segmented)
library(strucchange)

#colors
# "steelblue3"
# "darkorange"
# "purple"

#1.Plaice ----
#Stock assessment:
#SSB in billions
#Recruitment in billions 

plaice <- read.csv("SA_plaice_2021.csv", 
                   sep = ",")

plaice$SSB_lag <- lag(plaice$SSB)

#remove first year, is now NA
plaice <- plaice[-1,]

plaice <- arrange (plaice, SSB_lag)

#1.1.Segmented ----
mean(plaice$SSB_lag, na.rm = T) 
seg_plaice <- segmented::segmented(lm(R_1 ~SSB_lag, data = plaice), seg.Z =  ~SSB_lag, psi = mean(plaice$SSB_lag, na.rm = T))
summary(seg_plaice)
summary(seg_plaice)$psi
coef_plaice <-c(seg_plaice$fitted.values)
brpt_plaice <- seg_plaice$psi[2]  #227462.3 --> regime 1 up to 1998/ period 2 from 1999 on
brpt_ste_plaice <- seg_plaice$psi[3]


#plot
plaice <- read.csv("SA_plaice_2021.csv", 
                   sep = ",")

plaice$SSB_lag <- lag(plaice$SSB)

#remove first year, is now NA
plaice <- plaice[-1,]

#color regimes
color_regimes_plaice <- NULL
color_regimes_plaice[plaice$Year %in% c(1958:1998)] <- "darkorange"
color_regimes_plaice[plaice$Year %in% c(1999:2021)] <- "purple"
plaice$color_regimes_plaice <- color_regimes_plaice

SRR_plaice_1 <-
  ggplot(data = plaice, aes(x = SSB_lag/1000, y = R_1/1000))+
  geom_path( data = plaice,col = "grey30")+
  geom_point(data = plaice, col = plaice$color_regimes_plaice)+
  geom_smooth(data = plaice[plaice$Year<1999,],mapping=aes(x = SSB_lag/1000, y = R_1/1000), col = "darkorange", method = "lm")+
  geom_smooth(data = plaice[plaice$Year>=1999,],mapping=aes(x = SSB_lag/1000, y = R_1/1000), col = "purple", method = "lm")+
  
  geom_text_repel(data = plaice[1, ], aes(label = Year), #1958
                  point.padding = 0.2,nudge_y = -340, nudge_x = -25, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[42, ], aes(label = Year), #1999
                  point.padding = 0.2,nudge_y = -360, nudge_x = -10, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[64, ], aes(label = Year), #2021
                  point.padding = 0.2,nudge_y = -350, nudge_x = -100, size=3,col="gray30", segment.size =0.2 )+
  geom_vline(xintercept = 207288/1000, linetype="dashed",color="gray30")+#Blim
  geom_label(x = 207288/1000, y = 4000, label = expression("B"[lim]), color="gray30", size = 3.5, fontface = "bold")+
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



#2.Hake ----
#Stock assessment:
#SSB in 1000 t
#Recruitment in millions 

hake <- read.csv("SA_hake_2021.csv", sep = ",")

hake <- arrange(hake, SSB)

#2.1.Strucchange ----
bpts <- strucchange :: breakpoints(R_0 ~ SSB, data = hake)

plot(bpts)

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
bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #1
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- hake$SSB[bpts2$breakpoints]

best_brk #46620
hake$Year[bpts2$breakpoints]  # year --> 2008



#plot
hake <- read.csv("SA_hake_2021.csv", sep = ",")

#color regimes years
color_regimes_hake <- NULL
color_regimes_hake[hake$Year %in% c(1978:2007)] <- "darkorange"
color_regimes_hake[hake$Year %in% c(2008:2021)] <- "purple"
hake$color_regimes_hake <- color_regimes_hake

SRR_hake_1 <-
  ggplot(data = hake, aes(x = SSB/1000, y = R_0/1000))+
  geom_path( data = hake,col = "grey30")+
  geom_point(data = hake, col = hake$color_regimes_hake)+
  geom_smooth(data = hake[hake$Year<2008,],mapping=aes(x = SSB/1000, y = R_0/1000), col = "darkorange", method = "lm")+
  geom_smooth(data = hake[hake$Year>=2008,],mapping=aes(x = SSB/1000, y = R_0/1000), col = "purple", method = "lm")+
  
  geom_text_repel(data = hake[1, ], aes(label = Year), #1978
                  point.padding = 0.2,nudge_y = -50, nudge_x = -20, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = hake[31, ], aes(label = Year), #2008
                  point.padding = 0.2,nudge_y = 0, nudge_x = 30, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = hake[44, ], aes(label = Year), #2021
                  point.padding = 0.2,nudge_y = -50, nudge_x = -10, size=3,col="gray30", segment.size =0.2 )+
  geom_vline(xintercept = 40000/1000, linetype="dashed",color="gray30")+#Blim
  geom_label(x = 40000/1000, y = 700, label = expression("B"[lim]), color="gray30", size = 3.5, fontface = "bold")+
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


#3.Herring ----

#Stock assessment:
#SSB in million t
#Recruitment in billions 
herring <-read.csv("SA_herring_2021.csv", sep = ",")


#delete unneeded columns
herring <- herring[,-c(8:11)]
herring <- arrange(herring,SSB)

#3.1.Segmented----
mean(herring$SSB, na.rm = T) 
seg_herring <- segmented::segmented(lm(R_0 ~ SSB, data = herring), seg.Z =  ~SSB, psi = mean(herring$SSB, na.rm = T))
summary(seg_herring)
summary(seg_herring)$psi
coef_herring <-c(seg_herring$fitted.values)
brpt_herring <- seg_herring$psi[2]  #599661.6 --> regime 1 up to 1982/ period 2 from 1983 on
brpt_ste_herring <- seg_herring$psi[3]


#plot
herring <-read.csv("SA_herring_2021.csv", sep = ",")

#color regimes
color_regimes_herring <- NULL
color_regimes_herring[herring$Year %in% c(1947:1982)] <- "darkorange"
color_regimes_herring[herring$Year %in% c(1983:2021)] <- "purple"
herring$color_regimes_herring <- color_regimes_herring


SRR_herring_1 <-
  ggplot(data = herring, aes(x = SSB/1000, y = R_0/1000000))+
  geom_path( data = herring,col = "grey30")+
  geom_point(data = herring, col = herring$color_regimes_herring)+
  geom_smooth(data = herring[herring$Year<1983,],mapping=aes(x = SSB/1000, y = R_0/1000000), col = "darkorange", method = "lm")+
  geom_smooth(data = herring[herring$Year>=1983,],mapping=aes(x = SSB/1000, y = R_0/1000000), col = "purple", method = "lm")+
  
  geom_text_repel(data = herring[1, ], aes(label = Year), #1947
                  point.padding = 0.2,nudge_y = -10, nudge_x = 0, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = herring[37, ], aes(label = Year), #1983
                  point.padding = 0.2,nudge_y = 5, nudge_x = -350, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = herring[75, ], aes(label = Year), #2021
                  point.padding = 0.2,nudge_y = 0, nudge_x = -600, size=3,col="gray30", segment.size =0.2 )+
  geom_vline(xintercept = 874198/1000, linetype="dashed",color="gray30")+#Blim
  geom_label(x = 874198/1000, y = 5, label = expression("B"[lim]), color="gray30", size = 3.5, fontface = "bold")+
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



#4.Haddock ----
#Stock assessment:
#SSB in in 1000 t
#Recruitment in billions
haddock <- read.csv("SA_haddock_2021.csv", sep = ",")

haddock <- arrange(haddock, SSB)

#4.1.Strucchange----
bpts <- strucchange :: breakpoints(R_0 ~ SSB, data = haddock)

plot(bpts)
summary(bpts)  


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
bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks 
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- haddock$SSB[bpts2$breakpoints]

best_brk #263066
haddock$Year[bpts2$breakpoints] #--> year 2013



#plot
haddock <- read.csv("SA_haddock_2021.csv", sep = ",")

#color regimes
color_regimes_haddock <- NULL
color_regimes_haddock[haddock$Year %in% c(1972:2012)] <- "darkorange"
color_regimes_haddock[haddock$Year %in% c(2013:2021)] <- "purple"
haddock$color_regimes_haddock <- color_regimes_haddock

SRR_haddock_1 <-
  ggplot(data = haddock, aes(x = SSB/1000, R_0/1000000))+
  geom_path( data = haddock,col = "grey30")+
  geom_point(data = haddock, col = haddock$color_regimes_haddock)+
  geom_smooth(data = haddock[haddock$Year<2013,],mapping=aes(x = SSB/1000, R_0/1000000), col = "darkorange", method = "lm")+
  geom_smooth(data = haddock[haddock$Year>=2013,],mapping=aes(x = SSB/1000, R_0/1000000), col = "purple", method = "lm")+
  
  geom_text_repel(data = haddock[1, ], aes(label = Year), #1972
                  point.padding = 0.2,nudge_y = 0, nudge_x = 50, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = haddock[42, ], aes(label = Year), #2013
                  point.padding = 0.2,nudge_y = -8, nudge_x = 0, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = haddock[50, ], aes(label = Year), #2021
                  point.padding = 0.2,nudge_y = -4, nudge_x = 20, size=3,col="gray30", segment.size =0.2 )+
  geom_vline(xintercept = 94000/1000, linetype="dashed",color="gray30")+#Blim
  geom_label(x = 94000/1000, y = -2, label = expression("B"[lim]), color="gray30", size = 3.5, fontface = "bold")+
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

#5.Saithe ----
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

#5.1.Strucchange----
bpts <- strucchange :: breakpoints(R_3 ~ SSB_lag, data = saithe)

plot(bpts)
summary(bpts)  


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
bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #2
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- saithe$SSB[bpts2$breakpoints]

best_brk #256097 182692
saithe$Year[bpts2$breakpoints] #--> year 2006 2011

#plot
saithe<- read.csv("SA_saithe_2021.csv", sep = ",")

#lag SSB 3 times 
saithe$SSB_lag <- lag(saithe$SSB)
saithe$SSB_lag <- lag(saithe$SSB_lag)
saithe$SSB_lag <- lag(saithe$SSB_lag)

saithe <- saithe[-c(1:3),]

#color regimes
color_regimes_saithe <- NULL
color_regimes_saithe[saithe$Year %in% c(1970:2005)] <- "steelblue3"
color_regimes_saithe[saithe$Year %in% c(2006:2010)] <- "darkorange"
color_regimes_saithe[saithe$Year %in% c(2011:2021)] <- "purple"
saithe$color_regimes_saithe <- color_regimes_saithe


SRR_saithe_1 <-
  ggplot(data = saithe, aes(x = SSB_lag/1000, y = R_3/1000))+
  geom_path( data = saithe,col = "grey30")+
  geom_point(data = saithe, col = saithe$color_regimes_saithe)+
  geom_smooth(data = saithe[saithe$Year<2006,],mapping=aes(x = SSB_lag/1000, y = R_3/1000), col = "steelblue3", method = "lm")+
  geom_smooth(data = saithe[saithe$Year %in% c(2006:2010),],mapping=aes(x = SSB_lag/1000, y = R_3/1000), col = "darkorange", method = "lm")+
  geom_smooth(data = saithe[saithe$Year>=2011,],mapping=aes(x = SSB_lag/1000, y = R_3/1000), col = "purple", method = "lm")+
  
  geom_text_repel(data = saithe[1, ], aes(label = Year), #1970
                  point.padding = 0.2,nudge_y = 50, nudge_x = 0, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = saithe[37, ], aes(label = Year), #2006
                  point.padding = 0.2,nudge_y = 0, nudge_x = 70, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = saithe[42, ], aes(label = Year), #2011
                  point.padding = 0.2,nudge_y = 0, nudge_x = 50, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = saithe[52, ], aes(label = Year), #2021
                  point.padding = 0.2,nudge_y = -40, nudge_x = 40, size=3,col="gray30", segment.size =0.2 )+
  geom_vline(xintercept = 107297/1000, linetype="dashed",color="gray30")+#Blim
  geom_label(x = 107297/1000, y = 10, label = expression("B"[lim]), color="gray30", size = 3.5, fontface = "bold")+
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

#6.Cod ----
#Stock assessment:
#SSB in in 1000 t
#Recruitment in billions

cod <- read.csv("SA_cod_2021.csv", sep = ",")

cod <- cod %>% 
  mutate( SSB_lag = lag(SSB))
#remove NA year
cod <- cod[-1,]

cod <- arrange(cod,SSB )

#6.1.Strucchange----
cod <- read.csv("SA_cod_2021.csv", sep = ",")
cod <- cod %>%
  mutate( SSB_lag = lag(SSB))
#remove NA year
cod <- cod[-1,]
cod <- arrange(cod,SSB )
bpts <- strucchange :: breakpoints(R_1 ~ SSB_lag, data = cod)
plot(bpts)
summary(bpts)
opt_bpts <- function(x) {
  #x = bpts_sum$RSS[“BIC”,]
  n <- length(x)
  lowest <- vector("logical", length = n-1)
  lowest[1] <- FALSE
  for (i in 2:n) {
    lowest[i] <- x[i] < x[i-1] & x[i] < x[i+1]
  }
  out <- as.integer(names(x)[lowest])
  return(out)
}
bpts_sum <- summary(bpts)
opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
opt_brks #1
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- cod$SSB[bpts2$breakpoints]
best_brk #92444
cod$Year[bpts2$breakpoints] #--> year 1996
#plot
cod <- read.csv("SA_cod_2021.csv", sep = ",")
cod <- cod %>%
  mutate( SSB_lag = lag(SSB))
cod<- cod[-1, ]

#color regimes
color_regimes_cod <- NULL
color_regimes_cod[cod$Year %in% c(1964:1995)] <- "darkorange"
color_regimes_cod[cod$Year %in% c(1996:2021)] <- "purple"

cod$color_regimes_cod <- color_regimes_cod

SRR_cod_1 <-
  ggplot(data = cod, aes(x = SSB_lag/1000, R_1/1000000))+
  geom_path( data = cod,col = "grey30") +
  geom_point(data = cod, col = cod$color_regimes_cod)+
  geom_smooth(data = cod[cod$Year<1996,],mapping=aes(x = SSB_lag/1000, R_1/1000000), col = "darkorange", method = "lm")+
  geom_smooth(data = cod[cod$Year>=1996,],mapping=aes(x = SSB_lag/1000, R_1/1000000), col = "purple", method = "lm")+
  geom_text_repel(data = cod[1, ], aes(label = Year), #1964
                  point.padding = 0.2,nudge_y = -0.3, nudge_x = 0, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = cod[33, ], aes(label = Year), #1996
                  point.padding = 0.2,nudge_y = 0, nudge_x = -30, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = cod[58, ], aes(label = Year), #2021
                  point.padding = 0.2,nudge_y = -9, nudge_x = 0, size=3,col="gray30", segment.size =0.2 )+
  geom_vline(xintercept = 92444/1000, linetype="dashed",color="gray30")+#Blim
  geom_label(x = 92444/1000, y = 2.2, label = expression("B"[lim]), color="gray30", size = 3.5, fontface = "bold")+
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




# without sprat and pout 
SRR_plaice_1 + SRR_hake_1 + SRR_herring_1 + SRR_haddock_1 + SRR_saithe_1  + SRR_cod_1 + plot_layout(ncol =2)



