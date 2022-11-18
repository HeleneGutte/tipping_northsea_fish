#setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data/Stock_assessment_data_2021")

# packages ----
library(tidyverse)
library(readxl)
library(patchwork)

#Cod ######

cod <- read.csv("SA_cod_2021.csv", sep = ",")

#create data sets for SSB regimes
cod_1 <- cod %>%
  filter(Year %in% c(1963:1972))

cod_2 <- cod %>%
  filter (Year %in% c(1972:1999))

cod_3 <- cod %>%
  filter (Year %in% c(1999:2021))


## plot_SSB
#SSB in 1000 t
SSB_cod <- ggplot(cod)+
  geom_vline(aes(xintercept = 1972), col = "black")+
  geom_vline(aes(xintercept = 1999), col = "black", size = 0.9)+
  geom_ribbon(cod_1, mapping = aes(x=Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "steelblue3", alpha = 0.5)+ #SSB and regimes
  geom_ribbon(cod_2, mapping = aes(x=Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "darkorange", alpha = 0.5)+
  geom_ribbon(cod_3, mapping = aes(x=Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "purple")+ 
  geom_line(aes(x = Year, y = SSB/1000), col = "black")+
  ylim(0, 350)+
  xlim(1947, 2022)+
  geom_hline(yintercept = 97777/1000, col = "darkorange")+ #MSY Btrigger
  geom_hline(yintercept = 97777/1000, col = "gray50", linetype = "dashed")+ #Bpa
  geom_hline(yintercept = 69841/1000, col = "gray50", linetype = "dotted")+ #Blim
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))+
  xlab("")+ ylab("")
SSB_cod

## plot_recruitment
#R in billions
recruitment_cod <-  ggplot(cod, aes(Year, R_1/1000000))+
  geom_ribbon(data = cod_1, aes(x = Year, ymax = R_high/1000000, ymin = R_low/1000000),
              fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = cod_2, aes(x = Year, ymax = R_high/1000000, ymin = R_low/1000000),
              fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = cod_3, aes(x = Year, ymax = R_high/1000000, ymin = R_low/1000000),
              fill = "purple")+
  geom_line(color = "Black")+
  xlab("")+ylab("")+
  scale_y_continuous(breaks = c(0,1,2,3))+
  xlim(1947, 2022)+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
recruitment_cod


## Fishing
#ages 2-4
f_cod <- ggplot(cod)+
  geom_ribbon(data = cod_1, aes(x=Year,ymin = F_low, ymax = F_high), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = cod_2, aes(x=Year,ymin = F_low, ymax = F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = cod_3, aes(x=Year,ymin = F_low, ymax = F_high), fill = "purple")+#fishing pressure
  geom_line(aes(Year, F_2_6), col = "black")+
  geom_hline(yintercept = 0.28, col = "darkorange")+ #FMSY
  geom_hline(yintercept = 0.49, col = "gray50", lty = "dashed")+ #Fpa
  geom_hline(yintercept = 0.58, col = "gray50", lty = "dotted")+ #Flim
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1,1.2))+
  xlab("")+ ylab("")+
  xlim(1947, 2022)+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
f_cod


# Haddock####
haddock <- read.csv("SA_haddock_2021.csv", sep = ",")
#no lag needed. Age_0

#create data sets for SSB regimes (data starts in 1972)
#1) 1972-2001
#2) 2002-2019
haddock_1 <- haddock %>%
  filter(Year %in% c(1972:2001))

haddock_2 <- haddock %>%
  filter (Year %in% c(2001:2021))


## plot_SSB
#SSB in 1000t
SSB_haddock <- ggplot(haddock)+
  geom_vline(aes(xintercept = 2001), col = "black", size = 0.9)+
  geom_ribbon(haddock_1,mapping = aes(x=Year,ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "darkorange", alpha = 0.5)+ #SSB and regimes
  geom_ribbon(haddock_2,mapping = aes(x=Year,ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "purple")+
  geom_line(aes(Year, SSB/1000), col = "black")+
  geom_hline(yintercept = 132000/1000, col = "darkorange" )+ #MSY Btrigger
  geom_hline(yintercept = 132000/1000, col = "gray50", lty = "dashed")+ #Bpa
  geom_hline(yintercept = 94000/1000, col = "gray50", lty = "dotted" )+ #Blim
  xlab("")+ ylab("")+
  xlim(1947, 2022)+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

SSB_haddock

## plot_recruitment
#in billions, age 0
recruitment_haddock <-  ggplot(haddock, aes(Year, R_0/1000000))+
  geom_ribbon(data = haddock_1, aes(ymax = R_high/1000000, ymin = R_low/1000000), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = haddock_2, aes(ymax = R_high/1000000, ymin = R_low/1000000), fill = "purple")+
  geom_line(col = "black")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  ylim(0,80)+
  theme_test()+
  xlim(1947, 2022)+
  labs(y = "", x = "")+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))


recruitment_haddock

## Fishing
#ages 2-4

f_haddock <- ggplot(haddock, aes(Year))+
  geom_ribbon(data = haddock_1, aes(x=Year,ymin = F_low, ymax = F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = haddock_2, aes(x=Year,ymin = F_low, ymax = F_high), fill = "purple")+#fishing pressure
  geom_line(aes(Year, F_2_4), col = "black")+
  geom_hline(yintercept = 0.19, col = "darkorange")+ #FMSY
  geom_hline(yintercept = 0.194, col = "gray50", lty = "dashed")+ #Fpa
  geom_hline(yintercept = 0.39, col = "gray50", lty = "dotted")+ #Flim
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  theme_test()+
  xlab("") + ylab("")+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
f_haddock

# #Plaice ####
plaice <- read.csv("SA_plaice_2021.csv", sep = ",")

plaice[is.na(plaice)] <- 0
plaice <- plaice%>%
  mutate(totalcatch = Landings+Discards)%>%
  mutate(explr = totalcatch/SSB)%>%
  mutate(SSB_lag = lag(SSB),
         SSB_high_lag = lag(SSB_high),
         SSB_low_lag = lag(SSB_low))


#SSB in 1000t
SSB_plaice <- ggplot(data = plaice, aes(x = Year))+
  geom_vline(aes(xintercept = 1969), col = "gray60")+
  geom_vline(aes(xintercept = 1991), col = "gray60")+
  geom_vline(aes(xintercept = 2007), col = "black", size = 0.9)+
  geom_ribbon(data = plaice[plaice$Year <=1969, ],
              aes(x = Year , ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "gray20", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year  %in%c(1969:1991), ],
              aes(x = Year ,  ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "gray50")+
  geom_ribbon(data = plaice[plaice$Year %in%c(1991:2007), ],
            aes(x = Year ,  ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "gray80")+
  geom_ribbon(data = plaice[plaice$Year >=2007, ],
              aes(x = Year , ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "purple")+
  geom_line(aes(y = SSB/1000), color = "black")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  geom_hline(aes(yintercept = 564599/1000), color = "darkorange")+ #MSY B trigger
  geom_hline(aes(yintercept = 290203/1000), color = "gray50", lty = "dashed" )+ #Bpa
  geom_hline(aes(yintercept = 207288/1000), color = "gray50", lty = "dotted" )+ #Blim
  labs(y = "", x = "")+
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),

        axis.text = element_text(size = 12))


SSB_plaice

#in billions
R_plaice <- ggplot(data = plaice, aes(x = Year))+
  geom_ribbon(data = plaice[plaice$Year <=1969, ],
              aes(x = Year , ymin = R_low/1000000, ymax = R_high/1000000), fill = "gray20", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year  %in%c(1969:1991), ],
              aes(x = Year ,  ymin = R_low/1000000, ymax = R_high/1000000), fill = "gray50")+
  geom_ribbon(data = plaice[plaice$Year %in%c(1991:2007), ],
              aes(x = Year ,  ymin = R_low/1000000, ymax = R_high/1000000), fill = "gray80")+
  geom_ribbon(data = plaice[plaice$Year >=2007, ],
              aes(x = Year , ymin = R_low/1000000, ymax = R_high/1000000), fill = "purple")+
  geom_line(aes(y = R_1/1000000),  col = "black")+
  labs(y = "", x = "")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

R_plaice

#ages 2 - 6
F_plaice <- ggplot(data = plaice, aes(x = Year))+
  geom_ribbon(data = plaice[plaice$Year <=1969, ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "gray20", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year %in%c(1969:1991), ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "gray50")+
  geom_ribbon(data = plaice[plaice$Year %in%c(1991:2007), ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "gray80")+
  geom_ribbon(data = plaice[plaice$Year >=2007, ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "purple")+
  geom_line(aes(y = F_2_6))+
  geom_hline(aes(yintercept = 0.21), color = "darkorange")+ #FMSY
  geom_hline(aes(yintercept = 0.769), color = "gray50",lty = "dashed")+ #Fpa
  labs(y = "", x = "")+ #F (ages 2-6)
  theme_test()+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0), plot.background = element_blank(),

        axis.text = element_text(size = 12))+
  theme(legend.position = "none")

F_plaice

# Herring ##########
herring <-read.csv("SA_herring_2021.csv", sep = ",")

herring <- herring %>% mutate(
  exploit_rate = Total_Catch/SSB)

herring2<- herring %>%
  filter(Year %in% c(1947:1966))

herring3<-herring %>%
  filter(Year %in% c(1967:1983))

#SSB in 1000 t
ssb_herring <- ggplot(herring)+
  geom_vline(aes(xintercept = 1966), col = "black")+
  geom_vline(aes(xintercept = 1983), col = "black")+
  geom_vline(aes(xintercept = 2000), col = "black", size = 0.9)+
  geom_ribbon(data= herring[herring$Year < 1966, ],
              mapping = aes(Year, ymin= SSB_low/1000, ymax = SSB_high/1000,
                            fill = "Regime 1"),fill="green4", show.legend = F, alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1966:1983), ],
              mapping = aes(Year, ymin= SSB_low/1000, ymax = SSB_high/1000,
                            fill = "Regime 2"),fill="steelblue3", show.legend = F,alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1983:2000), ],
              mapping = aes(Year, ymin= SSB_low/1000, ymax = SSB_high/1000,
                            fill = "Regime 3"),fill="darkorange", show.legend = F,alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year >= 2000, ],
              mapping = aes(Year, ymin= SSB_low/1000, ymax = SSB_high/1000,
                            fill = "Regime 4"),fill="purple", show.legend = F)+
  geom_line(aes(Year, SSB/1000), color = "black")+
  geom_hline(yintercept = 1232828/1000, col="darkorange" ) + #Btrigger
  geom_hline(yintercept = 956483/1000, col="gray50",lty = "dashed") + #Bpa
  geom_hline(yintercept = 874198/1000, col="gray50",lty = "dotted") + #Blim
  theme_test() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  ylab("")+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))+
  labs(x = "", y = "")

ssb_herring

#Recruitment _Herring
#in billions
R_herring <- ggplot(data = herring, aes(x = Year, y = R_0/1000000)) +
  geom_ribbon(data= herring[herring$Year < 1966, ],
              aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "green4", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1966:1983), ],
              aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1983:2001), ],
              aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year >= 2000, ],
              aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "purple")+
  geom_line(col = "black")+
  theme_test() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  scale_y_continuous(breaks = c(0, 50, 100))+
  labs(x = "", y = "")+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
R_herring

#Fishing

#ages 2-6
f_herring <- ggplot(herring, aes(x = Year))+
  geom_ribbon(data = herring[herring$Year < 1966, ],
              aes(ymin = F_low, ymax = F_high), fill = "green4", alpha = 0.5)+
  geom_ribbon(data = herring[herring$Year %in% c(1966:1983), ],
              aes(ymin = F_low, ymax = F_high), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = herring[herring$Year %in% c(1983:2000), ],
              aes(ymin = F_low, ymax = F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = herring[herring$Year >= 2000, ],
              aes(ymin = F_low, ymax = F_high), fill = "purple")+
  geom_line(aes(x = Year,y = F_2_6), col="black")+
  geom_hline(yintercept = 0.31, col = "darkorange") + #Fmsy
  geom_hline(yintercept = 0.31, col = "gray50", lty = "dashed" ) + #Fpa
  geom_hline(yintercept = 0.4, col = "gray50", lty = "dotted" ) + #Flim
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  xlab("")+ylab("") + #winter ringers
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

f_herring




# Saithe ####
df_saithe<- read.csv("SA_saithe_2021.csv", sep = ",")

df_saithe2<-df_saithe %>%
  filter(Year %in% c(1967:1975))

df_saithe3<-df_saithe %>%
  filter(Year %in% c(1975:2010))

df_saithe4 <- df_saithe%>%
  filter(Year %in% c(2010:2022))

#ages 4 - 7
saithe_f <- ggplot(df_saithe)+
  geom_ribbon(data = df_saithe2, aes(x=Year, ymin=F_low, ymax=F_high),
              fill="steelblue3", alpha = 0.5)+
  geom_ribbon(data = df_saithe3, aes(x=Year, ymin=F_low, ymax=F_high),
              fill="darkorange", alpha = 0.5)+
  geom_ribbon(data = df_saithe4, aes(x=Year, ymin=F_low, ymax=F_high),
              fill="purple")+
  geom_line(aes(Year, F_4_7), color="black")+
  xlab(" ")+
  ylab("")+
  geom_hline(yintercept = 0.363, color = "darkorange")+ #FMSY
  geom_hline(yintercept = 0.576, color = "gray50",lty = "dashed")+ #Fpa
  geom_hline(yintercept = 0.668, color = "gray50",lty = "dotted")+ #Flim
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8))+
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

saithe_f

#SSB in 1000t
saithe_SSB <-ggplot(df_saithe)+
  geom_vline(aes(xintercept = 1975), col = "black")+
  geom_vline(aes(xintercept = 2010), col = "black", size = 0.9)+
  geom_ribbon(data = df_saithe2, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "steelblue3", show.legend = F, alpha = 0.5)+
  geom_ribbon(data =df_saithe3, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "darkorange", show.legend = F, alpha = 0.5)+
  geom_ribbon(data = df_saithe4, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "purple", show.legend = F)+
  geom_line(aes(Year, SSB/1000), color="black")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  ylab("") + xlab(" ")+
  geom_hline(yintercept = 149098/1000, color = "darkorange")+ #Btrigger
  geom_hline(yintercept = 149098/1000, color = "gray50", lty = "dashed")+ #Bpa
  geom_hline(yintercept = 107297/1000, color = "gray50", lty = "dotted")+ #Blim
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

saithe_SSB

#R in billions
saithe_R <-ggplot(df_saithe)+
  geom_ribbon(data = df_saithe2, aes(x=Year, ymin=R_low/100000, ymax=R_high/100000),
              fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = df_saithe3, aes(x=Year, ymin=R_low/100000, ymax=R_high/100000),
              fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = df_saithe4, aes(x=Year, ymin=R_low/100000, ymax=R_high/100000),
              fill = "purple")+
  geom_line(aes(Year, R_3/100000), col = "black")+
  ylab("") + xlab(" ")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
saithe_R

# Hake ####
df_hake <- read.csv("SA_hake_2021.csv", sep = ",")

df_hake$Discards[is.na(df_hake$Discards)] <- 0
df_hake$Landing <- as.numeric(df_hake$Landing)
df_hake$Discards <- as.numeric(df_hake$Discards)
df_hake$R_high <- as.numeric(df_hake$R_high)
df_hake$R_low <- as.numeric(df_hake$R_low)

df_hake<-df_hake %>% mutate(
  catches = Landing+Discards,
  exploit_rate = catches/SSB,
  SSB = SSB/1000,
  R_0 = R_0/1000,
  R_low = R_low/1000,
  R_high = R_high/1000)

df_hake2<-df_hake %>%
  filter(Year %in% c(1978:1985))

df_hake3<-df_hake %>%
  filter(Year %in% c(1985:2010))

df_hake4<-df_hake %>%
  filter(Year %in% c(2010:2022))

#Fishing
#length  15 - 80 cm
hake_f<-ggplot(df_hake)+
  geom_ribbon(data = df_hake2, aes(x = Year, ymin = F_low, ymax = F_high),
              fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = df_hake3, aes(x = Year, ymin = F_low, ymax = F_high),
              fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = df_hake4, aes(x = Year, ymin = F_low, ymax = F_high),
              fill = "purple")+
  geom_line(aes(Year, F), color="black")+
  labs(x = "", y = "")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  geom_hline(yintercept = 0.26, color = "darkorange")+ # FMSY
  geom_hline(yintercept = 1.02, color = "gray50", lty = "dashed")+ #Fpa
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

hake_f

#SSB
hake_SSB <-ggplot(df_hake)+
  geom_vline(aes(xintercept = 1985), col = "black")+
  geom_vline(aes(xintercept = 2010), col = "black", size = 0.9)+
  geom_ribbon(data = df_hake2, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "steelblue3", show.legend = F, alpha = 0.5)+
  geom_ribbon(data = df_hake3, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "darkorange", show.legend = F, alpha = 0.5)+
  geom_ribbon(data = df_hake4,aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000),
              fill = "purple", show.legend = F)+
  geom_line(aes(Year, SSB), color="black")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  ylab("") + xlab(" ")+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))+
  xlim(1947, 2022)+
  geom_hline(yintercept = 56000/1000, color = "darkorange")+ #Btrigger
  geom_hline(yintercept = 56000/1000, color = "gray50", lty = "dashed")+ #Bpa
  geom_hline(yintercept = 40000/1000, color = "gray50", lty = "dotted") #Blim

hake_SSB

#no cpts, in billions
hake_R <-ggplot(df_hake)+
  geom_ribbon(data = df_hake2, aes(x=Year, ymin=R_low/100, ymax=R_high/100), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = df_hake3, aes(x=Year, ymin=R_low/100, ymax=R_high/100), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = df_hake4, aes(x=Year, ymin=R_low/100, ymax=R_high/100), fill = "purple")+
  geom_line(aes(Year, R_0/100), color = "black")+
  ylab("") + xlab(" ")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

hake_R


# arrange all figures ----
#install.packages("patchwork")
library(patchwork)

F_plaice + SSB_plaice + R_plaice +
  hake_f + hake_SSB + hake_R +
  f_herring + ssb_herring + R_herring +
  f_haddock + SSB_haddock + recruitment_haddock +
  saithe_f + saithe_SSB + saithe_R +
  f_cod + SSB_cod + recruitment_cod +
  plot_layout(ncol = 3)

