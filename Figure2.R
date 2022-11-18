#Figure 2

library(tidyverse)
library(ggrepel)
library(readxl)

##### plaice #####
plaice <- read.csv("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Data_2021/SA_plaice_2021.csv", 
                   sep = ",")

plaice <- plaice%>%
  mutate(totalcatch = Landings+Discards)%>%
  mutate(explr = totalcatch/SSB)%>%
  mutate(SSB_lag = lag(SSB),
         SSB_high_lag = lag(SSB_high),
         SSB_low_lag = lag(SSB_low))

color_regimes_plaice <- NULL
color_regimes_plaice[plaice$Year %in% c(1957:1969)] <- "green4"
color_regimes_plaice[plaice$Year %in% c(1969:1991)] <- "steelblue3"
color_regimes_plaice[plaice$Year %in% c(1991:2007)] <- "darkorange"
color_regimes_plaice[plaice$Year %in% c(2007:2021)] <- "purple"

Hyst_plaice <- ggplot(data = plaice, aes(x = F_2_6, y = SSB/1000))+
  geom_hline(yintercept = 564.599, linetype="dashed",color="gray30")+
  geom_vline(xintercept = 0.21, linetype="dashed",color="gray30")+
  geom_label(x = 0.21, y = 220, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  #geom_label(x = 0.8, y = 564.599, label = expression("MSY B"[trigger]), color="gray30", size = 3.5, fontface = "bold")+
  geom_label(x = 0.4, y = 564.599, label = expression("MSY B"[trigger]), color="gray30", size = 3.5, fontface = "bold")+
  geom_path()+
  geom_point(col = color_regimes_plaice, size = 2)+
  labs(x = "", y = "")+
  geom_text_repel(data = plaice[1, ], aes(label = Year), #1957
                  point.padding = 0.2,nudge_y = 0, nudge_x = -0.1, size=3,col="gray30", segment.size =0.2 )+ 
  geom_text_repel(data = plaice[13, ], aes(label = Year), #1969
                  point.padding = 0.2, nudge_y =-145,nudge_x= 0, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[35, ], aes(label = Year), #1991
                  point.padding = 0.2, nudge_y = 0, nudge_x = 0.2, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[51, ], aes(label = Year), #2007
                  point.padding = 0.2, nudge_y = 0,nudge_x= -0.2, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[64, ], aes(label = Year), #2021
                  point.padding = 0.2, nudge_x = -0.1, size=3, col="gray30", segment.size =0.2 )+
  xlim(0, 0.8)+ 
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9),
        plot.background = element_blank(), 
        axis.text = element_text(size = 12))
  

Hyst_plaice

#### Hake ####
library(readxl)
df_hake <- read.csv("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Data_2021/SA_hake_2021.csv")

df_hake<-df_hake %>% mutate(
  R = R_0,
  Fishing = F,
  SSB = SSB/1000)

decade_names <- NULL
decade_names[df_hake$Year %in% 1970] <- "1970"
decade_names[df_hake$Year %in% 1980] <- "1980"
decade_names[df_hake$Year %in% 1990] <- "1990"
decade_names[df_hake$Year %in% 2000] <- "2000"
decade_names[df_hake$Year %in% 2010] <- "2010"
decade_names[df_hake$Year %in% 2018] <- "2018"

df_hake$decade_names <- decade_names

Regimes <- NULL
Regimes[df_hake$Year > 2009] <- "purple"
Regimes[df_hake$Year < 2009] <- "darkorange"
Regimes[df_hake$Year < 1985] <- "steelblue3"
df_hake$Regimes <- Regimes


Hyst_hake <- ggplot(data = df_hake, aes(x = Fishing, y = SSB))+
  geom_hline(yintercept = 56, linetype="dashed", color="gray30")+
  geom_vline(xintercept = 0.26, linetype="dashed", color="gray30")+
  geom_label(x = 0.26, y = 120, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  #geom_label(x = 0.15, y = 56, label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_label(x = 0.15, y = 56, label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path()+
  geom_point(col = Regimes, size = 2) +
  labs(x = "", y = "")+
  geom_text_repel(data = df_hake[1, ], aes(label = Year), #1978
                  point.padding = 0.2,nudge_y = -40, nudge_x = 0,size=3,col="gray30",segment.size =0.2 ) +
  geom_text_repel(data = df_hake[8, ], aes(label = Year), #1985
                  point.padding = 0.2,nudge_y = 45, nudge_x = 0,size=3,col="gray30",segment.size =0.2 ) +
  geom_text_repel(data = df_hake[33, ], aes(label = Year), #2010
                  point.padding = 0.2, nudge_y = 0,nudge_x= 0.2,size=3,col="gray30",segment.size =0.2 ) +
  geom_text_repel(data = df_hake[43, ], aes(label = Year), #2021
                  point.padding = 0.2, nudge_y = 0,nudge_x= 0.1,size=3,col="gray30",segment.size = 0.2 ) +
  xlim(0, 1.3)+ #run with 0, 1.3 when all should have the same x range
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
 

Hyst_hake

####Herring####
herring <-read.csv("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Data_2021/SA_herring_2021.csv")
colors_regimes <- NULL
colors_regimes[herring$Year %in% c(1947:1966)] <- "green4"
colors_regimes[herring$Year %in% c(1966:1983)] <- "steelblue3"
colors_regimes[herring$Year %in% c(1983:2000)] <- "darkorange"
colors_regimes[herring$Year %in% c(2000:2021)] <- "purple"

Hyst_herring <- ggplot(data = herring, aes(x = F_2_6, y = SSB/1000))+
  geom_hline(yintercept = 1232.828,linetype = "dashed", color = "gray30")+
  geom_vline(xintercept = 0.26,linetype = "dashed", color = "gray30")+
  geom_label(x = 0.26, y = 4600, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 1, y = 1232.828, label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path()+
  geom_point(col = colors_regimes, size = 2) +
  labs(x = "", y = " ") +
  geom_text_repel(data = herring[1, ], aes(label = Year), #1947
                  point.padding = 0.2, nudge_x = 0.3,col="gray30",size=3,segment.size = 0.2)+ 
  geom_text_repel(data = herring[17, ], aes(label = Year), #1963
                  point.padding = 0.2, nudge_x = 0.2,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[37, ], aes(label = Year), #1983
                  point.padding = 0.2, nudge_x = 0.2,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[54, ], aes(label = Year), #2000
                  point.padding = 0.2, nudge_x = 0.25,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[74, ], aes(label = Year), #2020
                  point.padding = 0.2, nudge_x = -0.084, nudge_y = -1000,col="gray30",size=3,segment.size = 0.2)+
  #xlim(0,1.3)+
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
  

Hyst_herring

####Haddock#####----
haddock <- read.csv("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Data_2021/SA_haddock_2021.csv")

haddock <- haddock[-50,]

#all years without legend 

decade_names <- NULL
decade_names[haddock$Year %in% 1972] <- "1972"
decade_names[haddock$Year %in% 1980] <- "1980"
decade_names[haddock$Year %in% 1990] <- "1990"
decade_names[haddock$Year %in% 2000] <- "2000"
decade_names[haddock$Year %in% 2010] <- "2010"
decade_names[haddock$Year %in% 2018] <- "2018"

haddock$decade_names <- decade_names

#add color to points to highlight regimes
colors_regimes <- NULL
colors_regimes[haddock$Year %in% c(1972:2001)] <- "darkorange"
colors_regimes[haddock$Year %in% c(2001:2021)] <- "purple"
haddock$colors_regimes <- colors_regimes

Hyst_haddock <- ggplot(data = haddock, aes(x = F_2_4, y = SSB/1000))+
  geom_hline(yintercept=132,linetype="dashed",color="gray30")+
  geom_vline(xintercept=0.19,linetype="dashed",color="gray30")+
  geom_label(x = 0.2, y = 520, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 0.1, y = 132, label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path()+
  geom_point(col = colors_regimes, size = 2) +
  labs(x = "", y = "") +
  geom_text_repel(data = haddock[1, ], aes(label = Year), #1972
                  point.padding = 0, nudge_y =150, nudge_x=0,col="gray30",size=3,segment.size = 0.2) + 
  geom_text_repel(data = haddock[30, ], aes(label = Year), #2001
                  point.padding = 0, nudge_x =0.12,col="gray30",size=3,segment.size = 0.2) +
  geom_text_repel(data = haddock[49, ], aes(label = Year), #2020
                  point.padding = 0.2,nudge_y = -100, nudge_x = 0.04,col="gray30",size=3,segment.size = 0.2)+
  xlim(0, 1)+
  theme_test()+
  theme(plot.tag.position = c(0.85, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
  

Hyst_haddock

####Saithe ####

df_saithe <- read.csv("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Data_2021/SA_saithe_2021.csv")

df_saithe <- df_saithe[-55,]

df_saithe<-df_saithe %>% mutate(
  catches = Landings+Discards,
  exploit_rate = catches/SSB,
  SSB = SSB/1000,
  R = R_3/1000,
  R_low = R_low/1000,
  R_high = R_high/1000)

decade_names <- NULL
decade_names[df_saithe$Year %in% 1976] <- "1976"
decade_names[df_saithe$Year %in% 1970] <- "1970"
decade_names[df_saithe$Year %in% 1980] <- "1980"
decade_names[df_saithe$Year %in% 1990] <- "1990"
decade_names[df_saithe$Year %in% 2000] <- "2000"
decade_names[df_saithe$Year %in% 2010] <- "2010"
decade_names[df_saithe$Year %in% 2016] <- "2016"

df_saithe$decade_names <- decade_names

colors_regimes <- NULL
colors_regimes[df_saithe$Year %in% c(1967:1975)] <- "green4"
colors_regimes[df_saithe$Year %in% c(1975:2010)] <- "steelblue3"
colors_regimes[df_saithe$Year %in% c(2010:2020)] <- "palevioletred4"
df_saithe$colors_regimes <- colors_regimes

Hyst_saithe <- ggplot(data = df_saithe, aes(x = F_4_7, y = SSB))+
  geom_hline(yintercept=149.098,linetype="dashed",color="gray30")+
  geom_vline(xintercept=0.363,linetype="dashed",color="gray30")+
  geom_label(x = 0.363, y = 550, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 0.1, y = 149.098,label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path()+
  geom_point(col =colors_regimes, size = 2) +
  labs(x = "", y = "") +
  geom_text_repel(data = df_saithe[1, ], aes(label = Year), #1967
                  point.padding = 0.2, nudge_y =30,nudge_x= -0.2,col="gray30",size=3,segment.size = 0.2)+ 
  geom_text_repel(data = df_saithe[10, ], aes(label = Year), #1976
                  point.padding = 0.2, nudge_y =0,nudge_x= 0.2,col="gray30",size=3,segment.size = 0.2)+ 
  geom_text_repel(data = df_saithe[54, ], aes(label = Year), #2018
                  point.padding = 0.1,nudge_y =0 ,  nudge_x = -0.2,col="gray30",size=3,segment.size = 0.2)+
  xlim(0, 0.8)+
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
  

Hyst_saithe

#### Cod ####
cod <- read.csv ("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Data_2021/SA_cod_2021.csv")

#all years without legend 
decade_names <- NULL
decade_names[cod$Year %in% 1963] <- "1963"
decade_names[cod$Year %in% 1970] <- "1970"
decade_names[cod$Year %in% 1980] <- "1980"
decade_names[cod$Year %in% 1990] <- "1990"
decade_names[cod$Year %in% 2000] <- "2000"
decade_names[cod$Year %in% 2010] <- "2010"
decade_names[cod$Year %in% 2018] <- "2018"

cod$decade_names <- decade_names

#add color to points to highlight regimes
colors_regimes <- NULL
colors_regimes[cod$Year %in% c(1963:1972)] <- "steelblue3"
colors_regimes[cod$Year %in% c(1972:1999)] <- "darkorange"
colors_regimes[cod$Year %in% c(1999:2020)] <- "purple"
cod$colors_regimes <- colors_regimes

Hyst_cod <- ggplot(data = cod, aes(x = F_2_6, y = SSB/1000))+
  geom_hline(yintercept=97777/1000,linetype="dashed",color="gray30")+
  geom_vline(xintercept=0.28,linetype="dashed",color="gray30")+
  geom_label(aes(x = 0.28, y = 230), label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(aes(x = 0.15, y = 97777/1000), label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path()+
  geom_point(col = colors_regimes, size = 2) +
  labs(x = "", y = "") +
  geom_text_repel(data = cod[1, ], aes(label = Year), #1963
                  point.padding = 0.2, nudge_x = 0,nudge_y = 40,col="gray30",size=3,segment.size = 0.2)+ 
  geom_text_repel(data = cod[10, ], aes(label = Year), #1972
                  point.padding = 0.2, nudge_y = , nudge_x = 0.2,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = cod[37, ], aes(label = Year), #1999
                  point.padding = 0., nudge_y = 30, nudge_x = 0.1,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = cod[58, ], aes(label = Year), #2020
                  point.padding = 0.1,nudge_y = 20,  nudge_x = 0,col="gray30",size=3,segment.size = 0.2)+
  xlim(0, 1.2)+
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
  

Hyst_cod

#install.packages("patchwork")
library(patchwork)

Hyst_plaice+Hyst_hake+
  Hyst_herring+Hyst_haddock+
  Hyst_saithe+ Hyst_cod+
  plot_layout(ncol =2)






