setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data/Stock_assessment_data_2021")

library(tidyverse)
library(readxl)
library(patchwork)
#colors regimes
# palevioletred4
# green4
# steelblue3
# darkorange
# purple

#Cod ######

cod <- read.csv("SA_cod_2021.csv", sep = ",")
#View(cod)

#lag SSB by one year (age_1)
# cod <- cod %>% 
#   mutate( SSB = lag(SSB),
#           SSB_low_lag = lag(SSB_low),
#           SSB_high_lag = lag(SSB_high))

#create data sets for SSB regimes
cod_1 <- cod %>% 
  filter(Year %in% c(1963:1972))

cod_2 <- cod %>% 
  filter (Year %in% c(1972:1999))

cod_3 <- cod %>% 
  filter (Year %in% c(1999:2021))


#####plot_SSB
#codrange <- mean(cod$SSB, na.rm = T)/5

#SSB in 1000 t
SSB_cod <- ggplot(cod)+
  geom_vline(aes(xintercept = 1972), col = "black")+ 
  #geom_text(label = 1975, x = 1977, y = 320, size = 3)+  
  geom_vline(aes(xintercept = 1999), col = "black", size = 0.9)+ 
  #geom_text(label = 2006, x = 2008, y = 320, size = 3)+
  geom_ribbon(cod_1, mapping = aes(x=Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "steelblue3", alpha = 0.5)+ #SSB and regimes
  geom_ribbon(cod_2, mapping = aes(x=Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(cod_3, mapping = aes(x=Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "purple")+
  geom_line(aes(x = Year, y = SSB/1000), col = "black")+
  #scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  ylim(0, 350)+
  xlim(1947, 2022)+
  geom_hline(yintercept = 97777/1000, col = "darkorange")+ #MSY Btrigger
  geom_hline(yintercept = 97777/1000, col = "gray50", linetype = "dashed")+ #Bpa 
  geom_hline(yintercept = 69841/1000, col = "gray50", linetype = "dotted")+ #Blim
  #annotate("text", x = 2013, y = 160000/1000, label = "MSY Btrigger", col="black", size = 3.5)+
  #xlab("Year")+ ylab("SSB in 1000 t")+
  #ggtitle("Cod")+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(), 
        axis.text = element_text(size = 12))+
  #theme(axis.text.x = element_text(angle = 45))+
  xlab("")+ ylab("")
  #labs(tag = "Cod")+
SSB_cod

#####plot_recruitment
#1979, 1986, 1997
#R in billions
recruitment_cod <-  ggplot(cod, aes(Year, R_1/1000000))+
  #geom_vline(aes(xintercept = 1972), col = "black")+ 
  #geom_text(label = 1975, x = 1977, y = 320, size = 3)+  
  #geom_vline(aes(xintercept = 1999), col = "black", size = 0.9)+ 
  geom_ribbon(data = cod_1, aes(x = Year, ymax = R_high/1000000, ymin = R_low/1000000), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = cod_2, aes(x = Year, ymax = R_high/1000000, ymin = R_low/1000000), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = cod_3, aes(x = Year, ymax = R_high/1000000, ymin = R_low/1000000), fill = "purple")+
  geom_line(color = "Black")+
  xlab("")+ylab("")+
  scale_y_continuous(breaks = c(0,1,2,3))+
  #scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  xlim(1947, 2022)+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
recruitment_cod


#####f and exploitation rate 
#catch/SBB = exploitation rate
cod <- cod %>%
  mutate(all_catches = Landings+Discards,
         exploi_rate = all_catches/SSB)


#ages 2-4
f_expl_cod <- ggplot(cod)+
  #geom_area(aes(y=exploi_rate/5, alpha = 0.7),fill = "skyblue2")+
  geom_ribbon(data = cod_1, aes(x=Year,ymin = F_low, ymax = F_high), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = cod_2, aes(x=Year,ymin = F_low, ymax = F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = cod_3, aes(x=Year,ymin = F_low, ymax = F_high), fill = "purple")+#fishing pressure
  geom_line(aes(Year, F_2_6), col = "black")+
  geom_hline(yintercept = 0.28, col = "darkorange")+ #FMSY
  geom_hline(yintercept = 0.49, col = "gray50", lty = "dashed")+ #Fpa
  geom_hline(yintercept = 0.58, col = "gray50", lty = "dotted")+ #Flim
  #scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1,1.2))+
  xlab("")+ ylab("")+
  xlim(1947, 2022)+
  #ggtitle("Fishing pressure and exploitation rate cod")+
  #annotate("text", x = 2020, y = 0.34, label = "FMSY", col="black", size = 3.5)+
  #scale_y_continuous(sec.axis = sec_axis(~.*5))+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
f_expl_cod




#Haddock####
haddock <- read.csv("SA_haddock_2021.csv", sep = ",")
#View(haddock)
#no lag needed. Age_0

#create data sets for SSB regimes (data starts in 1972)
#1) 1965-2001
#2) 2002-2019
haddock_1 <- haddock %>% 
  filter(Year %in% c(1965:2001))

haddock_2 <- haddock %>% 
  filter (Year %in% c(2001:2021))


#plot_SSB
#hadrange <- mean(haddock$SSB)/5
#SSB in 1000t
SSB_haddock <- ggplot(haddock)+
  geom_vline(aes(xintercept = 2001), col = "black", size = 0.9)+
  #geom_label(label = 2001, x = 2001, y = 700, size = 3)+
  geom_ribbon(haddock_1,mapping = aes(x=Year,ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "darkorange", alpha = 0.5)+ #SSB and regimes
  geom_ribbon(haddock_2,mapping = aes(x=Year,ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "purple")+
  geom_line(aes(Year, SSB/1000), col = "black")+
  #scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  geom_hline(yintercept = 132000/1000, col = "darkorange" )+ #MSY Btrigger
  geom_hline(yintercept = 132000/1000, col = "gray50", lty = "dashed")+ #Bpa
  geom_hline(yintercept = 94000/1000, col = "gray50", lty = "dotted" )+ #Blim
  #annotate("text", x = 1973.5, y = 119000/1000, label = "MSY Btrigger", col="black", size = 3.5)+
  xlab("")+ ylab("")+
  #ggtitle("Haddock")+
  xlim(1947, 2022)+
  theme_test()+
  theme(plot.margin = margin(0,0,0,0), 
        plot.background = element_blank(),
        axis.text = element_text(size = 12))

SSB_haddock

#####plot_recruitment
#1973, 1979
#in billions, age 0
recruitment_haddock <-  ggplot(haddock, aes(Year, R_0/1000000))+
  #geom_vline(aes(xintercept = 2001), col = "black", size = 0.9)+
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




#####f and exploitation rate 
#catch/SBB = exploitation rate
#ages 2-4
haddock <- haddock %>%
  mutate(all_catches = Landings+Discards+Industrial_bycatch,
         exploi_rate = all_catches/SSB)


f_expl_haddock <- ggplot(haddock, aes(Year))+
  #geom_area(aes(y=exploi_rate/5, alpha = 0.7),fill = "skyblue2")+
  geom_ribbon(data = haddock_1, aes(x=Year,ymin = F_low, ymax = F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = haddock_2, aes(x=Year,ymin = F_low, ymax = F_high), fill = "purple")+#fishing pressure
  geom_line(aes(Year, F_2_4), col = "black")+
  geom_hline(yintercept = 0.19, col = "darkorange")+ #FMSY
  geom_hline(yintercept = 0.194, col = "gray50", lty = "dashed")+ #Fpa
  geom_hline(yintercept = 0.39, col = "gray50", lty = "dotted")+ #Flim
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  #ylim(0,3.5)+
  #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1,1.2))+
  #xlab("Year")+ ylab("F (ages 2-4)")+
  #ggtitle("Fishing pressure and exploitation rate cod")+
  #annotate("text", x = 2020, y = 0.34, label = "FMSY", col="black", size = 3.5)+
  #scale_y_continuous(sec.axis = sec_axis(~.*5, name = ""))+
  theme_test()+
  xlab("") + ylab("")+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0), 
        plot.background = element_blank(),
        
        axis.text = element_text(size = 12))
  #theme(axis.title.x = element_blank(),
       # axis.title.y = element_blank(),
        #legend.position ="none")
f_expl_haddock





#Norway Pout ----
# pout <- read.csv("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Norway_Pout/Norwayp_ICES_assessment.csv", sep = ";")
# pout <- pout[-c(37:56), ]
# pout[36, 10] <- NA
# names(pout) <- c("Year", "R_age0", "R_high", "R_low", "SSB", "SSB_high", "SSB_low", "Catches",
#                  "F1_2", "F_high", "F_low")
# View(pout)
# pout <- pout%>%
#   mutate(explr = Catches/SSB)
# 
# 
# poutrange <- mean(pout$SSB)/5
# SSB_pout <- ggplot(data = pout, aes(x = Year))+
#   geom_vline(aes(xintercept = 1995), col = "black", linetype = 2)+
#   #geom_label(aes(label = "1995", x = 1995, y = 20), size = 3)+
#   geom_vline(aes(xintercept = 2002), col = "black", linetype = 2)+
#   #geom_label(label = 2002, x = 2002, y = 250, size = 3)+
#   geom_vline(aes(xintercept = 2012), col = "black", linetype = 2)+
#   #geom_label(label = 2012, x = 2012, y = 20, size = 3)+
#   geom_ribbon(data = pout[pout$Year <=1995, ],
#               aes(x = Year , ymin = SSB/1000-poutrange/1000, ymax = SSB/1000+poutrange/1000), fill = "green4", alpha = 0.75)+
#   geom_ribbon(data = pout[pout$Year %in%c(1995:2002), ],
#               aes(x = Year , ymin = SSB/1000-poutrange/1000, ymax = SSB/1000+poutrange/1000), fill = "steelblue3", alpha = 0.75)+
#   geom_ribbon(data = pout[pout$Year  %in%c(2002:2012), ],
#               aes(x = Year ,  ymin = SSB/1000-poutrange/1000, ymax = SSB/1000+poutrange/1000), fill = "palevioletred4", alpha = 0.75)+
#   geom_ribbon(data = pout[pout$Year >=2012, ],
#               aes(x = Year ,  ymin = SSB/1000-poutrange/1000, ymax = SSB/1000+poutrange/1000), fill = "darkorange", alpha = 0.75)+
#   geom_line(aes(y = SSB/1000))+
#   geom_hline(aes(yintercept = 39450/1000), col = "darkorange")+ #lim
#   scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
#   scale_y_continuous(breaks = c(0, 50, 100, 150, 200))+
#   labs(y = "SSB in 1000 t", x = "")+ #SSB in 1000 t
#   theme_test()+
#   theme(plot.margin = margin(0,0,0,0))+
#   annotate("text", x = 1990, y = 200, label = "N. pout")
#   #labs(tag = "N. pout")
# 
# SSB_pout
# 
# #1989, 1999, 2013
# R_pout <- ggplot(data = pout, aes(x = Year))+
#   geom_vline(xintercept = 1989, linetype = 2)+
#   geom_vline(xintercept = 1999, linetype = 2)+
#   geom_vline(xintercept = 2013, linetype = 2)+
#   geom_ribbon(aes(ymin = R_low/1000, ymax = R_high/1000), fill = "gray80")+
#   geom_line(aes(y = R_age0/1000),  col = "black")+
#   labs(y = "Recruitment in billions", x = "")+
#   scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
#   theme_test()+
#   theme(plot.margin = margin(0,0,0,0))
# 
# R_pout
# 
# F_expl_pout <- ggplot(data = pout, aes(x = Year))+
#   geom_ribbon(aes(ymin = F_low, ymax = F_high), fill = "gray80")+
#   geom_line(aes(y = F1_2))+
#   #geom_area(aes(y = explr/5, alpha = 0.7), fill = "skyblue2")+
#   geom_hline(aes(yintercept = 0.7), col = "darkorange")+ #F cap
#   #scale_y_continuous(sec.axis = sec_axis(~.*5, name = " "))+
#   labs(y = "F ages 1-2", x = " ")+ #F ages 1-2
#   scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
#   theme_test()+
#   theme(plot.margin = margin(0,0,0,0))+
#   theme(legend.position = "none")
# 
# F_expl_pout
# 
# #Plaice ####
plaice <- read.csv("SA_plaice_2021.csv", 
                   sep = ",")

plaice[is.na(plaice)] <- 0
plaice <- plaice%>%
  mutate(totalcatch = Landings+Discards)%>%
  mutate(explr = totalcatch/SSB)%>%
  mutate(SSB_lag = lag(SSB),
         SSB_high_lag = lag(SSB_high),
         SSB_low_lag = lag(SSB_low))
#View(plaice)

#plarange <- mean(plaice$SSB)/5
#SSB in 1000t
SSB_plaice <- ggplot(data = plaice, aes(x = Year))+
  geom_vline(aes(xintercept = 1969), col = "black")+
  #geom_label(label = 1970, x = 1970, y = 1000, size = 3)+
  geom_vline(aes(xintercept = 1991), col = "black")+
  #geom_label(label = 1985, x = 1985, y = 700, size = 3)+
  geom_vline(aes(xintercept = 2007), col = "black", size = 0.9)+
  #geom_label(label = 2008, x = 2008, y = 1000, size = 3)+
  geom_ribbon(data = plaice[plaice$Year <=1969, ],
              aes(x = Year , ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "green4", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year  %in%c(1969:1991), ],
              aes(x = Year ,  ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year %in%c(1991:2007), ],
            aes(x = Year ,  ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "darkorange", alpha = 0.5)+
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
  #geom_vline(aes(xintercept = 1969), col = "black")+
  #geom_vline(aes(xintercept = 1991), col = "black")+
  #geom_vline(aes(xintercept = 2007), col = "black", size = 0.9)+
  geom_ribbon(data = plaice[plaice$Year <=1969, ],
              aes(x = Year , ymin = R_low/1000000, ymax = R_high/1000000), fill = "green4", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year  %in%c(1969:1991), ],
              aes(x = Year ,  ymin = R_low/1000000, ymax = R_high/1000000), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year %in%c(1991:2007), ],
              aes(x = Year ,  ymin = R_low/1000000, ymax = R_high/1000000), fill = "darkorange", alpha = 0.5)+
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
F_expl_plaice <- ggplot(data = plaice, aes(x = Year))+
  geom_ribbon(data = plaice[plaice$Year <=1969, ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "green4", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year %in%c(1969:1991), ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year %in%c(1991:2007), ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = plaice[plaice$Year >=2007, ],
              aes(x = Year , ymin = F_low, ymax = F_high), fill = "purple")+
  geom_line(aes(y = F_2_6))+
  #geom_area(aes(y = explr/2, alpha = 0.7), fill = "skyblue2")+
  geom_hline(aes(yintercept = 0.21), color = "darkorange")+ #FMSY
  geom_hline(aes(yintercept = 0.769), color = "gray50",lty = "dashed")+ #Fpa
  #scale_y_continuous(sec.axis = sec_axis(~.*2, name = ""))+
  #xlim(1945, 2020)+
  labs(y = "", x = "")+ #F (ages 2-6)
  theme_test()+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0), plot.background = element_blank(),
        
        axis.text = element_text(size = 12))+
  theme(legend.position = "none")

F_expl_plaice

#Sprat ####
# sprat <- read.csv("~/Desktop/Uni/M.Sc.MarineÖkosystemwissenschaften/WS19:20/Article_AF/Sprat/Sprat_ICES_data.csv", sep = ";")
# names(sprat) <- c("Year", "R_age0", "R_high", "R_low", "SSB", "SSB_high", "SSB_low", "Catches",
#                   "F1_2", "F_high", "F_low")
# 
# sprat <-sprat%>%
#   mutate(explr = Catches/SSB)
# View(sprat)
# 
# spratrange <- mean(sprat$SSB)/5
# SSB_sprat <- ggplot(data = sprat, aes(x = Year))+
#   geom_vline(aes(xintercept = 1982), col = "black", linetype = 2)+
#   #geom_label(label = 1982, x = 1982, y = 690, size = 3)+
#   geom_ribbon(data = sprat[sprat$Year <=1982, ],
#               aes(x = Year , ymin = SSB/1000-spratrange/1000, ymax = SSB/1000+spratrange/1000), fill = "green4", alpha = 0.75)+
#   geom_ribbon(data = sprat[sprat$Year >=1982, ],
#               aes(x = Year , ymin = SSB/1000-spratrange/1000, ymax = SSB/1000+spratrange/1000), fill = "steelblue3", alpha = 0.75)+
#   geom_line(aes(y = SSB/1000))+
#   geom_hline(aes(yintercept = 125000/1000), colour = "darkorange")+ #escapement
#   labs(y = "SSB in 1000 t ", x = " ")+ #SSB in 1000 t
#   scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
#   annotate("text", x = 2000, y = 600, label = "Sprat")+
#   theme_test()+
#   theme(plot.margin = margin(0,0,0,0))
#   
# 
# SSB_sprat
# 
# #1975, 1977, 1979
# R_sprat <- ggplot(data = sprat, aes(x = Year))+
#   geom_vline(xintercept = 1975, linetype = 2)+
#   geom_vline(xintercept = 1977, linetype = 2)+
#   geom_vline(xintercept = 1979, linetype = 2)+
#   geom_ribbon(aes(ymin = R_low/1000000, ymax = R_high/1000000), fill = "gray80")+
#   geom_line(aes(y = R_age0/1000000),  col = "black")+
#   labs(y = "Recruitment in billions", x = "")+ #R in billions
#   scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
#   theme_test()+
#   theme(plot.margin = margin(0,0,0,0))
# 
# R_sprat
# 
# F_expl_sprat <- ggplot(data = sprat, aes(x = Year))+
#   geom_ribbon(aes(ymin = F_low, ymax = F_high), fill = "gray80")+
#   geom_line(aes(y = F1_2))+
#   #geom_area(aes(y = explr/3, alpha = 0.7), fill = "skyblue2")+
#   #scale_y_continuous(sec.axis = sec_axis(~.*3, name = " "))+
#   scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
#   geom_hline(aes(yintercept = 0.69), color = "darkorange")+ #Fcap
#   labs(y = "F ages 1-2", x = " ")+ #F ages 1-2
#   theme_test()+
#   theme(plot.margin = margin(0,0,0,0))+
#   theme(legend.position = "none")
# 
# F_expl_sprat


########## Herring ##########
#SSB


herring <-read.csv("SA_herring_2021.csv", sep = ",")
#View(herring)

herring <- herring %>% mutate(
  exploit_rate = t_catch/SSB)

herring2<- herring %>%
  filter(Year %in% c(1947:1966))

herring3<-herring %>%
  filter(Year %in% c(1967:1983))


#herrange <- mean(herring$SSB)/5
#SSB in 1000 t
ssb_herring <- ggplot(herring)+ ylab ("")+
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
  #annotate("text",x=1955,y=1, label= " MSYBtrigger", col="darkorange" ,size=5  ,fontface="bold" ) + 
  theme_test() + 
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        
        axis.text = element_text(size = 12))+
  labs(x = "", y = "")


ssb_herring 

#Recruitment _Herring
#in billions
R_herring<- ggplot(data=herring, aes(x=Year, y=R_0/1000000)) + ylab("")+ xlab("")+
  #geom_vline(aes(xintercept = 1966), col = "black")+
  #geom_vline(aes(xintercept = 1983), col = "black")+
  #geom_vline(aes(xintercept = 2000), col = "black", size = 0.9)+
  geom_ribbon(data= herring[herring$Year < 1966, ],
              aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "green4", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1966:1983), ],
              aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1983:2001), ],
              aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year >= 2000, ], aes(ymin = R_low/1000000,  ymax = R_high/1000000), fill = "purple")+
  geom_line(col = "black")+
  #geom_errorbar(aes(ymin=l_R, ymax=h_R), width=.2,position=position_dodge(.9))+
  theme_test() + 
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  scale_y_continuous(breaks = c(0, 50, 100))+
  labs(x = "", y = "")+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        
        axis.text = element_text(size = 12))
#theme(axis.text = element_text(colour = "black",size = "15",face = "bold"))
R_herring

#F/Exploitation rate

#ages 2-6 
f_exp_herring <- ggplot(herring, aes(Year))+
  #geom_area(aes(y=exploit_rate/10),alpha=0.7, fill="skyblue2")+
  geom_ribbon(data= herring[herring$Year < 1966, ],
              aes(ymin= F_low, ymax= F_high), fill = "green4", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1966:1983), ],
              aes(ymin= F_low, ymax= F_high), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year %in% c(1983:2000), ],
              aes(ymin= F_low, ymax= F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data= herring[herring$Year >= 2000, ], 
              aes(ymin= F_low, ymax= F_high), fill = "purple")+
  geom_line(aes(Year, F_2_6), col="black")+
  geom_hline(yintercept = 0.31, col="darkorange") + #Fmsy
  geom_hline(yintercept = 0.31, col="gray50", lty = "dashed" ) + #Fpa
  geom_hline(yintercept = 0.4, col="gray50", lty = "dotted" ) + #Flim
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  #  ylim(0,0.5,1.5)+
  #scale_y_continuous(breaks =c(0,0.2,0.4,0.6,0.8,1,1.2)) +
  xlab("")+ylab("") + #winter ringers
  #scale_y_continuous(sec.axis = sec_axis(~.*10,name= "")) +
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        
        axis.text = element_text(size = 12))

f_exp_herring




### Saithe ####


df_saithe<- read.csv("SA_saithe_2021.csv", sep = ",")

#View(df_saithe)

# df_saithe<-df_saithe %>% mutate(
#   # catches = Wanted_catch+Unwanted_catch,
#   # exploit_rate = catches/SSB,
#   SSB = SSB/1000,
#   R_age3 = R_3/1000,
#   R_low = R_low/1000,
#   R_high = R_high/1000)

df_saithe2<-df_saithe %>%
  filter(Year %in% c(1967:1975))

df_saithe3<-df_saithe %>%
  filter(Year %in% c(1975:2010))

df_saithe4 <- df_saithe%>%
  filter(Year %in% c(2010:2022))

# df_saithe$exploit_rate<-df_saithe$exploit_rate/4

#ages 4 - 7
saithe_F_Expl<-ggplot(df_saithe)+
  geom_ribbon(data = df_saithe2, aes(x=Year, ymin=F_low, ymax=F_high), fill="steelblue3", alpha = 0.5)+
  geom_ribbon(data = df_saithe3, aes(x=Year, ymin=F_low, ymax=F_high), fill="darkorange", alpha = 0.5)+
  geom_ribbon(data = df_saithe4, aes(x=Year, ymin=F_low, ymax=F_high), fill="purple")+
  #geom_area(mapping=aes(x=Year, y=exploit_rate, alpha=0.7),fill="skyblue2", show.legend = F)+
  geom_line(aes(Year, F_4_7), color="black")+
  xlab(" ")+ 
  ylab("")+
  #scale_y_continuous(sec.axis = sec_axis(~./4))+
  geom_hline(yintercept = 0.363, color = "darkorange")+ #FMSY
  geom_hline(yintercept = 0.576, color = "gray50",lty = "dashed")+ #Fpa
  geom_hline(yintercept = 0.668, color = "gray50",lty = "dotted")+ #Flim
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8))+
  #annotate("text", label = "Fmsy", x=1990, y=0.30, size=3.5, color="darkorange")+
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        
        axis.text = element_text(size = 12))

saithe_F_Expl

#sairange <- mean(df_saithe$SSB)/5
#SSB in 1000t
saithe_SSB <-ggplot(df_saithe)+
  geom_vline(aes(xintercept = 1975), col = "black")+
  geom_vline(aes(xintercept = 2010), col = "black", size = 0.9)+
  #geom_label(label = 1982, x = 1982, y = 550, size = 3)+
  geom_ribbon(data=df_saithe2, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "steelblue3", show.legend = F, alpha = 0.5)+
  geom_ribbon(data=df_saithe3, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "darkorange", show.legend = F, alpha = 0.5)+
  geom_ribbon(data=df_saithe4, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "purple", show.legend = F)+
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

#1976
#R in billions
saithe_R <-ggplot(df_saithe)+
  #geom_vline(aes(xintercept = 1975), col = "black")+
  #geom_vline(aes(xintercept = 2010), col = "black", size = 0.9)+
  geom_ribbon(data = df_saithe2, aes(x=Year, ymin=R_low/100000, ymax=R_high/100000), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = df_saithe3, aes(x=Year, ymin=R_low/100000, ymax=R_high/100000), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = df_saithe4, aes(x=Year, ymin=R_low/100000, ymax=R_high/100000), fill = "purple")+
  geom_line(aes(Year, R_3/100000), col = "black")+
  ylab("") + xlab(" ")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        
        axis.text = element_text(size = 12))
saithe_R

### Hake ####

df_hake <- read.csv("SA_hake_2021.csv", sep = ",")
#View(df_hake)

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

df_hake$exploit_rate<-df_hake$exploit_rate/4

#length  15 - 80 cm
hake_F_Expl<-ggplot(df_hake)+
  #geom_area(mapping=aes(x=Year, y=exploit_rate, alpha=0.7),fill="skyblue2", show.legend = F)+
  geom_ribbon(data = df_hake2, aes(x = Year, ymin = F_low, ymax = F_high), fill = "steelblue3", alpha = 0.5)+
  geom_ribbon(data = df_hake3, aes(x = Year, ymin = F_low, ymax = F_high), fill = "darkorange", alpha = 0.5)+
  geom_ribbon(data = df_hake4, aes(x = Year, ymin = F_low, ymax = F_high), fill = "purple")+
  geom_line(aes(Year, F), color="black")+
  labs(x = "", y = "")+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2018))+
  #scale_y_continuous(name = expression(""), sec.axis = sec_axis(~./4,name = ""))+
  geom_hline(yintercept = 0.26, color = "darkorange")+ # FMSY
  geom_hline(yintercept = 1.02, color = "gray50", lty = "dashed")+ #Fpa
  theme_test()+
  xlim(1947, 2022)+
  theme(plot.margin = margin(0,0,0,0), 
        plot.background = element_blank(),
        
        axis.text = element_text(size = 12))

hake_F_Expl

#hakerange <- mean(df_hake$SSB)/5
hake_SSB <-ggplot(df_hake)+
  geom_vline(aes(xintercept = 1985), col = "black")+ 
  #geom_label(label = 1986, x = 1986, y = 300, size = 3)+
  geom_vline(aes(xintercept = 2010), col = "black", size = 0.9)+ 
  #geom_label(label = 2008, x = 2008, y = 300, size =3)+
  geom_ribbon(data=df_hake2, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "steelblue3", show.legend = F, alpha = 0.5)+
  geom_ribbon(data=df_hake3, aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "darkorange", show.legend = F, alpha = 0.5)+
  geom_ribbon(data=df_hake4,aes(Year, ymin = SSB_low/1000, ymax = SSB_high/1000), fill = "purple", show.legend = F)+
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
  #geom_vline(aes(xintercept = 1985), col = "black")+ 
  #geom_vline(aes(xintercept = 2010), col = "black", size = 0.9)+ 
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


#grid arrange all figures ----

# gridExtra::grid.arrange(grobs= list(SSB_plaice, r_plaice, F_expl_plaice,
#                                     hake_SSB, hake_R, hake_F_Expl,
#                                     ssb_herring,R_herring,f_exp_herring,
#                                     SSB_sprat, R_sprat, F_expl_sprat,
#                                     SSB_haddock, recruitment_haddock,f_expl_haddock,
#                                     saithe_SSB, saithe_R, saithe_F_Expl,
#                                     SSB_pout, R_pout, F_expl_pout,
#                                     SSB_cod, recruitment_cod, f_expl_cod),
#                         nrow = 8, ncol = 3)


#same margins 
#install.packages("patchwork")
library(patchwork)
SSB_plaice + R_plaice + F_expl_plaice+
  hake_SSB+ hake_R+ hake_F_Expl+
  ssb_herring+R_herring+f_exp_herring+
  SSB_sprat+ R_sprat+ F_expl_sprat+
  SSB_haddock+ recruitment_haddock+f_expl_haddock+ 
  saithe_SSB+ saithe_R+ saithe_F_Expl+
  SSB_pout+ R_pout+ F_expl_pout+
  SSB_cod+ recruitment_cod+ f_expl_cod +
  plot_layout(ncol = 3)


#without sprat and Norway Pout
F_expl_plaice + SSB_plaice + R_plaice +
  hake_F_Expl + hake_SSB + hake_R +
  f_exp_herring + ssb_herring + R_herring +
  f_expl_haddock + SSB_haddock + recruitment_haddock +
  saithe_F_Expl + saithe_SSB + saithe_R +
  f_expl_cod + SSB_cod + recruitment_cod +
  plot_layout(ncol = 3)

#For R
R_plaice + F_expl_plaice + SSB_plaice +
  hake_R + hake_F_Expl + hake_SSB +
  R_herring + f_exp_herring + ssb_herring +
  recruitment_haddock + f_expl_haddock + SSB_haddock +
  saithe_R + saithe_F_Expl + saithe_SSB +
  recruitment_cod + f_expl_cod + SSB_cod +
  plot_layout(ncol = 3)


# 
# 
#first 4 species
#same margins
# SSB_plaice + R_plaice+ F_expl_plaice+
#   hake_SSB+ hake_R+ hake_F_Expl+
#   ssb_herring+R_herring+f_exp_herring+
#   SSB_sprat+ R_sprat+ F_expl_sprat +
#   plot_layout(ncol = 3)
# #second 4 species
# SSB_haddock+ recruitment_haddock+f_expl_haddock+
#   saithe_SSB+ saithe_R+ saithe_F_Expl+
#   SSB_pout+ R_pout+ F_expl_pout+
#   SSB_cod+ recruitment_cod+ f_expl_cod +
#   plot_layout(ncol = 3)

