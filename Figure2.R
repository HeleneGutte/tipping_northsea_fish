#2_Hysteresis_analysis ----

library(tidyverse)
library(ggrepel)
library(readxl)

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

#setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data/Stock_Assessment_Data_2021")

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

## plotting hyst break ----
hyst_phases_plaice <- NULL
hyst_phases_plaice[plaice$Year <= best_brk_years[1]] <- "steelblue3"
hyst_phases_plaice[plaice$Year > best_brk_years[1] & plaice$Year <= best_brk_years[2]] <- "darkorange"
hyst_phases_plaice[plaice$Year > best_brk_years[2]] <- "purple"

plaice_breakpoint_hyst <- ggplot(data = plaice, aes(x = F_2_6, y = SSB_lag/1000))+
  geom_path(colour = "grey80")+
  geom_hline(yintercept = 564.599, linetype="dashed",color="gray30")+
  geom_vline(xintercept = 0.21, linetype="dashed",color="gray30")+
  geom_label(x = 0.21, y = 1150, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 0.55, y = 564.599, label = expression("MSY B"[trigger]), color="gray30", size = 3.5, fontface = "bold")+
  #geom_text_repel(aes(x = F_2_6, y = SSB/1000, label = Year), colour = hyst_phases_plaice)+
  geom_point(colour = hyst_phases_plaice)+
  geom_smooth(data = plaice[plaice$Year <= best_brk_years[1], ], aes(x = F_2_6, y = SSB_lag/1000),
              method = "lm", colour = "steelblue3")+
  geom_smooth(data = plaice[plaice$Year > best_brk_years[1] & plaice$Year <= best_brk_years[2], ],
              aes(x = F_2_6, y = SSB_lag/1000), method = "lm", colour = "darkorange")+
  geom_smooth(data = plaice[plaice$Year > best_brk_years[2], ], aes(x = F_2_6, y = SSB_lag/1000),
              method = "lm", colour = "purple")+
  labs(x = "", y = "")+
  geom_text_repel(data = plaice[4, ], aes(label = Year), #1960
                  point.padding = 0.2,nudge_y = 100, nudge_x = -0.05, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[37, ], aes(label = Year), #1991
                  point.padding = 0.2, nudge_y = 0, nudge_x = 0.2, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[53, ], aes(label = Year), #2007
                  point.padding = 0.2, nudge_y = 0,nudge_x= -0.2, size=3,col="gray30", segment.size =0.2 )+
  geom_text_repel(data = plaice[64, ], aes(label = Year), #2021
                  point.padding = 0.2, nudge_x = -0.1, size=3, col="gray30", segment.size =0.2 )+
  xlim(0, 0.8)+
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9),
        plot.background = element_blank(),
        axis.text = element_text(size = 12))
plaice_breakpoint_hyst



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


## plotting hyst breakpoints ----
hyst_phases_hake <- NULL
hyst_phases_hake[df_hake$Year <= 1993] <- "green4"
hyst_phases_hake[df_hake$Year > 1993 & df_hake$Year <= 2008] <- "steelblue3"
hyst_phases_hake[df_hake$Year > 2008 & df_hake$Year <= 2014] <- "darkorange"
hyst_phases_hake[df_hake$Year > 2014] <- "purple"


hake_breakpoint_hyst <- ggplot(data = df_hake, aes(x = Fishing, y = SSB))+
  geom_hline(yintercept = 56, linetype="dashed", color="gray30")+
  geom_vline(xintercept = 0.26, linetype="dashed", color="gray30")+
  geom_label(x = 0.26, y = 150, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 0.15, y = 56, label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path(colour = "grey80")+
  geom_point(colour = hyst_phases_hake)+
  geom_smooth(data = df_hake[df_hake$Year <= 1993, ], aes(x = F, y = SSB), method = "lm", colour = "green4")+
  geom_smooth(data = df_hake[df_hake$Year > 1993 & df_hake$Year <= 2008, ], aes(x = F, y = SSB), method = "lm", colour = "steelblue3")+
  geom_smooth(data = df_hake[df_hake$Year > 2008 & df_hake$Year <= 2014, ], aes(x = F, y = SSB), method = "lm", colour = "darkorange")+
  geom_smooth(data = df_hake[df_hake$Year > 2014, ], aes(x = F, y = SSB), method = "lm", colour = "purple")+
  labs(x = "", y = "")+
  geom_text_repel(data = df_hake[1, ], aes(label = Year), #1978
                  point.padding = 0.2,nudge_y = -40, nudge_x = 0,size=3,col="gray30",segment.size =0.2 ) +
  geom_text_repel(data = df_hake[17, ], aes(label = Year), #1994
                  point.padding = 0.2,nudge_y = 45, nudge_x = 0,size=3,col="gray30",segment.size =0.2 ) +
  geom_text_repel(data = df_hake[32, ], aes(label = Year), #2009
                  point.padding = 0.2, nudge_y = 35,nudge_x= -0.15,size=3,col="gray30",segment.size =0.2 ) +
  geom_text_repel(data = df_hake[43, ], aes(label = Year), #2020
                  point.padding = 0.2, nudge_y = 0,nudge_x= -0.2,size=3,col="gray30",segment.size = 0.2 ) +
  xlim(0, 1.3)+ #run with 0, 1.3 when all should have the same x range
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
hake_breakpoint_hyst

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

## plotting hyst breakpoints ----
hyst_phases_herring <- NULL
hyst_phases_herring[herring$Year <= best_brk_years[1]] <- "gold"
hyst_phases_herring[herring$Year > best_brk_years[1] & herring$Year <= best_brk_years[2]] <- "green4"
hyst_phases_herring[herring$Year > best_brk_years[2] & herring$Year <= best_brk_years[3]] <- "steelblue3"
hyst_phases_herring[herring$Year > best_brk_years[3] & herring$Year <= best_brk_years[4]] <- "darkorange"
hyst_phases_herring[herring$Year > best_brk_years[4]] <- "purple"

herring_breakpoint_hyst <- ggplot(data = herring, aes(x = F_2_6, y = SSB/1000))+
  geom_hline(yintercept = 1232.828,linetype = "dashed", color = "gray30")+
  geom_vline(xintercept = 0.26,linetype = "dashed", color = "gray30")+
  geom_label(x = 0.26, y = 4600, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 1.3, y = 1232.828, label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path(colour = "grey80")+
  geom_point(colour = hyst_phases_herring)+
  geom_smooth(data = herring[herring$Year <= best_brk_years[1], ], aes(x = F_2_6, y = SSB/1000), method = "lm", colour = "gold")+
  geom_smooth(data = herring[herring$Year > best_brk_years[1] & herring$Year <= best_brk_years[2], ], aes(x = F_2_6, y = SSB/1000), method = "lm", colour = "green4")+
  geom_smooth(data = herring[herring$Year > best_brk_years[2] & herring$Year <= best_brk_years[3], ], aes(x = F_2_6, y = SSB/1000), method = "lm", colour = "steelblue3")+
  geom_smooth(data = herring[herring$Year > best_brk_years[3] & herring$Year <= best_brk_years[4], ], aes(x = F_2_6, y = SSB/1000), method = "lm", colour = "darkorange")+
  geom_smooth(data = herring[herring$Year > best_brk_years[4], ], aes(x = F_2_6, y = SSB/1000), method = "lm", colour = "purple")+
  labs(x = "", y = " ") +
  geom_text_repel(data = herring[1, ], aes(label = Year), #1947
                  point.padding = 0.2, nudge_x = 0.3,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[12, ], aes(label = Year), #1958
                  point.padding = 0.2, nudge_x = 0.4,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[23, ], aes(label = Year), #1969
                  point.padding = 0.2, nudge_x = -0.2,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[38, ], aes(label = Year), #1984
                  point.padding = 0.2, nudge_x = 0.2,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[55, ], aes(label = Year), #2001
                  point.padding = 0.2, nudge_x = -0.1, nudge_y = 500, col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = herring[74, ], aes(label = Year), #2020
                  point.padding = 0.2, nudge_x = -0.084, nudge_y = -1000,col="gray30",size=3,segment.size = 0.2)+
  #xlim(0,1.3)+
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
herring_breakpoint_hyst


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

## plotting hyst breakpoints ----
hyst_phases_haddock <- NULL
hyst_phases_haddock[haddock$Year <= best_brk_years[1]] <- "steelblue3"
hyst_phases_haddock[haddock$Year > best_brk_years[1] & haddock$Year <= best_brk_years[2]] <- "darkorange"
hyst_phases_haddock[haddock$Year > best_brk_years[2] ] <- "purple"

haddock_breakpoint_hyst <- ggplot(data = haddock, aes(x = F_2_4, y = SSB/1000))+
  geom_hline(yintercept=132,linetype="dashed",color="gray30")+
  geom_vline(xintercept=0.19,linetype="dashed",color="gray30")+
  geom_label(x = 0.2, y = 0, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 0.1, y = 132, label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path(colour = "grey80")+
  geom_point(colour = hyst_phases_haddock)+
  geom_smooth(data = haddock[haddock$Year <= best_brk_years[1], ], aes(x = F_2_4, y = SSB/1000), method = "lm", colour = "steelblue3")+
  geom_smooth(data = haddock[haddock$Year > best_brk_years[1] & haddock$Year <= best_brk_years[2], ], aes(x = F_2_4, y = SSB/1000), method = "lm", colour = "darkorange")+
  geom_smooth(data = haddock[haddock$Year > best_brk_years[2], ], aes(x = F_2_4, y = SSB/1000), method = "lm", colour = "purple")+
  labs(x = "", y = "") +
  geom_text_repel(data = haddock[1, ], aes(label = Year), #1972
                  point.padding = 0, nudge_y =150, nudge_x=0,col="gray30",size=3,segment.size = 0.2) +
  geom_text_repel(data = haddock[19, ], aes(label = Year), #1990
                  point.padding = 0, nudge_x =0.12, nudge_y = -100, col="gray30",size=3,segment.size = 0.2) +
  geom_text_repel(data = haddock[33, ], aes(label = Year), #2004
                  point.padding = 0, nudge_x =0.12,col="gray30",size=3,segment.size = 0.2) +
  geom_text_repel(data = haddock[49, ], aes(label = Year), #2020
                  point.padding = 0.2,nudge_y = 0, nudge_x = -0.1,col="gray30",size=3,segment.size = 0.2)+
  xlim(0, 1)+
  theme_test()+
  theme(plot.tag.position = c(0.85, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))

haddock_breakpoint_hyst

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

## plotting hyst breakpoints ----

hyst_phases_saithe <- NULL
hyst_phases_saithe[df_saithe$Year <= best_brk_years[1]] <- "green4"
hyst_phases_saithe[df_saithe$Year > best_brk_years[1] & df_saithe$Year <= best_brk_years[2]] <- "steelblue3"
hyst_phases_saithe[df_saithe$Year > best_brk_years[2] & df_saithe$Year <= best_brk_years[3]] <- "darkorange"
hyst_phases_saithe[df_saithe$Year > best_brk_years[3]] <- "purple"

saithe_breakpoint_hyst <- ggplot(data = df_saithe, aes(x = F_4_7, y = SSB_lag))+
  geom_hline(yintercept = 149.098, linetype = "dashed", color = "gray30")+
  geom_vline(xintercept = 0.363, linetype = "dashed", color = "gray30")+
  geom_label(x = 0.363, y = 550, label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(x = 0.1, y = 149.098,label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path(colour = "grey80")+
  geom_point(colour = hyst_phases_saithe)+
  geom_smooth(data = df_saithe[df_saithe$Year <= best_brk_years[1], ], aes(x = F_4_7, y = SSB_lag),
              method = "lm", colour = "green4")+
  geom_smooth(data = df_saithe[df_saithe$Year > best_brk_years[1] & df_saithe$Year <= best_brk_years[2], ],
              aes(x = F_4_7, y = SSB_lag), method = "lm", colour = "steelblue3")+
  geom_smooth(data = df_saithe[df_saithe$Year > best_brk_years[2] & df_saithe$Year <= best_brk_years[3], ],
              aes(x = F_4_7, y = SSB_lag), method = "lm", colour = "darkorange")+
  geom_smooth(data = df_saithe[df_saithe$Year > best_brk_years[3], ], aes(x = F_4_7, y = SSB_lag),
              method = "lm", colour = "purple")+
  labs(x = "", y = "") +
  geom_text_repel(data = df_saithe[5, ], aes(label = Year), #1967
                  point.padding = 0.2, nudge_y =50, nudge_x= -0.2, col="gray30", size=3, segment.size = 0.2)+
  geom_text_repel(data = df_saithe[8, ], aes(label = Year), #1974
                  point.padding = 0.2, nudge_y =0,nudge_x= 0.2,col="gray30", size=3, segment.size = 0.2)+
  geom_text_repel(data = df_saithe[18, ], aes(label = Year), #1984
                  point.padding = 0.2, nudge_y =0,nudge_x= 0.2,col="gray30", size=3, segment.size = 0.2)+
  geom_text_repel(data = df_saithe[31, ], aes(label = Year), #1997
                  point.padding = 0.2, nudge_y =-100,nudge_x= 0,col="gray30", size=3, segment.size = 0.2)+
  geom_text_repel(data = df_saithe[54, ], aes(label = Year), #2020
                  point.padding = 0.1, nudge_y = -150,  nudge_x = -0.15, col="gray30", size=3, segment.size = 0.2)+
  xlim(0, 0.8)+
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))

saithe_breakpoint_hyst

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

## plotting hyst breakpoints ----
hyst_phases_cod <- NULL
hyst_phases_cod[cod$Year <= best_brk_years[1]] <- "gold"
hyst_phases_cod[cod$Year > best_brk_years[1] & cod$Year <= best_brk_years[2]] <- "green4"
hyst_phases_cod[cod$Year > best_brk_years[2] & cod$Year <= best_brk_years[3]] <- "steelblue3"
hyst_phases_cod[cod$Year > best_brk_years[3] & cod$Year <= best_brk_years[4]] <- "darkorange"
hyst_phases_cod[cod$Year > best_brk_years[4]] <- "purple"

cod_breakpoint_hyst <- ggplot(data = cod, aes(x = F_2_6, y = SSB_lag/1000))+
  geom_hline(yintercept=97777/1000,linetype="dashed",color="gray30")+
  geom_vline(xintercept=0.28,linetype="dashed",color="gray30")+
  geom_label(aes(x = 0.28, y = 230), label = expression("F"[MSY]), color="gray30", size = 3.5)+
  geom_label(aes(x = 0.15, y = 97777/1000), label = expression("MSY B"[trigger]), color="gray30", size = 3.5)+
  geom_path(colour = "grey80")+
  geom_point(colour = hyst_phases_cod)+
  geom_smooth(data = cod[cod$Year <= best_brk_years[1], ], aes(x = F_2_6, y = SSB_lag/1000),
              method = "lm", colour = "gold")+
  geom_smooth(data = cod[cod$Year > best_brk_years[1] & cod$Year <= best_brk_years[2], ],
              aes(x = F_2_6, y = SSB_lag/1000), method = "lm", colour = "green4")+
  geom_smooth(data = cod[cod$Year > best_brk_years[2] & cod$Year <= best_brk_years[3], ],
              aes(x = F_2_6, y = SSB_lag/1000), method = "lm", colour = "steelblue3")+
  geom_smooth(data = cod[cod$Year > best_brk_years[3] & cod$Year <= best_brk_years[4], ],
              aes(x = F_2_6, y = SSB_lag/1000), method = "lm", colour = "darkorange")+
  geom_smooth(data = cod[cod$Year > best_brk_years[4], ], aes(x = F_2_6, y = SSB_lag/1000),
              method = "lm", colour = "purple")+
  labs(x = "", y = "") +
  geom_text_repel(data = cod[6, ], aes(label = Year), #1963
                  point.padding = 0.2, nudge_x = -0, nudge_y = -20,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = cod[11, ], aes(label = Year), #1973
                  point.padding = 0.2, nudge_x = 0.1, nudge_y = 50, col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = cod[22, ], aes(label = Year), #1984
                  point.padding = 0., nudge_y = -10, nudge_x = -0.2,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = cod[38, ], aes(label = Year), #2000
                  point.padding = 0., nudge_y = 30, nudge_x = 0.05,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = cod[46, ], aes(label = Year), #2008
                  point.padding = 0., nudge_y = -5, nudge_x = 0.1,col="gray30",size=3,segment.size = 0.2)+
  geom_text_repel(data = cod[58, ], aes(label = Year), #2020
                  point.padding = 0.05,nudge_y = 20,  nudge_x = 0,col="gray30",size=3,segment.size = 0.2)+
  xlim(0, 1.2)+
  theme_test()+
  theme(plot.tag.position = c(0.9, 0.9), plot.background = element_blank(),
        axis.text = element_text(size = 12))
cod_breakpoint_hyst


# 7.Final plots #####
library(patchwork)

plaice_breakpoint_hyst + hake_breakpoint_hyst +
  herring_breakpoint_hyst + haddock_breakpoint_hyst +
  saithe_breakpoint_hyst + cod_breakpoint_hyst +
  plot_layout(ncol = 2)

