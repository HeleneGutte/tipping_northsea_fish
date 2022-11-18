#tGAMS

library(mgcv)#gam package
library(INDperform)#tGAM package
library(readxl)
library(readr)
library(tidyverse)
library(patchwork)


#get temperature 
#setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data/")
load("temp_ns_v5.RData")
#max =  11.576256
#min =   8.705205

temp_ns <- tot_sst $nsTmean
Year    <- c(1940:2019)

temp_ns <- as.data.frame(cbind(Year,temp_ns))

#setwd
#setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data/Stock_assessment_data_2021")


#Saithe ---- 
saithe <- read.csv("SA_saithe_2021.csv", sep = ",")

#add temp to data
saithe <- merge(saithe, temp_ns, by = "Year")

#lag SSB 3 times 
saithe$SSB_lag <- lag(saithe$SSB)
saithe$SSB_lag <- lag(saithe$SSB_lag)
saithe$SSB_lag <- lag(saithe$SSB_lag)

saithe <- saithe[-c(1:3),]

saithe<-arrange(saithe, SSB_lag)

#tGAM 
y <- saithe$R_3

x <- saithe$SSB_lag

x2 <- saithe$temp_ns

time <- saithe$Year

mod <- gam(y ~ s(x,k=3)) 

tmod_saithe <- thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2", 
                           k = 4, a = 0.2, b = 0.8)                             
summary(tmod_saithe)

tmod_saithe$mr # 10.17838

#test_interaction
loocv_thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                 k = 4, a = 0.2, b = 0.8,time =  time ) #[1] TRUE

#Diagnostic plots
tmod_saithe$train_na <- "NA"
tmod_saithe$train_na <- rep(FALSE, times = 56) 

plot_diagnostics(tmod_saithe)$all_plots

#add vector with predicted values to data set
saithe$tgam_pred <- predict(tmod_saithe)  

#divide vector into before and after threshold
tgam_ab <- NULL
tgam_ab[saithe$temp_ns <= tmod_saithe$mr] <- "low_temp"
tgam_ab[saithe$temp_ns > tmod_saithe$mr]  <- "high_temp"
saithe$tgam_ab <- tgam_ab


#plot
tGAM_saithe_p <- ggplot(saithe, aes(x = SSB_lag/1000, y = R_3/1000))+
  
  geom_point(data = saithe[saithe$tgam_ab == "low_temp",], col = "blue")+
  geom_point(data = saithe[saithe$tgam_ab == "high_temp",], col = "red")+
  
  geom_smooth(saithe,mapping = aes(SSB/1000,tgam_pred/1000, col = tgam_ab), method = "lm",shape= 5)+
  labs(x = "", y = "")+
  theme_test()
tGAM_saithe_p