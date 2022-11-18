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

#1.Plaice  ----
plaice <- read.csv("SA_plaice_2021.csv", 
                   sep = ",")

#add temp
plaice <- merge(plaice,temp_ns, by = "Year", na.rm = T)


#SSB_lag
plaice$SSB_lag <- lag(plaice$SSB)
plaice <- plaice[-1, ] 
plaice <- arrange(plaice, SSB_lag)

#tGAM
y <- plaice$R_1

x <- plaice$SSB_lag

x2 <- plaice$temp_ns

time <- plaice$Year

mod <- gam(y ~ s(x,k=3)) 

tmod_plaice <- thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                       k = 4, a = 0.2, b = 0.8)                             
#test_interaction
loocv_thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                 k = 4, a = 0.2, b = 0.8,time =  time ) #[1] FALSE

summary(tmod_plaice)

tmod_plaice$mr # 9.789377

tmod_plaice$train_na <- "NA"
tmod_plaice$train_na <- rep(FALSE, times = 56) 

plot_diagnostics(tmod_plaice)$all_plots

#add vector with predicted values to data set
tgam_pred <- predict(tmod_plaice) 



#2.Hake ----
hake <-read.csv("SA_hake_2021.csv", sep = ",")


#add temp to data
hake <- merge(hake, temp_ns, by = "Year")

hake <- arrange(hake, SSB)


#tGAM
y <- hake$R_0

x <- hake$SSB

x2 <- hake$temp_ns

time <- hake$Year

mod <- gam(y ~ s(x,k=3))

tmod_hake <- thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                          k = 4, a = 0.2, b = 0.8)                             


summary(tmod_hake)

tmod_hake$mr # 9.774661

#test_interaction
loocv_thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                 k = 4, a = 0.2, b = 0.8,time =  time ) #[1] FALSE


#Diagnostic plots
tmod_hake$train_na <- "NA"
tmod_hake$train_na <- rep(FALSE, times = 56) 

plot_diagnostics(tmod_hake)$all_plots

#add vector with predicted values to data set
hake$tgam_pred <- predict(tmod_hake)



#3.Herring ----
herring <-read.csv("SA_herring_2021.csv", sep = ",")

#add temp
herring <- merge(herring, temp_ns, by = "Year", na.rm = T)

#tGAM
y <- herring$R_0

x <- herring$SSB

x2 <- herring$temp_ns

mod <- gam(y ~ s(x,k=3)) 

time <- herring$Year

tmod_herring <- thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                        k = 4, a = 0.2, b = 0.8)                           

#test_interaction
loocv_thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                 k = 4, a = 0.2, b = 0.8,time =  time ) #[1] FALSE

summary(tmod_herring)

tmod_herring$mr # 10.68912

#Diagnostic plots
tmod_herring$train_na <- "NA"
tmod_herring$train_na <- rep(FALSE, times = 56) 
plot_diagnostics(tmod_herring)$all_plots

#add vector with predicted values to data set
herring$tgam_pred <- predict(tmod_herring)  



#4.Haddock ----
haddock <- read.csv("SA_haddock_2021.csv", sep = ",")

haddock <- merge ( haddock, temp_ns, by = "Year", na.rm = TRUE)

#tGAM 
y <- haddock$R_0

x <- haddock$SSB

x2 <- haddock$temp_ns

time <- haddock$Year
mod <- gam(y ~ s(x,k=3)) 

tmod_haddock <- thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                           k = 4, a = 0.2, b = 0.8)                            

summary(tmod_haddock)

tmod_haddock$mr # 10.36664

#test_interaction
loocv_thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                 k = 4, a = 0.2, b = 0.8,time =  time ) #[1] FALSE

#Diagnostic plots
tmod_haddock$train_na <- "NA"
tmod_haddock$train_na <- rep(FALSE, times = 56)

plot_diagnostics(tmod_haddock)$all_plots



#add vector with predicted values to data set
haddock$tgam_pred <- predict(tmod_haddock) 



#5.Saithe ---- 
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



#6.Cod ----
cod <- read.csv("SA_cod_2021.csv", sep = ",")

#add ErSSTv5 to data

cod <- merge (cod, temp_ns , by = "Year")


cod <- cod %>% 
  mutate( SSB_lag = lag(SSB))

cod <- cod [-1, ] 

y <- cod$R_1

x <- cod$SSB_lag

x2 <- cod$temp_ns

time <- cod$Year

mod <- gam(y ~ s(x,k=3)) 

tmod_cod <- thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                  k = 4, a = 0.2, b = 0.8)                            

summary(tmod_cod)

tmod_cod$mr #10.11786

#test_interaction
loocv_thresh_gam(model = mod, ind_vec = y, press_vec = x, t_var = x2, name_t_var = "x2",
                 k = 4, a = 0.2, b = 0.8,time =  time ) #[1] FALSE


#Diagnostic plots
tmod_cod$train_na <- "NA"
tmod_cod$train_na <- rep(FALSE, times = 56) 

plot_diagnostics(tmod_cod)$all_plots


#add vector with predicted values to data set
cod$tgam_pred <- predict(tmod_cod)
