#Cross validation SRR functions

# packages ----
library(tidyverse)
library(FSA)
library(segmented)

# functions ----
rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}

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

cross_valid <- function(dataset, runs) {
  rmse_values <- data.frame("run" = c(1:runs),
                            "linear model" = c(1:runs),
                            "beverton"= c(1:runs),
                            "ricker"= c(1:runs),
                            "segmented"= c(1:runs),
                            "segmented log"= c(1:runs),
                            "segmented best glm"= c(1:runs))

  rmse_values_means <- data.frame("linear model" = 0,
                                  "beverton"= 0,
                                  "ricker"= 0,
                                  "segmented"= 0,
                                  "segmented log"= 0,
                                  "segmented best glm"= 0)

  for(i in 1:runs){
    #get training and test data set for each run
    random_points <- sample(x = 1:length(dat$Year), size = 5)
    train_data <- dat[-random_points, ]
    test_data <- dat[random_points, ]

    #linear model
    m1 <- lm(R ~ SSB_lag, data = train_data)
    fitI <- predict(m1, newdata = test_data)
    rmse_values$linear.model[i] <- rmse(sim = fitI, obs = test_data$R)

    #Beverton-Holt
    svR <- srStarts(R ~ SSB_lag, data=train_data, type="BevertonHolt")
    bh <- srFuns("BevertonHolt")
    srR_beverton <- nls(log(R)~log(bh(SSB_lag,a,b)), data=train_data, start=svR)
    cbind(estimates=coef(srR_beverton), confint(srR_beverton))
    pR_beverton <- bh(test_data$SSB_lag, a=coef(srR_beverton))
    rmse_values$beverton[i] <- rmse(sim = pR_beverton, obs = test_data$R)

    #Ricker
    svR <- srStarts(R ~ SSB_lag, data=train_data, type="Ricker")
    ##fit the Ricker function to data
    rckr <- srFuns("Ricker")
    srR_ricker <- nls(log(R)~log(rckr(SSB_lag,a,b)), data=train_data, start=svR)
    #for a and b
    cbind(estimates=coef(srR_ricker),confint(srR_ricker))
    ###prediction
    pR_ricker <- rckr(test_data$SSB_lag, a=coef(srR_ricker))
    rmse_values$ricker[i] <- rmse(sim = pR_ricker, obs = test_data$R)

    #segmented
    seg <- segmented::segmented(lm(R ~ SSB_lag, data = train_data), seg.Z =  ~SSB_lag,
                                psi = mean(train_data$SSB_lag, na.rm = T))
    fit_segmented <- predict.segmented(seg, newdata = test_data)
    rmse_values$segmented[i] <- rmse(sim = fit_segmented, obs = test_data$R)

    #segmented log
    seg_log <- segmented::segmented(lm(R_log ~ SSB_lag_log, data = train_data), seg.Z =  ~ SSB_lag_log,
                                    psi = mean(train_data$SSB_lag_log, na.rm = T))
    fit_segmented_log <- predict.segmented(seg_log, newdata = test_data)
    rmse_values$segmented.log[i] <- rmse(sim = fit_segmented_log, obs = test_data$R) #oder muss hier mit R_log verglichen werden?

    #segmented best glm
    seg_negbi <- segmented::segmented(glm.nb(R ~SSB_lag, data = train_data), seg.Z =  ~SSB_lag,
                                      psi = mean(train_data$SSB_lag, na.rm = T))
    fit_segmented_negbi <- predict.segmented(seg_negbi, newdata = test_data)
    rmse_values$segmented.best.glm[i] <- rmse(sim = fit_segmented_negbi, obs = test_data$R)
  }

  for(i in 2:ncol(rmse_values)){
    rmse_values_means[i-1] <- mean(rmse_values[, i])
  }
  return(rmse_values_means)
}

# plaice ----

#prepare and load data
plaice <- read.csv("SA_plaice_2021.csv",
                   sep = ",")


plaice$SSB_lag <- lag(plaice$SSB)
plaice <- plaice[-1, ]
plaice <- arrange(plaice, SSB_lag)
plaice$r_log <- log(plaice$R_1)
plaice$SSB_lag_log <- log(plaice$SSB_lag)

# #set up rmse dataframe
# rmse_values_plaice <- data.frame("run" = c(1:10),
#                           "linear model" = c(1:10),
#                           "beverton"= c(1:10),
#                           "ricker"= c(1:10),
#                           "segmented"= c(1:10),
#                           "segmented log"= c(1:10),
#                           "segmented best glm"= c(1:10))
# #prepare data
dat <- plaice[ ,c(1, 2, 13, 14, 15)]

names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")
#
# for(i in 1:10){
#   #get training and test data set for each run
#   random_points <- sample(x = 1:length(dat$Year), size = 5)
#   train_data <- dat[-random_points, ]
#   test_data <- dat[random_points, ]
#
#   #linear model
#   m1 <- lm(R ~ SSB_lag, data = train_data)
#   fitI <- predict(m1, newdata = test_data)
#   rmse_values_plaice$linear.model[i] <- rmse(sim = fitI, obs = test_data$R)
#
#   #Beverton-Holt
#   svR <- srStarts(R ~ SSB_lag, data=train_data, type="BevertonHolt")
#   bh <- srFuns("BevertonHolt")
#   srR_beverton <- nls(log(R)~log(bh(SSB_lag,a,b)), data=train_data, start=svR)
#   cbind(estimates=coef(srR_beverton), confint(srR_beverton))
#   pR_beverton <- bh(test_data$SSB_lag, a=coef(srR_beverton))
#   rmse_values_plaice$beverton[i] <- rmse(sim = pR_beverton, obs = test_data$R)
#
#   #Ricker
#   svR <- srStarts(R ~ SSB_lag, data=train_data, type="Ricker")
#   ##fit the Ricker function to data
#   rckr <- srFuns("Ricker")
#   srR_ricker <- nls(log(R)~log(rckr(SSB_lag,a,b)), data=train_data, start=svR)
#   #for a and b
#   cbind(estimates=coef(srR_ricker),confint(srR_ricker))
#   ###prediction
#   pR_ricker <- rckr(test_data$SSB_lag, a=coef(srR_ricker))
#   rmse_values_plaice$ricker[i] <- rmse(sim = pR_ricker, obs = test_data$R)
#
#   #segmented
#   seg_plaice <- segmented::segmented(lm(R ~ SSB_lag, data = train_data), seg.Z =  ~SSB_lag,
#                                      psi = mean(train_data$SSB_lag, na.rm = T))
#   fit_segmented <- predict.segmented(seg_plaice, newdata = test_data)
#   rmse_values_plaice$segmented[i] <- rmse(sim = fit_segmented, obs = test_data$R)
#
#   #segmented log
#   seg_plaice_log <- segmented::segmented(lm(R_log ~ SSB_lag_log, data = train_data), seg.Z =  ~ SSB_lag_log,
#                                          psi = mean(train_data$SSB_lag_log, na.rm = T))
#   fit_segmented_log <- predict.segmented(seg_plaice_log, newdata = test_data)
#   rmse_values_plaice$segmented.log[i] <- rmse(sim = fit_segmented_log, obs = test_data$R) #oder muss hier mit R_log verglichen werden?
#
#   #segmented best glm
#   seg_plaice_negbi <- segmented::segmented(glm.nb(R ~SSB_lag, data = train_data), seg.Z =  ~SSB_lag,
#                                            psi = mean(train_data$SSB_lag, na.rm = T))
#   fit_segmented_negbi <- predict.segmented(seg_plaice_negbi, newdata = test_data)
#   rmse_values_plaice$segmented.best.glm[i] <- rmse(sim = fit_segmented_negbi, obs = test_data$R)
#
#
# }
# rmse_values_plaice
#
# rmse_values_plaice_means <- data.frame("linear model" = 0,
#                                        "beverton"= 0,
#                                        "ricker"= 0,
#                                        "segmented"= 0,
#                                        "segmented log"= 0,
#                                        "segmented best glm"= 0)
# for(i in 2:ncol(rmse_values_plaice)){
#   rmse_values_plaice_means[i-1] <- mean(rmse_values_plaice[, i])
# }
# rmse_values_plaice_means

#Use the self made function
#You have to prepare a dataset containg: the variables Year, R, SSB_lag, R_log, SSB_lag_log
#They MUST be named in the following way
#NOTE: It can happen that beverton holt and ricker crash the function because of invalid input (wrong starting values),
#       in that case please simply rerun the cross_valid function
#names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")


rmse_mean_plaice <- cross_valid(dataset = dat, runs = 10)
# linear.model  beverton  ricker  segmented     segmented.log     segmented.best.glm
#     697169.1 700953.6 705647  751021.3       1386693            1386693



# Cod ----
cod <- read.csv("SA_cod_2021.csv",
                sep = ",")
cod <- cod %>%
  mutate( SSB_lag = lag(SSB))
cod$r_log <- log(cod$R_1)
cod$ssb_log <- log(cod$SSB_lag)
cod <- arrange(cod, SSB_lag)


#prepare data
dat <- cod%>%
  dplyr::select(Year, R_1, SSB_lag, r_log, ssb_log)

names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")

rmse_mean_cod <- cross_valid(dataset = dat, runs = 10)
# linear.model  beverton   ricker     segmented     segmented.log   segmented.best.glm
#   326052.9    301658.1    302154.4  368146.7      659581.7           659581.6

# Haddock ----
haddock <- read.csv("SA_haddock_2021.csv",
                    sep = ",")
haddock$r_log <- log(haddock$R_0)
haddock$ssb_log <- log(haddock$SSB)
haddock <- arrange(haddock, SSB)
dat <- haddock%>%
  dplyr::select(Year, R_0, SSB, r_log, ssb_log)
names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")
rmse_mean_haddock <- cross_valid(dataset = dat, runs = 10)


# Herring ----
herring <- read.csv("SA_herring_2021.csv",
                    sep = ",")
herring <- arrange(herring, SSB)
herring$r_log <- log(herring$R_0)
herring$SSB_lag_log <- log(herring$SSB)

dat <- herring%>%
  dplyr::select(Year, R_0, SSB, r_log, SSB_lag_log)
names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")
rmse_mean_herring <- cross_valid(dataset = dat, runs = 10)
# linear.model beverton   ricker      segmented     segmented.log   segmented.best.glm
# 15220293      14001937    14551867  13080299      32896754           32896754

# Hake ----
hake <- read.csv("SA_hake_2021.csv", sep = ",")

hake <- arrange(hake, SSB)
hake$r_log <- log(hake$R_0)
hake$SSB_log <- log(hake$SSB)

dat <- hake%>%
  dplyr::select(Year, R_0, SSB, r_log, SSB_log)
names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")
rmse_mean_hake <- cross_valid(dataset = dat, runs = 10)

# Saithe ----
saithe<-read.csv("SA_saithe_2021.csv", sep = ",")
#lag SSB 3 times
saithe$SSB_lag <- lag(saithe$SSB, 3)
saithe$r_log <- log(saithe$R_3)
saithe$ssb_log <- log(saithe$SSB_lag)
saithe<-arrange(saithe, SSB_lag)

dat <- saithe%>%
  dplyr::select(Year, R_3, SSB_lag, r_log, ssb_log)
names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")
rmse_mean_saithe <- cross_valid(dataset = dat, runs = 10)

