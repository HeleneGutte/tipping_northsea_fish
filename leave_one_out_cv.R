library(caret)
library(tidyverse)
library(FSA)

# ctrl <- trainControl(method = "LOOCV")
#
#
# m1 <- train(R ~ SSB_lag, data = dat, method = "lm", trControl = ctrl)
# m1
#
# seg <- train(segmented::segmented(lm(R ~ SSB_lag, data = dat), seg.Z =  ~SSB_lag,
#                             psi = mean(dat$SSB_lag, na.rm = T)), trControl = ctrl)
#
# fitI <- predict(m1, newdata = test_data)
# rmse_values$linear.model[i] <- rmse(sim = fitI, obs = test_data$R)

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

rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}


cross_valid <- function(dataset) {
  rmse_values <- data.frame("run" = c(1:nrow(dataset)),
                            "linear model" = c(1:nrow(dataset)),
                            "beverton"= c(1:nrow(dataset)),
                            "ricker"= c(1:nrow(dataset)),
                            "segmented"= c(1:nrow(dataset)),
                            "segmented log"= c(1:nrow(dataset)),
                            "segmented best glm"= c(1:nrow(dataset)),
                            "strucchange" = c(1:nrow(dataset)))

  rmse_values_means <- data.frame("linear model" = 0,
                                  "beverton"= 0,
                                  "ricker"= 0,
                                  "segmented"= 0,
                                  "segmented log"= 0,
                                  "segmented best glm"= 0,
                                  "strucchange" = 0)


  rmse_values_train <- data.frame("run" = c(1:nrow(dataset)),
                            "linear model" = c(1:nrow(dataset)),
                            "beverton"= c(1:nrow(dataset)),
                            "ricker"= c(1:nrow(dataset)),
                            "segmented"= c(1:nrow(dataset)),
                            "segmented log"= c(1:nrow(dataset)),
                            "segmented best glm"= c(1:nrow(dataset)),
                            "strucchange" = c(1:nrow(dataset)))

  rmse_values_means_train <- data.frame("linear model" = 0,
                                  "beverton"= 0,
                                  "ricker"= 0,
                                  "segmented"= 0,
                                  "segmented log"= 0,
                                  "segmented best glm"= 0,
                                  "strucchange" = 0)

  for(i in 1:nrow(dataset)){
    #get training and test data set for each run
    train_data <- dat[-i, ]
    test_data <- dat[i, ]

    #linear model
    m1 <- lm(R ~ SSB_lag, data = train_data)
    fitI <- predict(m1, newdata = test_data)
    rmse_values$linear.model[i] <- rmse(sim = fitI, obs = test_data$R)
    fitI <- predict(m1, newdata = train_data)
    rmse_values_train$linear.model[i] <- rmse(sim = fitI, obs = train_data$R)

    #Beverton-Holt
    svR <- srStarts(R ~ SSB_lag, data=train_data, type="BevertonHolt")
    bh <- srFuns("BevertonHolt")
    srR_beverton <- nls(log(R)~log(bh(SSB_lag,a,b)), data=train_data, start=svR)
    cbind(estimates=coef(srR_beverton), confint(srR_beverton))
    pR_beverton <- bh(test_data$SSB_lag, a=coef(srR_beverton))
    rmse_values$beverton[i] <- rmse(sim = pR_beverton, obs = test_data$R)

    pR_beverton <- bh(train_data$SSB_lag, a=coef(srR_beverton))
    rmse_values_train$beverton[i] <- rmse(sim = pR_beverton, obs = train_data$R)

    #Ricker
    svR <- srStarts(R ~ SSB_lag, data=train_data, type="Ricker")
    ##fit the Ricker function to data
    rckr <- srFuns("Ricker")
    srR_ricker <- nls(log(R)~log(rckr(SSB_lag,a,b)), data=train_data, start=svR)
    #for a and b
    cbind(estimates=coef(srR_ricker), confint(srR_ricker))
    ###prediction
    pR_ricker <- rckr(test_data$SSB_lag, a=coef(srR_ricker))
    rmse_values$ricker[i] <- rmse(sim = pR_ricker, obs = test_data$R)

    pR_ricker <- rckr(train_data$SSB_lag, a=coef(srR_ricker))
    rmse_values_train$ricker[i] <- rmse(sim = pR_ricker, obs = train_data$R)

    #segmented
    seg <- segmented::segmented(lm(R ~ SSB_lag, data = train_data), seg.Z =  ~SSB_lag,
                                psi = mean(train_data$SSB_lag, na.rm = T))
    fit_segmented <- predict.segmented(seg, newdata = test_data)
    rmse_values$segmented[i] <- rmse(sim = fit_segmented, obs = test_data$R)

    fit_segmented <- predict.segmented(seg, newdata = train_data)
    rmse_values_train$segmented[i] <- rmse(sim = fit_segmented, obs = train_data$R)

    #segmented log
    seg_log <- segmented::segmented(lm(R_log ~ SSB_lag_log, data = train_data), seg.Z =  ~ SSB_lag_log,
                                    psi = mean(train_data$SSB_lag_log, na.rm = T))
    fit_segmented_log <- predict.segmented(seg_log, newdata = test_data)
    rmse_values$segmented.log[i] <- rmse(sim = fit_segmented_log, obs = test_data$R) #oder muss hier mit R_log verglichen werden?

    fit_segmented_log <- predict.segmented(seg_log, newdata = train_data)
    rmse_values_train$segmented.log[i] <- rmse(sim = fit_segmented_log, obs = train_data$R)

    #segmented best glm
    seg_negbi <- segmented::segmented(glm.nb(R ~SSB_lag, data = train_data), seg.Z =  ~SSB_lag,
                                      psi = mean(train_data$SSB_lag, na.rm = T))
    fit_segmented_negbi <- predict.segmented(seg_negbi, newdata = test_data)
    rmse_values$segmented.best.glm[i] <- rmse(sim = fit_segmented_negbi, obs = test_data$R)

    fit_segmented_negbi <- predict.segmented(seg_negbi, newdata = train_data)
    rmse_values_train$segmented.best.glm[i] <- rmse(sim = fit_segmented_negbi, obs = train_data$R)

    #strucchange
    bpts <- strucchange::breakpoints(R ~ SSB_lag, data = train_data)
    bpts_sum <- summary(bpts)
    opt_brks <- opt_bpts(bpts_sum$RSS["BIC",])
    if(is_empty(opt_brks)){
      next
    }
    if(opt_brks [[1]]== 1){
      bpts2 <- strucchange::breakpoints(bpts, breaks = opt_brks[[1]])
      best_brk <- train_data$SSB_lag[bpts2$breakpoints]

      strucc_model <- lm(R ~ SSB_lag*(SSB_lag <= best_brk[1]) +
                           SSB_lag*(SSB_lag >= best_brk[1]), data = train_data)
      fm1_coef <- coef(strucc_model)

      fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[5])*train_data$SSB_lag[train_data$SSB_lag <= best_brk[1]]
      fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2])*train_data$SSB_lag[train_data$SSB_lag > best_brk[1]]

      rmse_values_train$strucchange[i] <- rmse(sim = c(fit1, fit2), obs = train_data$R)

      if(test_data$SSB_lag <= best_brk[1]){
        fit_struc_test <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[5])*test_data$SSB_lag
      }else{
        fit_struc_test <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2])*test_data$SSB_lag
      }

      rmse_values$strucchange[i] <- rmse(sim = fit_struc_test, obs = test_data$R)

    }else if(opt_brks [[1]] == 2){
      bpts2 <- strucchange::breakpoints(bpts, breaks = opt_brks[[1]])
      best_brk <- train_data$SSB_lag[bpts2$breakpoints]

      strucc_model <- lm(R ~ SSB_lag*(SSB_lag <= best_brk[1]) +
                           SSB_lag*(SSB_lag >= best_brk[1] & SSB_lag <= best_brk[2]) +
                           SSB_lag*(SSB_lag >= best_brk[2]), data = train_data)
      fm1_coef <- coef(strucc_model)

      fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[6])*train_data$SSB_lag[train_data$SSB_lag <= best_brk[1]]
      fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2] + fm1_coef[7])*
        train_data$SSB_lag[train_data$SSB_lag > best_brk[1] & train_data$SSB_lag <= best_brk[2]]
      fit3 <- (fm1_coef[1] + fm1_coef[5]) + (fm1_coef[2] + fm1_coef[8])*train_data$SSB_lag[train_data$SSB_lag> best_brk[2]]

      rmse_values_train$strucchange[i] <- rmse(sim = c(fit1, fit2, fit3), obs = train_data$R)

      if(test_data$SSB_lag <= best_brk[1]){
        fit_struc_test <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[6])*test_data$SSB_lag
      }else if(test_data$SSB_lag > best_brk[2]){
        fit_struc_test <- (fm1_coef[1] + fm1_coef[5]) + (fm1_coef[2] + fm1_coef[8])*test_data$SSB_lag
      }else{
        fit_struc_test <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2] + fm1_coef[7])*test_data$SSB_lag
      }
      rmse_values$strucchange[i] <- rmse(sim = fit_struc_test, obs = test_data$R)

    }

  }#end for loop 1

  for(i in 2:ncol(rmse_values)){
    rmse_values_means[i-1] <- mean(rmse_values[, i])
    rmse_values_means_train[i-1] <- mean(rmse_values_train[, i])
  }#end for loop 2

  return(list(test = rmse_values_means, train = rmse_values_means_train))
}


#Plaice ----
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

rmse_mean_plaice <- cross_valid(dataset = dat)
rmse_mean_plaice
# linear.model beverton ricker segmented segmented.log segmented.best.glm
# 1     422888.8   409266   32.5  438236.3       1136697            1136697

# Cod ----
cod <- read.csv("SA_cod_2021.csv",
                sep = ",")
cod <- cod %>%
  mutate( SSB_lag = lag(SSB))
cod <- cod[-1, ]
cod$r_log <- log(cod$R_1)
cod$ssb_log <- log(cod$SSB_lag)
cod <- arrange(cod, SSB_lag)


#prepare data
dat <- cod%>%
  dplyr::select(Year, R_1, SSB_lag, r_log, ssb_log)

names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")

rmse_mean_cod <- cross_valid(dataset = dat)


# Haddock ----
haddock <- read.csv("SA_haddock_2021.csv",
                    sep = ",")
haddock$r_log <- log(haddock$R_0)
haddock$ssb_log <- log(haddock$SSB)
haddock <- arrange(haddock, SSB)
dat2 <- haddock%>%
  dplyr::select(Year, R_0, SSB, r_log, ssb_log)
names(dat2) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")

rmse_mean_haddock <- cross_valid(dataset = dat2)
# linear.model    beverton ricker     segmented     segmented.log       segmented.best.glm
# 1     353472.3   327969   328159.6  367786.2      635500.7           635500.5

# Saithe ----
saithe<-read.csv("SA_saithe_2021.csv", sep = ",")
#lag SSB 3 times
saithe$SSB_lag <- lag(saithe$SSB, 3)
saithe <- saithe[-c(1:3), ]
saithe$r_log <- log(saithe$R_3)
saithe$ssb_log <- log(saithe$SSB_lag)
saithe <- arrange(saithe, SSB_lag)

dat <- saithe%>%
  dplyr::select(Year, R_3, SSB_lag, r_log, ssb_log)
names(dat) <- c("Year", "R", "SSB_lag", "R_log", "SSB_lag_log")

cross_valid_saithe <- function(dataset) {
  rmse_values <- data.frame("run" = c(1:nrow(dataset)),
                            "linear model" = c(1:nrow(dataset)),
                            "beverton"= c(1:nrow(dataset)),
                            "ricker"= c(1:nrow(dataset)),
                            "segmented"= c(1:nrow(dataset)),
                            "segmented log"= c(1:nrow(dataset)),
                            "segmented best glm"= c(1:nrow(dataset)))

  rmse_values_means <- data.frame("linear model" = 0,
                                  "beverton"= 0,
                                  "ricker"= 0,
                                  "segmented"= 0,
                                  "segmented log"= 0,
                                  "segmented best glm"= 0)

  for(i in 1:nrow(dataset)){
    #get training and test data set for each run
    train_data <- dat[-i, ]
    test_data <- dat[i, ]

    #linear model
    m1 <- lm(R ~ SSB_lag, data = train_data)
    fitI <- predict(m1, newdata = test_data)
    rmse_values$linear.model[i] <- rmse(sim = fitI, obs = test_data$R)

    #Beverton-Holt
    #svR <- srStarts(R ~ SSB_lag, data=train_data, type="BevertonHolt")
    bh <- srFuns("BevertonHolt")
    srR_beverton <- nls(log(R)~log(bh(SSB_lag, a, b)), data=train_data, start=list(a = 76.972011, b = 0.005))
    cbind(estimates=coef(srR_beverton), confint(srR_beverton))
    pR_beverton <- bh(test_data$SSB_lag, a=coef(srR_beverton))
    rmse_values$beverton[i] <- rmse(sim = pR_beverton, obs = test_data$R)

    #Ricker
    svR <- srStarts(R ~ SSB_lag, data=train_data, type="Ricker")
    ##fit the Ricker function to data
    rckr <- srFuns("Ricker")
    srR_ricker <- nls(log(R)~log(rckr(SSB_lag,a,b)), data=train_data, start=svR)
    #for a and b
    cbind(estimates=coef(srR_ricker), confint(srR_ricker))
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

    #strucchange
    #rmse_values$segmented.strucchange[i] <- rmse(sim = c(fit1, fit2, fit3), obs = plaice$R_age1)
  }

  for(i in 2:ncol(rmse_values)){
    rmse_values_means[i-1] <- mean(rmse_values[, i])
  }
  return(rmse_values_means)
}



rmse_mean_saithe <- cross_valid_saithe(dataset = dat)
# linear.model beverton   ricker segmented segmented.log segmented.best.glm
# 1     61203.27     26.5 61337.45  64596.69      148081.2             148081

