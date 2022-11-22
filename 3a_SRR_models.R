setwd("~/Desktop/Courses/1_Advanced fisheries science/Paper/All_data")

#Stock-recruitment relationships for all eight species

#load packages ----
library(tidyverse)
library(bcp)
library(changepoint)
library(FSA)
library(MASS)
library(readxl)

#colors
# "green4"
# "steelblue3"
# "darkorange"
# "purple"

#1. Plaice ----
plaice <- read.csv("SA_plaice_2021.csv",
                            sep = ",")
View(plaice)

plaice$SSB_lag <- lag(plaice$SSB)
plaice <- plaice[-1, ]
plaice <- arrange(plaice, SSB_lag)

color_regimes_plaice <- NULL
color_regimes_plaice[plaice$Year %in% c(1957:1969)] <- "green4"
color_regimes_plaice[plaice$Year %in% c(1969:1991)] <- "steelblue3"
color_regimes_plaice[plaice$Year %in% c(1991:2007)] <- "darkorange"
color_regimes_plaice[plaice$Year %in% c(2007:2021)] <- "purple"
plaice$color_regimes_plaice <- color_regimes_plaice

SRR_plaice <- ggplot(data = plaice, aes(x = SSB_lag/100, y = R_1/1000))+
  geom_point(col = color_regimes_plaice)+
  labs(x = "SSB in thousands t", y = "R in billions", title = "Plaice")+
  theme_test()
SRR_plaice

ns_data <- plaice
ns_data$R <- ns_data$R_1
ns_data$ssb <- ns_data$SSB_lag

#1.1. Indipendence Model ----
m1 <- lm(ns_data$R~ns_data$ssb)
summary(m1)
fitI <- fitted(m1)
#high overdispersion

#1.2. Beverton Holt ----
svR <- srStarts(R ~ ssb, data=ns_data, type="BevertonHolt")
svR
###stock-recruitement function (NB, the function is log based!)
bh <- srFuns("BevertonHolt")
srR_beverton <- nls(log(R)~log(bh(ssb,a,b)), data=ns_data, start=svR)


##results tell you significance of parameters and residual standard error
#resisual standard error is the square root of the residual sum of squares
#divided by degrees of freedom
summary(srR_beverton)
cbind(estimates=coef(srR_beverton), confint(srR_beverton))
### make predictions to then plot!
pR_beverton <- bh(ns_data$ssb, a=coef(srR_beverton))
ns_data$pR_beverton <- pR_beverton
###quasi-r2 value, if low model does not fit well!
cor(bh(ns_data$ssb, a=coef(srR_beverton)), ns_data$R)^2

#1.3. Ricker ----
svR <- srStarts(R ~ ssb, data=ns_data, type="Ricker")
svR
##fit the Ricker function to data
rckr <- srFuns("Ricker")
srR_ricker <- nls(log(R)~log(rckr(ssb,a,b)), data=ns_data, start=svR)
#for a and b
cbind(estimates=coef(srR_ricker),confint(srR_ricker))
###prediction
#plot
pR_ricker <- rckr(ns_data$ssb, a=coef(srR_ricker))
ns_data$pR_ricker <- pR_ricker

#1.4. Segmented ----
mean(plaice$SSB_lag, na.rm = T) #1930650
seg_plaice <- segmented::segmented(lm(R_1 ~SSB_lag, data = plaice), seg.Z =  ~SSB_lag, psi = mean(plaice$SSB_lag, na.rm = T))
summary(seg_plaice)
summary(seg_plaice)$psi
coef_plaice <-c(seg_plaice$fitted.values)
brpt_plaice <- seg_plaice$psi[2]
brpt_ste_plaice <- seg_plaice$psi[3]

#1.5. Segmented log ----
plaice$r_log <- log(plaice$R_1)
plaice$SSB_lag_log <- log(plaice$SSB_lag)
#View(plaice)

mean(plaice$SSB_lag_log, na.rm = T) #12.89022
seg_plaice_log <- segmented::segmented(lm(r_log ~ SSB_lag_log, data = plaice), seg.Z =  ~ SSB_lag_log, psi = mean(plaice$SSB_lag_log, na.rm = T))
summary(seg_plaice_log)
summary(seg_plaice_log)$psi

coef_plaice_log <-c(seg_plaice_log$fitted.values)
brpt_plaice_log <- seg_plaice_log$psi[2]
brpt_ste_plaice_log <- seg_plaice_log$psi[3]

#1.6. Segmented with best performing GLM ----
#1.6.1. glm with gaussian
plaice_gaus <- glm(R_1 ~ SSB_lag, data = plaice, family = gaussian)
summary(plaice_gaus)
2.4868e+13/62 #401096774194, clear overdispersion
par(mfrow = c(2,2))
plot(plaice_gaus)
# bad model fit, dont necessary to visualize

#1.6.2. glm with poisson
plaice_pois <- glm(R_1 ~ SSB_lag, data = plaice, family = poisson)
summary(plaice_pois)
17744220/62 #17744220, clear overdispersion

plot(plaice_pois)
#better, but overdispersed

#1.6.3. glm with quasipoisson
plaice_qpois <- glm(R_1 ~ SSB_lag, data = plaice, family = quasipoisson)
summary(plaice_qpois)

plot(plaice_qpois)
17744220/62#286197.1
#not better modelfit, but no overdispersion

#1.6.4. glm with negative binomial

plaice_negbi <- glm.nb(R_1 ~ SSB_lag, data = plaice)
summary(plaice_negbi)
66.352/62 #1.070194 -> no over or underdispersion

plot(plaice_negbi)
#slightly better than the quasipoisson model -> use negative binomial for the segmented analysis

mean(plaice$SSB_lag, na.rm = T)
seg_plaice_negbi <- segmented::segmented(glm.nb(R_1 ~SSB_lag, data = plaice), seg.Z =  ~SSB_lag, psi = mean(plaice$SSB_lag, na.rm = T))
summary(seg_plaice_negbi)
summary(seg_plaice_negbi)$psi

coef_plaice_negbi <-c(seg_plaice_negbi$fitted.values)
brpt_plaice_negbi <- seg_plaice_negbi$psi[2]
brpt_ste_plaice_negbi <- seg_plaice_negbi$psi[3]


#1.7. Strucchange ----
bpts <- strucchange::breakpoints(R_1 ~ SSB_lag, data = plaice)
#bpts <- breakpoints(Year ~ Year, data = data_SA)


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
opt_brks #0 or 2, if 0 following part of this method does (obviously) not work
bpts2 <- strucchange::breakpoints(bpts, breaks = opt_brks)
best_brk <- plaice$SSB_lag[bpts2$breakpoints]

best_brk #277411

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(R_age1 ~ SSB_lag, data = plaice, type = "p")
for (i in 1: opt_brks) {
  abline(v = plaice$SSB_lag[ci_mod$confint[i,2]], col = "blue")
  abline(v = plaice$SSB_lag[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = plaice$SSB_lag[ci_mod$confint[i,3]], col = "red", lty = 3)
}


## fit null hypothesis model
fm0 <- lm(R_1 ~ SSB_lag, data = plaice)
# fit model with 2 breakpoint but formula different than in previous time series:

######cannot find "best_brk"
strucc_plaice <- lm(R_1 ~ SSB_lag*(SSB_lag <= best_brk[1]) +
                      SSB_lag*(SSB_lag >= best_brk[1] & SSB_lag <= best_brk[2]) +
                      SSB_lag*(SSB_lag >= best_brk[2]), data = plaice)
fm1_coef <- coef(strucc_plaice)


fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[6])*plaice$SSB_lag[plaice$SSB_lag <= best_brk[1]]
fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2] + fm1_coef[7])*
  plaice$SSB_lag[plaice$SSB_lag > best_brk[1] & plaice$SSB_lag <= best_brk[2]]
fit3 <- (fm1_coef[1] + fm1_coef[5]) + (fm1_coef[2] + fm1_coef[8])*plaice$SSB_lag[plaice$SSB_lag> best_brk[2]]
# add to previous plot
lines(plaice$SSB_lag, fitted(fm0), col = 3)
lines(plaice$SSB_lag[plaice$SSB_lag <= best_brk[1]], fit1, col = "orange")
lines(plaice$SSB_lag[plaice$SSB_lag > best_brk[1] & plaice$SSB_lag<= best_brk[2]], fit2, col = "orange")
lines(plaice$SSB_lag[plaice$SSB_lag > best_brk[2]], fit3, col = "orange")

#1.8. Visualize and compare with RMSE ----
SRR_plaice_models <- SRR_plaice +
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_plaice_negbi/1000), linetype = 2)+
  #geom_segment(y = 800, yend = 800, x = brpt_plaice_negbi/1000000-brpt_ste_plaice_negbi/1000000,
  #            xend = brpt_plaice_negbi/1000000+brpt_ste_plaice_negbi/1000000, col = "red")+
  geom_line(aes(y = coef_plaice_negbi/1000, col = "seg. neg bi"))+
  geom_vline(aes(col = "segmented", xintercept = brpt_plaice/1000), linetype = 2)+
  #geom_segment(y = 20, yend = 20, x = brpt_plaice/1000000-brpt_ste_plaice/1000000,
  #            xend = brpt_plaice/1000000+brpt_ste_plaice/1000000, col = "red")+
  geom_line(aes(y = coef_plaice/1000, col = "segmented"))+
  geom_vline(aes(col = "segmented log", xintercept = exp(brpt_plaice_log)/1000), linetype = 2)+
  #geom_segment(y = 0.5, yend = 0.5, x = exp(brpt_plaice_log)/1000000-exp(brpt_ste_plaice_log)/1000000,
  #            xend = exp(brpt_plaice_log)/1000000+exp(brpt_ste_plaice_log)/1000000, col = "red")+
  geom_line(aes(y = exp(coef_plaice_log)/1000, col = "segmented log"))+
  geom_line(data = ns_data, aes(ssb/100,pR_ricker/1000, col = "Ricker"), show.legend = TRUE)+ #Ricker
  geom_line(data=ns_data, aes(ssb/100,pR_beverton/1000, col = "Beverton-Holt"), show.legend = TRUE)+ #BH
  geom_line(data = ns_data, aes(ssb/100, fitI/1000, col = "Indipendence"), show.legend = T)+
  #geom_line(data = plaice[plaice$SSB_lag <= best_brk[1], ], aes(x = SSB_lag/1000, y = fit1/1000, col = "strucchange"), show.legend = TRUE)+
  #geom_line(data = plaice[plaice$SSB_lag > best_brk[1] & plaice$SSB_lag<= best_brk[2], ], aes(x = SSB_lag/1000, y = fit2/100000, col = "strucchange"), show.legend = TRUE)+
  #geom_line(data = plaice[plaice$SSB_lag > best_brk[2], ], aes(SSB_lag/1000, fit3/1000, col = "strucchange"), show.legend = TRUE)+
  #geom_vline(aes(xintercept = best_brk[1]/1000, col = "strucchange"), linetype = 2)+
  #geom_vline(aes(xintercept = best_brk[2]/1000, col = "strucchange"), linetype = 2)+
  labs(col = "Model")

SRR_plaice_models
#Note: must abline strucchange in different way (if it works)

rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}

comparison <- NULL
comparison <- AIC(m1, srR_beverton, srR_ricker, seg_plaice, seg_plaice_log, seg_plaice_negbi
                  #, strucc_plaice
                  )
comparison[1, 3] <- rmse(sim = fitI, obs = plaice$R_1)
comparison[2, 3] <- rmse(sim = ns_data$pR_beverton, obs = plaice$R_1)
comparison[3, 3] <- rmse(sim = ns_data$pR_ricker, obs = plaice$R_1)
comparison[4, 3] <- rmse(sim = seg_plaice$fitted.values, obs = plaice$R_1)
comparison[5, 3] <- rmse(sim = seg_plaice_log$fitted.values, obs = plaice$R_1)
comparison[6, 3] <- rmse(sim = seg_plaice_negbi$fitted.values, obs = plaice$R_1)
comparison[7, 3] <- rmse(sim = c(fit1, fit2, fit3), obs = plaice$R_1)
comparison

#                  df        AIC      RMSE
# df        AIC        V3
# m1                3 1895.51026  623348.1
# srR_beverton      3   89.85131  635361.0
# srR_ricker        3   92.07941  636346.8
# seg_plaice        5 1897.98251  615952.3
# seg_plaice_log    5   91.37013 1299309.2
# seg_plaice_negbi  5 1864.84281  616685.6
# strcucchange                    607903.3
#lowest RMSE has the simple segmented analysis

#2.Cod ----
cod <- read.csv("SA_cod_2021.csv",
                   sep = ",")
cod <- cod %>%
  mutate( SSB_lag = lag(SSB))
cod <- cod[-1, ]
cod <- arrange(cod, SSB_lag)

color_regimes_cod <- NULL
color_regimes_cod[cod$Year %in% c(1963:1972)] <- "darkorange"
color_regimes_cod[cod$Year %in% c(1972:1999)] <- "darkorange"
color_regimes_cod[cod$Year %in% c(1999:2021)] <- "purple"

ns_data <- cod
ns_data$R <- ns_data$R_1
ns_data$ssb <- ns_data$SSB_lag # add SSB_lag as ssb to keep code unchanged

#3.1. Indipendence Model ----
m1 <- lm(ns_data$R~ns_data$ssb)
summary(m1)
fitI <- fitted(m1)


#3.2. Beverton Holt ----
svR <- srStarts(R ~ ssb, data=ns_data, type="BevertonHolt")
svR
###stock-recruitement function (NB, the function is log based!)
bh <- srFuns("BevertonHolt")
srR_beverton <- nls(log(R)~log(bh(ssb,a,b)), data=ns_data, start=svR)


##results tell you significance of parameters and residual standard error
#resisual standard error is the square root of the residual sum of squares
#divided by degrees of freedom
summary(srR_beverton)
cbind(estimates=coef(srR_beverton), confint(srR_beverton))
### make predictions to then plot!
pR_beverton <- bh(ns_data$ssb, a=coef(srR_beverton))
ns_data$pR_beverton <- pR_beverton
###quasi-r2 value, if low model does not fit well!
cor(bh(ns_data$ssb, a=coef(srR_beverton)), ns_data$R)^2

#3.3. Ricker ----
svR <- srStarts(R ~ ssb, data=ns_data, type="Ricker")
svR
##fit the Ricker function to data
rckr <- srFuns("Ricker")
srR_ricker <- nls(log(R)~log(rckr(ssb,a,b)), data=ns_data, start=svR)
#for a and b
cbind(estimates=coef(srR_ricker),confint(srR_ricker))
###prediction
#plot
pR_ricker <- rckr(ns_data$ssb, a=coef(srR_ricker))
ns_data$pR_ricker <- pR_ricker

#3.4. Segmented ----
mean(cod$SSB_lag, na.rm = T) #102314.2
seg_cod <- segmented::segmented(lm(R_1 ~SSB_lag, data = cod), seg.Z =  ~SSB_lag, psi = mean(cod$SSB_lag, na.rm = T))
summary(seg_cod)
summary(seg_cod)$psi
coef_cod <-c(seg_cod$fitted.values)
brpt_cod <- seg_cod$psi[2]
brpt_ste_cod <- seg_cod$psi[3]

#3.5. Segmented log ----
cod$r_log <- log(cod$R_1)
cod$ssb_log <- log(cod$SSB_lag)

mean(cod$ssb_log, na.rm = T) #11.41022
seg_cod_log <- segmented::segmented(lm(r_log ~ ssb_log, data = cod), seg.Z =  ~ ssb_log, psi = mean(cod$ssb_log, na.rm = T))
summary(seg_cod_log)
summary(seg_cod_log)$psi

coef_cod_log <-c(seg_cod_log$fitted.values)
brpt_cod_log <- seg_cod_log$psi[2]
brpt_ste_cod_log <- seg_cod_log$psi[3]

#3.6. Segmented with best performing GLM ----
#3.6.1. glm with gaussian ----
cod_gaus <- glm(R_1 ~ SSB_lag, data = cod, family = gaussian)
summary(cod_gaus)
1.3619e+13  /56#243196428571, clear overdispersion
par(mfrow = c(2,2))
plot(cod_gaus)
# semigood model fit, dont necessary to visualize

#3.6.2. glm with poisson ----
cod_pois <- glm(R_1 ~ SSB_lag, data = cod, family = poisson)
summary(cod_pois)
18305604/56 #326885.8, clear overdispersion

plot(cod_pois)
#better, but overdispersed

#3.6.3. glm with quasipoisson ----
cod_qpois <- glm(R_1 ~ SSB_lag, data = cod, family = quasipoisson)
summary(cod_qpois)
18305604/56#326885.8
plot(cod_qpois)
#ok modelfit

#3.6.4. glm with negative binomial ----
library(MASS)

cod_negbi <- glm.nb(R_1 ~ SSB_lag, data = cod)
summary(cod_negbi)
62.45/56 #1.115179 -> no over or underdispersion

plot(cod_negbi)
#slightly better than the quasipoisson model -> use negative binomial for the segmented analysis

mean(cod$SSB_lag, na.rm = T)
seg_cod_negbi <- segmented::segmented(glm.nb(R_1 ~SSB_lag, data = cod), seg.Z =  ~SSB_lag, psi = mean(cod$SSB_lag, na.rm = T))
summary(seg_cod_negbi)
summary(seg_cod_negbi)$psi

coef_cod_negbi <-c(seg_cod_negbi$fitted.values)
brpt_cod_negbi <- seg_cod_negbi$psi[2]
brpt_ste_cod_negbi <- seg_cod_negbi$psi[3]

#3.7. Strucchange ----
bpts <- breakpoints(R_1 ~ SSB_lag, data = cod)

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
opt_brks #
bpts2 <- breakpoints(bpts, breaks = opt_brks)
best_brk <- cod$SSB_lag[bpts2$breakpoints]

best_brk #98224

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(R_1 ~ SSB_lag, data = cod, type = "p")
for (i in 1:opt_brks) {
  abline(v = cod$SSB_lag[ci_mod$confint[i,2]], col = "blue")
  abline(v = cod$SSB_lag[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = cod$SSB_lag[ci_mod$confint[i,3]], col = "red", lty = 3)
}


## fit null hypothesis model
fm0 <- lm(R_1 ~ SSB_lag, data = cod)
# fit model with 1 breakpoint but formula different then in previous time series:



######cannot find "best_brk"
#best_brk 1
strucc_cod1 <- lm(R_1 ~ SSB_lag*(SSB_lag <= best_brk[1]) + SSB_lag*(SSB_lag > best_brk[1]), data = cod)
fm1_coef1 <- coef(strucc_cod1)
fit_strucc1 <- fitted(strucc_cod1)

fit11 <- (fm1_coef1[1] + fm1_coef1[3]) + (fm1_coef1[2] + fm1_coef1[5])*cod$SSB_lag[cod$SSB_lag <= best_brk[1]]
fit12 <- (fm1_coef1[1] + fm1_coef1[4]) + (fm1_coef1[2])*cod$SSB_lag[cod$SSB_lag >= best_brk[1]]

#brest_brk 2
strucc_cod2 <- lm(R_1 ~ SSB_lag*(SSB_lag < best_brk[2]) + SSB_lag*(SSB_lag > best_brk[2]), data = cod)
fm1_coef2 <- coef(strucc_cod2)
fit_strucc2 <- fitted(strucc_cod2)

fit21 <- (fm1_coef2[1] + fm1_coef2[3]) + (fm1_coef2[2] + fm1_coef2[5])*cod$SSB_lag[cod$SSB_lag <= best_brk[2]]
fit22 <- (fm1_coef2[1] + fm1_coef2[4]) + (fm1_coef2[2])*cod$SSB_lag[cod$SSB_lag>= best_brk[2]]

# add to previous plot
lines(cod$SSB_lag, fitted(fm0), col = 3)
lines(cod$SSB_lag[cod$SSB_lag <= best_brk[1]], fit11, col = "orange")
lines(cod$SSB_lag[cod$SSB_lag >= best_brk[1]], fit12, col = "orange")
lines(cod$SSB_lag[cod$SSB_lag <= best_brk[2]], fit21, col = "orange")
lines(cod$SSB_lag[cod$SSB_lag >= best_brk[2]], fit22, col = "orange")

lines(cod$SSB_lag[cod$SSB_lag <= best_brk[1]], fit_strucc1[1:bpts2[[1]]], col = "orange")
lines(cod$SSB_lag[cod$SSB_lag >= best_brk[1]], fit_strucc1[bpts2[[1]]:nrow(cod)], col = "orange")


#3.8. Visualize and compare with RMSE ----
SRR_cod_models <- ggplot(ns_data, aes(SSB_lag/1000, R_1/1000000)) +
  geom_point(col=color_regimes_cod)+
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_cod_negbi/1000000), linetype = 2)+
  geom_line(aes(y = coef_cod_negbi/1000000, col = "seg. neg bi"))+
  geom_line(aes(y = coef_cod/1000000, col = "segmented"))+
  geom_vline(aes(col = "segmented log", xintercept = exp(brpt_cod_log)/1000000), linetype = 2)+
  geom_line(aes(y = exp(coef_cod_log)/1000000, col = "segmented log"))+
  geom_line(data = ns_data, aes(ssb/1000,pR_ricker/1000000, col = "Ricker"), show.legend = TRUE)+ #Ricker
  geom_line(data=ns_data, aes(ssb/1000,pR_beverton/1000000, col = "Beverton-Holt"), show.legend = TRUE)+ #BH
  geom_line(data = ns_data, aes(ssb/1000, fitI/1000000, col = "Indipendence"), show.legend = T)+
  geom_line(data = ns_data, aes(ssb/1000, fit_strucc1/1000000, col = "strucchange"))+
  labs(col = "Model")+
  ggtitle("Cod")

SRR_cod_models

rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}

comparison <- NULL
comparison <- AIC(m1, srR_beverton, srR_ricker, seg_cod, seg_cod_log, seg_cod_negbi, strucc_cod1)
comparison[1, 3] <- rmse(sim = fitI, obs = cod$R_1)
comparison[2, 3] <- rmse(sim = ns_data$pR_beverton, obs = cod$R_1)
comparison[3, 3] <- rmse(sim = ns_data$pR_ricker, obs = cod$R_1)
comparison[4, 3] <- rmse(sim = seg_cod$fitted.values, obs = cod$R_1)
comparison[5, 3] <- rmse(sim = seg_cod_log$fitted.values, obs = cod$R_1)
comparison[6, 3] <- rmse(sim = seg_cod_negbi$fitted.values, obs = cod$R_1)
comparison[7, 3] <- rmse(sim = fit_strucc1, obs = cod$R_1)
comparison



###############Outcome

# df                    AIC       V3
# m1             3 1689.1566 491364.7
# srR_beverton   3  129.5213 507607.1
# srR_ricker     3  129.5185 507589.7
# seg_cod        5 1681.3869 464153.8
# seg_cod_log    5  130.5644 874222.8
# seg_cod_negbi  5 1642.5051 451471.7
# strucc_cod     6 1687.5439 481125.8
#lowest RMSE has the segmented negative binomial





#4.Haddock ----

haddock <- read.csv("SA_haddock_2021.csv",
                       sep = ",")
haddock <- arrange(haddock, SSB)

color_regimes_haddock <- NULL
color_regimes_haddock[haddock$Year %in% c(1972:2001)] <- "darkorange"
color_regimes_haddock[haddock$Year %in% c(2001:2021)] <- "purple"

ns_data <- haddock
ns_data$R <- ns_data$R_0
ns_data$ssb <- ns_data$SSB

#4.1. Indipendence Model ----
m1 <- lm(ns_data$R~ns_data$ssb)
summary(m1)
fitI <- fitted(m1)
#high overdispersion

#4.2. Beverton Holt ----
svR <- srStarts(R ~ SSB, data=ns_data, type="BevertonHolt")
svR #negative starting values



#choose own starting values for a and b
svR2 <- list(a = 1000, b = 0.007)
svR2

###stock-recruitement function (NB, the function is log based!)
bh <- srFuns("BevertonHolt")


srR_beverton <- nls(log(R)~log(bh(SSB,a,b)), data=ns_data, start=list(a = 76.79568, b = 5.458455e-06))#######doesn't work, no matter which starting values are tried
summary(srR_beverton)
cbind(estimates=coef(srR_beverton), confint(srR_beverton))
### make predictions to then plot!
pR_beverton <- bh(ns_data$SSB, a=coef(srR_beverton))
ns_data$pR_beverton <- pR_beverton
###quasi-r2 value, if low model does not fit well!
cor(bh(ns_data$SSB, a=coef(srR_beverton)), ns_data$R)^2

#4.3. Ricker ----
svR <- srStarts(R ~ SSB, data=ns_data, type="Ricker")
svR
##fit the Ricker function to data
rckr <- srFuns("Ricker")
srR_ricker <- nls(log(R)~log(rckr(SSB,a,b)), data=ns_data, start=svR)
#for a and b
cbind(estimates=coef(srR_ricker),confint(srR_ricker))
###prediction
#plot
pR_ricker <- rckr(ns_data$SSB, a=coef(srR_ricker))
ns_data$pR_ricker <- pR_ricker

#4.4. Segmented ----
mean(haddock$SSB, na.rm = T) #213457.4
seg_haddock <- segmented::segmented(lm(R_0 ~SSB, data = haddock), seg.Z =  ~SSB, psi = mean(haddock$SSB, na.rm = T))
summary(seg_haddock)
summary(seg_haddock)$psi
coef_haddock <-c(seg_haddock$fitted.values)
brpt_haddock <- seg_haddock$psi[2]
brpt_ste_haddock <- seg_haddock$psi[3]

#4.5. Segmented log ----
haddock$r_log <- log(haddock$R_0)
haddock$ssb_log <- log(haddock$SSB)

mean(haddock$ssb_log, na.rm = T) #12.15036
seg_haddock_log <- segmented::segmented(lm(r_log ~ ssb_log, data = haddock), seg.Z =  ~ ssb_log, psi = mean(haddock$ssb_log, na.rm = T))
summary(seg_haddock_log)
summary(seg_haddock_log)$psi

coef_haddock_log <-c(seg_haddock_log$fitted.values)
brpt_haddock_log <- seg_haddock_log$psi[2]
brpt_ste_haddock_log <- seg_haddock_log$psi[3]

#4.6. Segmented with best performing GLM ----
#4.6.1. glm with gaussian ----
haddock_gaus <- glm(R_0 ~ SSB, data = haddock, family = gaussian)
summary(haddock_gaus)
7.3909e+15/48 #1.539771e+14, clear overdispersion
par(mfrow = c(2,2))
plot(haddock_gaus)

#4.6.2. glm with poisson ----
haddock_pois <- glm(R_0 ~ SSB, data = haddock, family = poisson)
summary(haddock_pois)
574558105/48 #11969961, clear overdispersion

plot(haddock_pois)
#better, but overdispersed

#4.6.3. glm with quasipoisson ----
haddock_qpois <- glm(R_0 ~ SSB, data = haddock, family = quasipoisson)
summary(haddock_qpois)

plot(haddock_qpois)
#ok modelfit

#4.6.4. glm with negative binomial ----
library(MASS)
haddock_negbi <- glm.nb(R_0 ~ SSB, data = haddock)
summary(haddock_negbi)
59.456/48 #1.249422 -> no over or underdispersion

plot(haddock_negbi)
#slightly better than the quasipoisson model -> use negative binomial for the segmented analysis

mean(haddock$SSB, na.rm = T)
seg_haddock_negbi <- segmented::segmented(glm.nb(R_0 ~SSB, data = haddock), seg.Z =  ~SSB, psi = mean(haddock$SSB, na.rm = T))
summary(seg_haddock_negbi)
summary(seg_haddock_negbi)$psi

coef_haddock_negbi <-c(seg_haddock_negbi$fitted.values)
brpt_haddock_negbi <- seg_haddock_negbi$psi[2]
brpt_ste_haddock_negbi <- seg_haddock_negbi$psi[3]

#4.7. Strucchange ----
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
opt_brks #1
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- haddock$SSB[bpts2$breakpoints]

best_brk #295226

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(R_0 ~ SSB, data = haddock, type = "p")
for (i in 1: opt_brks) {
  abline(v = haddock$SSB[ci_mod$confint[i,2]], col = "blue")
  abline(v = haddock$SSB[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = haddock$SSB[ci_mod$confint[i,3]], col = "red", lty = 3)
}


## fit null hypothesis model
fm0 <- lm(R_0 ~ SSB, data = haddock)
# fit model with 1 breakpoint but formula different then in previous time series:



######cannot find "best_brk"
strucc_haddock <- lm(R_0 ~ SSB*(SSB < best_brk) + SSB*(SSB > best_brk), data = haddock)
fm1_coef <- coef(strucc_haddock)

fit_strucc <- fitted(strucc_haddock)

fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[5])*haddock$SSB[haddock$SSB <= best_brk]
fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2])*haddock$SSB[haddock$SSB>= best_brk]

# add to previous plot
lines(haddock$SSB, fitted(fm0), col = 3)
lines(haddock$SSB[haddock$SSB <= best_brk], fit1, col = "orange")
lines(haddock$SSB[haddock$SSB >= best_brk], fit2, col = "orange")


#4.8. Visualize and compare with RMSE ----
SRR_haddock_models <- ggplot(haddock, aes(SSB/1000, R_0/1000000)) +
  geom_point()+
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_haddock_negbi/1000), linetype = 2)+
  geom_line(aes(y = coef_haddock_negbi/1000000, col = "seg. neg bi"))+
  geom_vline(aes(col = "segmented", xintercept = brpt_haddock/1000), linetype = 2)+
  geom_line(aes(y = coef_haddock/1000000, col = "segmented"))+
  geom_vline(aes(col = "segmented log", xintercept = exp(brpt_haddock_log)/1000), linetype = 2)+
  geom_line(aes(y = exp(coef_haddock_log)/1000, col = "segmented log"))+
  geom_line(data = ns_data, aes(ssb/1000,pR_ricker/1000000, col = "Ricker"), show.legend = TRUE)+ #Ricker
  #geom_line(data=ns_data, aes(ssb/1000000,pR_beverton/1000000, col = "Beverton-Holt"), show.legend = TRUE)+ #BH doesn't work because of starting values
  geom_line(data = ns_data, aes(ssb/1000, fitI/1000000, col = "Indipendence"), show.legend = T)+
  geom_line(data = ns_data, aes(ssb/1000, fit_strucc/1000000, col = "strucchange"))+
  labs(col = "Model")

SRR_haddock_models


rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}


#leave out srR_beverton --> doesn't work with NAs

comparison <- NULL
comparison <- AIC(m1,srR_ricker, seg_haddock, seg_haddock_log, seg_haddock_negbi, strucc_haddock)
comparison[1, 3] <- rmse(sim = fitI, obs = haddock$R_0)
comparison[2, 3] <- rmse(sim = ns_data$pR_beverton, obs = haddock$R_0)
comparison[2, 3] <- rmse(sim = ns_data$pR_ricker, obs = haddock$R_0)
comparison[3, 3] <- rmse(sim = seg_haddock$fitted.values, obs = haddock$R_0)
comparison[4, 3] <- rmse(sim = seg_haddock_log$fitted.values, obs = haddock$R_0)
comparison[5, 3] <- rmse(sim = seg_haddock_negbi$fitted.values, obs = haddock$R_0)
comparison[6, 3] <- rmse(sim = fit_strucc, obs = haddock$R_0)
comparison


#                    df       AIC       V3
# m1                 3 1779.2438 12158017
# srR_ricker         3  182.0078 13279671
# seg_haddock        5 1781.9817 12005541
# seg_haddock_log    5  183.5226 15751640
# seg_haddock_negbi  5 1716.0561 12026556
# strucc_haddock     6 1781.0536 11659101





#5.Herring ----

herring <- read.csv("SA_herring_2021.csv",
                    sep = ",")
herring <- arrange(herring, SSB)
#herring <- arrange(herring, SSB)
color_regimes_herring <- NULL
color_regimes_herring[herring$Year %in% c(1947:1966)] <- "steelblue3"
color_regimes_herring[herring$Year %in% c(1966:1983)] <- "darkorange"
color_regimes_herring[herring$Year %in% c(1983:2021)] <- "purple"
herring$color_regimes_herring <- color_regimes_herring



RR_herring <- ggplot(data = herring)+
  geom_point((aes(x = SSB/1000, y = R_0/1000000)),col = color_regimes_herring)+
  theme_test()+
  labs(title = "Herring")
SRR_herring


ns_data <- herring
ns_data$R <- ns_data$R_0
ns_data$ssb <- ns_data$SSB

#1.1. Indipendence Model ----
m1 <- lm(ns_data$R~ns_data$ssb)
summary(m1)
fitI <- fitted(m1)
#high overdispersion

#1.2. Beverton Holt ----
svR <- srStarts(R ~ ssb, data=ns_data, type="BevertonHolt")
svR
###stock-recruitement function (NB, the function is log based!)
bh <- srFuns("BevertonHolt")
srR_beverton <- nls(log(R)~log(bh(ssb,a,b)), data=ns_data, start=svR)


##results tell you significance of parameters and residual standard error
#resisual standard error is the square root of the residual sum of squares
#divided by degrees of freedom
summary(srR_beverton)
cbind(estimates=coef(srR_beverton), confint(srR_beverton))
### make predictions to then plot!
pR_beverton <- bh(ns_data$ssb, a=coef(srR_beverton))
ns_data$pR_beverton <- pR_beverton
###quasi-r2 value, if low model does not fit well!
cor(bh(ns_data$ssb, a=coef(srR_beverton)), ns_data$R)^2

#1.3. Ricker ----
svR <- srStarts(R ~ ssb, data=ns_data, type="Ricker")
svR
##fit the Ricker function to data
rckr <- srFuns("Ricker")
srR_ricker <- nls(log(R)~log(rckr(ssb,a,b)), data=ns_data, start=svR)
#for a and b
cbind(estimates=coef(srR_ricker),confint(srR_ricker))
###prediction
#plot
pR_ricker <- rckr(ns_data$ssb, a=coef(srR_ricker))
ns_data$pR_ricker <- pR_ricker

#1.4. Segmented ----
mean(herring$SSB, na.rm = T) #1930650
seg_herring <- segmented::segmented(lm(R_0 ~SSB, data = herring), seg.Z =  ~SSB, psi = mean(herring$SSB, na.rm = T))
summary(seg_herring)
summary(seg_herring)$psi
coef_herring <-c(seg_herring$fitted.values)
brpt_herring <- seg_herring$psi[2]
brpt_ste_herring <- seg_herring$psi[3]

#1.5. Segmented log ----
herring$r_log <- log(herring$R_0)
herring$SSB_lag_log <- log(herring$SSB)
#View(herring)

mean(herring$SSB_lag_log, na.rm = T) #14.01547
seg_herring_log <- segmented::segmented(lm(r_log ~ SSB_lag_log, data = herring), seg.Z =  ~ SSB_lag_log, psi = mean(herring$SSB_lag_log, na.rm = T))
summary(seg_herring_log)
summary(seg_herring_log)$psi

coef_herring_log <-c(seg_herring_log$fitted.values)
brpt_herring_log <- seg_herring_log$psi[2]
brpt_ste_herring_log <- seg_herring_log$psi[3]

#1.6. Segmented with best performing GLM ----
#1.6.1. glm with gaussian
herring_gaus <- glm(R_0 ~ SSB, data = herring, family = gaussian)
summary(herring_gaus)
1.6091e+16/73 #2.204247e+14, clear overdispersion
par(mfrow = c(2,2))
plot(herring_gaus)
# bad model fit, dont necessary to visualize

#1.6.2. glm with poisson
herring_pois <- glm(R_0 ~ SSB, data = herring, family = poisson)
summary(herring_pois)
569510983/73 #7801520, clear overdispersion

plot(herring_pois)
#better, but overdispersed

#1.6.3. glm with quasipoisson
herring_qpois <- glm(R_0 ~ SSB, data = herring, family = quasipoisson)
summary(herring_qpois)

plot(herring_qpois)
569510983/73#569510983
#not better modelfit, but no overdispersion

#1.6.4. glm with negative binomial

herring_negbi <- glm.nb(R_0 ~ SSB, data = herring)
summary(herring_negbi)
78.842/73 #1.080027 -> no over or underdispersion

plot(herring_negbi)
#slightly better than the quasipoisson model -> use negative binomial for the segmented analysis

mean(herring$SSB, na.rm = T)
seg_herring_negbi <- segmented::segmented(glm.nb(R_0 ~SSB, data = herring), seg.Z =  ~SSB, psi = mean(herring$SSB, na.rm = T))
summary(seg_herring_negbi)
summary(seg_herring_negbi)$psi

coef_herring_negbi <-c(seg_herring_negbi$fitted.values)
brpt_herring_negbi <- seg_herring_negbi$psi[2]
brpt_ste_herring_negbi <- seg_herring_negbi$psi[3]

#1.7. Strucchange ----
bpts <- strucchange :: breakpoints(R_0 ~ SSB, data = herring)


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
opt_brks #1
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- herring$SSB[bpts2$breakpoints]

best_brk #1202006

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(R_0 ~ SSB, data = herring, type = "p")
for (i in 1: opt_brks) {
  abline(v = haddock$SSB[ci_mod$confint[i,2]], col = "blue")
  abline(v = haddock$SSB[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = haddock$SSB[ci_mod$confint[i,3]], col = "red", lty = 3)
}


## fit null hypothesis model
fm0 <- lm(R_0 ~ SSB, data = herring)
# fit model with 1 breakpoint but formula different then in previous time series:



######cannot find "best_brk"
strucc_herring <- lm(R_0 ~ SSB*(SSB < best_brk) + SSB*(SSB > best_brk), data = herring)
fm1_coef <- coef(strucc_herring)

fit_strucc <- fitted(strucc_herring)

fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[5])*herring$SSB[herring$SSB <= best_brk]
fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2])*herring$SSB[herring$SSB>= best_brk]

# add to previous plot
lines(herring$SSB, fitted(fm0), col = 3)
lines(herring$SSB[herring$SSB <= best_brk], fit1, col = "orange")
lines(herring$SSB[herring$SSB >= best_brk], fit2, col = "orange")

#1.8. Visualize and compare with RMSE ----
SRR_herring_models <- SRR_herring +
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_herring_negbi/1000), linetype = 2)+
  #geom_segment(y = 800, yend = 800, x = brpt_herring_negbi/1000000-brpt_ste_herring_negbi/1000000,
  #            xend = brpt_herring_negbi/1000000+brpt_ste_herring_negbi/1000000, col = "red")+
  geom_line(aes(y = coef_herring_negbi/10000000, col = "seg. neg bi"))+
  geom_vline(aes(col = "segmented", xintercept = brpt_herring/1000), linetype = 2)+
  #geom_segment(y = 20, yend = 20, x = brpt_herring/1000000-brpt_ste_herring/1000000,
  #            xend = brpt_herring/1000000+brpt_ste_herring/1000000, col = "red")+
  geom_line(aes(y = coef_herring/1000000, col = "segmented"))+
  geom_vline(aes(col = "segmented log", xintercept = exp(brpt_herring_log)/1000), linetype = 2)+
  #geom_segment(y = 0.5, yend = 0.5, x = exp(brpt_herring_log)/1000000-exp(brpt_ste_herring_log)/1000000,
  #            xend = exp(brpt_herring_log)/1000000+exp(brpt_ste_herring_log)/1000000, col = "red")+
  geom_line(aes(y = exp(coef_herring_log)/1000000, col = "segmented log"))+
  geom_line(data = ns_data, aes(ssb/1000,pR_ricker/1000000, col = "Ricker"), show.legend = TRUE)+ #Ricker
  geom_line(data=ns_data, aes(ssb/1000,pR_beverton/1000000, col = "Beverton-Holt"), show.legend = TRUE)+ #BH
  geom_line(data = ns_data, aes(ssb/1000, fitI/1000000, col = "Indipendence"), show.legend = T)+
  #geom_line(data = herring[herring$SSB_lag <= best_brk[1], ], aes(x = SSB_lag/1000, y = fit1/1000, col = "strucchange"), show.legend = TRUE)+
  #geom_line(data = herring[herring$SSB_lag > best_brk[1] & herring$SSB_lag<= best_brk[2], ], aes(x = SSB_lag/1000, y = fit2/100000, col = "strucchange"), show.legend = TRUE)+
  #geom_line(data = herring[herring$SSB_lag > best_brk[2], ], aes(SSB_lag/1000, fit3/1000, col = "strucchange"), show.legend = TRUE)+
  #geom_vline(aes(xintercept = best_brk[1]/1000, col = "strucchange"), linetype = 2)+
  #geom_vline(aes(xintercept = best_brk[2]/1000, col = "strucchange"), linetype = 2)+
  labs(col = "Model")

SRR_herring_models
#Note: must abline strucchange in different way (if it works)

rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}

comparison <- NULL
comparison <- AIC(m1, srR_beverton, srR_ricker, seg_herring, seg_herring_log, seg_herring_negbi
                  , strucc_herring
)
comparison[1, 3] <- rmse(sim = fitI, obs = herring$R_0)
comparison[2, 3] <- rmse(sim = ns_data$pR_beverton, obs = herring$R_0)
comparison[3, 3] <- rmse(sim = ns_data$pR_ricker, obs = herring$R_0)
comparison[4, 3] <- rmse(sim = seg_herring$fitted.values, obs = herring$R_0)
comparison[5, 3] <- rmse(sim = seg_herring_log$fitted.values, obs = herring$R_0)
comparison[6, 3] <- rmse(sim = seg_herring_negbi$fitted.values, obs = herring$R_0)
comparison[7, 3] <- rmse(sim = c(fit1, fit2, fit3), obs = herring$R_0)
comparison

#                   df        AIC       V3
# m1                 3 2693.80705 14647426
# srR_beverton       3  104.64122 14057361
# srR_ricker         3  105.35588 14148385
# seg_herring        5 2673.94919 12493546
# seg_herring_log    5   86.77138 33696376
# seg_herring_negbi  5 2645.65262 12559099
# strucc_herring     3 2693.80705 20110467


#7.Hake ----

hake <- read.csv("SA_hake_2021.csv", sep = ",")

hake <- arrange(hake, SSB)
color_regimes_hake <- NULL
color_regimes_hake[hake$Year %in% c(1978:1985)] <- "steelblue3"
color_regimes_hake[hake$Year %in% c(1985:2010)] <- "darkorange"
color_regimes_hake[hake$Year %in% c(2010:2021)] <- "purple"



SRR_hake <- ggplot(data = hake, aes(x = SSB/1000, y = R_0/1000))+
  geom_point(col = color_regimes_hake)+
  labs(x = "Spawning stock biomass", y = "Recruitment", title = "Hake")+
  theme_test()
SRR_hake

ns_data <- hake
ns_data$R <- ns_data$R_0
ns_data$ssb <- ns_data$SSB


#7.1. Indipendence Model ----
m1 <- lm(ns_data$R~ns_data$ssb)
summary(m1)
fitI <- fitted(m1)
#high overdispersion

#7.2. Beverton Holt ----
svR <- srStarts(R ~ ssb, data=ns_data, type="BevertonHolt")
svR
###stock-recruitement function (NB, the function is log based!)
bh <- srFuns("BevertonHolt")
srR_beverton <- nls(log(R)~log(bh(ssb,a,b)), data=ns_data, start=list(a = svR$a, b = svR$b))


##results tell you significance of parameters and residual standard error
#resisual standard error is the square root of the residual sum of squares
#divided by degrees of freedom
summary(srR_beverton)
cbind(estimates=coef(srR_beverton), confint(srR_beverton))
### make predictions to then plot!
pR_beverton <- bh(ns_data$ssb, a=coef(srR_beverton))
ns_data$pR_beverton <- pR_beverton
###quasi-r2 value, if low model does not fit well!
cor(bh(ns_data$ssb, a=coef(srR_beverton)), ns_data$R)^2

#7.3. Ricker ----
svR <- srStarts(R ~ ssb, data=ns_data, type="Ricker")
svR
##fit the Ricker function to data
rckr <- srFuns("Ricker")
srR_ricker <- nls(log(R)~log(rckr(ssb,a,b)), data=ns_data, start=list(a = svR$a, b = svR$b))
#for a and b
cbind(estimates=coef(srR_ricker),confint(srR_ricker))
###prediction
#plot
pR_ricker <- rckr(ns_data$ssb, a=coef(srR_ricker))
ns_data$pR_ricker <- pR_ricker

#7.4. Segmented ----
mean(hake$SSB, na.rm = T) #99821.3
seg_hake <- segmented::segmented(lm(R_0 ~SSB, data = hake), seg.Z =  ~SSB, psi = mean(hake$SSB, na.rm = T))
summary(seg_hake)
summary(seg_hake)$psi
coef_hake <-c(seg_hake$fitted.values)
brpt_hake <- seg_hake$psi[2]
brpt_ste_hake <- seg_hake$psi[3]

#7.5. Segmented log ----
hake$r_log <- log(hake$R_0)
hake$SSB_log <- log(hake$SSB)
#View(hake)

mean(hake$SSB_log, na.rm = T) #11.95621
seg_hake_log <- segmented::segmented(lm(r_log ~ SSB_log, data = hake), seg.Z =  ~ SSB_log, psi = mean(hake$SSB_log, na.rm = T))
summary(seg_hake_log)
summary(seg_hake_log)$psi

coef_hake_log <-c(seg_hake_log$fitted.values)
brpt_hake_log <- seg_hake_log$psi[2]
brpt_ste_hake_log <- seg_hake_log$psi[3]

#7.6. Segmented with best performing GLM ----
#7.6.1. glm with gaussian ----
hake_gaus <- glm(R_0 ~ SSB, data = hake, family = gaussian)
summary(hake_gaus)
7.7347e+11/42 #18783500000, clear overdispersion
par(mfrow = c(2,2))
plot(hake_gaus)
# bad model fit, dont necessary to visualize

#7.6.2. glm with poisson ----
hake_pois <- glm(R_0 ~ SSB, data = hake, family = poisson)
summary(hake_pois)
2071973/42 #49332.69, clear overdispersion

plot(hake_pois)
#better, but overdispersed

#7.6.3. glm with quasipoisson ----
hake_qpois <- glm(R_0 ~ SSB, data = hake, family = quasipoisson)
summary(hake_qpois)

plot(hake_qpois)
#not better modelfit, but no overdispersion

#7.6.4. glm with negative binomial -----
library(MASS)
hake_negbi <- glm.nb(R_0 ~ SSB, data = hake)
summary(hake_negbi)
44.963/42 #1.073625 -> no over or underdispersion

plot(hake_negbi)
#slightly better than the quasipoisson model -> use negative binomial for the segmented analysis

mean(hake$SSB, na.rm = T)
seg_hake_negbi <- segmented::segmented(glm.nb(R_0 ~SSB, data = hake), seg.Z =  ~SSB, psi = mean(hake$SSB, na.rm = T))
summary(seg_hake_negbi)
summary(seg_hake_negbi)$psi

coef_hake_negbi <-c(seg_hake_negbi$fitted.values)
brpt_hake_negbi <- seg_hake_negbi$psi[2]
brpt_ste_hake_negbi <- seg_hake_negbi$psi[3]

#7.7. Strucchange ----
bpts <- strucchange :: breakpoints(R_0 ~ SSB, data = hake)


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
opt_brks #1
bpts2 <-strucchange :: breakpoints(bpts, breaks = opt_brks)
best_brk <- hake$SSB[bpts2$breakpoints]

best_brk #70773

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(R_0 ~ SSB, data = hake, type = "p")
for (i in 1: opt_brks) {
  abline(v = haddock$SSB[ci_mod$confint[i,2]], col = "blue")
  abline(v = haddock$SSB[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = haddock$SSB[ci_mod$confint[i,3]], col = "red", lty = 3)
}


## fit null hypothesis model
fm0 <- lm(R_0 ~ SSB, data = hake)
# fit model with 1 breakpoint but formula different then in previous time series:



######cannot find "best_brk"
strucc_hake <- lm(R_0 ~ SSB*(SSB < best_brk) + SSB*(SSB > best_brk), data = hake)
fm1_coef <- coef(strucc_hake)

fit_strucc <- fitted(strucc_hake)

fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[5])*hake$SSB[hake$SSB <= best_brk]
fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2])*hake$SSB[hake$SSB>= best_brk]

# add to previous plot
lines(hake$SSB, fitted(fm0), col = 3)
lines(hake$SSB[hake$SSB <= best_brk], fit1, col = "orange")
lines(hake$SSB[hake$SSB >= best_brk], fit2, col = "orange")

#Strucchange in ggplot with 2 breakpoints:

SRR_hake_strucchange <- SRR_hake+
  geom_line(data = hake[hake$SSB <= best_brk[1], ], aes(SSB/1000, fit1/1000, col = "strucchange"), show.legend = TRUE)+
  geom_vline(aes(xintercept = best_brk[1]/1000, col = "strucchange"), linetype = 2, show.legend = T)+
  geom_line(data = hake[hake$SSB > best_brk[1] > best_brk[1], ], aes(SSB/1000, fit2/1000, col = "strucchange"), show.legend = TRUE)+
  geom_line(data = hake[hake$SSB < best_brk[1], ], aes(SSB/1000, fit3/1000, col = "strucchange"), show.legend = TRUE)+
  labs(col = "Model")

SRR_hake_strucchange

#7.8. Visualize and compare with RMSE ----
SRR_hake_models <- SRR_hake +
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_hake_negbi/1000), linetype = 2)+
  geom_line(aes(y = coef_hake_negbi/1000, col = "seg. neg bi"))+
  geom_vline(aes(col = "segmented", xintercept = brpt_hake/1000), linetype = 2)+
  geom_line(aes(y = coef_hake/1000, col = "segmented"))+
  geom_vline(aes(col = "segmented log", xintercept = exp(brpt_hake_log)/1000), linetype = 2)+
  geom_line(aes(y = exp(coef_hake_log)/1000, col = "segmented log"))+
  geom_line(data = ns_data, aes(ssb/1000,pR_ricker/1000, col = "Ricker"), show.legend = TRUE)+ #Ricker
  geom_line(data=ns_data, aes(ssb/1000,pR_beverton/1000, col = "Beverton-Holt"), show.legend = TRUE)+ #BH
  geom_line(data = ns_data, aes(ssb/1000, fitI/1000, col = "Independence"), show.legend = T)+
  geom_line(data = hake[hake$SSB <= best_brk[1], ], aes(SSB/1000, fit1/1000, col = "strucchange"), show.legend = TRUE)+
  geom_vline(aes(xintercept = best_brk[1]/1000, col = "strucchange"), linetype = 2, show.legend = T)+
  geom_vline(aes(xintercept = best_brk[2]/1000, col = "strucchange"), linetype = 2, show.legend = T)+
  geom_line(data = hake[hake$SSB > best_brk[1] & hake$SSB<= best_brk[2], ], aes(SSB/1000, fit2/1000, col = "strucchange"), show.legend = TRUE)+
  geom_line(data = hake[hake$SSB > best_brk[2], ], aes(SSB/1000, fit3/1000, col = "strucchange"), show.legend = TRUE)+
  labs(col = "Model")

SRR_hake_models
#Note: must abline strucchange in different way

rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}

comparison <- NULL
comparison <- AIC(m1, srR_beverton, srR_ricker, seg_hake, seg_hake_log, seg_hake_negbi)
comparison[1, 3] <- rmse(sim = fitI, obs = hake$R_0)
comparison[2, 3] <- rmse(sim = ns_data$pR_beverton, obs = hake$R_0)
comparison[3, 3] <- rmse(sim = ns_data$pR_ricker, obs = hake$R_0)
comparison[4, 3] <- rmse(sim = seg_hake$fitted.values, obs = hake$R_0)
comparison[5, 3] <- rmse(sim = seg_hake_log$fitted.values, obs = hake$R_0)
comparison[6, 3] <- rmse(sim = seg_hake_negbi$fitted.values, obs = hake$R_0)
comparison[7, 3] <- rmse(sim = fit_strucc, obs = hake$R_0)
comparison

#                df        AIC       V3
# m1              3 1168.82491 132585.2
# srR_beverton    3   43.42206 135802.1
# srR_ricker      3   46.60204 138000.9
# seg_hake        5 1166.21486 122991.0
# seg_hake_log    5   41.42349 375905.0
# seg_hake_negbi  5 1158.36815 122318.7
# 7              NA         NA 127453.9

#8. Saithe ----

saithe<-read.csv("SA_saithe_2021.csv", sep = ",")



#lag SSB 3 times
saithe$SSB_lag <- lag(saithe$SSB)
saithe$SSB_lag <- lag(saithe$SSB_lag)
saithe$SSB_lag <- lag(saithe$SSB_lag)

saithe<-arrange(saithe, SSB_lag)

ns_data <- saithe
ns_data$R <- ns_data$R_3
ns_data$ssb <- ns_data$SSB_lag

#8.1. Independence Model ----
m1 <- lm(ns_data$R~ns_data$ssb)
summary(m1)
fitI <- fitted(m1)
#high overdispersion

#8.2. Beverton Holt ----
svR <- srStarts(R ~ ssb, data=ns_data, type="BevertonHolt")
svR
###stock-recruitement function (NB, the function is log based!)
bh <- srFuns("BevertonHolt")
srR_beverton <- nls(log(R)~log(bh(ssb,a,b)), data=ns_data, start=svR)


##results tell you significance of parameters and residual standard error
#resisual standard error is the square root of the residual sum of squares
#divided by degrees of freedom
summary(srR_beverton)
cbind(estimates=coef(srR_beverton), confint(srR_beverton))
### make predictions to then plot!
pR_beverton <- bh(ns_data$ssb, a=coef(srR_beverton))
ns_data$pR_beverton <- pR_beverton
###quasi-r2 value, if low model does not fit well!
cor(bh(ns_data$ssb, a=coef(srR_beverton)), ns_data$R)^2

#8.3. Ricker ----
svR <- srStarts(R ~ ssb, data=ns_data, type="Ricker")
svR
##fit the Ricker function to data
rckr <- srFuns("Ricker")
srR_ricker <- nls(log(R)~log(rckr(ssb,a,b)), data=ns_data, start=svR)
#for a and b
cbind(estimates=coef(srR_ricker),confint(srR_ricker))
###prediction
#plot
pR_ricker <- rckr(ns_data$ssb, a=coef(srR_ricker))
ns_data$pR_ricker <- pR_ricker

#8.4. Segmented ----
mean(saithe$SSB_lag, na.rm = T) #234546.7
seg_saithe <- segmented::segmented(lm(R_3 ~SSB_lag, data = saithe), seg.Z =  ~SSB_lag, psi = mean(saithe$SSB_lag, na.rm = T))
summary(seg_saithe)
summary(seg_saithe)$psi
coef_saithe <-c(seg_saithe$fitted.values)
brpt_saithe <- seg_saithe$psi[2]
brpt_ste_saithe <- seg_saithe$psi[3]

#8.5. Segmented log ----
saithe$r_log <- log(saithe$R_3)
saithe$ssb_log <- log(saithe$SSB_lag)
#View(herring)

mean(saithe$ssb_log, na.rm = T) #12.27937
seg_saithe_log <- segmented::segmented(lm(r_log ~ ssb_log, data = saithe), seg.Z =  ~ ssb_log, psi = mean(saithe$ssb_log, na.rm = T))
summary(seg_saithe_log)
summary(seg_saithe_log)$psi

coef_saithe_log <-c(seg_saithe_log$fitted.values)
brpt_saithe_log <- seg_saithe_log$psi[2]
brpt_ste_saithe_log <- seg_saithe_log$psi[3]

#8.6. Segmented with best performing GLM ----
#8.6.1. glm with gaussian ----
saithe_gaus <- glm(R_3 ~ SSB_lag, data = saithe, family = gaussian)
summary(saithe_gaus)
# clear overdispersion

#8.6.2. glm with poisson
saithe_pois <- glm(R_3 ~ SSB_lag, data = saithe, family = poisson)
summary(saithe_pois)
# clear overdispersion

#8.6.3. glm with quasipoisson
saithe_qpois <- glm(R_3 ~ SSB_lag, data = saithe, family = quasipoisson)
summary(saithe_qpois)

#8.6.4. glm with negative binomial
library(MASS)
saithe_negbi <- glm.nb(R_3 ~ SSB_lag, data = saithe)
summary(saithe_negbi)
54.180 /50#1.0836 -> no over or underdispersion

mean(saithe$SSB_lag, na.rm = T) # 234546.7
seg_saithe_negbi <- segmented::segmented(glm.nb(R_3 ~SSB_lag, data = saithe), seg.Z =  ~SSB_lag, psi = mean(saithe$SSB_lag, na.rm = T))
summary(seg_saithe_negbi)
summary(seg_saithe_negbi)$psi

coef_saithe_negbi <-c(seg_saithe_negbi$fitted.values)
brpt_saithe_negbi <- seg_saithe_negbi$psi[2]
brpt_ste_saithe_negbi <- seg_saithe_negbi$psi[3]

#8.7. Strucchange ----
bpts <- strucchange::breakpoints(R_3 ~ SSB_lag, data = saithe)
#bpts <- breakpoints(Year ~ Year, data = data_SA)


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
bpts2 <- strucchange::breakpoints(bpts, breaks = opt_brks)
best_brk <- saithe$SSB_lag[bpts2$breakpoints]

best_brk #202253 243151

par(mfrow = c(1,1))
ci_mod <- confint(bpts, breaks = opt_brks)
plot(R_3 ~ SSB_lag, data = saithe, type = "p")
for (i in 1: opt_brks) {
  abline(v = saithe$SSB_lag[ci_mod$confint[i,2]], col = "blue")
  abline(v = saithe$SSB_lag[ci_mod$confint[i,1]], col = "red", lty = 3)
  abline(v = saithe$SSB_lag[ci_mod$confint[i,3]], col = "red", lty = 3)
}


## fit null hypothesis model
fm0 <- lm(R_3 ~ SSB_lag, data = saithe)
# fit model with 2 breakpoint but formula different than in previous time series:

######cannot find "best_brk"
strucc_saithe <- lm(R_3 ~ SSB_lag*(SSB_lag <= best_brk[1]) +
                      SSB_lag*(SSB_lag >= best_brk[1] & SSB_lag <= best_brk[2]) +
                      SSB_lag*(SSB_lag >= best_brk[2]), data = saithe)
fm1_coef <- coef(strucc_saithe)


fit1 <- (fm1_coef[1] + fm1_coef[3]) + (fm1_coef[2] + fm1_coef[6])*saithe$SSB_lag[saithe$SSB_lag <= best_brk[1]]
fit2 <- (fm1_coef[1] + fm1_coef[4]) + (fm1_coef[2] + fm1_coef[7])*
  saithe$SSB_lag[saithe$SSB_lag > best_brk[1] & saithe$SSB_lag <= best_brk[2]]
fit3 <- (fm1_coef[1] + fm1_coef[5]) + (fm1_coef[2] + fm1_coef[8])*saithe$SSB_lag[saithe$SSB_lag> best_brk[2]]
# add to previous plot
lines(saithe$SSB_lag, fitted(fm0), col = 3)
lines(saithe$SSB_lag[saithe$SSB_lag <= best_brk[1]], fit1, col = "orange")
lines(saithe$SSB_lag[saithe$SSB_lag > best_brk[1] & saithe$SSB_lag<= best_brk[2]], fit2, col = "orange")
lines(saithe$SSB_lag[saithe$SSB_lag > best_brk[2]], fit3, col = "orange")

#1.8. Visualize and compare with RMSE ----
color_regimes_saithe <- NULL
color_regimes_saithe[saithe$Year %in% c(1967:1975)] <- "steelblue3"
color_regimes_saithe[saithe$Year %in% c(1975:2010)] <- "darkorange"
color_regimes_saithe[saithe$Year %in% c(2010:2021)] <- "purple"
saithe$color_regimes_saithe <- color_regimes_saithe

SRR_saithe <- ggplot(data = saithe, aes(x = SSB_lag/1000, y = R_3/1000))+
  geom_point(col = color_regimes_saithe)+
  labs(x = "Spawning stock biomass", y = "Recruitment", title = "Saithe")+
  theme_test()
SRR_saithe

SRR_saithe_models <- SRR_saithe +
  geom_vline(aes(col = "seg. neg bi",xintercept = brpt_saithe_negbi/1000), linetype = 2)+
  geom_line(aes(y = coef_saithe_negbi/1000, col = "seg. neg bi"))+
  geom_vline(aes(col = "segmented", xintercept = brpt_saithe/1000), linetype = 2)+
  geom_line(aes(y = coef_saithe/1000, col = "segmented"))+
  geom_vline(aes(col = "segmented log", xintercept = exp(brpt_saithe_log)/1000), linetype = 2)+
  geom_line(aes(y = exp(coef_saithe_log)/1000, col = "segmented log"))+
  geom_line(data = ns_data, aes(ssb/1000,pR_ricker/1000, col = "Ricker"), show.legend = TRUE)+ #Ricker
  geom_line(data=ns_data, aes(ssb/1000,pR_beverton/1000, col = "Beverton-Holt"), show.legend = TRUE)+ #BH
  geom_line(data = ns_data, aes(ssb/1000, fitI/1000, col = "Independence"), show.legend = T)+
  geom_line(data = saithe[saithe$SSB_lag <= best_brk[1], ], aes(SSB_lag/1000, fit1/1000, col = "strucchange"), show.legend = TRUE)+
  geom_vline(aes(xintercept = best_brk[1]/1000, col = "strucchange"), linetype = 2, show.legend = T)+
  geom_line(data = saithe[saithe$SSB_lag > best_brk[1] & saithe$SSB_lag<= best_brk[2], ], aes(SSB_lag/1000, fit2/1000, col = "strucchange"), show.legend = TRUE)+
  geom_line(data = saithe[saithe$SSB_lag > best_brk[1], ], aes(SSB_lag/1000, fit3/1000, col = "strucchange"), show.legend = TRUE)+
  labs(col = "Model")

SRR_saithe_models

rmse <- function(sim, obs) {
  sqrt(mean((obs-sim)^2))
}

comparison <- NULL
comparison <- AIC(m1, srR_beverton, srR_ricker, seg_saithe, seg_saithe_log, seg_saithe_negbi, strucc_saithe)
comparison[1, 3] <- rmse(sim = fitI, obs = saithe$R_3)
comparison[2, 3] <- rmse(sim = ns_data$pR_beverton, obs = saithe$R_3[-c(53:55)])
comparison[3, 3] <- rmse(sim = ns_data$pR_ricker, obs = saithe$R_3[-c(53:55)])
comparison[4, 3] <- rmse(sim = seg_saithe$fitted.values, obs = saithe$R_3)
comparison[5, 3] <- rmse(sim = seg_saithe_log$fitted.values, obs = saithe$R_3)
comparison[6, 3] <- rmse(sim = seg_saithe_negbi$fitted.values, obs = saithe$R_3)
comparison[7, 3] <- rmse(sim = fit_strucc, obs = saithe$R_3)
comparison
#lowest RMSE has the simple segmented analysis
#                 df        AIC        V3
# m1                3 1326.07490  79692.51
# srR_beverton      3   87.76174  82343.52
# srR_ricker        3   90.81553  82853.26
# seg_saithe        5 1328.94564  77819.88
# seg_saithe_log    5   86.81357 171123.12
# seg_saithe_negbi  5 1312.96880  77898.49
# strucc_saithe     9 1317.54126 215206.41
