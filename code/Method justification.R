library(ggeffects)
library(ggplot2)

##### Comparing two methods of measuring HSB density #####

# use the human data set as a demonstration
human.all.data <-read.csv("./data/Human decussation all data.csv")

# the estimate of HSB density should be the inverse of average bandwith, or 
# 1/(Diazone w + Parazone w), measured in mm
# the data for diazone w and parazone w are in microns

human.all.data <- human.all.data %>% mutate(B.HSBD = 1/(B.Dia.W/1000 + B.Para.W/1000))

human.all.data <- human.all.data %>% mutate(L.HSBD = 1/(L.Dia.W/1000 + L.Para.W/1000))

human.UM1 <- human.all.data %>%
  filter(Tooth=="UM1")

human.UM2 <- human.all.data %>%
  filter(Tooth=="UM2")

human.UM3 <- human.all.data %>%
  filter(Tooth=="UM3")

human.LM1 <- human.all.data %>%
  filter(Tooth=="LM1")

human.LM2 <- human.all.data %>%
  filter(Tooth=="LM2")

human.LM3 <- human.all.data %>%
  filter(Tooth=="LM3")

human.U <- rbind(human.UM1, human.UM2, human.UM3)
human.L <- rbind(human.LM1, human.LM2, human.LM3)

par(mfrow=c(2,2)) #800X760
# Upper Buccal
plot(human.UM1$B.PD.mi, human.UM1$B.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.5,
     main = "Upper Buccal", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.UM2$B.PD.mi, human.UM2$B.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.UM3$B.PD.mi, human.UM3$B.HSBD,
       pch=18,col=fill.10[8],cex=1.6)

# Upper lingual
plot(human.UM1$L.PD.mi, human.UM1$L.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.5,
     main = "Upper Lingual", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.UM2$L.PD.mi, human.UM2$L.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.UM3$L.PD.mi, human.UM3$L.HSBD,
       pch=18,col=fill.10[8],cex=1.6)

# lowerer Buccal
plot(human.LM1$B.PD.mi, human.LM1$B.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.5,
     main = "Lower Buccal", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.LM2$B.PD.mi, human.LM2$B.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.LM3$B.PD.mi, human.LM3$B.HSBD,
       pch=18,col=fill.10[8],cex=1.6)

# lower lingual
plot(human.LM1$L.PD.mi, human.LM1$L.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.5,
     main = "Lower Lingual", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.LM2$L.PD.mi, human.LM2$L.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.LM3$L.PD.mi, human.LM3$L.HSBD,
       pch=18,col=fill.10[8],cex=1.6)
legend("bottomright", legend=c("M1","M2","M3"), pch=18, col=fill.10[c(2,5,8)],cex=1.2)



# Paired comparisons
# Upper Buccal
U.B.PD <- cbind(human.U$B.PD.mi, human.U$B.HSBD)
colnames(U.B.PD) <- c("B.PD.mi","B.HSBD")
U.B.PD <- na.omit(U.B.PD)
U.B.PD <- data.frame(U.B.PD)

# Upper lingual
U.L.PD <- cbind(human.U$L.PD.mi, human.U$L.HSBD)
colnames(U.L.PD) <- c("L.PD.mi","L.HSBD")
U.L.PD <- na.omit(U.L.PD)
U.L.PD <- data.frame(U.L.PD)

# lowerer Buccal
L.B.PD <- cbind(human.L$B.PD.mi, human.L$B.HSBD)
colnames(L.B.PD) <- c("B.PD.mi","B.HSBD")
L.B.PD <- na.omit(L.B.PD)
L.B.PD <- data.frame(L.B.PD)

# lower lingual
L.L.PD <- cbind(human.L$L.PD.mi, human.L$L.HSBD)
colnames(L.L.PD) <- c("L.PD.mi","L.HSBD")
L.L.PD <- na.omit(L.L.PD)
L.L.PD <- data.frame(L.L.PD)

# all buccal
h.B.PD <- cbind(human.all.data$B.PD.mi, human.all.data$B.HSBD)
colnames(h.B.PD) <- c("B.PD.mi","B.HSBD")
h.B.PD <- na.omit(h.B.PD)
h.B.PD <- data.frame(h.B.PD)

# all lingual
h.L.PD <- cbind(human.all.data$L.PD.mi, human.all.data$L.HSBD)
colnames(h.L.PD) <- c("L.PD.mi","L.HSBD")
h.L.PD <- na.omit(h.L.PD)
h.L.PD <- data.frame(h.L.PD)


# log-transformed results for visualization
par(mfrow=c(1,2)) #800X760
plot(log(h.B.PD$B.PD.mi), log(h.B.PD$B.HSBD),
     xlim=c(1.6,2.8),ylim=c(1.6,2.8),pch=18,col=fill.10[4],cex=1.6,
     main = "Buccal", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)

plot(log(h.L.PD$B.PD.mi), log(h.L.PD$B.HSBD),
     xlim=c(1.6,2.8),ylim=c(1.6,2.8),pch=18,col=fill.10[4],cex=1.6,
     main = "Lingual", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)

# linear regression 1: using band width to estimate packing density
# upper buccal
UB.HSBD.loglm <- lm(log(B.PD.mi) ~ log(B.HSBD) + 0, data = U.B.PD)
summary(UB.HSBD.loglm)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# log(B.HSBD)  0.93555    0.03946   23.71 5.02e-15 ***

UB.grid <- data.frame(B.HSBD = seq(min(log(U.B.PD$B.HSBD)), max(log(U.B.PD$B.HSBD)), length.out = 100))
preds <- predict(UB.HSBD.loglm, newdata = UB.grid, se.fit = TRUE)

fitted <- exp(preds$fit)
lower  <- exp(preds$fit - (1.96 * preds$se.fit))
upper  <- exp(preds$fit + (1.96 * preds$se.fit))

UB.grid.fit <- data.frame(B.HSBD = exp(UB.grid), exp(fitted), exp(lower), exp(upper))

UB.grid.fit<- cbind(UB.grid.fit, exp(predict(UB.HSBD.loglm, newdata = exp(UB.grid), interval = "prediction", level = 0.95)))

ggplot(U.B.PD, aes(x = B.HSBD, y = B.PD.mi)) +
  geom_point(alpha = 0.5) +  # Raw data points
  
  geom_ribbon(data = UB.grid.fit, aes(y = fit, 
                                     ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") + # 95% CI
  geom_ribbon(data = UB.grid.fit, aes(y = exp.fitted., 
                                     ymin = exp.lower., ymax = exp.upper.), alpha = 0.2, fill = "blue") + # 95% CI
  geom_line(data = UB.grid.fit, aes(y = exp.fitted.), color = "blue", linewidth = 1) + # GLM curve
  geom_abline (slope=1, linetype = "dashed", color="Red") +
  theme_minimal() +
  labs(title = "Upper Buccal",
       y = "B.PD.mi", x = "B.HSBD")

# linear regression 2: using band width to estimate packing density
# upper lingual
UL.HSBD.loglm <- lm(log(L.PD.mi) ~ log(L.HSBD) + 0, data = U.L.PD)
summary(UL.HSBD.loglm)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# log(L.HSBD)  1.01474    0.03015   33.66   <2e-16 ***

UL.grid <- data.frame(L.HSBD = seq(min(log(U.L.PD$L.HSBD)), max(log(U.L.PD$L.HSBD)), length.out = 100))
preds <- predict(UL.HSBD.loglm, newdata = UL.grid, se.fit = TRUE)

fitted <- exp(preds$fit)
lower  <- exp(preds$fit - (1.96 * preds$se.fit))
upper  <- exp(preds$fit + (1.96 * preds$se.fit))

UL.grid.fit <- data.frame(L.HSBD = exp(UL.grid), exp(fitted), exp(lower), exp(upper))

UL.grid.fit<- cbind(UL.grid.fit, exp(predict(UL.HSBD.loglm, newdata = exp(UL.grid), interval = "prediction", level = 0.95)))

ggplot(U.L.PD, aes(x = L.HSBD, y = L.PD.mi)) +
  geom_point(alpha = 0.5) +  # Raw data points
  
  geom_ribbon(data = UL.grid.fit, aes(y = fit, 
                                      ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") + # 95% CI
  geom_ribbon(data = UL.grid.fit, aes(y = exp.fitted., 
                                      ymin = exp.lower., ymax = exp.upper.), alpha = 0.2, fill = "blue") + # 95% CI
  geom_line(data = UL.grid.fit, aes(y = exp.fitted.), color = "blue", linewidth = 1) + # GLM curve
  geom_abline (slope=1, linetype = "dashed", color="Red") +
  theme_minimal() +
  labs(title = "Upper Lingual",
       y = "L.PD.mi", x = "L.HSBD")

# linear regression 3: using band width to estimate packing density
# upper buccal
LB.HSBD.loglm <- lm(log(B.PD.mi) ~ log(B.HSBD) + 0, data = L.B.PD)
summary(LB.HSBD.loglm)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# log(B.HSBD)   0.9783     0.0278   35.19   <2e-16 ***

LB.grid <- data.frame(B.HSBD = seq(min(log(L.B.PD$B.HSBD)), max(log(L.B.PD$B.HSBD)), length.out = 100))
preds <- predict(LB.HSBD.loglm, newdata = LB.grid, se.fit = TRUE)

fitted <- exp(preds$fit)
lower  <- exp(preds$fit - (1.96 * preds$se.fit))
upper  <- exp(preds$fit + (1.96 * preds$se.fit))

LB.grid.fit <- data.frame(B.HSBD = exp(LB.grid), exp(fitted), exp(lower), exp(upper))

LB.grid.fit<- cbind(LB.grid.fit, exp(predict(LB.HSBD.loglm, newdata = exp(LB.grid), interval = "prediction", level = 0.95)))

ggplot(L.B.PD, aes(x = B.HSBD, y = B.PD.mi)) +
  geom_point(alpha = 0.5) +  # Raw data points
  
  geom_ribbon(data = LB.grid.fit, aes(y = fit, 
                                      ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") + # 95% CI
  geom_ribbon(data = LB.grid.fit, aes(y = exp.fitted., 
                                      ymin = exp.lower., ymax = exp.upper.), alpha = 0.2, fill = "blue") + # 95% CI
  geom_line(data = LB.grid.fit, aes(y = exp.fitted.), color = "blue", linewidth = 1) + # GLM curve
  geom_abline (slope=1, linetype = "dashed", color="Red") +
  theme_minimal() +
  labs(title = "Lower Buccal",
       y = "B.PD.mi", x = "B.HSBD")

# linear regression 4: using band width to estimate packing density
# upper buccal
LL.HSBD.loglm <- lm(log(L.PD.mi) ~ log(L.HSBD) + 0, data = L.L.PD)
summary(LL.HSBD.loglm)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# log(L.HSBD)  0.95337    0.02642   36.09   <2e-16 ***

LL.grid <- data.frame(L.HSBD = seq(min(log(L.L.PD$L.HSBD)), max(log(L.L.PD$L.HSBD)), length.out = 100))
preds <- predict(LL.HSBD.loglm, newdata = LL.grid, se.fit = TRUE)

fitted <- exp(preds$fit)
lower  <- exp(preds$fit - (1.96 * preds$se.fit))
upper  <- exp(preds$fit + (1.96 * preds$se.fit))

LL.grid.fit <- data.frame(L.HSBD = exp(LL.grid), exp(fitted), exp(lower), exp(upper))

LL.grid.fit<- cbind(LL.grid.fit, exp(predict(LL.HSBD.loglm, newdata = exp(LL.grid), interval = "prediction", level = 0.95)))

ggplot(L.L.PD, aes(x = L.HSBD, y = L.PD.mi)) +
  geom_point(alpha = 0.5) +  # Raw data points
  
  geom_ribbon(data = LL.grid.fit, aes(y = fit, 
                                      ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") + # 95% CI
  geom_ribbon(data = LL.grid.fit, aes(y = exp.fitted., 
                                      ymin = exp.lower., ymax = exp.upper.), alpha = 0.2, fill = "blue") + # 95% CI
  geom_line(data = LL.grid.fit, aes(y = exp.fitted.), color = "blue", linewidth = 1) + # GLM curve
  geom_abline (slope=1, linetype = "dashed", color="Red") +
  theme_minimal() +
  labs(title = "Lower Lingual",
       y = "L.PD.mi", x = "L.HSBD")


# linear regression 5: 
# all buccal 

B.HSBD.loglm <- lm(log(B.PD.mi) ~ log(B.HSBD) + 0, data = h.B.PD)
summary(B.HSBD.loglm)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# log(B.HSBD)  0.95636    0.02441   39.18   <2e-16 ***

B.grid <- data.frame(B.HSBD = seq(min(log(h.B.PD$B.HSBD)), max(log(h.B.PD$B.HSBD)), length.out = 100))
B.preds <- predict(B.HSBD.loglm, newdata = B.grid, se.fit = TRUE)

fitted <- exp(B.preds$fit)
lower  <- exp(B.preds$fit - (1.96 * B.preds$se.fit))
upper  <- exp(B.preds$fit + (1.96 * B.preds$se.fit))

B.grid.fit <- data.frame(B.HSBD = exp(B.grid), exp(fitted), exp(lower), exp(upper))

B.grid.fit<- cbind(B.grid.fit, exp(predict(B.HSBD.loglm, newdata = exp(B.grid), interval = "prediction", level = 0.95)))

ggplot(h.B.PD, aes(x = B.HSBD, y = B.PD.mi)) +
  geom_point(alpha = 0.5) +  # Raw data points

  geom_ribbon(data = B.grid.fit, aes(y = fit, 
                                     ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") + # 95% CI
  geom_ribbon(data = B.grid.fit, aes(y = exp.fitted., 
                                     ymin = exp.lower., ymax = exp.upper.), alpha = 0.2, fill = "blue") + # 95% CI
  geom_line(data = B.grid.fit, aes(y = exp.fitted.), color = "blue", linewidth = 1) + # GLM curve
  geom_abline (slope=1, linetype = "dashed", color="Red") +
  theme_minimal() +
  labs(title = "Buccal",
       y = "B.PD.mi", x = "B.HSBD")

# linear regression 6: 
# all lingual 
L.HSBD.loglm <- lm(log(L.PD.mi) ~ log(L.HSBD) + 0, data = h.L.PD)
summary(L.HSBD.loglm)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# log(L.HSBD)  0.98735    0.02068   47.74   <2e-16 ***

L.grid <- data.frame(L.HSBD = seq(min(log(h.L.PD$L.HSBD)), max(log(h.L.PD$L.HSBD)), length.out = 100))
L.preds <- predict(L.HSBD.loglm, newdata = L.grid, se.fit = TRUE)

fitted <- exp(L.preds$fit)
lower  <- exp(L.preds$fit - (1.96 * L.preds$se.fit))
upper  <- exp(L.preds$fit + (1.96 * L.preds$se.fit))

L.grid.fit <- data.frame(L.HSBD = exp(L.grid), exp(fitted), exp(lower), exp(upper))

L.grid.fit<- cbind(L.grid.fit, exp(predict(L.HSBD.loglm, newdata = exp(L.grid), interval = "prediction", level = 0.95)))

ggplot(h.L.PD, aes(x = L.HSBD, y = L.PD.mi)) +
  geom_point(alpha = 0.5) +  # Raw data points
  
  geom_ribbon(data = L.grid.fit, aes(y = fit, 
                                     ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") + # 95% CI
  geom_ribbon(data = L.grid.fit, aes(y = exp.fitted., 
                                     ymin = exp.lower., ymax = exp.upper.), alpha = 0.2, fill = "blue") + # 95% CI
  geom_line(data = L.grid.fit, aes(y = exp.fitted.), color = "blue", linewidth = 1) + # GLM curve
  geom_abline (slope=1, linetype = "dashed", color="Red") +
  theme_minimal() +
  labs(title = "Lingual",
       y = "L.PD.mi", x = "L.HSBD")


t.test(L.B.PD$B.PD.mi, L.L.PD$L.PD.mi, paired=TRUE,conf.level=0.95)
# t = 1.9018, df = 16, p-value = 0.07536
t.test(U.B.PD$B.PD.mi, U.L.PD$L.PD.mi, paired=TRUE,conf.level=0.95)
# t = -4.0904, df = 18, p-value = 0.0006867

t.test(L.B.PD$B.HSBD, L.L.PD$L.HSBD, paired=TRUE,conf.level=0.95)
# t = 1.0621, df = 16, p-value = 0.304
t.test(U.B.PD$B.HSBD, U.L.PD$L.HSBD, paired=TRUE,conf.level=0.95)
# t = -1.165, df = 18, p-value = 0.2592
# not considered equivalent

# upper functional vs lower functional
t.test(L.B.PD$B.PD.mi, U.L.PD$L.PD.mi, conf.level=0.95)
# t = -1.7382, df = 31.108, p-value = 0.09206
t.test(L.B.PD$B.HSBD, U.L.PD$L.HSBD, conf.level=0.95)
# t = -0.45532, df = 29.725, p-value = 0.6522

# upper guiding vs lower guiding
t.test(L.L.PD$L.PD.mi, U.B.PD$B.PD.mi, conf.level=0.95)
# t = 0.070742, df = 33.903, p-value = 0.944
t.test(L.L.PD$L.HSBD, U.B.PD$B.HSBD, conf.level=0.95)
# t = -0.17209, df = 33.646, p-value = 0.8644


# can we reach the same conclusions?
# 1 functional vs guiding sides
e.HSBD.L <- data.frame(LG = human.L$L.HSBD, LF = human.L$B.HSBD)

e.HSBD.U <- data.frame(UG = human.U$B.HSBD, UF = human.U$L.HSBD)

t.test(e.HSBD.L$LG, e.HSBD.L$LF, paired=TRUE,conf.level=0.95)
# data:  e.HSBD.L$LG and e.HSBD.L$LF
# t = -2.0189, df = 26, p-value = 0.05392
# approaching significance

boxplot(e.HSBD.L$LG, e.HSBD.L$LF)


t.test(e.HSBD.U$UG, e.HSBD.U$UF, paired=TRUE,conf.level=0.95)
# data:  e.HSBD.U$UG and e.HSBD.U$UF
# t = -2.3016, df = 28, p-value = 0.02901
boxplot(e.HSBD.U$UG, e.HSBD.U$UF) # significant

# conclusion: yes, functional side  still display higher HSB density, but only signficant in the upper molars, while approaching significance in the lower


# upper vs lower
t.test(e.HSBD.L$LG, e.HSBD.U$UG, conf.level=0.95)
# data:  e.HSBD.L$LG and e.HSBD.U$UG
# t = -0.023001, df = 53.692, p-value = 0.9817
# no signifcant difference

boxplot(e.HSBD.L$LG, e.HSBD.U$UG) # This is with na.omit not significant


t.test(e.HSBD.L$LF, e.HSBD.U$UF, conf.level=0.95)
# data:  e.HSBD.L$LF and e.HSBD.U$UF
# t = -0.62865, df = 54, p-value = 0.5322
boxplot(e.HSBD.L$LF, e.HSBD.U$UF) # insignificant