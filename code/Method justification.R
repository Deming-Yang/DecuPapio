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

par(mfrow=c(2,2)) #800X760
# Upper Buccal
plot(human.UM1$B.PD.mi, human.UM1$B.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.6,
     main = "Upper Buccal", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.UM2$B.PD.mi, human.UM2$B.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.UM3$B.PD.mi, human.UM3$B.HSBD,
       pch=18,col=fill.10[8],cex=1.6)

# Upper lingual
plot(human.UM1$L.PD.mi, human.UM1$L.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.6,
     main = "Upper Lingual", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.UM2$L.PD.mi, human.UM2$L.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.UM3$L.PD.mi, human.UM3$L.HSBD,
       pch=18,col=fill.10[8],cex=1.6)

# lowerer Buccal
plot(human.LM1$B.PD.mi, human.LM1$B.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.6,
     main = "Lower Buccal", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.LM2$B.PD.mi, human.LM2$B.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.LM3$B.PD.mi, human.LM3$B.HSBD,
       pch=18,col=fill.10[8],cex=1.6)

# lower lingual
plot(human.LM1$L.PD.mi, human.LM1$L.HSBD,
     xlim=c(4,14),ylim=c(4,14),pch=18,col=fill.10[2],cex=1.6,
     main = "Lower Lingual", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
points(human.LM2$L.PD.mi, human.LM2$L.HSBD,
       pch=18,col=fill.10[5],cex=1.6)
points(human.LM3$L.PD.mi, human.LM3$L.HSBD,
       pch=18,col=fill.10[8],cex=1.6)
legend("bottomright", legend=c("M1","M2","M3"), pch=18, col=fill.10[c(2,5,8)],cex=1.2)

# Paired comparisons
h.B.PD <- cbind(human.all.data$B.PD.mi, human.all.data$B.HSBD)
colnames(h.B.PD) <- c("B.PD.mi","B.HSBD")
h.B.PD <- na.omit(h.B.PD)

h.L.PD <- cbind(human.all.data$L.PD.mi, human.all.data$L.HSBD)
colnames(h.L.PD) <- c("B.PD.mi","B.HSBD")
h.L.PD <- na.omit(h.L.PD)

# log-transformed results for visualization
par(mfrow=c(1,2)) #800X760
plot(log(h.B.PD$B.PD.mi), log(h.B.PD$B.HSBD),
     xlim=c(1.6,2.8),ylim=c(1.6,2.8),pch=18,col=fill.10[4],cex=1.6,
     main = "Buccal", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)

plot(log(h.L.PD$L.PD.mi), log(h.L.PD$L.HSBD),
     xlim=c(1.6,2.8),ylim=c(1.6,2.8),pch=18,col=fill.10[4],cex=1.6,
     main = "Lingual", xlab = "HSB Packing density", ylab = "Estimate from band width")
abline(a=0,b=1)
