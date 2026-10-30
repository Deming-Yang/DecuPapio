setwd("/Users/Carrie/Dropbox/Research Projects/CURRENT_Grine et al_Papio decussation/Working_directory/")
library(ggplot2)


data_all <- read.table("Grine_PAPIO_all data_20260411.txt", header=T,  sep= "\t")


#################################################################################################
#UPPER M1#
UM1 <- data_all[which(data_all$Tooth == "UM1"),]

x <- as.data.frame(cbind( 
  c(UM1$Specimen, UM1$Specimen),      
  c(UM1$B_Dia_K,UM1$L_Dia_K),
  c(UM1$B_Para_K, UM1$L_Para_K),
  c(rep("buccal", length(UM1[,1])), rep("lingual", length(UM1[,1])))
))

colnames(x) <-c("Specimen", "diazone", "parazone", "side")
x$diazone <- as.numeric(x$diazone)
x$parazone <- as.numeric(x$parazone)


col <- c(rep("black", length(UM1[,1])), rep("red", length(UM1[,1])))

plot(x$diazone ~ x$parazone, pch=16, col=col, xlim=c(0,1), ylim=c(0,2), xlab="Parazone K", ylab="Diazone K", main="Upper M1")
legend("bottomright", legend=c("buccal", "lingual"),
       col=c("black", "red"),  cex=0.8, pch=16)


#test for difference in diazone ~ parazone slope between buccal and lingual sides
mod1 <- aov(diazone ~ parazone*side, data=x)
summary(mod1)

#test for difference in diazone ~ parazone intercept between buccal and lingual sides
mod2 <- aov(diazone ~ parazone+side, data=x)
summary(mod2)

#Compare the models
#if P non significant, removing the interaction does not significantly affect the fit of the model -- so difference in intercept, but not slope
anova(mod1, mod2)

#################################################################################################

#UPPER M2#
UM2 <- data_all[which(data_all$Tooth == "UM2"),]

x <- as.data.frame(cbind( 
  c(UM2$Specimen, UM2$Specimen),      
  c(UM2$B_Dia_K,UM2$L_Dia_K),
  c(UM2$B_Para_K, UM2$L_Para_K),
  c(rep("buccal", length(UM2[,1])), rep("lingual", length(UM2[,1])))
))

colnames(x) <-c("Specimen", "diazone", "parazone", "side")
x$diazone <- as.numeric(x$diazone)
x$parazone <- as.numeric(x$parazone)


col <- c(rep("black", length(UM2[,1])), rep("red", length(UM2[,1])))

plot(x$diazone ~ x$parazone, pch=16, col=col, xlim=c(0,1), ylim=c(0,2), xlab="Parazone K", ylab="Diazone K", main="Upper M2")
legend("bottomright", legend=c("buccal", "lingual"),
       col=c("black", "red"),  cex=0.8, pch=16)


#test for difference in diazone ~ parazone slope between buccal and lingual sides
mod1 <- aov(diazone ~ parazone*side, data=x)
summary(mod1)
#There is a significant difference in slopes between sides
#test for difference in diazone ~ parazone intercept between buccal and lingual sides
mod2 <- aov(diazone ~ parazone+side, data=x)
summary(mod2)

#Compare the models
#if P non significant, removing the interaction does not significantly affect the fit of the model -- so difference in intercept, but not slope
anova(mod1, mod2)

#Intercept of the buccal side is significantly lower than the lingual size

#################################################################################################

#UPPER M3
UM3 <- data_all[which(data_all$Tooth == "UM3"),]

x <- as.data.frame(cbind( 
  c(UM3$Specimen, UM3$Specimen),      
  c(UM3$B_Dia_K,UM3$L_Dia_K),
  c(UM3$B_Para_K, UM3$L_Para_K),
  c(rep("buccal", length(UM3[,1])), rep("lingual", length(UM3[,1])))
))

colnames(x) <-c("Specimen", "diazone", "parazone", "side")
x$diazone <- as.numeric(x$diazone)
x$parazone <- as.numeric(x$parazone)


col <- c(rep("black", length(UM3[,1])), rep("red", length(UM3[,1])))

plot(x$diazone ~ x$parazone, pch=16, col=col, xlim=c(0,1), ylim=c(0,2), xlab="Parazone K", ylab="Diazone K", main="Upper M3")
legend("bottomright", legend=c("buccal", "lingual"),
       col=c("black", "red"),  cex=0.8, pch=16)


#test for difference in diazone ~ parazone slope between buccal and lingual sides
mod1 <- aov(diazone ~ parazone*side, data=x)
summary(mod1)
#No significant difference in slopes

#test for difference in diazone ~ parazone intercept between buccal and lingual sides
mod2 <- aov(diazone ~ parazone+side, data=x)
summary(mod2)
#No significant difference in intercept

#Compare the models
#if P non significant, removing the interaction does not significantly affect the fit of the model -- so difference in intercept, but not slope
#anova(mod1, mod2)

#################################################################################################
#LOWER M1
LM1 <- data_all[which(data_all$Tooth == "LM1"),]

x <- as.data.frame(cbind( 
  c(LM1$Specimen, LM1$Specimen),      
  c(LM1$B_Dia_K,LM1$L_Dia_K),
  c(LM1$B_Para_K, LM1$L_Para_K),
  c(rep("buccal", length(LM1[,1])), rep("lingual", length(LM1[,1])))
))

colnames(x) <-c("Specimen", "diazone", "parazone", "side")
x$diazone <- as.numeric(x$diazone)
x$parazone <- as.numeric(x$parazone)


col <- c(rep("black", length(LM1[,1])), rep("red", length(LM1[,1])))

plot(x$diazone ~ x$parazone, pch=16, col=col, xlim=c(0,1), ylim=c(0,2), xlab="Parazone K", ylab="Diazone K", main="Lower M1")
legend("bottomright", legend=c("buccal", "lingual"),
       col=c("black", "red"),  cex=0.8, pch=16)


#test for difference in diazone ~ parazone slope between buccal and lingual sides
mod1 <- aov(diazone ~ parazone*side, data=x)
summary(mod1)
#No significant difference in slopes

#test for difference in diazone ~ parazone intercept between buccal and lingual sides
mod2 <- aov(diazone ~ parazone+side, data=x)
summary(mod2)
#No significant difference in intercept

#Compare the models
#if P non significant, removing the interaction does not significantly affect the fit of the model -- so difference in intercept, but not slope
#anova(mod1, mod2)

#################################################################################################
#LOWER M2
LM2 <- data_all[which(data_all$Tooth == "LM2"),]

x <- as.data.frame(cbind( 
  c(LM2$Specimen, LM2$Specimen),      
  c(LM2$B_Dia_K,LM2$L_Dia_K),
  c(LM2$B_Para_K, LM2$L_Para_K),
  c(rep("buccal", length(LM2[,1])), rep("lingual", length(LM2[,1])))
))

colnames(x) <-c("Specimen", "diazone", "parazone", "side")
x$diazone <- as.numeric(x$diazone)
x$parazone <- as.numeric(x$parazone)


col <- c(rep("black", length(LM2[,1])), rep("red", length(LM2[,1])))

plot(x$diazone ~ x$parazone, pch=16, col=col, xlim=c(0,1), ylim=c(0,2), xlab="Parazone K", ylab="Diazone K", main="Lower M2")
legend("bottomright", legend=c("buccal", "lingual"),
       col=c("black", "red"),  cex=0.8, pch=16)


#test for difference in diazone ~ parazone slope between buccal and lingual sides
mod1 <- aov(diazone ~ parazone*side, data=x)
summary(mod1)
#No significant difference in slopes

#test for difference in diazone ~ parazone intercept between buccal and lingual sides
mod2 <- aov(diazone ~ parazone+side, data=x)
summary(mod2)
#No significant difference in intercept

#Compare the models
#if P non significant, removing the interaction does not significantly affect the fit of the model -- so difference in intercept, but not slope
#anova(mod1, mod2)

#################################################################################################
#LOWER M3
LM3 <- data_all[which(data_all$Tooth == "LM3"),]

x <- as.data.frame(cbind( 
  c(LM3$Specimen, LM3$Specimen),      
  c(LM3$B_Dia_K,LM3$L_Dia_K),
  c(LM3$B_Para_K, LM3$L_Para_K),
  c(rep("buccal", length(LM3[,1])), rep("lingual", length(LM3[,1])))
))

colnames(x) <-c("Specimen", "diazone", "parazone", "side")
x$diazone <- as.numeric(x$diazone)
x$parazone <- as.numeric(x$parazone)


col <- c(rep("black", length(LM3[,1])), rep("red", length(LM3[,1])))

plot(x$diazone ~ x$parazone, pch=16, col=col, xlim=c(0,1), ylim=c(0,2), xlab="Parazone K", ylab="Diazone K", main="Lower M3")
legend("bottomright", legend=c("buccal", "lingual"),
       col=c("black", "red"),  cex=0.8, pch=16)


#test for difference in diazone ~ parazone slope between buccal and lingual sides
mod1 <- aov(diazone ~ parazone*side, data=x)
summary(mod1)
#No significant difference in slopes

#test for difference in diazone ~ parazone intercept between buccal and lingual sides
mod2 <- aov(diazone ~ parazone+side, data=x)
summary(mod2)
#No significant difference in intercept

#Compare the models
#if P non significant, removing the interaction does not significantly affect the fit of the model -- so difference in intercept, but not slope
#anova(mod1, mod2)



