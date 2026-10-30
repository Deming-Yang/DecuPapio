library(corrplot)
library(scales)
library(viridisLite)
library(ggplot2)

# Functional vs guiding within the same tooth

fill.10<-viridis_pal()(10)

## Plots in the main text ##


## Figure 6A
# comparison between functional and guiding sides
par(mfrow=c(2,2)) #800X760
# upper K index
plot(papio.UM1$B.KI, papio.UM1$L.KI,
     xlim=c(0,3),ylim=c(0,3),pch=18,col=fill.10[2],cex=1.6,
     main = "Upper", xlab = "Guiding lnKI", ylab = "Functional lnKI")
abline(a=0,b=1)
points(papio.UM2$B.KI, papio.UM2$L.KI,
       pch=18,col=fill.10[5],cex=1.6)
points(papio.UM3$B.KI, papio.UM3$L.KI,
       pch=18,col=fill.10[8],cex=1.6)

# Lower K index
plot(papio.LM1$L.KI, papio.LM1$B.KI,
     xlim=c(0,3),ylim=c(0,3),pch=18,col=fill.10[2],cex=1.6,
     main = "Lower", xlab = "Guiding lnKI", ylab = "Functional lnKI")
abline(a=0,b=1)
points(papio.LM2$L.KI, papio.LM2$B.KI,
       pch=18,col=fill.10[5],cex=1.6)
points(papio.LM3$L.KI, papio.LM3$B.KI,
       pch=18,col=fill.10[8],cex=1.6)
legend("bottomright", legend=c("M1","M2","M3"), pch=18, col=fill.10[c(2,5,8)],cex=1.2)


## Figure 6B
# upper
KI <- c(papio.up.dat$B.KI, papio.up.dat$L.KI)
Cusp <- c(rep("Guiding", 31), rep("Functional", 31))

Upper.KI <- data.frame(KI, Cusp)

boxplot(KI ~ Cusp , data = Upper.KI,
        ylim=c(0,3),col=NULL, main = "Upper", ylab = "lnKI")

for(i in 1: nrow(papio.up.dat)){
        lines(x= c(1:2), y = c(papio.up.dat$L.KI[i],papio.up.dat$B.KI[i]),col="gray60")
}
points (x = rep(1, nrow(papio.up.dat)), y = papio.up.dat$L.KI, col = "blue", pch = 16)
points (x = rep(2, nrow(papio.up.dat)), y = papio.up.dat$B.KI, col = "red", pch = 16)


# lower
KI <- c(papio.low.dat$L.KI, papio.low.dat$B.KI)
Cusp <- c(rep("Guiding", 29), rep("Functional", 29))

Lower.KI <- data.frame(KI, Cusp)

boxplot(KI ~ Cusp, data = Lower.KI,
        ylim=c(0,3),col=NULL, main = "Lower", ylab = "lnKI")

for(i in 1: nrow(papio.low.dat)){
        lines(x= c(1:2), y = c(papio.low.dat$B.KI[i],papio.low.dat$L.KI[i]),col="gray60")
}
points (x = rep(1, nrow(papio.low.dat)), y = papio.low.dat$B.KI, col = "blue", pch = 16)
points (x = rep(2, nrow(papio.low.dat)), y = papio.low.dat$L.KI, col = "red", pch = 16)
legend("topright", legend=c("Functional","Guiding"), pch=16, col=c("blue", "red"))


# no pooling of tooth positions is justified




##Fig 10 upper vs lower 
par(mfrow=c(1,2)) #800X600
# functional side
KI <- c(papio.up.dat$L.KI, papio.low.dat$B.KI)
Side <- c(rep("Upper", nrow(papio.up.dat)), rep("Lower", nrow(papio.low.dat)))

UvL.F.KI <- data.frame(KI, Side)

boxplot(KI ~ Side, data = UvL.F.KI,
        ylim=c(0,3),col=NULL, main = "Functional cusps", ylab = "lnKI")
stripchart(KI ~ Side, vertical = TRUE, data = UvL.F.KI,
           method = "jitter", add = TRUE, pch = 16, col = c("blue","red"))

# Guiding cusps
KI <- c(papio.up.dat$B.KI, papio.low.dat$L.KI)
Side <- c(rep("Upper", nrow(papio.up.dat)), rep("Lower", nrow(papio.low.dat)))

UvL.G.KI <- data.frame(KI, Side)

boxplot(KI ~ Side, data = UvL.G.KI,
        ylim=c(0,3),col=NULL, main = "Guiding cusps", ylab = "lnKI")
stripchart(KI ~ Side, vertical = TRUE, data = UvL.G.KI,
           method = "jitter", add = TRUE, pch = 16, col = c("blue","red"))
legend("topleft", legend=c("Lower","Upper"), pch=16, col=c("blue", "red"))


####################### band width ###
# comparison between functional and guiding sides
# par(mfrow=c(2,2)) #800X760
# # upper K index
# plot(papio.UM1$B.W, papio.UM1$L.W,
#      xlim=c(50,250),ylim=c(50,250),pch=18,col=fill.10[2],cex=1.6,
#      main = "Upper", xlab = "Guiding avg BW", ylab = "Functional avg BW")
# abline(a=0,b=1)
# points(papio.UM2$B.W, papio.UM2$L.W,
#        pch=18,col=fill.10[5],cex=1.6)
# points(papio.UM3$B.W, papio.UM3$L.W,
#        pch=18,col=fill.10[8],cex=1.6)
# 
# # Lower K index
# plot(papio.LM1$L.W, papio.LM1$B.W,
#      xlim=c(50,250),ylim=c(50,250),pch=18,col=fill.10[2],cex=1.6,
#      main = "Lower", xlab = "Guiding avg BW", ylab = "Functional avg BW")
# abline(a=0,b=1)
# points(papio.LM2$L.W, papio.LM2$B.W,
#        pch=18,col=fill.10[5],cex=1.6)
# points(papio.LM3$L.W, papio.LM3$B.W,
#        pch=18,col=fill.10[8],cex=1.6)
# legend("topleft", legend=c("M1","M2","M3"), pch=18, col=fill.10[c(2,5,8)],cex=1.2)



# figure 9
###### upper M1 vs lower M1 ################
par(mfrow=c(2,3)) #800X600
# functional side
F.W <- c(papio.UM1$L.W, papio.LM1$B.W)
Side <- c(rep("Upper", length(papio.UM1$L.W)), rep("Lower", length(papio.LM1$B.W)))

UvL.FW <- data.frame(F.W, Side)

boxplot(F.W ~ Side, data = UvL.FW,
        ylim=c(50,250),col=NULL, main = "M1 Functional cusps", ylab = "Avg band width")
stripchart(F.W ~ Side, vertical = TRUE, data = UvL.FW,
           method = "jitter", add = TRUE, cex = 1.3, pch = 16, col = c("blue","red"))
legend("topleft", legend=c("Lower","Upper"), pch=16, col=c("blue", "red"))


##### upper M2 vs lower M2 ####################
F.W <- c(papio.UM2$L.W, papio.LM2$B.W)
Side <- c(rep("Upper", length(papio.UM2$L.W)), rep("Lower", length(papio.LM2$B.W)))

UvL.FW <- data.frame(F.W, Side)

boxplot(F.W ~ Side, data = UvL.FW,
        ylim=c(50,250),col=NULL, main = "M2 Functional cusps", ylab = "Avg band width")
stripchart(F.W ~ Side, vertical = TRUE, data = UvL.FW,
           method = "jitter", add = TRUE, cex = 1.3, pch = 16, col = c("blue","red"))
# legend("topleft", legend=c("Lower","Upper"), pch=16, col=c("blue", "red"))

#### upper M3 vs lower M3 ####################
F.W <- c(papio.UM3$L.W, papio.LM3$B.W)
Side <- c(rep("Upper", length(papio.UM3$L.W)), rep("Lower", length(papio.LM3$B.W)))

UvL.FW <- data.frame(F.W, Side)

boxplot(F.W ~ Side, data = UvL.FW,
        ylim=c(50,250),col=NULL, main = "M3 Functional cusps", ylab = "Avg band width")
stripchart(F.W ~ Side, vertical = TRUE, data = UvL.FW,
           method = "jitter", add = TRUE, cex = 1.3, pch = 16, col = c("blue","red"))
# legend("topleft", legend=c("Lower","Upper"), pch=16, col=c("blue", "red"))

# Guiding side
##### upper M1 vs lower M1 ####################
G.W <- c(papio.UM1$B.W, papio.LM1$L.W)
Side <- c(rep("Upper", length(papio.UM1$B.W)), rep("Lower", length(papio.LM1$L.W)))

UvL.GW <- data.frame(G.W, Side)

boxplot(G.W ~ Side, data = UvL.GW,
        ylim=c(50,250),col=NULL, main = "M1 Guiding cusps", ylab = "Avg band width")
stripchart(G.W ~ Side, vertical = TRUE, data = UvL.GW,
           method = "jitter", add = TRUE, cex = 1.3, pch = 16, col = c("blue","red"))
##### upper M2 vs lower M2 ####################
# Guiding side
G.W <- c(papio.UM2$B.W, papio.LM2$L.W)
Side <- c(rep("Upper", length(papio.UM2$B.W)), rep("Lower", length(papio.LM2$L.W)))

UvL.GW <- data.frame(G.W, Side)

boxplot(G.W ~ Side, data = UvL.GW,
        ylim=c(50,250),col=NULL, main = "M2 Guiding cusps", ylab = "Avg band width")
stripchart(G.W ~ Side, vertical = TRUE, data = UvL.GW,
           method = "jitter", add = TRUE, cex = 1.3, pch = 16, col = c("blue","red"))

#### upper M3 vs lower M3 ####################
# Guiding side
G.W <- c(papio.UM3$B.W, papio.LM3$L.W)
Side <- c(rep("Upper", length(papio.UM3$B.W)), rep("Lower", length(papio.LM3$L.W)))

UvL.GW <- data.frame(G.W, Side)

boxplot(G.W ~ Side, data = UvL.GW,
        ylim=c(50,250),col=NULL, main = "M3 Guiding cusps", ylab = "Avg band width")
stripchart(G.W ~ Side, vertical = TRUE, data = UvL.GW,
           method = "jitter", add = TRUE, cex = 1.3, pch = 16, col = c("blue","red"))

# change this to HSB density?
# Figure 10 band width comparisons for metameric trends Box plots
par(mfrow=c(2,2)) #800X760

boxplot(B.W ~ Tooth, data = papio.up.dat,
        ylim=c(50,250), col=NULL, main = "Upper Buccal", ylab = "Average Band Width")
stripchart(B.W ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])
legend("topright", legend=c("M1","M2","M3"), pch=18, col=fill.10[c(2,5,8)],cex=1.3)

boxplot(L.W ~ Tooth, data = papio.up.dat,
        ylim=c(50,250), col=NULL, main = "Upper Lingual", ylab = "Average Band Width")
stripchart(L.W ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

boxplot(B.W ~ Tooth, data = papio.low.dat,
        ylim=c(50,250), col=NULL, main = "Lower Buccal", ylab = "Average Band Width")
stripchart(B.W ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

boxplot(L.W ~ Tooth, data = papio.low.dat,
        ylim=c(50,250), col=NULL, main = "Lower Lingual", ylab = "Average Band Width")
stripchart(L.W ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])


# Use HSB packing density

par(mfrow=c(2,2)) #800X760

boxplot(B.HSBD ~ Tooth, data = papio.up.dat,
        ylim=c(4,18), col=NULL, main = "Upper Buccal", ylab = "Est. HSB density")
stripchart(B.HSBD ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])
legend("topright", legend=c("M1","M2","M3"), pch=18, col=fill.10[c(2,5,8)],cex=1.3)

boxplot(L.HSBD ~ Tooth, data = papio.up.dat,
        ylim=c(4,18), col=NULL, main = "Upper Lingual", ylab = "Est. HSB density")
stripchart(L.HSBD ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

boxplot(B.HSBD ~ Tooth, data = papio.low.dat,
        ylim=c(4,18), col=NULL, main = "Lower Buccal", ylab = "Est. HSB density")
stripchart(B.HSBD ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

boxplot(L.HSBD ~ Tooth, data = papio.low.dat,
        ylim=c(4,18), col=NULL, main = "Lower Lingual", ylab = "Est. HSB density")
stripchart(L.HSBD ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

################## other figures #########################

par(mfrow=c(2,2)) #800X760

boxplot(B.KI ~ Tooth, data = papio.up.dat,
        ylim=c(0,3), col=NULL, main = "Upper Buccal", ylab = "lnKI")
stripchart(B.KI ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

boxplot(L.KI ~ Tooth, data = papio.up.dat,
        ylim=c(0,3), col=NULL, main = "Upper Lingual", ylab = "lnKI")
stripchart(L.KI ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

boxplot(B.KI ~ Tooth, data = papio.low.dat,
        ylim=c(0,3), col=NULL, main = "Lower Buccal", ylab = "lnKI")
stripchart(B.KI ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])

boxplot(L.KI ~ Tooth, data = papio.low.dat,
        ylim=c(0,3), col=NULL, main = "Lower Lingual", ylab = "lnKI")
stripchart(L.KI ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 18, cex = 1.5, col=fill.10[c(2,5,8)])
legend("topright", legend=c("M1","M2","M3"), pch=18, col=fill.10[c(2,5,8)],cex=1.3)

# comparison between functional and guiding sides
par(mfrow=c(2,2)) #800X600
# upper Diazone K
plot(papio.UM1$B.Dia.K, papio.UM1$L.Dia.K,
     xlim=c(0.3,1.8),ylim=c(0.3,1.8),pch=18,col=fill.10[2],cex=2,
     main = "Upper Diazone K", xlab = "Guiding", ylab = "Functional")
abline(a=0,b=1)
points(papio.UM2$B.Dia.K, papio.UM2$L.Dia.K,
       pch=18,col=fill.10[5],cex=2)
points(papio.UM3$B.Dia.K, papio.UM3$L.Dia.K,
       pch=18,col=fill.10[8],cex=2)
legend("topleft", legend=c("UM1","UM2","UM3"), pch=18, col=fill.10[c(2,5,8)],cex=1.2)

# upper parazone K
plot(papio.UM1$B.Para.K, papio.UM1$L.Para.K,
     xlim=c(0,0.5),ylim=c(0,0.5),pch=18,col=fill.10[2],cex=2,
     main = "Upper Parazone K", xlab = "Guiding", ylab = "Functional")
abline(a=0,b=1)
points(papio.UM2$B.Para.K, papio.UM2$L.Para.K,
       pch=18,col=fill.10[5],cex=2)
points(papio.UM3$B.Para.K, papio.UM3$L.Para.K,
       pch=18,col=fill.10[8],cex=2)

# Lower Diazone K
plot(papio.LM1$L.Dia.K, papio.LM1$B.Dia.K,
     xlim=c(0.3,1.8),ylim=c(0.3,1.8),pch=18,col=fill.10[2],cex=2,
     main = "Lower Diazone K", xlab = "Guiding", ylab = "Functional")
abline(a=0,b=1)
points(papio.LM2$L.Dia.K, papio.LM2$B.Dia.K,
       pch=18,col=fill.10[5],cex=2)
points(papio.LM3$L.Dia.K, papio.LM3$B.Dia.K,
       pch=18,col=fill.10[8],cex=2)
legend("topleft", legend=c("LM1","LM2","LM3"), pch=18, col=fill.10[c(2,5,8)],cex=1.2)

# Lower parazone K
plot(papio.LM1$L.Para.K, papio.LM1$B.Para.K,
     xlim=c(0,0.5),ylim=c(0,0.5),pch=18,col=fill.10[2],cex=2,
     main = "Lower Parazone K", xlab = "Guiding", ylab = "Functional")
abline(a=0,b=1)
points(papio.LM2$L.Para.K, papio.LM2$B.Para.K,
       pch=18,col=fill.10[5],cex=2)
points(papio.LM3$L.Para.K, papio.LM3$B.Para.K,
       pch=18,col=fill.10[8],cex=2)


# K values
# Functional diazone upper
par(mfrow=c(2,2)) #800X600
boxplot(L.Dia.K ~ Tooth, data = papio.up.dat, ylim=c(30,120),
        main = "Metameric trend: Upper Func Diazone K")
stripchart(L.Dia.K ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Guiding diazone upper
boxplot(B.Dia.K ~ Tooth, data = papio.up.dat,ylim=c(30,120),
        main = "Metameric trend: Upper Guid Diazone K")
stripchart(B.Dia.K ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Functional diazone lower
boxplot(B.Dia.K ~ Tooth, data = papio.low.dat,ylim=c(30,120),
        main = "Metameric trend: Lower Func Diazone K")
stripchart(B.Dia.K ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Guiding diazone lower
boxplot(L.Dia.K ~ Tooth, data = papio.low.dat,ylim=c(30,120),
        main = "Metameric trend: Lower Guid Diazone K")
stripchart(L.Dia.K ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# plots in supplementary
# Diazone and parazone width Fig S1
par(mfrow=c(4,2)) #860X500
boxplot(L.Dia.W ~ Tooth, data = papio.up.dat,ylim=c(20,140), ylab = "Diazone W",
        main = "Metameric trend: Upper Func Diazone W")
stripchart(L.Dia.W ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Guiding diazone upper
boxplot(B.Dia.W ~ Tooth, data = papio.up.dat,ylim=c(20,140), ylab = "Diazone W",
        main = "Metameric trend: Upper Guid Diazone W")
stripchart(B.Dia.W ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Functional diazone lower
boxplot(B.Dia.W ~ Tooth, data = papio.low.dat,ylim=c(20,140), ylab = "Diazone W",
        main = "Metameric trend: Lower Func Diazone W")
stripchart(B.Dia.W ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Guiding diazone lower
boxplot(L.Dia.W ~ Tooth, data = papio.low.dat,ylim=c(20,140), ylab = "Diazone W",
        main = "Metameric trend: Lower Guid Diazone W")
stripchart(L.Dia.W ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# parazone
boxplot(L.Para.W ~ Tooth, data = papio.up.dat,ylim=c(20,140), ylab = "Parazone W",
        main = "Metameric trend: Upper Func Parazone W")
stripchart(L.Para.W ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Guiding diazone upper
boxplot(B.Para.W ~ Tooth, data = papio.up.dat,ylim=c(20,140), ylab = "Parazone W",
        main = "Metameric trend: Upper Guid Parazone W")
stripchart(B.Para.W ~ Tooth, vertical = TRUE, data = papio.up.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Functional diazone lower
boxplot(B.Para.W ~ Tooth, data = papio.low.dat,ylim=c(20,140), ylab = "Parazone W",
        main = "Metameric trend: Lower Func Parazone W")
stripchart(B.Para.W ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)

# Guiding diazone lower
boxplot(L.Para.W ~ Tooth, data = papio.low.dat,ylim=c(20,140), ylab = "Parazone W",
        main = "Metameric trend: Lower Guid Parazone W")
stripchart(L.Para.W ~ Tooth, vertical = TRUE, data = papio.low.dat,
           method = "jitter", add = TRUE, pch = 20, cex = 1.5)


# generate correlation matrices for visualization
papio.up.wo.bw.dat <- papio.up.dat[,c(4,6,8,10,12:15)]

papio.low.wo.bw.dat <- papio.low.dat[,c(4,6,8,10,12:15)]

# isolate KI

# filter out KI, because they are secondary data

# M.upp.KI = cor(papio.up.wo.K)
# M.low.KI = cor(papio.low.wo.K)

M.upp.dat = cor(na.omit(papio.up.wo.bw.dat))
M.low.dat = cor(na.omit(papio.low.wo.bw.dat))

par(mfrow=c(1,1))
# check covariance between K and band widths
corrplot(M.upp.dat, method = 'ellipse', order = 'alphabet', type = 'upper' , 
         addCoef.col = 'black', tl.pos = 'd', cl.pos = 'n' ,
         main = "Upper Molars", mar=c(0,0,4,0), tl.offset = 1)
# no strong correlation in upper


corrplot(M.low.dat, method = 'ellipse', order = 'alphabet', type = 'upper' , 
         addCoef.col = 'black', tl.pos = 'd', cl.pos = 'n' ,
         main = "Lower Molars", mar=c(0,0,4,0), tl.offset = 1)
# a few strong correlations in lower, especially with widths
# no clear covariation patter between K and widths




