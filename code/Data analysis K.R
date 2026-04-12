library(dplyr)
library(car)

se <- function(x) {
  sd(x) / sqrt(length(x))
}

# import papio band width data
papio.all.data <-read.csv("./data/Papio all data.csv")

# calculate log-ratio as lnK index (lnKI)
papio.all.data <- papio.all.data %>% mutate(B.KI = log(B.Dia.K/B.Para.K))
papio.all.data <- papio.all.data %>% mutate(L.KI = log(L.Dia.K/L.Para.K))

papio.up.dat <- papio.all.data %>%
  filter(JAW=="U")

papio.low.dat <- papio.all.data %>%
  filter(JAW=="L")

papio.UM1 <- papio.all.data %>%
  filter(Tooth=="UM1")

papio.UM2 <- papio.all.data %>%
  filter(Tooth=="UM2")

papio.UM3 <- papio.all.data %>%
  filter(Tooth=="UM3")

papio.LM1 <- papio.all.data %>%
  filter(Tooth=="LM1")

papio.LM2 <- papio.all.data %>%
  filter(Tooth=="LM2")

papio.LM3 <- papio.all.data %>%
  filter(Tooth=="LM3")

############ summary statistics of KI ############

# UM1 Buccal
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.KI = round(mean(B.KI), digits = 3),
    sd.KI = round(sd(B.KI), digits = 3),
    se.KI = round(se(B.KI), digits = 3),
    max.KI = round(max(B.KI), digits = 3),
    min.KI = round(min(B.KI), digits = 3)
  )
  
# UM1 Lingual
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.KI = round(mean(L.KI), digits = 3),
    sd.KI = round(sd(L.KI), digits = 3),
    se.KI = round(se(L.KI), digits = 3),
    max.KI = round(max(L.KI), digits = 3),
    min.KI = round(min(L.KI), digits = 3)
  )

# LM1 Buccal
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.KI = round(mean(B.KI), digits = 3),
    sd.KI = round(sd(B.KI), digits = 3),
    se.KI = round(se(B.KI), digits = 3),
    max.KI = round(max(B.KI), digits = 3),
    min.KI = round(min(B.KI), digits = 3)
  )

# LM1 Lingual
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.KI = round(mean(L.KI), digits = 3),
    sd.KI = round(sd(L.KI), digits = 3),
    se.KI = round(se(L.KI), digits = 3),
    max.KI = round(max(L.KI), digits = 3),
    min.KI = round(min(L.KI), digits = 3)
  )


# UM2 Buccal
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.KI = round(mean(B.KI), digits = 3),
    sd.KI = round(sd(B.KI), digits = 3),
    se.KI = round(se(B.KI), digits = 3),
    max.KI = round(max(B.KI), digits = 3),
    min.KI = round(min(B.KI), digits = 3)
  )

# UM2 Lingual
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.KI = round(mean(L.KI), digits = 3),
    sd.KI = round(sd(L.KI), digits = 3),
    se.KI = round(se(L.KI), digits = 3),
    max.KI = round(max(L.KI), digits = 3),
    min.KI = round(min(L.KI), digits = 3)
  )

# LM2 Buccal
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.KI = round(mean(B.KI), digits = 3),
    sd.KI = round(sd(B.KI), digits = 3),
    se.KI = round(se(B.KI), digits = 3),
    max.KI = round(max(B.KI), digits = 3),
    min.KI = round(min(B.KI), digits = 3)
  )

# LM2 Lingual
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.KI = round(mean(L.KI), digits = 3),
    sd.KI = round(sd(L.KI), digits = 3),
    se.KI = round(se(L.KI), digits = 3),
    max.KI = round(max(L.KI), digits = 3),
    min.KI = round(min(L.KI), digits = 3)
  )


# UM3 Buccal
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.KI = round(mean(B.KI), digits = 3),
    sd.KI = round(sd(B.KI), digits = 3),
    se.KI = round(se(B.KI), digits = 3),
    max.KI = round(max(B.KI), digits = 3),
    min.KI = round(min(B.KI), digits = 3)
  )

# UM3 Lingual
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.KI = round(mean(L.KI), digits = 3),
    sd.KI = round(sd(L.KI), digits = 3),
    se.KI = round(se(L.KI), digits = 3),
    max.KI = round(max(L.KI), digits = 3),
    min.KI = round(min(L.KI), digits = 3)
  )

# LM3 Buccal
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.KI = round(mean(B.KI), digits = 3),
    sd.KI = round(sd(B.KI), digits = 3),
    se.KI = round(se(B.KI), digits = 3),
    max.KI = round(max(B.KI), digits = 3),
    min.KI = round(min(B.KI), digits = 3)
  )

# LM3 Lingual
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.KI = round(mean(L.KI), digits = 3),
    sd.KI = round(sd(L.KI), digits = 3),
    se.KI = round(se(L.KI), digits = 3),
    max.KI = round(max(L.KI), digits = 3),
    min.KI = round(min(L.KI), digits = 3)
  )

# buccal and lingual combined
# UM1
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.KI = round(mean(c(B.KI, L.KI)), digits = 3),
    sd.KI = round(sd(c(B.KI, L.KI)), digits = 3),
    se.KI = round(se(c(B.KI, L.KI)), digits = 3),
    max.KI = round(max(c(B.KI, L.KI)), digits = 3),
    min.KI = round(min(c(B.KI, L.KI)), digits = 3)
  )

# UM2
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.KI = round(mean(c(B.KI, L.KI)), digits = 3),
    sd.KI = round(sd(c(B.KI, L.KI)), digits = 3),
    se.KI = round(se(c(B.KI, L.KI)), digits = 3),
    max.KI = round(max(c(B.KI, L.KI)), digits = 3),
    min.KI = round(min(c(B.KI, L.KI)), digits = 3)
  )

# UM3
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.KI = round(mean(c(B.KI, L.KI)), digits = 3),
    sd.KI = round(sd(c(B.KI, L.KI)), digits = 3),
    se.KI = round(se(c(B.KI, L.KI)), digits = 3),
    max.KI = round(max(c(B.KI, L.KI)), digits = 3),
    min.KI = round(min(c(B.KI, L.KI)), digits = 3)
  )

# LM1
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.KI = round(mean(c(B.KI, L.KI)), digits = 3),
    sd.KI = round(sd(c(B.KI, L.KI)), digits = 3),
    se.KI = round(se(c(B.KI, L.KI)), digits = 3),
    max.KI = round(max(c(B.KI, L.KI)), digits = 3),
    min.KI = round(min(c(B.KI, L.KI)), digits = 3)
  )

# LM2
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.KI = round(mean(c(B.KI, L.KI)), digits = 3),
    sd.KI = round(sd(c(B.KI, L.KI)), digits = 3),
    se.KI = round(se(c(B.KI, L.KI)), digits = 3),
    max.KI = round(max(c(B.KI, L.KI)), digits = 3),
    min.KI = round(min(c(B.KI, L.KI)), digits = 3)
  )

# LM3
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.KI = round(mean(c(B.KI, L.KI)), digits = 3),
    sd.KI = round(sd(c(B.KI, L.KI)), digits = 3),
    se.KI = round(se(c(B.KI, L.KI)), digits = 3),
    max.KI = round(max(c(B.KI, L.KI)), digits = 3),
    min.KI = round(min(c(B.KI, L.KI)), digits = 3)
  )

##################### Metameric trends in upper and lower molars #####################

# Functional side in upper: Lingual

# 1. upper functional diazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Dia.K ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.2118 0.8104
#       28
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$L.Dia.K, papio.up.dat$Tooth,
                  p.adjust.method = "BH", pool.sd = FALSE)
#     UM1   UM2  
# UM2 0.076 -    
# UM3 0.260 0.260
# NS: UM1 seems different from UM2 and UM3 

# 2. upper guiding diazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Dia.K ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  1.1071 0.3445
#       28 
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$B.Dia.K, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     UM1   UM2  
# UM2 0.102 -    
# UM3 0.058 0.240
# NS: no clear metameric trend is observed, UM1 differ from UM3

# Functional side in lower: buccal

# 3. lower functional diazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Dia.K ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  1.8783  0.173
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$B.Dia.K, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.75 -   
# LM3 0.75 0.75
# NS: no clear metameric trend is observed 

# 4. lower guiding diazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Dia.K ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
# group  2   0.078 0.9252
#       26 
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$L.Dia.K, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1   LM2  
# LM2 0.071 -    
# LM3 0.122 0.713
# NS: no clear metameric trend, but LM1 is different from LM2 in Lingual diazone K (guiding side)

# K index trends
# functional
leveneTest(L.KI ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.0049 0.9951
#       28
# NS: t-test can be used

pairwise.t.test(papio.up.dat$L.KI, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)

#     UM1  UM2 
# UM2 0.24 -   
# UM3 0.24 0.79
# NS: no clear metameric trend is observed 

# guiding
leveneTest(B.KI ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.7181 0.4964
#       28
# NS, t-test can be used

pairwise.t.test(papio.up.dat$B.KI, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)

#     UM1  UM2 
# UM2 0.33 -   
# UM3 0.72 0.33
# NS: no clear metameric trend is observed 

# lower
# functional
leveneTest(B.KI ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  1.1366 0.3363
#       26 
# NS, t-test can be used

pairwise.t.test(papio.low.dat$B.KI, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.52 -   
# LM3 0.52 0.18
# NS: no clear metameric trend is observed 

# guiding
leveneTest(L.KI ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
# group  2  0.2349 0.7923
#       26 
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$L.KI, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.72 -   
# LM3 0.72 0.78
# NS: no clear metameric trend is observed 

# parazone

# 1. upper functional parazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Para.K ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.5402 0.5886
#       28
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$L.Para.K, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     UM1  UM2 
# UM2 0.86 -   
# UM3 0.86 0.86
# NS: no clear metameric trend is observed 

# 2. upper guiding diazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Para.K ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.3796 0.6876
#       28
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$B.Para.K, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     UM1  UM2 
# UM2 0.19 -   
# UM3 0.55 0.59
# NS: no clear metameric trend is observed 

# 3. lower parazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Para.K ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.6482 0.5312
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$L.Para.K, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.93 -   
# LM3 0.93 0.93
# NS: no clear metameric trend is observed 

# 2. upper guiding diazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Para.K ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.4225 0.6598
#       26 
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$B.Para.K, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.68 -   
# LM3 0.64 0.43
# NS: no clear metameric trend is observed 

##################### functional side vs guiding side #####################
# Diazon upper
t.test(papio.UM1$L.Dia.K, papio.UM1$B.Dia.K, paired=TRUE,conf.level=0.95)
# t = 0.86494, df = 10, p-value = 0.4073

t.test(papio.UM2$L.Dia.K, papio.UM2$B.Dia.K, paired=TRUE,conf.level=0.95)
# t = 4.1475, df = 10, p-value = 0.001988

t.test(papio.UM3$L.Dia.K, papio.UM3$B.Dia.K, paired=TRUE,conf.level=0.95)
# t = 0.24413, df = 8, p-value = 0.8133

# Parazon upper
t.test(papio.UM1$L.Para.K, papio.UM1$B.Para.K, paired=TRUE,conf.level=0.95)
# t = 0.96045, df = 10, p-value = 0.3595

t.test(papio.UM2$L.Para.K, papio.UM2$B.Para.K, paired=TRUE,conf.level=0.95)
# t = -1.7077, df = 10, p-value = 0.1185

t.test(papio.UM3$L.Para.K, papio.UM3$B.Para.K, paired=TRUE,conf.level=0.95)
# t = -0.92764, df = 8, p-value = 0.3807

# upper M1 diazonal K and M3 parazonal K show significant differences. 

# Diazon Lower
t.test(papio.LM1$L.Dia.K, papio.LM1$B.Dia.K, paired=TRUE,conf.level=0.95)
# t = -1.1133, df = 9, p-value = 0.2944

t.test(papio.LM2$L.Dia.K, papio.LM2$B.Dia.K, paired=TRUE,conf.level=0.95)
# t = 0.46916, df = 9, p-value = 0.6501

t.test(papio.LM3$L.Dia.K, papio.LM3$B.Dia.K, paired=TRUE,conf.level=0.95)
# t = 0.76234, df = 8, p-value = 0.4677

# Parazon upper
t.test(papio.LM1$L.Para.K, papio.LM1$B.Para.K, paired=TRUE,conf.level=0.95)
# t = 0.34291, df = 9, p-value = 0.7395

t.test(papio.LM2$L.Para.K, papio.LM2$B.Para.K, paired=TRUE,conf.level=0.95)
# t = 1.7708, df = 9, p-value = 0.1104

t.test(papio.LM3$L.Para.K, papio.LM3$B.Para.K, paired=TRUE,conf.level=0.95)
# t = -0.35904, df = 8, p-value = 0.7289

#KI 

# upper
t.test(papio.UM1$L.KI, papio.UM1$B.KI, paired=TRUE,conf.level=0.95)
# t = -0.77443, df = 10, p-value = 0.4566

t.test(papio.UM2$L.KI, papio.UM2$B.KI, paired=TRUE,conf.level=0.95)
# t = 4.8044, df = 10, p-value = 0.0007189

t.test(papio.UM3$L.KI, papio.UM3$B.KI, paired=TRUE,conf.level=0.95)
# t = 0.51932, df = 8, p-value = 0.6176

# upper M2 KI shows significant difference. 

# Lower
t.test(papio.LM1$L.KI, papio.LM1$B.KI, paired=TRUE,conf.level=0.95)
# t = -0.77926, df = 9, p-value = 0.4558

t.test(papio.LM2$L.KI, papio.LM2$B.KI, paired=TRUE,conf.level=0.95)
# t = -1.632, df = 9, p-value = 0.1371

t.test(papio.LM3$L.KI, papio.LM3$B.KI, paired=TRUE,conf.level=0.95)
# t = 0.97966, df = 8, p-value = 0.3559


# paired t-test for buccal vs lingual results, Upper molars
t.test(papio.up.dat$L.Dia.K, papio.up.dat$B.Dia.K, paired=TRUE,conf.level=0.95)

# data:  papio.up.dat$L.Dia.K and papio.up.dat$B.Dia.K
# t = 2.5618, df = 30, p-value = 0.01568
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  0.03730334 0.33056763
# sample estimates:
# mean difference 
#       0.1839355 

# see table S6. The significance is mostly driven by UM2

t.test(papio.up.dat$L.Para.K, papio.up.dat$B.Para.K, paired=TRUE,conf.level=0.95)

# data:  papio.up.dat$L.Para.K and papio.up.dat$B.Para.K
# t = -0.28041, df = 30, p-value = 0.7811
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -0.07909065  0.05999387
# sample estimates:
# mean difference 
#    -0.009548387 

# paired t-test for lingual vs buccal results, lower molars

t.test(papio.low.dat$B.Dia.K, papio.low.dat$L.Dia.K, paired=TRUE,conf.level=0.95)

# data:  papio.low.dat$B.Dia.K and papio.low.dat$L.Dia.K
# t = 0.072176, df = 28, p-value = 0.943
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -0.08591895  0.09219481
# sample estimates:
# mean difference 
#     0.003137931 

t.test(papio.low.dat$B.Para.K, papio.low.dat$L.Para.K, paired=TRUE,conf.level=0.95)

# data:  papio.low.dat$B.Para.K and papio.low.dat$L.Para.K
# t = -0.82773, df = 28, p-value = 0.4148
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -0.08399223  0.03564740
# sample estimates:
# mean difference 
#     -0.02417241 

######### testing K indeces Functional vs Guiding#########
t.test(papio.up.dat$L.KI, papio.up.dat$B.KI, paired=TRUE,conf.level=0.95)

# t = 1.3916, df = 30, p-value = 0.1743
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -0.07059304  0.37254724
# sample estimates:
# mean difference 
#       0.1509771 

t.test(papio.low.dat$B.KI, papio.low.dat$L.KI, paired=TRUE,conf.level=0.95)

# t = 0.96214, df = 28, p-value = 0.3442
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#  -0.1016251  0.2816491
# sample estimates:
# mean difference 
#      0.09001199 

######### testing K indeces upper vs lower#########
# functional side
t.test(papio.UM1$L.KI, papio.LM1$B.KI, conf.level=0.95)
# t = 0.45125, df = 18.637, p-value = 0.657

t.test(papio.UM2$L.KI, papio.LM2$B.KI, conf.level=0.95)
# t = 1.5011, df = 18.584, p-value = 0.1501

t.test(papio.UM3$L.KI, papio.LM3$B.KI, conf.level=0.95)
# t = 2.8135, df = 14.05, p-value = 0.01377

# upper M3  KI shows significant difference. 

# guiding side
t.test(papio.UM1$B.KI, papio.LM1$L.KI, conf.level=0.95)
# t = 2.162, df = 18.203, p-value = 0.04417

t.test(papio.UM2$B.KI, papio.LM2$L.KI, conf.level=0.95)
# t = -0.01034, df = 18.177, p-value = 0.9919

t.test(papio.UM3$B.KI, papio.LM3$L.KI, conf.level=0.95)
# t = 1.5113, df = 11.866, p-value = 0.1569

# lower M1  KI shows significant difference. 


# Functional side
t.test(papio.up.dat$L.KI, papio.low.dat$B.KI,conf.level=0.95)

# data:  papio.up.dat$L.KI and papio.low.dat$B.KI
# t = 2.5094, df = 57.379, p-value = 0.01494
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.06714626 0.59719550
# sample estimates:
# mean of x mean of y 
#  1.425567  1.093396 

# Guiding side
t.test(papio.up.dat$B.KI, papio.low.dat$L.KI,conf.level=0.95)

# data:  papio.up.dat$B.KI and papio.low.dat$L.KI
# t = 2.1875, df = 54.933, p-value = 0.03298
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.02274068 0.51967087
# sample estimates:
# mean of x mean of y 
#  1.274590  1.003384 

