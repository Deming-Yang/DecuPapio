library(dplyr)
library(car)

se <- function(x) {
  sd(x) / sqrt(length(x))
}

# import papio band width data
papio.all.data <-read.csv("./data/Papio all data.csv")

# calculate lnKI
papio.all.data <- papio.all.data %>% mutate(B.KI = log(B.Dia.K/B.Para.K))
papio.all.data <- papio.all.data %>% mutate(L.KI = log(L.Dia.K/L.Para.K))

# calculate average band width using Diazone w + parazone w
papio.all.data <- papio.all.data %>% mutate(B.W = B.Dia.W + B.Para.W)
papio.all.data <- papio.all.data %>% mutate(L.W = L.Dia.W + L.Para.W)

# the estimate of HSB density should be the inverse of average bandwith, or 
# 1/(Diazone w + Parazone w), measured in mm
# the data for diazone w and parazone w are in microns

papio.all.data <- papio.all.data %>% mutate(B.HSBD = 1/(B.Dia.W/1000 + B.Para.W/1000))
papio.all.data <- papio.all.data %>% mutate(L.HSBD = 1/(L.Dia.W/1000 + L.Para.W/1000))

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

############ summary statistics of BW ############

# Diazone
# UM1 Buccal
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.W = round(mean(B.Dia.W), digits = 3),
    sd.W = round(sd(B.Dia.W), digits = 3),
    se.W = round(se(B.Dia.W), digits = 3),
    max.W = round(max(B.Dia.W), digits = 3),
    min.W = round(min(B.Dia.W), digits = 3)
  )
  
# UM1 Lingual
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.W = round(mean(L.Dia.W), digits = 3),
    sd.W = round(sd(L.Dia.W), digits = 3),
    se.W = round(se(L.Dia.W), digits = 3),
    max.W = round(max(L.Dia.W), digits = 3),
    min.W = round(min(L.Dia.W), digits = 3)
  )

# LM1 Buccal
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.W = round(mean(B.Dia.W), digits = 3),
    sd.W = round(sd(B.Dia.W), digits = 3),
    se.W = round(se(B.Dia.W), digits = 3),
    max.W = round(max(B.Dia.W), digits = 3),
    min.W = round(min(B.Dia.W), digits = 3)
  )

# LM1 Lingual
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.W = round(mean(L.Dia.W), digits = 3),
    sd.W = round(sd(L.Dia.W), digits = 3),
    se.W = round(se(L.Dia.W), digits = 3),
    max.W = round(max(L.Dia.W), digits = 3),
    min.W = round(min(L.Dia.W), digits = 3)
  )


# UM2 Buccal
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.W = round(mean(B.Dia.W), digits = 3),
    sd.W = round(sd(B.Dia.W), digits = 3),
    se.W = round(se(B.Dia.W), digits = 3),
    max.W = round(max(B.Dia.W), digits = 3),
    min.W = round(min(B.Dia.W), digits = 3)
  )

# UM2 Lingual
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.W = round(mean(L.Dia.W), digits = 3),
    sd.W = round(sd(L.Dia.W), digits = 3),
    se.W = round(se(L.Dia.W), digits = 3),
    max.W = round(max(L.Dia.W), digits = 3),
    min.W = round(min(L.Dia.W), digits = 3)
  )

# LM2 Buccal
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.W = round(mean(B.Dia.W), digits = 3),
    sd.W = round(sd(B.Dia.W), digits = 3),
    se.W = round(se(B.Dia.W), digits = 3),
    max.W = round(max(B.Dia.W), digits = 3),
    min.W = round(min(B.Dia.W), digits = 3)
  )

# LM2 Lingual
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.W = round(mean(L.Dia.W), digits = 3),
    sd.W = round(sd(L.Dia.W), digits = 3),
    se.W = round(se(L.Dia.W), digits = 3),
    max.W = round(max(L.Dia.W), digits = 3),
    min.W = round(min(L.Dia.W), digits = 3)
  )


# UM3 Buccal
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.W = round(mean(B.Dia.W), digits = 3),
    sd.W = round(sd(B.Dia.W), digits = 3),
    se.W = round(se(B.Dia.W), digits = 3),
    max.W = round(max(B.Dia.W), digits = 3),
    min.W = round(min(B.Dia.W), digits = 3)
  )

# UM3 Lingual
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.W = round(mean(L.Dia.W), digits = 3),
    sd.W = round(sd(L.Dia.W), digits = 3),
    se.W = round(se(L.Dia.W), digits = 3),
    max.W = round(max(L.Dia.W), digits = 3),
    min.W = round(min(L.Dia.W), digits = 3)
  )

# LM3 Buccal
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.W = round(mean(B.Dia.W), digits = 3),
    sd.W = round(sd(B.Dia.W), digits = 3),
    se.W = round(se(B.Dia.W), digits = 3),
    max.W = round(max(B.Dia.W), digits = 3),
    min.W = round(min(B.Dia.W), digits = 3)
  )

# LM3 Lingual
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.W = round(mean(L.Dia.W), digits = 3),
    sd.W = round(sd(L.Dia.W), digits = 3),
    se.W = round(se(L.Dia.W), digits = 3),
    max.W = round(max(L.Dia.W), digits = 3),
    min.W = round(min(L.Dia.W), digits = 3)
  )

#################
# Parazone
# UM1 Buccal
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.W = round(mean(B.Para.W), digits = 3),
    sd.W = round(sd(B.Para.W), digits = 3),
    se.W = round(se(B.Para.W), digits = 3),
    max.W = round(max(B.Para.W), digits = 3),
    min.W = round(min(B.Para.W), digits = 3)
  )

# UM1 Lingual
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.W = round(mean(L.Para.W), digits = 3),
    sd.W = round(sd(L.Para.W), digits = 3),
    se.W = round(se(L.Para.W), digits = 3),
    max.W = round(max(L.Para.W), digits = 3),
    min.W = round(min(L.Para.W), digits = 3)
  )

# LM1 Buccal
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.W = round(mean(B.Para.W), digits = 3),
    sd.W = round(sd(B.Para.W), digits = 3),
    se.W = round(se(B.Para.W), digits = 3),
    max.W = round(max(B.Para.W), digits = 3),
    min.W = round(min(B.Para.W), digits = 3)
  )

# LM1 Lingual
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.W = round(mean(L.Para.W), digits = 3),
    sd.W = round(sd(L.Para.W), digits = 3),
    se.W = round(se(L.Para.W), digits = 3),
    max.W = round(max(L.Para.W), digits = 3),
    min.W = round(min(L.Para.W), digits = 3)
  )


# UM2 Buccal
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.W = round(mean(B.Para.W), digits = 3),
    sd.W = round(sd(B.Para.W), digits = 3),
    se.W = round(se(B.Para.W), digits = 3),
    max.W = round(max(B.Para.W), digits = 3),
    min.W = round(min(B.Para.W), digits = 3)
  )

# UM2 Lingual
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.W = round(mean(L.Para.W), digits = 3),
    sd.W = round(sd(L.Para.W), digits = 3),
    se.W = round(se(L.Para.W), digits = 3),
    max.W = round(max(L.Para.W), digits = 3),
    min.W = round(min(L.Para.W), digits = 3)
  )

# LM2 Buccal
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.W = round(mean(B.Para.W), digits = 3),
    sd.W = round(sd(B.Para.W), digits = 3),
    se.W = round(se(B.Para.W), digits = 3),
    max.W = round(max(B.Para.W), digits = 3),
    min.W = round(min(B.Para.W), digits = 3)
  )

# LM2 Lingual
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.W = round(mean(L.Para.W), digits = 3),
    sd.W = round(sd(L.Para.W), digits = 3),
    se.W = round(se(L.Para.W), digits = 3),
    max.W = round(max(L.Para.W), digits = 3),
    min.W = round(min(L.Para.W), digits = 3)
  )


# UM3 Buccal
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.W = round(mean(B.Para.W), digits = 3),
    sd.W = round(sd(B.Para.W), digits = 3),
    se.W = round(se(B.Para.W), digits = 3),
    max.W = round(max(B.Para.W), digits = 3),
    min.W = round(min(B.Para.W), digits = 3)
  )

# UM3 Lingual
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.W = round(mean(L.Para.W), digits = 3),
    sd.W = round(sd(L.Para.W), digits = 3),
    se.W = round(se(L.Para.W), digits = 3),
    max.W = round(max(L.Para.W), digits = 3),
    min.W = round(min(L.Para.W), digits = 3)
  )

# LM3 Buccal
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.W = round(mean(B.Para.W), digits = 3),
    sd.W = round(sd(B.Para.W), digits = 3),
    se.W = round(se(B.Para.W), digits = 3),
    max.W = round(max(B.Para.W), digits = 3),
    min.W = round(min(B.Para.W), digits = 3)
  )

# LM3 Lingual
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.W = round(mean(L.Para.W), digits = 3),
    sd.W = round(sd(L.Para.W), digits = 3),
    se.W = round(se(L.Para.W), digits = 3),
    max.W = round(max(L.Para.W), digits = 3),
    min.W = round(min(L.Para.W), digits = 3)
  )

# Average band width, buccal side
# UM1
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.W = round(mean(B.W), digits = 3),
    sd.W = round(sd(B.W), digits = 3),
    se.W = round(se(B.W), digits = 3),
    max.W = round(max(B.W), digits = 3),
    min.W = round(min(B.W), digits = 3)
  )

# UM2
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.W = round(mean(B.W), digits = 3),
    sd.W = round(sd(B.W), digits = 3),
    se.W = round(se(B.W), digits = 3),
    max.W = round(max(B.W), digits = 3),
    min.W = round(min(B.W), digits = 3)
  )

# UM3
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.W = round(mean(B.W), digits = 3),
    sd.W = round(sd(B.W), digits = 3),
    se.W = round(se(B.W), digits = 3),
    max.W = round(max(B.W), digits = 3),
    min.W = round(min(B.W), digits = 3)
  )

# LM1
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.W = round(mean(B.W), digits = 3),
    sd.W = round(sd(B.W), digits = 3),
    se.W = round(se(B.W), digits = 3),
    max.W = round(max(B.W), digits = 3),
    min.W = round(min(B.W), digits = 3)
  )

# LM2
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.W = round(mean(B.W), digits = 3),
    sd.W = round(sd(B.W), digits = 3),
    se.W = round(se(B.W), digits = 3),
    max.W = round(max(B.W), digits = 3),
    min.W = round(min(B.W), digits = 3)
  )

# LM3
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.W = round(mean(B.W), digits = 3),
    sd.W = round(sd(B.W), digits = 3),
    se.W = round(se(B.W), digits = 3),
    max.W = round(max(B.W), digits = 3),
    min.W = round(min(B.W), digits = 3)
  )

# Average band width, lingual side
# UM1
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  summarise(
    mean.W = round(mean(L.W), digits = 3),
    sd.W = round(sd(L.W), digits = 3),
    se.W = round(se(L.W), digits = 3),
    max.W = round(max(L.W), digits = 3),
    min.W = round(min(L.W), digits = 3)
  )

# UM2
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  summarise(
    mean.W = round(mean(L.W), digits = 3),
    sd.W = round(sd(L.W), digits = 3),
    se.W = round(se(L.W), digits = 3),
    max.W = round(max(L.W), digits = 3),
    min.W = round(min(L.W), digits = 3)
  )

# UM3
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  summarise(
    mean.W = round(mean(L.W), digits = 3),
    sd.W = round(sd(L.W), digits = 3),
    se.W = round(se(L.W), digits = 3),
    max.W = round(max(L.W), digits = 3),
    min.W = round(min(L.W), digits = 3)
  )

# LM1
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  summarise(
    mean.W = round(mean(L.W), digits = 3),
    sd.W = round(sd(L.W), digits = 3),
    se.W = round(se(L.W), digits = 3),
    max.W = round(max(L.W), digits = 3),
    min.W = round(min(L.W), digits = 3)
  )

# LM2
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  summarise(
    mean.W = round(mean(L.W), digits = 3),
    sd.W = round(sd(L.W), digits = 3),
    se.W = round(se(L.W), digits = 3),
    max.W = round(max(L.W), digits = 3),
    min.W = round(min(L.W), digits = 3)
  )

# LM3
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  summarise(
    mean.W = round(mean(L.W), digits = 3),
    sd.W = round(sd(L.W), digits = 3),
    se.W = round(se(L.W), digits = 3),
    max.W = round(max(L.W), digits = 3),
    min.W = round(min(L.W), digits = 3)
  )

# Estimated HSB Packing density, buccal side
# UM1
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(B.HSBD), digits = 2),
    sd.W = round(sd(B.HSBD), digits = 2),
    se.W = round(se(B.HSBD), digits = 2),
    max.W = round(max(B.HSBD), digits = 2),
    min.W = round(min(B.HSBD), digits = 2)
  )

# UM2
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(B.HSBD), digits = 2),
    sd.W = round(sd(B.HSBD), digits = 2),
    se.W = round(se(B.HSBD), digits = 2),
    max.W = round(max(B.HSBD), digits = 2),
    min.W = round(min(B.HSBD), digits = 2)
  )

# UM3
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(B.HSBD), digits = 2),
    sd.W = round(sd(B.HSBD), digits = 2),
    se.W = round(se(B.HSBD), digits = 2),
    max.W = round(max(B.HSBD), digits = 2),
    min.W = round(min(B.HSBD), digits = 2)
  )

# LM1
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(B.HSBD), digits = 2),
    sd.W = round(sd(B.HSBD), digits = 2),
    se.W = round(se(B.HSBD), digits = 2),
    max.W = round(max(B.HSBD), digits = 2),
    min.W = round(min(B.HSBD), digits = 2)
  )

# LM2
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(B.HSBD), digits = 2),
    sd.W = round(sd(B.HSBD), digits = 2),
    se.W = round(se(B.HSBD), digits = 2),
    max.W = round(max(B.HSBD), digits = 2),
    min.W = round(min(B.HSBD), digits = 2)
  )

# LM3
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(B.HSBD), digits = 2),
    sd.W = round(sd(B.HSBD), digits = 2),
    se.W = round(se(B.HSBD), digits = 2),
    max.W = round(max(B.HSBD), digits = 2),
    min.W = round(min(B.HSBD), digits = 2)
  )

# Estimated HSB Packing density, lingual side
# UM1
papio.all.data %>%
  filter(Tooth == "UM1") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(L.HSBD), digits = 2),
    sd.W = round(sd(L.HSBD), digits = 2),
    se.W = round(se(L.HSBD), digits = 2),
    max.W = round(max(L.HSBD), digits = 2),
    min.W = round(min(L.HSBD), digits = 2)
  )

# UM2
papio.all.data %>%
  filter(Tooth == "UM2") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(L.HSBD), digits = 2),
    sd.W = round(sd(L.HSBD), digits = 2),
    se.W = round(se(L.HSBD), digits = 2),
    max.W = round(max(L.HSBD), digits = 2),
    min.W = round(min(L.HSBD), digits = 2)
  )

# UM3
papio.all.data %>%
  filter(Tooth == "UM3") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(L.HSBD), digits = 2),
    sd.W = round(sd(L.HSBD), digits = 2),
    se.W = round(se(L.HSBD), digits = 2),
    max.W = round(max(L.HSBD), digits = 2),
    min.W = round(min(L.HSBD), digits = 2)
  )

# LM1
papio.all.data %>%
  filter(Tooth == "LM1") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(L.HSBD), digits = 2),
    sd.W = round(sd(L.HSBD), digits = 2),
    se.W = round(se(L.HSBD), digits = 2),
    max.W = round(max(L.HSBD), digits = 2),
    min.W = round(min(L.HSBD), digits = 2)
  )

# LM2
papio.all.data %>%
  filter(Tooth == "LM2") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(L.HSBD), digits = 2),
    sd.W = round(sd(L.HSBD), digits = 2),
    se.W = round(se(L.HSBD), digits = 2),
    max.W = round(max(L.HSBD), digits = 2),
    min.W = round(min(L.HSBD), digits = 2)
  )

# LM3
papio.all.data %>%
  filter(Tooth == "LM3") %>%
  na.omit() %>%
  summarise(
    mean.W = round(mean(L.HSBD), digits = 2),
    sd.W = round(sd(L.HSBD), digits = 2),
    se.W = round(se(L.HSBD), digits = 2),
    max.W = round(max(L.HSBD), digits = 2),
    min.W = round(min(L.HSBD), digits = 2)
  )

##################### Metameric trends in upper and lower molars #####################

# Functional side in upper: Lingual

# 1. upper functional diazone W
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Dia.W ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.2921 0.7491
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$L.Dia.W, papio.up.dat$Tooth,
                  p.adjust.method = "BH", pool.sd = FALSE)
#     UM1   UM2  
# UM2 0.70 -   
# UM3 0.81 0.70
# NS: no clear metameric trend is observed 

# 2. upper guiding diazone W
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Dia.W ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.2361 0.7913
#       27 
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$B.Dia.W, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     UM1   UM2  
# UM2 0.54 -   
# UM3 0.54 0.54
# NS: no clear metameric trend is observed

# Functional side in lower: buccal

# 3. lower functional diazone W
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Dia.W ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.7732 0.4719
#       26 
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$B.Dia.W, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.0027 -     
# LM3 0.0108 0.4970
# ** LM1 is different from LM2 and LM3, while LM2 and LM3 are not different 

# 4. lower guiding diazone K
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Dia.W ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
# group  2  1.0267 0.3722
#       26 
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$L.Dia.W, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1   LM2  
# LM2 0.0029 -     
# LM3 0.0057 0.5432
# ** LM1 is different from LM2 and LM3, while LM2 and LM3 are not different 

# parazone

# 1. upper functional parazone W
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Para.W ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.9072 0.4161
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$L.Para.W, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     UM1  UM2 
# UM2 0.45 -   
# UM3 0.45 0.97
# NS: no clear metameric trend is observed 

# 2. upper guiding parazone W
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Para.W ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.3277 0.7234
#       27
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$B.Para.W, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     UM1  UM2 
# UM2 0.027 -    
# UM3 0.027 0.977
# ** UM1 is different from UM2 and UM3, while UM2 and UM3 are not different 

# 3. lower parazone W
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.Para.W ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.4291 0.6556
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$L.Para.W, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.028 -    
# LM3 0.040 0.979
# ** LM1 is different from LM2 and LM3, while LM2 and LM3 are not different 

# 2. upper guiding parazone W
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(B.Para.W ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2   0.104 0.9016
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$B.Para.W, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.011 -    
# LM3 0.017 0.710
# ** LM1 is different from LM2 and LM3, while LM2 and LM3 are not different

# Average band width trends
# functional
leveneTest(L.W ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.4079 0.6692
#       26
# NS: t-test can be used

pairwise.t.test(papio.up.dat$L.W, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)

#     UM1  UM2 
# UM2 0.57 -   
# UM3 0.65 0.65
# NS: no clear metameric trend is observed 

# guiding
leveneTest(B.W ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.3928 0.6789
#       27 
# NS, t-test can be used

pairwise.t.test(papio.up.dat$B.W, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)

#     UM1  UM2 
# UM2 0.067 -    
# UM3 0.108 0.605
# NS: no clear metameric trend is observed 

# lower
# functional
leveneTest(B.W ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.1684  0.846
#       26 
# NS, t-test can be used

pairwise.t.test(papio.low.dat$B.W, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.00084 -      
# LM3 0.00527 0.58902
# ** LM1 is different from LM2 and LM3, while LM2 and LM3 are not different

# guiding
leveneTest(L.W ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value  Pr(>F)  
# group  2  0.5688 0.5731
#       26  
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$L.W, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
#     LM1  LM2 
# LM2 0.0056 -     
# LM3 0.0125 0.8094
# ** LM1 is different from LM2 and LM3, while LM2 and LM3 are not different

# Estimated HSB packing density
# 1. upper functional (Lingual)
# Testing metameric trend, Upper molars with assumption on variance

leveneTest(L.HSBD ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  0.8576 0.4358
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$L.HSBD, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)
  
#     UM1  UM2 
# UM2 0.51 -   
# UM3 0.51 0.51
# NS: no clear metameric trend is observed 

summary(lm(L.HSBD ~ Tooth, data = papio.up.dat))
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   6.9588     0.7644   9.104 1.44e-09 ***
# ToothUM2      1.4765     1.0562   1.398    0.174    
# ToothUM3      0.6812     1.1466   0.594    0.558 
# Not significant

# 2. upper guiding (Buccal)
leveneTest(B.HSBD ~ Tooth, data = papio.up.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  1.0149 0.3758
#       27
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.up.dat$B.HSBD, papio.up.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)

#     UM1  UM2 
# UM2 0.082 -    
# UM3 0.196 0.580
# B.HSBD in UM1 & UM2 approaching significance

summary(lm(B.HSBD ~ Tooth, data = papio.up.dat))

# 3. lower functional (Buccal) 
leveneTest(B.HSBD ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  1.9327  0.165
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$B.HSBD, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)

#     LM1    LM2   
# LM2 0.0034 -     
# LM3 0.0096 0.6208
# ***LM1 significantly different from LM2 & LM3 

# 4. upper guiding (Lingual)
leveneTest(L.HSBD ~ Tooth, data = papio.low.dat) #NS, t-test can be used
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  1.1701 0.3261
#       26
# NS, t-test can be used

#pairwise t-test
pairwise.t.test(papio.low.dat$L.HSBD, papio.low.dat$Tooth,
                p.adjust.method = "BH", pool.sd = FALSE)

#     LM1  LM2 
# LM2 0.010 -    
# LM3 0.011 0.962
# ***LM1 significantly different from LM2 & LM3 

##################### functional side vs guiding side #####################
# Diazon upper
t.test(papio.UM1$L.Dia.W, papio.UM1$B.Dia.W, paired=TRUE,conf.level=0.95)
# t = 1.8099, df = 9, p-value = 0.1037

t.test(papio.UM2$L.Dia.W, papio.UM2$B.Dia.W, paired=TRUE,conf.level=0.95)
# t = 1.2476, df = 10, p-value = 0.2406

t.test(papio.UM3$L.Dia.W, papio.UM3$B.Dia.W, paired=TRUE,conf.level=0.95)
# t = 2.1325, df = 7, p-value = 0.07041

# Parazon upper
t.test(papio.UM1$L.Para.W, papio.UM1$B.Para.W, paired=TRUE,conf.level=0.95)
# t = -1.3645, df = 9, p-value = 0.2056

t.test(papio.UM2$L.Para.W, papio.UM2$B.Para.W, paired=TRUE,conf.level=0.95)
# t = 0.068101, df = 10, p-value = 0.947

t.test(papio.UM3$L.Para.W, papio.UM3$B.Para.W, paired=TRUE,conf.level=0.95)
# t = 0.0071038, df = 7, p-value = 0.9945

# NO significant difference. 

# Diazone Lower
t.test(papio.LM1$L.Dia.W, papio.LM1$B.Dia.W, paired=TRUE,conf.level=0.95)
# t = 3.2335, df = 9, p-value = 0.01027*

t.test(papio.LM2$L.Dia.W, papio.LM2$B.Dia.W, paired=TRUE,conf.level=0.95)
# t = 1.2193, df = 9, p-value = 0.2537

t.test(papio.LM3$L.Dia.W, papio.LM3$B.Dia.W, paired=TRUE,conf.level=0.95)
# t = 0.63707, df = 8, p-value = 0.5419

# Parazone upper
t.test(papio.LM1$L.Para.W, papio.LM1$B.Para.W, paired=TRUE,conf.level=0.95)
# t = 0.52087, df = 9, p-value = 0.615

t.test(papio.LM2$L.Para.W, papio.LM2$B.Para.W, paired=TRUE,conf.level=0.95)
# t = 0.97805, df = 9, p-value = 0.3536

t.test(papio.LM3$L.Para.W, papio.LM3$B.Para.W, paired=TRUE,conf.level=0.95)
# t = 0.41899, df = 8, p-value = 0.6862

# LM1 Diazonal width is different between buccal and lingual sides.

# Average width 

# upper
t.test(papio.UM1$L.W, papio.UM1$B.W, paired=TRUE,conf.level=0.95)
# t = 0.6726, df = 9, p-value = 0.5181

t.test(papio.UM2$L.W, papio.UM2$B.W, paired=TRUE,conf.level=0.95)
# t = 0.88387, df = 10, p-value = 0.3975

t.test(papio.UM3$L.W, papio.UM3$B.W, paired=TRUE,conf.level=0.95)
# t = 1.9275, df = 7, p-value = 0.09527

# NO significant difference.

# Lower
t.test(papio.LM1$L.W, papio.LM1$B.W, paired=TRUE,conf.level=0.95)
# t = 1.6097, df = 9, p-value = 0.1419

t.test(papio.LM2$L.W, papio.LM2$B.W, paired=TRUE,conf.level=0.95)
# t = 1.1786, df = 9, p-value = 0.2688

t.test(papio.LM3$L.W, papio.LM3$B.W, paired=TRUE,conf.level=0.95)
# t = 0.53067, df = 8, p-value = 0.6101

# Estimated Packing density 

# upper
t.test(papio.UM1$L.HSBD, papio.UM1$B.HSBD, paired=TRUE,conf.level=0.95)
# t = -0.50286, df = 9, p-value = 0.6271

t.test(papio.UM2$L.HSBD, papio.UM2$B.HSBD, paired=TRUE,conf.level=0.95)
# t = -0.28063, df = 10, p-value = 0.7847

t.test(papio.UM3$L.HSBD, papio.UM3$B.HSBD, paired=TRUE,conf.level=0.95)
# t = -2.0304, df = 7, p-value = 0.08187

# NO significant difference.

# Lower
t.test(papio.LM1$L.HSBD, papio.LM1$B.HSBD, paired=TRUE,conf.level=0.95)
# t = -1.6785, df = 9, p-value = 0.1276

t.test(papio.LM2$L.HSBD, papio.LM2$B.HSBD, paired=TRUE,conf.level=0.95)
# t = -1.1173, df = 9, p-value = 0.2928

t.test(papio.LM3$L.HSBD, papio.LM3$B.HSBD, paired=TRUE,conf.level=0.95)
# t = -0.54427, df = 8, p-value = 0.6011


# paired t-test for buccal vs lingual results, Upper molars
t.test(papio.up.dat$L.Dia.W, papio.up.dat$B.Dia.W, paired=TRUE,conf.level=0.95)
# data:  papio.up.dat$L.Dia.W and papio.up.dat$B.Dia.W
# t = 2.828, df = 28, p-value = 0.008557**

# see table S16. The significance is mostly driven by sample size (df)

t.test(papio.up.dat$L.Para.W, papio.up.dat$B.Para.W, paired=TRUE,conf.level=0.95)
# data:  papio.up.dat$L.Para.W and papio.up.dat$B.Para.W
# t = -0.61892, df = 28, p-value = 0.541


# paired t-test for lingual vs buccal results, lower molars

t.test(papio.low.dat$B.Dia.W, papio.low.dat$L.Dia.W, paired=TRUE,conf.level=0.95)

# data:  papio.low.dat$B.Dia.W and papio.low.dat$L.Dia.W
# t = -2.4766, df = 28, p-value = 0.01957

# see table S16. The significance is mostly driven by sample size and LM1


t.test(papio.low.dat$B.Para.W, papio.low.dat$L.Para.W, paired=TRUE,conf.level=0.95)

# data:  papio.low.dat$B.Para.W and papio.low.dat$L.Para.W
# t = -1.1244, df = 28, p-value = 0.2704 


######### testing average width upper vs lower#########
# functional side
t.test(papio.UM1$L.W, papio.LM1$B.W, conf.level=0.95)
# t = 0.41388, df = 17.936, p-value = 0.6839

t.test(papio.UM2$L.W, papio.LM2$B.W, conf.level=0.95)
# t = 2.8336, df = 17.951, p-value = 0.01103*

t.test(papio.UM3$L.W, papio.LM3$B.W, conf.level=0.95)
# t = 2.4336, df = 14.113, p-value = 0.02882*

# UM2 & UM3 shows significantly higher band widths. 

# guiding side
t.test(papio.UM1$B.W, papio.LM1$L.W, conf.level=0.95)
# t = -1.0927, df = 14.532, p-value = 0.2923

t.test(papio.UM2$B.W, papio.LM2$L.W, conf.level=0.95)
# t = 1.2587, df = 18.896, p-value = 0.2235

t.test(papio.UM3$B.W, papio.LM3$L.W, conf.level=0.95)
# t = 1.1086, df = 13.278, p-value = 0.2873

# No significant difference in the guiding side. 

####### testing estimated HSB packing density ######
# functional side
t.test(papio.UM1$L.HSBD, papio.LM1$B.HSBD, conf.level=0.95)
# t = -0.36031, df = 17.996, p-value = 0.7228

t.test(papio.UM2$L.HSBD, papio.LM2$B.HSBD, conf.level=0.95)
# t = -2.1339, df = 18.917, p-value = 0.04616*

t.test(papio.UM3$L.HSBD, papio.LM3$B.HSBD, conf.level=0.95)
# t = -2.6507, df = 13.678, p-value = 0.0193*

# UM2 & UM3 shows significantly lower packing densities. 

# guiding side
t.test(papio.UM1$B.HSBD, papio.LM1$L.HSBD, conf.level=0.95)
# t = 0.85777, df = 17.673, p-value = 0.4025

t.test(papio.UM2$B.HSBD, papio.LM2$L.HSBD, conf.level=0.95)
# t = -1.2316, df = 15.907, p-value = 0.236

t.test(papio.UM3$B.HSBD, papio.LM3$L.HSBD, conf.level=0.95)
# t = -1.4977, df = 13.577, p-value = 0.1571

# No significant difference in the guiding side. 
