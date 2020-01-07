library(pwr)

sd <- sqrt(0.52)
diff <- 0.5
effect_size <- diff/sd
pwr.t2n.test(n1 = 30, d = effect_size, power = 0.9 )


power.t.test(n = 20, sd = sqrt(0.52), sig.level = 0.05, power = 0.9)

# power = 0.7

power_0.7_40 <- c(power.t.test(n = 40, sd = 1.5, power = 0.7))
power_0.7_60 <- c(power.t.test(n = 60, sd = 1.5, power = 0.7))

power_0.9_40 <- c(power.t.test(n = 40, sd = 1.5, power = 0.9))
power_0.9_60 <- c(power.t.test(n = 60, sd = 1.5, power = 0.9))

df <- data.frame(c(power_0.7_40$delta, power_0.7_60$delta), c(power_0.9_40$delta, power_0.9_60$delta))

colnames(df) <- c('Power 70%', 'Power 90%') 
df
r1 <- c((df$`Power 70%`[1] - df$`Power 70%`[2]), (df$`Power 90%`[1] - df$`Power 90%`[2]))
df <- rbind(df, r1)
rownames(df) <- c('n = 40', 'n = 60', 'n = 40 - n = 60')
df


## 5.1.6

MGS <- c(120000, 124000, 215000, 90000, 67000, 67000, 126000, 95000, 190000,
         180000, 135000, 399000, 65000)
MNGS <- c(12000, 20000, 112000, 32000, 60000, 40000, 18000)

# Amostras independentes, teste sobre valor médio
# Test T, verificar normalidade dos dados

shapiro.test(MGS)
shapiro.test(MNGS)

# p value < 0.05, reject normality hyphotesis

# Usar teste não paramétrico, wilcox-mann whitney
library(coin)
wilcox.test(MGS, MNGS)

# problems with ties
# trying wilcox_test
# create data frame

dataVector1 <- MGS
dataVector2 <- MNGS

factor1 <- factor(rep(c('MGS', 'MNGS'), c(length(dataVector1), length(dataVector2))))
df <- data.frame(c(dataVector1, dataVector2), factor1)
colnames(df) <- c('Plaquets', 'Treatment')

wilcox_test(df$Plaquets ~ df$Treatment, conf.int = T)

## p value < 0.05 reject H0 there is evidence to suggest that the two treatments produce differente outcomes

## 5.2.1

mtr <- matrix(c(62, 40, 635, 658, 635 + 62, 658 + 40), nrow = 3, byrow = T)
colnames(mtr) <- c('Placebo', 'Metroprolol')
rownames(mtr) <- c('Died', 'Survived', 'total')
x <- c(mtr[1], mtr[4])
n <- c(mtr[3], mtr[6])

prop.test(x, n)

# p value < 0.05, reject H0, there is evidence that the two treatments produce diferent outcomes

## 5.2.2 

power.prop.test(p1 = 0.9, p2 = 0.95, power = 0.9)

# n for each group = 582

## 5.2.2 cont

power.prop.test(n = 300, p1 = 0.9, p2 = 0.95)

# power would be aprox: 0.64


## 5.2.3 ???????????????
#3 we can aproach the two porportion to a normal distribuition with expect value p, variance p(1-p)/n

# n is equal 
pooledVariance <-  0.45 / 0.25

power.t.test(delta = 0.2, sd = pooledVariance, power = 0.9)


# for each group 1704 aprox.

## 5.2

dimdifs(0.45, 0.25, 0.2, 0.05, 0.9, 2)

# n1 should be at least 95 and n2 should be 189




