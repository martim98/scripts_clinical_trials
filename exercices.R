######################################################################
                    ## WORK SHEET 2###
######################################################################
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


######################################################################
                    ##WORK SHEET 3###
######################################################################

## 6.1.1

antes <- c(70,80, 72, 76, 76, 76, 72, 78, 82, 64, 74, 92, 74, 68, 84)
depois <- c(68, 72, 62, 70, 58, 66, 68, 52, 64, 72, 74, 60, 74, 72, 74)

d <- antes - depois
sd <- sd(d)

## sample of differences computed
## check for normality assumption

qqnorm(d)
qqline(d)

shapiro.test(d)

## p value > 0.05 (0.6428), we dont reject H0 we assume normality of d sample
t.test(d)
t.test(antes, depois, paired = TRUE)
## p value = 0.007749, we reject H0 there is considerable evidence that the new drug reduces systolic blood pressure
## IC =  ]2.72, 14.88[

## power a)

power.t.test(n = 15, delta = mean(antes) - mean(depois), sd = sd, type = 'paired')
pwr.t.test(n = 15, d = (mean(antes) - mean(depois)) / sd, type = 'paired') 

# power = 0.822

## power b)
power.t.test( delta = 5, sd = sd, power = 0.9, type = 'paired')

# n = 53

## 6.1.2
df <- read.delim('Ex612.txt')
df$Ind <- NULL
d <- df$Ocasiao1 - df$Ocasiao2
# check for normality
qqnorm(d)
qqline(d)
shapiro.test(d)
sd <- sd(d)
# p value > 0.05 we cannot reject normality assumption

t.test(d)

# p value = 0.01618, for alpha >0.01618 we reject H0 and conclude there are differences
# IC ]0.10, 0.76[ we conclude at 95% confidence that the treatment reduced the score for drepression

## 6.2.1
## paired sample, qualitative response, mcNemar

mtr <- matrix(c(11, 37, 20, 28), nrow = 2, byrow = T)
mcnemar.test(mtr)

## p value = 0.03407, we reject H0, conclude that there is evidence to assume that the
## treatments are different

power.prop.test(p1 = 0.5, p2 = 0.20, power = 0.8)
# n should be at leat 39

######################################################################
                    ## WORK SHEET 4##
######################################################################

# 7.1.1 Analysis of variance

#data and df formulation

A <- c(7, 7, 15, 11, 9)
B <- c(12, 17, 12, 18, 18)
C <- c(14, 18, 18, 19, 19)
D <- c(19, 25, 22, 19, 23)
E <- c(7, 10, 11, 15, 11)

finalVector <- c(A, B, C, D, E)
factor1 <- factor(rep(c('A', 'B', 'C', 'D', 'E'), c(length(A), length(B), length(C), length(D), length(E))))
df <- data.frame(finalVector, factor1)
df
model <- aov(finalVector ~ factor1, data = df)
summary(model)

# p value < 0.05 
# Assumption validation
# normality 
library(car)
qqnorm(model$res)
qqline(model$res)
qqPlot(model$res)
boxplot(model$res)

plot(fitted(model), model$res)
boxplot(model$res ~ factor1)

shapiro.test(model$res)

# we do not reject normality with shapiro test

# variances
bartlett.test(finalVector, factor1)
library(car)
leveneTest(finalVector, factor1)
# p value of both test > 0.05, variances assumed

## post Hoc comparisations,
## asumptions validated
## tukey multiple comparisations

tukeyModel <- TukeyHSD(model, conf.level = 0.95)
tukeyModel
plot(tukeyModel)




func <- function(A,B,C,D,E){
  a <- t.test(A, B)$p.value
  b <- t.test(A, C)$p.value
  c <- t.test(A, D)$p.value
  d <- t.test(A, E)$p.value
  
  data <- data.frame(c('A-B', 'A-C', 'A-D', 'A-E'),
                     p.adjust(c(a,b,c,d), method = 'bonferroni'))
  colnames(data) <- c('t.test', 'p value Bonferroni' )
  return(data)
}


