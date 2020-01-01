# Anova analysis and multiple comparisasions / Non parametric tests
# logistic regression

######################################################################
                  ##CONTINUOUS###
######################################################################
## Data
## n number of vectors corresponding to the treatments
A <- c()
B <- c()
C <- c()
D <- c()
E <- c()

finalVector <- c(A, B, C, D, E)
factor1 <- factor(rep(c(), c(length(A), length(B), length(C), length(D), lenght(E))))

df <- data.frame(finalVector, factor1)

options(contrasts = c('contr.treatment', 'contr.poly')) #ver melhor
contrasts(df$factor1)

## modelo anova
model <- aov(finalVector ~ factor1, data = df)
summary(model)

## Extracting values
anovaModel <- anova(model)

DFE <- anova(model)[2, 1]
MSE <- anova(model)[2, 3]

##p-value manually calculation
F0<-anovaModel[1,4]
F0

glTrat<-anovaModel[1,1]
glTrat

1-pf(F0, glTrat, DFE)

qf(0.95, 4, 20)

## Assumptions validation
## Normality of residuals
plot(model) #obtains 4 plots of the residuals

library(car)
qqnorm(model$res)
qqline(model$res)
qqPlot(model$res)
boxplot(model$res)

plot(fitted(model), model$res)
boxplot(model$res ~ factor1)

shapiro.test(model$Re)

## variances

bartlett.test(finalVector, factor1)
library(car)
leveneTest(finalVector, factor1)

## power of the test

power.anova.test()

## nonparametric

krsukal.test(x ~ y, data = )

## Multple comparisations
## tukey

tukeyModel <- TukeyHSD(model, conf.level = )
tukeyModel
plot(tukeyModel, las = 2)

## dunnet

library(multcomp)
library(mvtnorm)
dunnetModel <- glht(model, linfct = mcp(factor1 = 'Dunnet'))
summary(dunnetModel)
plot(dunnetModel)

######################################################################
                    ##BINARY DATA###
######################################################################

## Data
total <- c()
observ <- c()
factor1 <-factor(factor1)
df <- data.frame(factor1, total, observ)


options(contrasts = c('contr.tratment', 'contr.poly'))
contrasts(df$factor1)

# defining reference class

df$factor1<-relevel(factor(df$factor1),ref=)
contrasts(df$factor1)

# add another column of response

response<-cbind(observ, total-observ)

model<-glm(response ~ factor1 ,family=binomial("logit"),data=df)
summary(model)

## glm objects
predict()
fitted()

## OR and ODDS R
b<-model$coefficients
odd0<-exp(b[1])
cat("O odds para o Placebo È:","\n",odd0,"\n")

b<-model$coefficients
odd1<-exp(b[[1]]+b[[2]])
cat("O odds para o Metoprolol È:","\n",odd1,"\n")

OR<-exp(b[[2]])
cat("O odds ratio È:","\n",OR,"\n")

# confidence interval
confint.default(modelo)

b<-summary(model)$coefficients[,1]


# TESTE DE WALD

library(lmtest)
waldtest(model,test="Chisq")

# Correlation matrix

Fisher1<-vcov(model)


