# First chapters 
# T test for independent and paired samples / non parametric tests
# Contingency tables
######################################################################
              ##QUANTITATIVE##            
######################################################################
# Data

df <- 
matrix <-
vector <-
  
## Data as vectors
vectorA <-
vectorB <-

## graphical representation
qqline()
qqnorm()
boxplot()

## validation of Assumptions
shapiro.test()
var.test()

## t test
t.test()

## in welsh solutions we calculate k degrees of freedom

## non central T student, have to calculate manually

## power of test
power.t.test()
library(pwr)

diff <-
sd <-
effect <- diff/sd
pwr.t2n.test(n1, n2, d = effect, power)

## wilcoxon
library(coin)

dataVector1 <- c()
dataVector2 <- c()
factor1 <- factor(rep(c('col1', 'col2'), c(length(dataVector1), length(dataVector2))))
df <- data.frame(c(dataVector1, dataVector2), factor1)
colnames(df) <-c()

wilcox.test(col1 ~ col2, data =, exact = T,
            con.int = T)

wilcox.test(dataVector1, dataVector2, exact = T )

## Paired samples

t.test(paired = T)
wilcox.test(ham1,ham2,alternative="greater",paired=TRUE)

wilcox.test(ham1,ham2,paired=TRUE,conf.int=TRUE)

wilcox.exact(ham1,ham2,alternative="greater",paired=TRUE)


######################################################################
              ##QUALITATIVE###
######################################################################

## Data (no need for totals)
mtr <- matrix(, nrow = 2, byrow = T)
colnames(mtr) <- c()
rownames(mtr) <- c()

## proportion tests
## extract cases and totals
x <- c(mtr[1], mtr[4]) #expect vector of cases
n <- c(mtr[3], mtr[6]) #expect vector of totals

prop.test(x, n, correct=F) #continuity correction not needed

## or put directly the matrix
prop.test(mtr)

## fisher test for 2x2
fisher.test(mtr)

## power of test

power.prop.test()

## dimdifs below

## odds, odds ratio and RR

## Data
a <- 
b <-
c <-
d <- 

alfa<-0.05
conf<-(1-0.05)*100

#OR
m<-log((a*d)/(b*c))
OR<-exp(m)
desvio<-sqrt((1/a)+(1/b)+(1/c)+(1/d))

l<-m-qnorm(1-(alfa/2))*desvio
lOR<-exp(l)

L<-m+qnorm(1-(alfa/2))*desvio
LOR<-exp(L)

cat("OR:", OR,"\n")
cat("Estimativa do IC:","\n","(",lOR,",",LOR,")","\n",
    "NÌvel de confianÁa:",conf,"%","\n")

#RR

RR<-(a*(c+d))/(c*(a+b))
r<-log(RR)

v<-((1/a)-(1/(a+b)))+((1/c)-(1/(c+d)))
sterrorr<-sqrt(v)

lRR<-exp(r-qnorm(1-(alfa/2))*sterrorr)
LRR<-exp(r+qnorm(1-(alfa/2))*sterrorr)

cat("RR:", RR,"\n")
cat("Estimativa do IC:","\n","(",lRR,",",LRR,")","\n",
    "NÌvel de confianÁa:",conf,"%","\n")



## paired sample

mtr <- matrix()
mcnemar.test(mtr)

binom.test(mtr[1,2],(mtr[1,2]+mtr[2,1]),p=0.5)

binom.test(mtr[2,1],(mtr[1,2]+mtr[2,1]),p=0.5)



#Dimensao para diferenÁa detect·vel

dimdifs<-function(p1,p2,dif,alfa,pot,r,n1)
{
  q1<-1-p1
  q2<-1-p2
  p0<-(p1+r*p2)/(r+1)
  q0<-1-p0
  B1<-qnorm((1-alfa/2))*sqrt((r+1)*p0*q0)
  B2<-qnorm(pot)*sqrt(r*p1*q1+p2*q2)
  B<-(B1+B2)^2
  A1<-(2*(r+1)*dif)/B
  A2<-4*r*(dif^2)
  A3<-(1+sqrt(1+A1))^2
  n1<-B*A3/A2
  n2<-n1*r
  cat("Dimens„o da populaÁo 1:", n1,"\n","Dimens„o da populaÁo 2:", n2,"\n",
      "DiferenÁa MÌnima Detect·vel:", dif,"\n",
      "NÌvel de signific‚ncia:",alfa,"\n",
      "PotÍncia:",pot,"\n")
}

#p1   - proporÁ„o do grupo1
#p2   - proporÁ„o do grupo1
#dif  - diferenÁa mÌnima detect·vel
#alfa - nivel de signific‚ncia
#pot  - potenncia
#r    - raao entre as dimensıes dos dois grupos(r=n2/n1) 


