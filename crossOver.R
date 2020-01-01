# Cross over desgin studies analysis

## DATA

subject <- c()
sequence <- c()
period <- c()
trat <- c()
observ <- c()

df <- data.frame(subject, sequence, period, trat, observ)

## graphical analysis

library(nlme)
library(lattice)#default
#trellis.device()#default
#trellis.device(color = FALSE)# no color

df2<-groupedData(observ~period|subject,df,labels=list(x="period", y=""), 
                    units=list(x="", y=" "))

df2$group<-c(rep(c()))

plot(df2, outer=~Group,layout=c(2,1),aspect=1.8,key=FALSE) 

tapply(df$observ,list(df$trat,df$period),mean)

tapply(df$observ,list(df$sequence,df$period),mean)

tapply(df$observ,list(df$trat,df$sequence),mean)

interaction.plot(period,trat,observ,ylab="mean of PEFR") # Seq, periodo*trat e carry-over

interaction.plot(sequence,trat,observ,ylab="mean of PEFR") # PerÌodo e Seq*trat

## Carry over
t1<- df[]
t2<- df[]

# Numerator

num<-mean(t1)-mean(t2)

# Denominator

n1<-length(t1)
n2<-length(t2)
v<-((n1-1)*var(t1)+(n2-1)*var(t2))/(n1+n2-2)

## Statistics

t<-num/sqrt(((n1+n2)/(n1*n2))*v)
t

# p-value

gl<-n1+n2-2
2*min(pt(t,gl),1-pt(t,gl))

# Period
## Data
## (assuming lambda1=lambda2)

## Data
d1<- df[]
d2<- df[]

c1<-d1
c2<--d2

## Numerator

num<-(mean(c1)-mean(c2))/2

## Denominator

n1<-length(d1)
n2<-length(d2)
vc<-((n1-1)*var(d1)+(n2-1)*var(d2))/(n1+n2-2)

## Statistics

dc<-num/sqrt(((n1+n2)/(n1*n2))*(vc/4))
dc

## p-value

gl<-n1+n2-2
2*min(pt(dc,gl),1-pt(dc,gl))


## Treatments
## (assuming lambda1=lambda2)
## Numerator

num<-(mean(d1)-mean(d2))/2

## Denominator

n1<-length(d1)
n2<-length(d2)
vd<-((n1-1)*var(d1)+(n2-1)*var(d2))/(n1+n2-2)

## Statistics

d<-num/sqrt(((n1+n2)/(n1*n2))*(vd/4))
d

## p-value 

gl<-n1+n2-2
2*min(pt(d,gl),1-pt(d,gl))

### Power
## for cross over, the n is the total n
power.t.test()