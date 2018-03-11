# Note you need to attach the air data (as in Lab 2)
library(splines)
source('pspline2Dfitter.R')
Data = read.table('air.txt', header = T)
head(Data)
attach(Data)

psp2dG(cbind(wind,temperature,ozone),
Pars= rbind(c(1,21,10, 3,1,2),c(55,98,10,3,1,2)), 
x.lab="Wind", y.lab="Temp", z.lab="Ozone",se=F)

# Input:
#   Data: 3 columns, giving x, y, 
#   Pars: 2 rows with P-spline parameters: [min max nseg deg lambda pdeg]
#   Note: tuning parameters are 5th entry


#Note you need to attach(kyphosis) in session [a built in R dataset]
load('Kyphosis.Rdata')
attach(kyphosis)

KKyphosis=as.numeric(Kyphosis)-1 # creates a 0/1 response

psp2dG(Data=cbind( Start, Age, KKyphosis),
Pars= rbind( c(0,19,10,3,.1,2),c(0,207,10, 3,.1,2)), 
x.lab='Start', y.lab='Age', z.lab='Kyphosis',family='binomial', se=F)

# Note Input:
#   Data: 3 columns, giving x, y, z
#   Pars: 2 rows with P-spline parameters: [min max nseg deg lambda pdeg]
#   Thus tuning parameters are 5th entry.