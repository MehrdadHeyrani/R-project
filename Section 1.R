################### R codes ########################
#                                                  #
#                 R Workshop                       #
#                                                  #
#                Mehrdad Heyrani                   #
#       MehrdadHeyrani@alum.sharif.edu             #
####################################################

setwd("E:/Master/Econometrics/22-R intro") 
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 1              ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################


# R class s1



log2(32) 
sqrt(2)

X <- seq(0, 5, length=6)

plot(sin(seq(0, 2*pi, length=100)), col="darkgreen",
     type="l",lwd=5,lty=1)
####################################################
?? adf.test
####################################################

gdp = c(9402,98147,68471, 70310)

names(gdp) <- c("Iran","China","Japan","Germany")
gdp

gdp[1:1]
gdp[2:3]
gdp["Japan"]
gdp[c("Iran","Germany")]
####################################################
A <- matrix(1:6, nrow=2, ncol=3)
A
t(A)
dim(A)
nrow(A)
ncol(A)

A[1,]
A[,1]
A[1,2]

####################################################
if(ncol(A)==nrow(A)){
  det(A)
} else {
  print("Matrix Not Square")
}

ifelse(ncol(A)==nrow(A),det(A), "Matrix Not Square")
####################################################
x <- c(6:-4)
sqrt(x)  #- gives warning
sqrt(ifelse(x >= 0, x, NA))  # no warning
####################################################
z <- NA
x <- c(1:5)

for(i in 2:5){
  z[i] <- x[i] - x[i-1]
}
z
####################################################
for(i in 1:10) {
  print(i*i)
}


  
}
####################################################
add = function(a,b) { 
  result = a+b*a
  return(result)
}

add(2,3)
####################################################
f1 <- function(x) {
     x + 10
  }

f1(1)
f1(5)




f2 = function(x) {
  x * 10
}

f2(2)

####################################################
x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
h()

###################################################
f <- function(x) {
  if (x < 10) {
    "Mehrdad"
  } else {
    "Heyrani"
  }
  }
    f(5)
    f(12)
####################################################
arc <- function(x)
  2*asin(sqrt(x))

arc(0.5)
x <- c(0,1,2,3,4)
x <- x / 10
arc(x)
plot(arc(x)~x,
     pch=21,cex=2,xlim=c(0,1),ylim=c(0,pi),
      main="The Arcsine Transformation")
lines(c(0,1),c(0,pi),col="red",lwd=2)
####################################################
install.packages("AER")
library(AER)
install.packages("ctv")
library(ctv)
list.of.views <- available.views()
list.of.views
install.views("Econometrics")
pckgs <- c("AER","ctv","ggplot2","dynlm","plm","systemfit",
           "effects","forecast","urca")
####################################################
library(AER)
data(swiss)
dim(swiss)
names(swiss)
summary(swiss$Education)
hist(swiss$Education, col=3)
plot(Fertility ~ Education, data=swiss, col="black")

mymodel = lm(Fertility ~Education, data=swiss)
summary(mymodel)
abline(mymodel, col=2)

mymodel1=lm(Fertility~Education+Agriculture+Examination,data=swiss)
summary(mymodel1)

####################################################
curve(dnorm, from=-5, to=5, col="blue", lwd=3,
      main="Density of the standard normarl distribution") 
text(-5,0.3,expression
     (f(x)==frac(1,sigma ~ sqrt(2*pi))~~e^{-~frac((x-mu)^2,2*sigma^2)} ),                   adj=0)

x <- rnorm(150)
y <- rnorm(50,sd=3)
############################## Random Number Generation
set.seed(10)
rnorm(2)
rnorm(3)
####################################################
curve(dnorm,-3,3)
curve(pnorm,-3,3)
curve(qnorm,-3,3)
curve(rnorm,-3,3)
curve(dlnorm,0,10)
curve(dexp,0,10)
curve(dnorm,-3,3)
curve(dt(x,3), add=TRUE, col="blue")
####################################################
####################################################
############## set working directory    ############
####################################################
####################################################


############## load data    ############

myData = read.table("R_Tutorial_Data.txt",header=TRUE, sep="\t")

myData1 = read.csv("oil.csv",header=TRUE,sep="")


Condition=myData$Condition
Group=myData$Group
Pre1=myData$Pre1
Pre2=myData$Pre2
Pre3=myData$Pre3
Pre4=myData$Pre4
Learning=myData$Learning
X=myData$X
plot(myData)
####################################################
myData$Learning
myData$Learning[myData$Group=="A"]

t.test(myData$Pre2[Group=="A"],myData$Pre2[Group=="B"],paired=FALSE)
t.test(myData$Pre2[Group=="A"], myData$Pre2[Group=="B"], paired=FALSE, var.equal=TRUE)
t.test(myData$Pre4[Group=="A"], myData$Pre3[Group=="A"],paired=TRUE)
boxplot(myData$Pre4[Group=="A"],
        myData$Pre3[Group=="A"],
        col=c("#ffdddd","#ddddff"),
        names=c("Pre4","Pre3"),main="Group A")
####################################################
boxplot(myData$Learning[Group=="B"],
        names="Group B", ylab="Learning")
####################################################
cor.test(myData$Pre1,myData$Learning,method="pearson")
plot(myData$Pre1, myData$Learning)
####################################################
shapiro.test(myData$Learning[Condition=="High"&Group=="A"])
shapiro.test(myData$Learning[Condition=="Low"&Group=="A"])
####################################################
####################################################
myModel <- lm(Learning ~ Pre1 + Pre2 + Pre3 + Pre4)
par(mfrow=c(2,2)) # Set or Query Graphical Parameters 
plot(myModel)
par(mfrow=c(1,1))
summary(myModel)
step(myModel, direction="backward")

####################################################
library(lmtest)
library(AER)

data("USMacroG")
plot(USMacroG[,c("dpi","consumption")], lty=c(8,1), col=c(1,2)  ,  plot.type="single", ylab="")
legend("topleft", legend=c("income","consumption"), lty=c(8,1), col=c(1,2), bty="n")
library(dynlm)
cons_lm1 <- dynlm(consumption ~ dpi + L(dpi), data = USMacroG)
cons_lm2 <- dynlm(consumption ~ dpi + L(consumption), data = USMacroG)
summary(cons_lm1)
summary(cons_lm2)

plot(merge(as.zoo(USMacroG[,"consumption"]), fitted(cons_lm1),
           fitted(cons_lm2), 0, residuals(cons_lm1),
           residuals(cons_lm2)), screens = rep(1:2, c(3, 3)),
     lty = rep(1:3, 2), ylab = c("Fitted values", "Residuals"),
     xlab = "Time", main = "")

####################################################

library(AER) # Loads lmtest too
data("Journals")
journals <- Journals[, c("subs", "price")]
journals$citeprice <- Journals$price/Journals$citations
journals$age <- 2000 - Journals$foundingyear
jour_lm <- lm(log(subs) ~ log(citeprice), data = journals)
####################################################
# H0: Added regressors fit not different than base model
resettest(jour_lm) 
# H0: middle data fit not different than base model
raintest(jour_lm, order.by = ~ age, data = journals) 
# H0: mean of recursive residuals = 0
harvtest(jour_lm, order.by = ~age, data = journals)
####################################################
# H0 for all of these tests is homoskedasticity
bptest(jour_lm) # Koenker (1981) studentized version
bptest(jour_lm, studentize=FALSE) # Classical 1979 textbook version
bptest(jour_lm, ~ log(citeprice) + I(log(citeprice)^2),
       data = journals) # White Test
gqtest(jour_lm, order.by = ~ citeprice, data = journals)

####################################################
data("USMacroG")
library(dynlm)
consump1 <- dynlm(consumption ~ dpi + L(dpi), data=USMacroG)
dwtest(consump1)
Box.test(residuals(consump1), type="Box-Pierce")
Box.test(residuals(consump1), type="Ljung-Box")
bgtest(consump1)
####################################################
#  load quarterly database in R
setwd("E:/Master/Econometrics/22-R intro")
data=read.csv("s1.csv",header=T)
names(data)
date=ts(data[,1], start=1369, frequency=4)
ex=ts(data[,2],  start=1369, frequency=4)
oil=ts(data[,3], start=1369, frequency=4)
gdp=ts(data[,4], start=1369, frequency=4)

plot(gdp,type="l", lwd = 3, lty = 1, col=1)

####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 2-1            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
data=read.csv("oil.csv",header=T)
names(data)
date=ts(data[,1], start=1357, end=1387, frequency=1)
oil=ts(data[,3], start=1357, end=1387, frequency=1)
####################################################
acf(oil,lag.max=20, main="ACF for Oil") #AutoCrolation function
pacf(oil,main='Partial Autocorrelations') # Partial AutoCrolation function

# fitting ARMA(3,1)
armafit=arima(oil,order=c(3,0,0),method="ML") # method :"CSS-ML", "ML", "CSS"
armafit
############### calculate P-Value for arma fit model######################
(1-pnorm(abs(armafit$coef)/sqrt(diag(armafit$var.coef))))*2
library(lmtest)
coeftest(armafit)

################################ arma model Roots ########################
#plot
library(forecast)
library(fArma)
armaRoots(armafit$coef[1:3])
# roots
polyroot(c(1,- armafit$coef[1:3]))
abs(polyroot(c(1,- armafit$coef[1:3])))
############################ Residuals for ARMA model ####################
res <- residuals(armafit)
tsdisplay(res)

############################ forecast ARMA modeling ######################
plot(forecast(armafit, h = 10))
forecast(armafit, h=5)

predict(armafit, n.ahead = 10)

########### standardized Residals and P-Value foe Ljung-Box test statistic
tsdiag(armafit)
Box.test(armafit$residuals,lag=2)
####################################################
############## ARMA Order with Akaike & schwartz criteria Marix ###########
# Loop for ARMA Order
P=5
Q=5
x=oil 
bic.x=matrix(ncol=P, nrow=Q,  byrow = T)
aic.x=matrix(ncol=P, nrow=Q,  byrow = T)
obs=length(x)
#######################
x=oil
n=length(x)
d=matrix(ncol=1, nrow=n-1,  byrow = F)

dx=ts(diff(log(x)))
head(dx)
for(i in 1:n){
  ifelse(dx[,i]>0){
    d[,i]=1
  } else{
    d[,i]=0
      }
}





arma.fit=arima(x, c(0,0,0))
bic.x[1,1]=BIC(arma.fit)
aic.x[1,1]=AIC(arma.fit)

##
for (i in 1:P)
  for (j in 1:Q)
  {
    ii=i-1
    jj=j-1
    arma.fit= arima(x, c(ii,0,jj), method = c("CSS-ML"))
    ET=obs-ii
    bic.x[i,j]=BIC(arma.fit)
    aic.x[i,j]=AIC(arma.fit)
    
  }
##
for (i in 1:1)
  for (j in 2:Q)
  {
    jj=j-1
    arma.fit= arima(x, order=c(0,0,ii), method = c("CSS-ML")) 
    ET=obs-jj
    bic.x[i,j]=BIC(arma.fit)
    aic.x[i,j]=AIC(arma.fit)
  }
##
for (i in 2:P)
  for (j in 2:Q)
  {
    jj=j-1
    ii=i-1
    arma.fit= arima(x, c(ii,0,jj), method = c("CSS-ML")) 
    if (ii>=jj)
      ET=obs-ii
    else if (ii<jj)
      ET=obs-jj 
    bic.x[i,j]=BIC(arma.fit)
    aic.x[i,j]=AIC(arma.fit)
  }

# find min of matrix

for (i in 1:P)
  for (j in 1:Q)
  {
    if (aic.x[i,j]==min(aic.x))
    {
      p.aic=i-1
      q.aic=j-1
    }
  }
p.aic
q.aic

for (h in 1:P)
  for (f in 1:Q)
  {
    if (bic.x[h,f]==min(bic.x))
    {
      p.bic=h-1
      q.bic=f-1
    }
  }


p.bic
q.bic

############################### ARMA Fit ############################
arma.fit= arima(x, c(p.bic,0,q.bic), method = c("CSS-ML")) 
arma.fit
coeftest(arma.fit)

################################ arma model Roots ########################
#plot
armaRoots(arma.fit$coef[1:3])
# roots
polyroot(c(1,- arma.fit$coef[1:3]))
abs(polyroot(c(1,- arma.fit$coef[1:3])))

############################ Residuals for ARMA model ####################
res1 <- residuals(arma.fit)
tsdisplay(res1)

############################ forecast ARMA modeling ######################
plot(forecast(arma.fit, h = 15))
forecast(arma.fit, h=5)

predict(arma.fit, n.ahead = 10)

########### standardized Residals and P-Value foe Ljung-Box test statistic
tsdiag(arma.fit)
Box.test(arma.fit$residuals,lag=2)

####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 2-2            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
### load libraries
library(timeSeries)
library(forecast)
library(zoo)
library("lmtest")
library(fArma)
library("tseries")
library("urca")
library(timeDate)

###################### ARIMA model Identification ######################


cpi <- read.csv("CPI.csv", sep = ";", header=T) 
x <- ts(cpi$CPI, start=c(1351), end=c(1387), frequency=1)

par(mfrow = c(1,1))
plot(x, col="blue",lwd=3, main="IRAN CPI (1351-1387)", xlab="Year", ylab="Index Value")
grid()

par(mfrow = c(1,2)) 
acf(x, lag.max=15, main ="IRAN CPI ACF", ylab="" ) 
grid()
pacf(x, lag.max=15, main ="IRAN CPI PACF", ylab="")
grid()
par(mfrow = c(1,1))


# fitting AR(1)
arimafit.initial <- arima(x, order=c(1,0,0), method="ML")
coeftest(arimafit.initial)

par(mfrow = c(1,1))
abs(polyroot(c(1,- arimafit.initial$coef[1])))
armaRoots(arimafit.initial$coef[1])
res.initial <- residuals(arimafit.initial)
tsdisplay(res.initial)
par(mfrow = c(1,3))
tsdiag(arimafit.initial)
Box.test(arimafit.initial$residuals, lag=5, type = ("Ljung-Box"), fitdf = 1)

######################### ADF test#################################

adf.test(x, alternative = c("stationary"))

pp.test(x, alternative = c("stationary"))

####################### step A for x: (1&2&3&4) ########################
library(urca)

unitroot.A.x <- ur.df(x, type = c("trend"), selectlags = c("BIC"))
# Arguments: 
# type = c("none", "drift", "trend"); "drift": only intercept; "trend": both intercept and trend; "none": no drift & no intercept
# selectlags = c("Fixed", "AIC", "BIC"); Lag selection can be achieved according to the Akaike "AIC" or the Bayes "BIC" information criteria. The maximum number of lags considered is set by lags. The default is to use a "fixed" lag length set by lags.

summary(unitroot.A.x)
par(mfrow = c(2,2))
plot(unitroot.A.x)
par(mfrow = c(1,1))

###################### step B for x: (5&6&7) ###########################

unitroot.B.x <- ur.df(x, type = c("drift"), selectlags = c("BIC"))
summary(unitroot.B.x)
plot(unitroot.B.x)

######################## step C for x: (8&9) ###########################

unitroot.C.x <- ur.df(x, type = c("none"), selectlags = c("BIC"))
summary(unitroot.C.x)
plot(unitroot.C.x)

####################### step A for Dx: (1&2&3&4) #######################

Dx=diff(x)
par(mfrow = c(1,1))
plot(Dx, col="blue",lwd=3, main="IRAN Delta CPI (1351-1387)", xlab="Year", ylab="Delta CPI")
grid()

adf.test(Dx, alternative = "stationary")

unitroot.A.Dx <- ur.df(Dx, type = c("trend"), selectlags = c("BIC"))
summary(unitroot.A.Dx)
par(mfrow = c(2,2))
plot(unitroot.A.Dx)
par(mfrow = c(1,1))

####################### step B for Dx: (5&6&7) #########################

unitroot.B.Dx <- ur.df(Dx, type = c("drift"), selectlags = c("BIC"))
summary(unitroot.B.Dx)
plot(unitroot.B.Dx)

####################### step C for Dx: (8&9) ###########################

unitroot.C.Dx <- ur.df(Dx, type = c("none"), selectlags = c("BIC"))
summary(unitroot.C.Dx)
plot(unitroot.C.Dx)

####################### step A for D2x: (1&2&3&4) ######################

N=length(x)
D2x=rep(0,length=N-2)
for (t in 3:N)
{  
  D2x[t-2] = x[t]- 2*x[t-1] + x[t-2]
}

par(mfrow = c(1,1))
plot.ts(ts(D2x, start=c(1351), end=c(1387), frequency=1), col="blue",lwd=3, main="IRAN Delta 2 CPI (1351-1387)", xlab="Year", ylab="Delta 2 CPI")
grid()

adf.test(D2x, alternative = "stationary")

unitroot.A.D2x <- ur.df(D2x, type = c("trend"), selectlags = c("BIC"))
summary(unitroot.A.D2x)
par(mfrow = c(2,2))
plot(unitroot.A.D2x)

###################### step B for D2x: (5&6&7) #########################

unitroot.B.D2x <- ur.df(D2x, type = c("drift"), selectlags = c("BIC"))
summary(unitroot.B.D2x)
plot(unitroot.B.D2x)

###################### step C for D2x: (8&9) ###########################

unitroot.C.D2x <- ur.df(D2x, type = c("none"), selectlags = c("BIC"))
summary(unitroot.C.D2x)
plot(unitroot.C.D2x)

####################### step A for D3x: (1&2&3&4) ######################

N=length(x)
D3x=rep(0,length=N-3)
for (t in 4:N)
{  
  D3x[t-3] = x[t]- 3*x[t-1] + 3*x[t-2] - x[t-3]
}
par(mfrow = c(1,1))
plot.ts(ts(D3x, start=c(1351), end=c(1387), frequency=1), col="blue",lwd=3, main="IRAN Delta 3 CPI (1351-1387)", xlab="Year", ylab="Delta 3 CPI")
grid()

adf.test(D3x, alternative = "stationary")

unitroot.A.D3x <- ur.df(D3x, type = c("trend"), selectlags = c("BIC"))
summary(unitroot.A.D3x)
par(mfrow = c(2,2))
plot(unitroot.A.D3x)

########### ARIMA Order with Information criteria Matrices #############

# for d = 3 as for our hierarchical method

P3=10 
# maximum estimated order for AR part of the model
Q3=10
# maximum estimated order for MA part of the model

aic.x.3 <- matrix(ncol=P3, nrow=Q3,  byrow = T) 
bic.x.3 <- matrix(ncol=P3, nrow=Q3,  byrow = T)

for (i in 1:P3)
  for (j in 1:Q3)
  {
    arimafit.3 <- arima(x, order=c(i-1,2,j-1), method = c("ML"), optim.control = list(maxit=10000), optim.method = "BFGS")
    aic.x.3[i,j] <- AIC(arimafit.3)
    bic.x.3[i,j] <- BIC(arimafit.3)
  }

aic.min.3 <- min(aic.x.3)
bic.min.3 <- min(bic.x.3)

for (h in 1:P3)
  for (f in 1:Q3)
  {
    if (aic.x.3[h,f]==aic.min.3)
    {
      p.aic.3 <- h-1
      q.aic.3 <- f-1
    }
  }

for (h in 1:P3)
  for (f in 1:Q3)
  {
    if (bic.x.3[h,f]==bic.min.3)
    {
      p.bic.3 <- h-1
      q.bic.3 <- f-1
    }
  }

p.aic.3
q.aic.3
p.bic.3
q.bic.3

################# another model (named: auto model)#####################

auto.arima(x, max.p=10, max.q=10, max.P=0, max.Q=0, ic=c("aic"), test=c("adf"))
#Returns best ARIMA model according to either AIC, AICc or BIC value. The function conducts a search over possible model within the order constraints provided

############# defining optimal p & q for auto model  ###################

# for d = 2 as for our auto model

P2=11 
# maximum estimated order for AR part of the model
Q2=11
# maximum estimated order for MA part of the model

aic.x.2 <- matrix(ncol=P2, nrow=Q2,  byrow = T) 
bic.x.2 <- matrix(ncol=P2, nrow=Q2,  byrow = T)

for (i in 1:P2)
  for (j in 1:Q2)
  {
    arimafit.2 <- arima(x, order=c(i-1,2,j-1), method = c("ML"), optim.control = list(maxit=1000), optim.method = "BFGS", kappa = 1e6)
    aic.x.2[i,j] <- AIC(arimafit.2, k=2)
    bic.x.2[i,j] <- BIC(arimafit.2)
  }

aic.min.2 <- min(aic.x.2)
bic.min.2 <- min(bic.x.2)

for (h in 1:P2)
  for (f in 1:Q2)
  {
    if (aic.x.2[h,f]==aic.min.2)
    {
      p.aic.2 <- h-1
      q.aic.2 <- f-1
    }
  }

for (h in 1:P2)
  for (f in 1:Q2)
  {
    if (bic.x.2[h,f]==bic.min.2)
    {
      p.bic.2 <- h-1
      q.bic.2 <- f-1
    }
  }

p.aic.2
q.aic.2
p.bic.2
q.bic.2

############################ ARIMA modeling ############################

# for ARIMA model: (1,2,0)

arimafit.final.1.2.0 <- arima(x, order=c(1,2,0), method="ML")
coeftest(arimafit.final.1.2.0)
abs(polyroot(c(1,- arimafit.final.1.2.0$coef[1:1])))
par(mfrow = c(1,1))
armaRoots(arimafit.final.1.2.0$coef[1:1])


# for ARIMA model: (0,3,5)

arimafit.final.0.3.5 <- arima(x, order=c(0,3,5), method="ML")
coeftest(arimafit.final.0.3.5)

# for ARIMA model: (0,3,0)

arimafit.final.0.3.0 <- arima(x, order=c(0,3,0), method="ML")

# for ARIMA model: (0,2,2)

arimafit.final.0.2.2 <- arima(x, order=c(0,2,2), method="ML")
coeftest(arimafit.final.0.2.2)

####################### White Noise Residuals? ########################

# for ARIMA model: (1,2,0)

tsdiag(arimafit.final.1.2.0)
Box.test(arimafit.final.1.2.0$residuals, lag=5, type = ("Ljung-Box"), fitdf = 1)
res.final.1.2.0 <- residuals(arimafit.final.1.2.0)
par(mfrow = c(2,2))
tsdisplay(res.final.1.2.0)

# for ARIMA model: (0,3,5)

tsdiag(arimafit.final.0.3.5)
Box.test(arimafit.final.0.3.5$residuals, lag=9, type = ("Ljung-Box"), fitdf = 5)
res.final.0.3.5 <- residuals(arimafit.final.0.3.5)
tsdisplay(res.final.0.3.5)


# for ARIMA model: (0,3,0)

tsdiag(arimafit.final.0.3.0)
Box.test(arimafit.final.0.3.0$residuals, lag=4, type = ("Ljung-Box"), fitdf = 0)
res.final.0.3.0 <- residuals(arimafit.final.0.3.0)
tsdisplay(res.final.0.3.0)

# for ARIMA model: (0,2,2)

tsdiag(arimafit.final.0.2.2)
Box.test(arimafit.final.0.2.2$residuals, lag=6, type = ("Ljung-Box"), fitdf = 2)
res.final.0.2.2 <- residuals(arimafit.final.0.2.2)
par(mfrow = c(2,2))
tsdisplay(res.final.0.2.2)

####################### Accuracy of Estimation ########################

cpi4cast <- read.csv("CPI4cast.csv", sep = ";", header=T) 
y <- ts(cpi4cast$CPI, start=c(1351), end=c(1393), frequency=1)

arimafit.auto <- arima(y[1:37], order=c(1,2,0), method="ML")
arimafit.aic <- arima(y[1:37], order=c(0,3,5), method="ML")
arimafit.bic <- arima(y[1:37], order=c(0,3,0), method="ML")
arimafit.auto.ic <- arima(y[1:37], order=c(0,2,2), method="ML")

armafit.auto.4cast <- forecast(arimafit.auto, h=6)
armafit.aic.4cast <- forecast(arimafit.aic, h=6)
armafit.bic.4cast <- forecast(arimafit.bic, h=6)
armafit.auto.ic.4cast <- forecast(arimafit.auto.ic, h=6)

accuracy(armafit.auto.4cast, y[38:43])
accuracy(armafit.aic.4cast, y[38:43])
accuracy(armafit.bic.4cast, y[38:43])
accuracy(armafit.auto.ic.4cast, y[38:43])
# Returns range of summary measures of the forecast accuracy. If x is provided, the function measures out-of-sample (test set) forecast accuracy based on x-f. If x is not provided, the function only produces in-sample (training set) accuracy measures of the forecasts based on f["x"]-fitted(f). 

################################# Forecast ###########################

par(mfrow = c(2,2))

# for ARIMA model: (1,2,0)

pred.auto <- predict(arimafit.auto, n.ahead = 6)
y.auto <- ts(cpi4cast$CPI[38:43], start=c(1388), end=c(1393), frequency=1)
plot(y.auto, col="blue",lty=1, lwd=3, main="forecast based on ARIMA(1,2,0)")
lines(ts(pred.auto$pred+2*pred.auto$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)
lines(ts(pred.auto$pred-2*pred.auto$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)

# for ARIMA model: (0,3,5)

pred.aic <- predict(arimafit.aic, n.ahead = 6)
y.aic <- ts(cpi4cast$CPI[38:43], start=c(1388), end=c(1393), frequency=1)
plot(y.aic, col="blue",lty=1, lwd=3, main="forecast based on ARIMA(0,3,5)")
lines(ts(pred.aic$pred+2*pred.aic$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)
lines(ts(pred.aic$pred-2*pred.aic$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)

# for ARIMA model: (0,3,0)

pred.bic <- predict(arimafit.bic, n.ahead = 6)
y.bic <- ts(cpi4cast$CPI[38:43], start=c(1388), end=c(1393), frequency=1)
plot(y.bic, col="blue",lty=1, lwd=3, main="forecast based on ARIMA(0,3,0)")
lines(ts(pred.bic$pred+2*pred.bic$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)
lines(ts(pred.bic$pred-2*pred.bic$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)

# for ARIMA model: (0,2,2)

pred.auto.ic <- predict(arimafit.auto.ic, n.ahead = 6)
y.auto.ic <- ts(cpi4cast$CPI[38:43], start=c(1388), end=c(1393), frequency=1)
plot(y.auto.ic, col="blue",lty=1, lwd=3, main="forecast based on ARIMA(0,2,2)")
lines(ts(pred.auto.ic$pred+2*pred.auto.ic$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)
lines(ts(pred.auto.ic$pred-2*pred.auto.ic$se, start=c(1388), end=c(1393), frequency=1),col="red",lty=3, lwd=2)

par(mfrow = c(1,1))

####################################################

za.test <- ur.za(x, model="both", lag=2)
summary(za.test)

pp.test <- ur.pp(x, type="Z-tau", model="trend", lags="short")

summary(pp.test)

####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 2-3            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################

data=read.csv("s1.csv",header=T)
names(data)
date=ts(data[,1], start=1369, end=1391, frequency=4)
ex=ts(data[,2], start=1369, end=1391, frequency=4)
oil1=ts(data[,3], start=1369, end=1391, frequency=4)
gdp=ts(data[,4], start=1369, end=1391, frequency=4)
inf=ts(data[,5], start=1369, end=1391, frequency=4)
cpi=ts(data[,6], start=1369, end=1391, frequency=4)
####################################################
plot(gdp)
acf(gdp,lag.max=20)
pacf(gdp,main='Partial Autocorrelations')
sarima.auto=auto.arima(gdp)
summary(sarima.auto)
tsdisplay(gdp)

sarima.fit <- Arima(gdp, order=c(1,0,0), seasonal=c(0,1,1))
summary(sarima.fit)
coeftest(sarima.fit)
tsdisplay(residuals(sarima.fit))
plot(forecast(sarima.fit, h=12))
####################################################
library(uroot)
hegy.test(gdp, deterministic = c(1,1,1), lag.method = "fixed", maxlag = 1)
####################################################
getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}

getrmse(gdp,h=4,order=c(1,0,0),seasonal=c(0,1,1),lambda=0)
getrmse(gdp,h=4,order=c(1,0,0),seasonal=c(1,1,1),lambda=0)
getrmse(gdp,h=4,order=c(1,0,1),seasonal=c(1,1,1),lambda=0)
####################################################

####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 2-4            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
library("fracdiff")
####################################################
setwd("E:/Master/Econometrics/22-R intro")
data1=read.table("gold.txt",header=T)
head(data1)
gold=ts(data1$gold)
plot(gold,type="l", lwd = 3, lty = 1, pch = 1, col="blue")
acf(gold, main="Autocorrelations",lwd=3)
pacf(gold, main="Partial Autocorrelations",lwd=3)


fdGPH(gold)
fdSperio(gold)

arfima=fracdiff(gold,nar=1,nma=0)
summary(arfima)
plot(predict(arfima, n.ahead = 5), lwd=3, col="dark green")
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 3-1            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
library(tsDyn)
ifbdata=read.csv("ifbindex.csv",header=T)
head(ifbdata)

ifb=ts(ifbdata$ifb)
x <- (log(ifb))
par(mfrow=c(2,1))
plot(ifb,lwd=3)
plot(x, col="blue", lwd=3,lty=5)
par(mfrow=c(1,1))
library(sm)
par(mfrow=c(1,2))
autopairs(x, lag=1, type="regression")
autopairs(x, lag=2, type="regression")
acf(x)
pacf(x)
####################################################

mod.setar <- setar(x, m=2, mL=2, mH=2, thDelay=1,trim=0.05)
summary(mod.setar)
plot(mod.setar)
setarTest(x,m=2)
####################################################
mod <- list()
mod[["linear"]] <- linear(x, m=2)
mod[["setar"]] <- setar(x, m=2, thDelay=1)
mod[["lstar"]] <- lstar(x, m=2, thDelay=1)
mod[["nnetTs"]] <- nnetTs(x, m=2, size=3)
mod[["aar"]] <- aar(x, m=2)
sapply(mod, AIC)
sapply(mod, MAPE)
summary(mod[["setar"]])

####################################################
par(mfrow=c(2,1))
plot(mod[["setar"]], lwd=3)
par(mfrow=c(1,1))
####################################################
mod.test <- list()
x.train <- window(x, end=100)
x.test <- window(x, start=99)
mod.test[["linear"]] <- linear(x.train, m=2)
mod.test[["setar"]] <- setar(x.train, m=2, thDelay=1)
mod.test[["lstar"]] <- lstar(x.train, m=2, thDelay=1, trace=FALSE, control=list(maxit=1e5))
mod.test[["nnet"]]<-nnetTs(x.train,m=2, size=3, control=list(maxit=1e5))
frc.test <- lapply(mod.test, predict, n.ahead=50)
par(mfrow=c(1,1))
plot(x.test,ylim=range(x), lwd=2)
for(i in 1:length(frc.test))
lines(frc.test[[i]], lty=i+1, col=i+1, lwd=3)
legend( "bottomleft",lty=1:(length(frc.test)+1), col=1:(length(frc.test)+1), legend=c("observed",names(frc.test)),lwd=3)
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 3-2            ##########
##########                                ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
library(MSwM)
x <- log10(ifb)
mod=lm(x~1)
summary(mod)
m.mswm=msmFit(mod,k=2,p=1,sw=rep(TRUE,3)) # 3 is k+p-1
summary(m.mswm)
plot(m.mswm)
plotProb(m.mswm)          
####################################################



Ti=ts(1:80, frequency = 4, start = c(1378, 1)) # 2nd Quarter of 1378
Ti
TT=seq(as.Date("2000/1/1"), as.Date("2017/1/1"), "years")
TTT=seq(as.Date("2000/1/1"), as.Date("2017/1/1"), "quarter")
TTTT=seq(as.Date("2000/1/1"), as.Date("2017/1/1"), "month")


#####################################3


library(AER)
library(fGarch)
data(NYSESW)
NYSELOSS <- timeSeries(-1.0 * diff(log(NYSESW)) * 100,
                       char.vec = time(NYSESW))
## Function for ES of t-GARCH
ESgarch <- function(y, p = 0.99){
  gfit <- garchFit(formula = ~garch(1, 1), data = y,
                   cond.dist = "std", trace = FALSE)
  sigma <-  predict(gfit, n.ahead = 1)[3]
  df <- coef(gfit)["shape"]
  ES <- sigma * (dt(qt(p, df), df)/(1 - p)) *
    ((df + (qt(p, df))^2)/(df - 1))
  return(ES)
}
## Date vectors for backtest
from <- time(NYSELOSS)[-c((nrow(NYSELOSS) - 999) : nrow(NYSELOSS))]
to <- time(NYSELOSS)[-c(1:1000)]
NYSEES <- fapply(NYSELOSS, from = from, to = to, FUN = ESgarch)
NYSEESL1 <- lag(NYSEES, k = 1)
res <- na.omit(cbind(NYSELOSS, NYSEESL1))
colnames(res) <- c("NYSELOSS", "ES99")

par(mfrow = c(1, 1), mar = c(1.9, 1.9, 1.9, .5), mgp = c(2, .6, 0))

plot(res[, 2], col = "green", ylim = range(res),
     main = "NYSE: t-GARCH(1,1) ES 99%",
     ylab = "percentages", xlab = "", lwd=2)

points(res[, 1], type = "p", cex = 0.7, pch = 19
       ,col=ifelse(res[, 1]>=res[, 2], "red", "dark gray"))

legend("topleft", legend = c("Loss","Loss>ES", "ES"),
       col = c("dark gray","red", "green"), lty = c(NA,NA, 1)
       ,pch = c(19, 19, NA),lwd=c(1,1,2), bty="n") # bty for text border 




