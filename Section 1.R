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
# muhammad kabiri
B=matrix(34:201, nrow=14, ncol=12)
t(B)
if(nrow(B)== ncol(B)){"Matrix (B) is Square"} else{ dim(B)}
ifelse(nrow(B)== ncol(B),det(B), "Matrix (B) is Not Square")
####################################################
## Mehrdad

# step1: matrix
C <- matrix(c(3,6,7,3), nrow=2, ncol=2,byrow =T)
C
# step2: if
if(ncol(C)==nrow(C)){
  det(C)
} else {
  print("Matrix Not Square")
}
####################################################
# FARINA SAEI
D <- matrix(c(10:33), nrow=6, ncol=4,byrow =T)
D

if(ncol(D)==nrow(D)){
  det(D)
} else {
  print("Matrix Not Square") 
}
#FARINA SAEI
D <- matrix(c(11:46), nrow=6, ncol=6,byrow =F)
D

if(ncol(D)==nrow(D)){
  det(D)
} else {
  print("Matrix Not Square")
 #FARINA SAEI
    D <- matrix(c(75:99), nrow=5, ncol=5,byrow =T)
D

if(ncol(D)==nrow(D)){
  det(D)
} else {
  print("Matrix Not Square")
  
} 
  #################################
#Reyverdi
#EXAMPLE1
E <- matrix(c("Father", "Mother", "Sun", "daughter"), nrow = 2, ncol = 2,byrow = T)
E
if(ncol(E)>nrow(E)){dim(E)} else {print("Matrix is Square")}    

#####################################
#haniyeh-mohammadi

#STEP1: MATRIX 
F<- matrix(10:21, nrow = 2, ncol = 6, byrow = T)
F
     
#STEP2: IF
if(ncol(F)==nrow(F)){
  det(F)
} else {
  print("Matrix Not Square")
}        
####################################################
#Ghodsiyeh masihniya
G <- matrix(41:56,nrow=4,ncol=4)
G
if(ncol(G)==nrow(G)){
  det(G)
}else{
  print("matrix not square")
}
ifelse(ncol(G)==nrow(G),det(G),("matrix not square"))
#####################################################    
 ##Mercedeh
#step1:matrix
H <- matrix(C(3,6,7,3), nrow=2, ncol=3,byrow = T)
H

#STEP2:if
if(ncol(H)==nrow(H)){
  det(H)
} else {
  print("Matrix Not Square")
}
############################################
     
#foad
i=matrix1=matrix(c(1:25),ncol=5,nrow=25,byrow=TRUE)
matrix2=t(matrix1)



if(nrow(i)== ncol(i)){"Matrix (i) is Square"} else{ dim(i)}
ifelse(nrow(i)== ncol(i),det(i), "Matrix (i) is Not Square")
     

#####################################################
     
 #mansoor
J=matrix(seq(0, 5, length=20),nrow=4,ncol=5,byrow=T)
J
J2=matrix(seq(1,2,length=10),nrow=5,ncol=2,byrow=T)
J2

ifelse(ncol(J)==nrow(J2),matrix(J%*%J2,nrow=4,ncol=2),"not calculated")        
     
#####################################################

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
#fateme tazik

# step1: matrix
G <- matrix(c(1,3,5,7,9,11,13,15,17), nrow=3, ncol=3,byrow =T)
G

# step2: if
if(ncol(G)==nrow(G)){ 
  det(G)
} else {
  print("Matrix Not Square")
}
####################################################


