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
if(nrow(B)== ncol(B)){
     "Matrix (B) is Square"
}
else{ 
     dim(B)
}

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
     #FARINA SAEI
     D <- matrix(c(8, 10, 12, 14, 16, 18, 20), nrow=7, ncol=1,byrow =F)
D
# step2: if
if(ncol(D)==nrow(D)){
  det(D)
} else {
  print("Matrix Not Square")
}
  #FARINA SAEI
#FOR
for(i in 2:5) 
for (j in 3:6)
{ print(i*j)
  }
#FARINA SAEI
     #FOR
for(i in 2:5) 
  for (j in 3:6)
    for(k in 2)
  { print(i*j)/k
  }   
#FARINA SAEI
# FUNCTION
far= function (a,b,c){
  result= c-(a*b)
  return(result)
}
far(100,50,200 )
#farina saei
     #function
     FS=function(n,m){
  x
  result=(n^3)+log(n*m)
  return(result)
}
 FS(20,30) 
 #
#FARINA SAEI
# FUNCTION
 SD= function(A,B,C){
   result=(sin(A*B)+tan(A*C))/(cos(log(A*B*C)))
   return(result)
 }
 SD(25,45,90) 
     #farina saei
     # for & function
    for(i in 10:40)
 for(j in 15:25)

CA=function(i,j){
result=(i+j)/5
return(result)
}   
CA(20,20)   


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
#Sara Mohammadzade
K = matrix(360:399 , nrow = 8 , ncol = 5)
K
ifelse(nrow(K)==ncol(K) , det(K) , "Matrix Not Square")
sara = for(i in K){
       print(i+i)
}

add = function(a,b){
  (a*b)+b^2
}
add(4,5)

f1 = function(i){
  for(i in 20:50){
    print(i+i)
  }
}

f1(45)
#reg
mymodel = lm(Fertility ~ Agriculture , data = swiss)
plot(Fertility ~ Agriculture , data = swiss, col = "black")
     
setwd("C:/Users/Lenovo/Desktop/r class")

mydata = read.csv("sh.csv" , header = T)


lm1 = lm(rd ~ rop + rcp + w , data = mydata)
summary(lm1)
     
     
######################################################
##shirin
     
# step1: matrix
L <- matrix(c(1:21), nrow=3, ncol=7,byrow =TRUE)
L
     
     
# step2: if
if(ncol(L)==nrow(L)){
  det(C)
} else {
  print("Matrix Not Square")
}     
  

ifelse(ncol(L)==nrow(L),det(AL), "Matrix Not Square")
####################################################
#peran
#step1: matrix
M1 <- matrix(5:29, nrow = 5, ncol = 5, byrow = TRUE)
M2 <- matrix(1:25, nrow = 5, ncol= 5, byrow = FALSE)
     
#step2: if
if(nrow(M1)==ncol(M2)){
  M1*M2
} else {
  print("check dimentions of matrices")
}
     
#step3: for
x <- c(5, 10, 15, 20, 25)
y <- c(1:5)
new <- numeric(5)
for (i in 1:5) {
  new[i] = x[i] + y[i]
}

#step4: function
xy <- function(x,y) {
  xy=x*y
  }
output <- xy(x=51, y=73)
print(output)
####################################################
##maede mohamadi
#example1
N1=matrix(201:220,nrow=10, ncol=2, byrow=T)
N1
if(ncol(N1)==nrow(N1)){
  det(N1)
} else{
  print("Matrix Not Square")
}

#example2
N2=matrix(203:222,nrow=10, ncol=2, byrow=T)
N2
if(dim(N1)>=dim(N2)){
  print("Matrix N1 is greater than N2")
}else{
  print("Matrix N1 is not greater than N2")
}
  
###################################################
## Ahad Choupani Assignments of the 1st session
     ## Creating a matrix
     O=matrix(seq(1,25,3), nrow = 3, ncol = 3, byrow = TRUE,dimnames = list(c("Radif1", "Radif2","Radif3"), c("Sotoon1", "Sotoon2", "Sotoon3")))
     
###################################################
#S.M.Afghahi

#1
P <- matrix(1:28, nrow = 4, ncol = 7)

#2
if(ncol(P)==nrow(P)) { det(P)} else { print("Thats not square")}
###########################

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
######################################################
##################  Bagherpour #######################
######################################################
  #matrix
B <- matrix(20:31 , nrow=4 , ncol=3 , byrow=T)
B
ifelse(ncol(B)==nrow(B) , det(B) , "matrix is not square")

if ( ncol(B)==nrow(B)) {
     det(B)
     }else{
     print(dim(B))
     }
b=c(13 , 17, 19, 31 , 37 , 7, 23 , 5 , 3)
b
N <- ( matrix ( b ,nrow = 3, ncol = 3 , byrow = T ))
print(det(N))
 
######################################################
#FARINA SAEI
#FOR
for(i in 2:5) 
for (j in 3:6)
{ print(i*j)
  }
#FARINA SAEI
     #FOR
for(i in 2:5) 
  for (j in 3:6)
    for(k in 2)
  { print(i*j)/k
  }
################
#class2
#Rey Allahverdi
       #for
for(i in 1:3){print(A<-matrix(1:9, nrow = 3,ncol = 3))}
      #function
z=function(x,y){u=x^0.5+y^0.5
return(u)}
z(1,2)
      #For & Function
W<- function(z) {for(x in 1:10) { c = x^0.5
    print(c)}}
W(1)

####################################################

#### exercise  #### 2nd session ####

# Muhammad Kabiri

###  function #  for  #  if  ###

GDP <- c(170000000000 , 1200000000000 , 1500000000000 , 160000000000 , 1300000000000 , 200000000000)
Population <- c( 20000000 , 35000000 , 50000000 , 70000000 , 100000000 , 80000000)
  
 GDPpC <- function(GDP,Population){
   GDP / Population
 }
 
 for(i in 1:6){
 A[i] <- GDPpC(GDP[i],Population[i])
 
if ( A[i] >= 11000){
  print("More than the global average") 
} else {
  print("Less than the global average")
}
 }
##################################################
##################################################
# Mahsa Bazm
#exercise2

#for
for(i in 1:10) {
  print(i^3)
}

#function
f1=function(x){
  (x^2)+(2*x)+5
}

#for&function
f2=function(x,y){
  x= 1:100
  y=c()
  
  for(i in x){
    if(i%%3==0){
      y=c(y,i)
    }
  }
  print(y)
}
##################################################
##################################################

########## Mostafa_Saberi ##########
####### R_class ############
#### SESSION1&2 #########

#Matrices

M <- rbind(c(3, 2, 5, 1, 6), c(4,8,9,2,0), c(5,3,2,1,2))

colnames(M) <- c("Mon", "Tues", "Wed", "Thur", "Fri")
rownames(M) <- c("S&P", "App1", "Goog")
M
t(M)
dim(M)
NROW(M)
ncol(M)

###### Agar soton Matrices Bozorgtar az radif bashad.

if(ncol(M)>NROW(M)){
  print("HOORA")
} else {
  print("see u later")
}

### Function ###
SS <- mean(M)
mediA <- function(M) {
  SS <- sum(M)/length(M)
}
## now run ##
M
SS
mediA
##FOR & FUNCTION##
SS <- mean(M)
SS2 <- ("SS^2")
mediA <- function(M) {
  SS <- sum(M)/length(M)
 for (SS in 1:10) {
  print(SS*3)
}
  
###############################################
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
################## Reyhaneh Allahverdi-exercise 3
mydata=read.csv("demand.csv",header=T)
View(mydata)

library (lmtest)
library (ggplot2)
library (dplyr)
library (visreg)

df<-dplyr::select_if(mydata, is.numeric)
r<-cor(df, use="complete.obs")
round(r,2)


#ols
demand_lm<-lm(qc~pc+psh+pt+pch+expenditure, data=mydata)
summary(demand_lm)

library (visreg)
visreg(demand_lm, "expenditure", gg=TRUE)
visreg(demand_lm, "pc", gg=TRUE)
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
     
##################################################
# step1: set working directory 
##################################################

setwd("C:/Users/mehrd/Desktop/R class")

##################################################
# step 2: load packages 
##################################################
library(lmtest)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(visreg)
library(quantmod)



##################################################
# step 3: import data  or download data
##################################################

# read csv file:

Mydata=read.csv("Data1.csv",header = T)

Data14=abs(Mydata$D14)
Data14.1=abs(Mydata$D14)

plot(Data14, type="l", lwd=4, col="green")

summary(Data14)


### download data
symbol.vec = c("^GSPC") # get S&P 500
getSymbols(symbol.vec, from ="2017-01-03", to = "2020-01-03")

### extract log-returns of adjusted prices

SP500.return = CalculateReturns(GSPC[, "GSPC.Adjusted", drop=F], method="log")

SP500.ret = SP500.return[-1,]

colnames(SP500.ret) = "SP500.ret"


# rerun plot and density plot

qplot(x = 1:length(SP500.ret) , y = SP500.ret , geom = 'point') +
  geom_point(color = 'blue') + 
  geom_hline(yintercept = mean(SP500.ret) , color = 'red' , size = 1) + 
  labs(x = '' , y = 'SP500.ret')

qplot(SP500.ret , geom = 'density') + 
   geom_vline(xintercept = mean(SP500.ret) , color = 'red' , size = 1) +
  geom_density(fill = 'lightgreen' , alpha = 0.4) + labs(x = '')





###### ###### ###### ###### ###### ###### 
# Linear regression Example
###### ###### ###### ###### ###### ###### 


data(SaratogaHouses, package="mosaicData")

# select numeric variables
df <- dplyr::select_if(SaratogaHouses, is.numeric)

# calulate the correlations
r <- cor(df, use="complete.obs")
round(r,2)



library(ggplot2)
library(ggcorrplot)
ggcorrplot(r)


ggcorrplot(r,
           hc.order = T, 
           type = "lower",
           lab = TRUE)


# OLS regression 

houses_lm <- lm(price ~ lotSize+ age + landValue +
                  livingArea + bedrooms + bathrooms +
                  waterfront , 
                data = SaratogaHouses)

summary(houses_lm)



library(visreg)
visreg(houses_lm, "livingArea", gg = T) 



# conditional plot of price vs. waterfront location
visreg(houses_lm, "waterfront", gg = TRUE) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Relationship between price and location",
       subtitle = "controlling for lot size, age, land value, bedrooms and bathrooms",
       caption = "source: Saratoga Housing Data (2006)",
       y = "Home Price",
       x = "Waterfront")


###### ###### ###### ###### ###### ###### 
#Testing Linear Regression Models
###### ###### ###### ###### ###### ###### 


# perform Breusch-Pagan test
bptest(price ~ lotSize+ age + landValue +
         livingArea + bedrooms + bathrooms +
         waterfront , 
       data = SaratogaHouses)


## perform Durbin-Watson test
#Performs the Durbin-Watson test for autocorrelation of disturbances.
dwtest(price ~ lotSize+ age + landValue +
         livingArea + bedrooms + bathrooms +
         waterfront , 
       data = SaratogaHouses)

## perform Goldfeld-Quandt test
#Goldfeld-Quandt test against heteroskedasticity

gqtest(price ~ lotSize+ age + landValue +
         livingArea + bedrooms + bathrooms +
         waterfront , 
       data = SaratogaHouses)




# H0: Added regressors fit not different than base model
resettest(houses_lm) 

# H0: middle data fit not different than base model

raintest(houses_lm, order.by = ~ age, data = SaratogaHouses) 


# H0: mean of recursive residuals = 0
harvtest(houses_lm, order.by = ~age, data = SaratogaHouses)


###### ###### ###### ###### ###### ###### 
# Threshold regression
###### ###### ###### ###### ###### ###### 


library(astsa)
th=10 # Threshold value


less = (df$age<th)

P=df$price[less]
age=df$age[less ]
rooms=df$rooms[less ]
land=df$landValue[less ]


##Regression for values below the threshold

out1 = lm(P~age+rooms+land)
summary(out1)

##Regression for values above the threshold
greater = (df$age>=th)

P2=df$price[greater]
age2=df$age[greater]
rooms2=df$rooms[greater]
land2=df$landValue[greater]

out2 = lm(P2~age2+rooms2+land2)
summary(out2)

##Residuals
res1 = residuals(out1)
res2 = residuals(out2)
less[less==1]= res1
greater[greater==1] = res2
resid = less + greater
acf2(resid)

##Predicted values

fit1 = predict(out1)
fit2 = predict(out2)
less[less==1]= fit1
greater[greater==1] = fit2
fit = less + greater
plot(df$price, type="l")
lines(fit, col = "red", lty="dashed")


library(tsDyn)
flu.tar4.05=setar(df$price, m=2, thDelay=0, th=200000) #
summary(flu.tar4.05) #this shows the final model above and below .05
plot(flu.tar4.05) #cycles through fit and diagnostic plots 

#####################################

ifbdata=read.csv("ifbindex.csv",header=T)
head(ifbdata)
ifb=(ifbdata$ifb)
x <- (log10(ifb))
R <- diff(log10(ifb))
plot(R, type="l", col=2, lwd=2, main="IFB return")

######################## creating TAR   ##########################
start=-1/20
end=1/20
trim=seq(start, end, length.out = 11)

RSS=matrix(NA,nrow =length(trim), ncol=1 )


for(i in trim){ # hint:  trim the data
  TAR=ifelse(lag(R) >= i, 1, 0)
  Rt=R
  Rt1=lag(R,k=1)
  
  regim1=Rt1*TAR
  regim2=Rt1*(1-TAR)
  
  mod=lm(Rt~regim1)
  mod=lm(Rt~regim2) # hint:replace your model
  RSS[,i]=sum(resid(mod)^2)      
}

min(RSS)
RSS
r=0  
TAR=ifelse(lag(R) >= r, 1, 0)
Rt=R
Rt1=lag(R,k=1)

regim1=Rt1*TAR
regim2=Rt1*(1-TAR)

mod=lm(Rt~regim1+regim2) # hint:replace your model


##################################################
par(mfrow=c(2,1))
plot(ifb,lwd=3)
plot(x, col="blue", lwd=3,lty=5)

library(sm)
par(mfrow=c(1,2))
autopairs(x, lag=1, type="regression")
autopairs(x, lag=40, type="regression")
acf(x)
pacf(x)
par(mfrow=c(1,1))
####################################################
library(tsDyn)
x <- (log10(ifb))
mod.setar <- setar(x, m=2, mL=2, mH=2, thDelay=1)
mod.setar
plot(mod.setar)

mod2=lstar(x, m=2, thDelay=1)
mod2
plot(mod2)
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
##########                                ##########
##########         SECTION 3-2            ##########
##########                                ##########
####################################################
####################################################
####################################################
library(MSwM)
x <- diff(log10(ifb))
mod=lm(x~1)
summary(mod)
m.mswm=msmFit(mod,k=3,p=1,sw=rep(TRUE,3))
summary(m.mswm)
plot(m.mswm)
plotProb(m.mswm)          
####################################################


