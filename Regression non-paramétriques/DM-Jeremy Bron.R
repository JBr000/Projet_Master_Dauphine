setwd("C:/Users/jb/Google Drive/BIG DATA DAUPHINE/Regression non paramétrique")

library("KernSmooth")
library(tidyverse)
library(sm)
library(nortest)
library(goftest)
library(tseries)
install.packages("tseries")


rmarkdown::render("DM non param.Rmd" , output_file = "DMJB.html", quiet = T, encoding ="Windows-1252" )



d = read.csv("DataReg.csv", sep = ",", stringsAsFactors = F)
d = d[-1]


#Q 1.1
# bandwidth = h..

summary(d$X)

plot(density(d$X),xlim = c(-.5,1))
lines(res1$x,res1$y, col ="red")
lines(res2$x,res2$y, col ="blue")
lines(res3$x,res3$y, col ="green")

lines(res4$x,res4$y, col = "5")
lines(res5$x,res5$y)
lines(res6$x,res6$y)

# test des différents h
res1=bkde(d$X, kernel = "normal",canonical=FALSE, bandwidth=.1,gridsize = 500, truncate = TRUE)
plot(res1$x,res1$y,type="l")
lines(res2$x,res2$y)

res2=bkde(d$X, kernel = "normal",canonical=FALSE, bandwidth=.05,gridsize = 500, truncate = TRUE)
plot(res2$x,res2$y,type="l")

res3=bkde(d$X, kernel = "normal",canonical=FALSE, bandwidth=.01,gridsize = 500, truncate = TRUE)
plot(res3$x,res3$y,type="l")

res4=bkde(d$X, kernel = "normal",canonical=FALSE, bandwidth=.005,gridsize = 500, truncate = TRUE)

res5=bkde(d$X, kernel = "normal",canonical=FALSE, bandwidth=.09,gridsize = 500, truncate = TRUE)
res6=bkde(d$X, kernel = "normal",canonical=FALSE, bandwidth=.01,gridsize = 500, truncate = TRUE)


#plot de l'histgramme de X
ggplot(data = d, aes(X)) + 
  geom_histogram(
    fill = I("blue"),
    col = I("red"),
    bins = 50
  ) +
  labs(title="Histogramme de X") +
  labs(x="X", y="Count") +
  theme(plot.title = element_text( size=13, face="bold"),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))

?density


# 1.2

#fonction du cours

CVbwt=function(grid,X,Y){
  JJ=function(h){
    fhati=Vectorize(function(i) ksmooth(X[-i],Y[-i],kernel="normal",bandwidth=h,x.points=X[i])$y)
    Forecast=fhati(1:length(X))
    #return(integrate(function(x) fhat(x)^2,-Inf,Inf)$value-2*mean(FF))
    return(mean((Y-Forecast)**2))     
    print(paste0("nombre NaN Forecast : ", length(-which(is.na(Forecast)))))  
  }
  vy=Vectorize(JJ)(grid)
  
  return(vy)
  
}

# recherche du h optimal
grid=seq(0.01,0.05,length=5)
h_grid=CVbwt(grid,d$X,d$Y)
plot(grid,h_grid,type="l")
hcv(d$X, d$Y)
h.select(d$X)


#1.3
reso=bkde(d$X, kernel = "normal",canonical=FALSE, bandwidth=0.02994713,gridsize = 500, truncate = TRUE)
#test de QQplots
qqplot(reso$x,  reso$y)
qqplot( d$X, reso$x)
qqplot( reso$x, runif(500))
qqplot(d$X, runif(10000))
reso$y

# 2 - Reconstruction de r(x)

# 2.1
#dans le markdown




#2.2
demoKSR(d$X,d$Y)

resb1 = bkde(d$X,d$Y, kernel = "normal",canonical=FALSE, bandwidth=.02,gridsize = 500, truncate = TRUE)

plot(resb1$x,resb1$y,type="l")

resQ2.a = ksmooth(d$X,d$Y,bandwidth=0.5,x.points=d$X)
resQ2.b = ksmooth(d$X,d$Y,bandwidth=0.1,x.points=d$X)
resQ2.c = ksmooth(d$X,d$Y,bandwidth=0.01,x.points=d$X)
resQ2.d = ksmooth(d$X,d$Y,bandwidth=0.001,x.points=d$X)
resQ2.1 = ksmooth(d$X,d$Y,bandwidth=0.1,x.points=d$X)

plot(d$X,d$Y, col=rgb(0,0,0,alpha=0.3) , pch = 20)
lines(res2.a$x,res2.a$y, col ="red")
lines(res2.b$x,res2.b$y, col ="blue")
lines(res2.c$x,res2.c$y, col ="green")
lines(res2.d$x,res2.d$y, col =5)

h.select(d$X,d$Y, method = "cv")
?h.select


# focntion du cours
h_CVloc=function(X,Y,N_block){
  #N_block multiple de n
  n=length(X)
  h_loc=rep(-1,N_block)
  if((n/N_block)%%1!=0){ print("error n should be a multiple of N_block")}
  X_sort=sort(X)
  Y_sort=Y[order(X)]
  for(i in 1:N_block){
    h_loc[i]=dpill(X[((i-1)*(n/N_block)+1):(i*(n/N_block))],Y[((i-1)*(n/N_block)+1):(i*(n/N_block))])
  }
  h_locG=rep(h_loc,each=400/N_block)
  return(list("h_loc"=h_loc,"h_locG"=h_locG))
}


#estimation sur les blocks
hlock = h_CVloc(d$X,d$Y,5)
block = 5
resQ23b =list()
X_sort=sort(d$X)
Y_sort=d$Y[order(d$X)]
for (i in 0:(block-1)) {
  n = NROW(d)/block
  resQ23b[[i+1]] = ksmooth(X_sort[(1+n*i):(n*(1+i))],Y_sort[(1+n*i):(n*(1+i))],bandwidth=hlock$h_locG[i+1],x.points=d$X[(1+n*i):(n*(1+i))])
}


# attention supression des valeurs au extremes la ou les estimations se chevauchent

max(X_sort[1:2000])
min(X_sort[1:2000])

as.data.frame(resQ23b[[1]]) %>% filter(x <max(X_sort[1:2000]))

plot(d, col=rgb(0,0,0,alpha=0.3) , pch = 20)

for (i in 0:(block-1)) {

  resfilter = as.data.frame(resQ23b[[i+1]]) %>% filter(x <= max(X_sort[(1+n*i):(n*(1+i))])) %>% filter(x >= min(X_sort[(1+n*i):(n*(1+i))]))
  lines(resfilter$x,resfilter$y, col =i+2)
  
}


#3



U_n = function(X,Y,N){
    sum((Y[2:(N+1)]-Y[1:N])**2)/(2*(N-1))
  }

V_n = function(X,Y,N){
  X_sort=sort(X)
  Y_sort=Y[order(X)]
  sum((Y_sort[2:(N+1)]-Y_sort[1:N])**2)/(2*(N-1))
}

U_n(d$X,d$Y,9999)
V_n(d$X,d$Y,9999)

ys = d$Y[order(d$X)]
var(d$Y)
var(ys)


#3.3

hlock2 = h_CVloc(d$X,d$Y,2)
resQ33b = ksmooth(X_sort[5001:10000],Y_sort[5001:10000],bandwidth=hlock2$h_loc[2],x.points=X_sort[5001:10000])

Ytilde = Y_sort[5001:10000] - resQ33b$y

plot(Ytilde)

plot(rnorm(5000))


shapiro.test()


?cvm.test









---------------------
l
lines(resQ2.3$x,resQ2.3$y, col ="green")
resQ23b.1 = ksmooth(d$X,d$Y,bandwidth=0.5,x.points=d$X)
plot(sort(d$X),d$Y[order(d$X)])

h_CVopt=h_CVloc(d$X,d$Y,10)
h_CVopt$h_locG

if(length(which(is.na(h_CVopt$h_locG)))==0){h=h_CVopt$h_locG}else{h=h_CVopt$h_locG[-which(is.na(h_CVopt$h_locG))]}
h
unique(h)

hglobal=dpill(d$X,d$Y)
?dpill
?ksmooth






?dpill

?h.select


?bkde
