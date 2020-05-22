
library(caret)
library(tidyverse)
require(stats)
library(knitr)
require(ggpubr)
library(ggdistribute)
install.packages("ggstatsplot")
library(ggstatsplot)
install.packages('TMB', type = 'source')




setwd("C:/Users/jb/Google Drive/BIG DATA DAUPHINE/Bayésien")
mut = read.csv("mutations2.csv", stringsAsFactors = F)

#preparation des data:

{
  
  
  
  setwd("C:/Users/jb/Google Drive/BIG DATA DAUPHINE/Bayésien")
  mut = read.csv("mutations2.csv", stringsAsFactors = F)
  
  mut_nodup = mut %>% distinct()
  
  y = mut_nodup$Barre 
  #mat X pour les coef: sans les variables catégorielles
  X = as.matrix(mut_nodup[,-c(1:6)])
  
  summary(lm(y~X))
  betahat = (lm(y~X))$coefficients
  residuals = lm(y~X)$residuals
  s2 = t(residuals)%*%residuals
  
  
  #on utilise laprior de Zelner: loi non informative de paramètre g
  #pour simplifier les calculs on ajouter une colenne de 1 (pour l'intercepte)
  X = cbind(1,X)
  n = NROW(X)
  
  
  
  
  
  
  
  
  
  
}




glimpse(mut)

levels(mut$etablissement)


summary(mut)

#visualisation des données
#boxplot
par(mfrow = c(4,4))
par(mar = rep(2,4))
for (i in c(6:18)) {
  boxplot(mut[,i], main=colnames(mut[i]), col=c("azure4","cornflowerblue"))
        
}

#histogramme

par(mfrow = c(4,4))
par(mar = rep(2,4))
for (i in c(6:18)) {
  hist(mut[,i], main=colnames(mut[i]), col=c("cornflowerblue"))
  
}

#visualisation des matieres X etablissemrnts
mut %>%  distinct(code_etablissement) %>% summarise(nbmat = n())
mut %>%  distinct(Matiere) %>% summarise(nbmat = n())

mut_Graph = mut

NROW(levels(mut_Graph$code_etablissement))

mut_Graph$ct = 1
mut_Graph$id = 1:NROW(mut_Graph)

glimpse(mut)
glimpse(mut_Graph)

mut_Graph = mut_Graph %>% spread(key = Matiere, value = ct)

mut_Graph  = mut_Graph[-c(2:23)]
mut_Graph[is.na(mut_Graph)] = 0

mut_Graph =mut_Graph %>% group_by(code_etablissement) %>% summarise_all(sum)

summary(mut_Graph)

#107 etablissements alors que distinc etab

mut_Graph %>%  distinct(code_etablissement) %>% summarise(nbmat = n())

mut_Graph$code_etablissement = 1:NROW(mut_Graph)

#colnames(mut_Graph)[2:NCOL(mut_Graph)] = 1:(NCOL(mut_Graph)-1)


#generate graph data
{
  df_2dgraph = data.frame(
    x = rep(0,107*36),
    y = rep(0,107*36)) 

count = 1
for (x in  1:107){
    for (y in 2:36) {
      if (mut_Graph[x,y] > 0) {
        for (nb in 1:as.numeric((mut_Graph[x,y]))) {
          df_2dgraph$x[count]=x
          df_2dgraph$y[count]=y-.5
          count = count +1
                                                    }

                            }
                  }
                }
}

ggplot((df_2dgraph %>% filter(x!=0)), aes(x=x, y=y) ) +
  geom_bin2d(binwidth = c(1, 1)) +
  theme_classic()+
  scale_y_continuous(breaks = 1:36 + .5,
                     labels = c(colnames(mut_Graph)[2:NCOL(mut_Graph)],""))+
  theme(panel.grid.major.y = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "grey")
  )+
  xlab(label = "Etablissement ID")+
  ylab(label = "")+
  labs(title = "Représentation des couples Matières / Etablissements")
  


  
#------traitement des doublons

mut_nodup = mut %>% distinct()

#nombre de matieres par établissement
g1 = mut_nodup %>% group_by(code_etablissement) %>% summarise(nb = n())
g2 = mut_nodup %>% group_by(Matiere) %>% summarise(nb = n())


boxplot(g1$nb)

boxplot(g2$nb)


p1 = qplot(x = 1, y = nb, data = g1, xlab = "", geom = 'boxplot') + 
  coord_flip()+
  ylab("Nombre de matières")+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p2 = qplot(x = nb, data = g1, geom = 'histogram',
           binwidth =1 )+
  ylab( "Nombre d'établissement") +
  theme_bw()+
  theme(
    axis.title.x = element_blank()
  )

ggarrange(p2, p1, heights = c(2, 1), align = "hv", ncol = 1, nrow = 2)
quantile(g2$nb, seq(0,1,.05))



#nombre d'établissement par matières
g2 = mut_nodup %>% group_by(Matiere) %>% summarise(nb = n())


#correlation des variables




#regression lineraire gaussiene (classique)


y = mut_nodup$Barre
X = as.matrix(mut_nodup[,-c(1:6)])
X = cbind(1,X)

mutc = mut_nodup[,-c(2:4)]

glimpse(X)

lm(y~X)
summary(lm(y~X))
summary(lm(Barre~., data = mutc ))
summary(lm(Barre~. -etablissement, data = mutc ))
summary(lm(Barre~. -Matiere, data = mutc )) 
summary(lm(Barre~. -Matiere -etablissement, data = mutc ))



#regression linéaire Bayésienne.

#variables réponse
y = mut_nodup$Barre 
#mat X pour les coef: sans les variables catégorielles
X = as.matrix(mut_nodup[,-c(1:6)])
glimpse(mut_nodup)

glimpse(X)

summary(lm(y~X))
betahat = (lm(y~X))$coefficients
residuals = lm(y~X)$residuals
s2 = t(residuals)%*%residuals


#on utilise laprior de Zelner: loi non informative de paramètre g
#pour simplifier les calculs on ajouter une colenne de 1 (pour l'intercepte)
X = cbind(1,X) # on ajoute une colonne de 1 pour beta_0

n = nrow(X)

#choix de g: quantité d'information disponible dans la loi à priori -> vois cours p18
g = c(.01,.1,1,10,100,516,1000)
1/g

# espérance de beta0:
betahat[1]*g/(g+1)
# espérance de sigma^2
a = n/2
b = s2/2 + 1/(2*g+2) * ((t(betahat)%*%t(X))%*%(X%*%betahat))
b/(a-1)


#calcul de la vraissemblance

# fonction pour calculer la log-vraisemblance marginale
marglkd1 = function(gamma, X, g=n){
  q=sum(gamma)
  X1=X[,c(T,gamma)]
  if(q==0){return(q/2*log(g+1) - n/2*log(t(y)%*%y))}
  m = -q/2*log(g+1) -
    n/2*log(t(y)%*%y - g/(g+1)* t(y)%*% X1 %*%
              solve(t(X1)%*%X1) %*%t(X1)%*%y)
  return(m)
}



glimpse(X)
glimpse(gamma)
summary(X)

#echantillonage de gibbs:



niter = 2e4 # nombre d'iterations
gamma = matrix(F,nrow=niter,ncol=NCOL(X)-1)
gamma0 = sample(c(T,F),size=NCOL(X)-1, replace=TRUE) #valeur initiale aléatoire
lkd = rep(0,niter)
modelnumber = rep(0,niter)

oldgamma = gamma0
system.time(
for(i in 1:niter){
  newgamma = oldgamma
  for(j in 1:(NCOL(X)-1)){
    g1 = newgamma; g1[j]=TRUE
    g2 = newgamma; g2[j]=FALSE
    ml1 = marglkd1(g1, X)
    ml2 = marglkd1(g2, X)
    p = c(ml1,ml2)-min(ml1,ml2)
    # On souhaite tirer depuis une Bernoulli, avec probabilité de tirer TRUE égale à exp(p[1])/(exp(p[1])+exp(p[2])).
    # C'est ce que fait la ligne suivante. Notons que la fonction sample() calcule la constante de normalisation.
    newgamma[j] = sample(c(T,F), size=1, prob=exp(p)) 
  }
  gamma[i,] = newgamma
  lkd[i] = marglkd1(newgamma, X )
  modelnumber[i] = sum(newgamma*2^(0:(NCOL(X)-2)))
  oldgamma = newgamma
}
)

#17 variables + intercept : 18 colonnes, on itere sur 17 permutation car on car l intercept
#--------------------------


#sauvegarde du modèle:
gamma_e4iter_m1 = gamma
modelnumber_e4iter_m1 = modelnumber

gamma_e4iter_m1_noburn = gamma_e4iter_m1[(burnin+1):niter] 

gamma_e5iter_m1 = gamma
modelnumber_e5iter_m1 = modelnumber


gamma_2e4iter_m1 = gamma
modelnumber_2e4iter_m1 = modelnumber


save(gamma_2e4iter_m1, file = "gamma_2e4iter_m1")
save(modelnumber_2e4iter_m1, file = "modelnumber_2e4iter_m1")

save(gamma_e5iter_m1, file = "gamma_e5iter_m1")
save(modelnumber_e5iter_m1, file = "modelnumber_e5iter_m1")


apply(gamma, 2, "mean")

co1 = apply(gamma_e4iter_m1, 2, "mean")
co2 = apply(gamma_e5iter_m1, 2, "mean")

(co1-co2)/co1


# Vérifions le mélange de la chaîne de Markov à l'aide des autocorrélations.
par(mar = rep(3,4))
par(mfrow=c(5,4))
for(i in 1:17) acf(as.numeric(gamma[,i]), main = colnames(mut_nodup)[i+6], cex.main = .8)


for(i in 1:17) acf(as.numeric(gamma_e5iter_m1[,i]), main = colnames(mut_nodup)[i+6], cex.main = .8)

?acf



glimpse(gamma_e4iter_m1)

# Autocorrélation décroît rapidement. Pas besoin de sous-échantillonner.

# Vérifions la convergence + le mélange à l'aide de la trace (on utilise une moyenne glissante puisque les valeurs sont binaires).
require(zoo)
for(i in 1:17) plot(rollapply(gamma[,i], width=100, FUN=mean), type="l")

for(i in 1:17) plot(rollapply(gamma_e4iter_m1[,i], width=100, FUN=mean), type="l")
for(i in 1:17) plot(rollapply(gamma_e5iter_m1[,i], width=100, FUN=mean), type="l")

for(i in 1:17) hist(rollapply(gamma_e4iter_m1[,i], width=100, FUN=mean), main = colnames(mut_nodup)[i+6])

#tirage des best modeles

burnin = 2000 # 500 itérations de burn-in
gammab = modelnumber[(burnin+1):niter] 
res = as.data.frame(table(gammab))
odo = order(res$Freq, decreasing=T)[1:50]
modcho = res$gammab[odo]
probtop50 = res$Freq[odo]/(niter-burnin)

indices = match(modcho, modelnumber)
cbind(probtop50, gamma[indices, ])


#calcul des Beta des modeles -> gagner en te,mps de calcul: seuelement sur les modeles trouvés uniques apres burnin

gamma_b_unique = as.data.frame(cbind( gamma_2e4iter_m1[(burnin+1):niter,],modelnumber_2e4iter_m1[(burnin+1):niter]) )  %>% distinct()





glimpse(gamma_e4iter_m1)
glimpse(gamma_b_unique) #1855 modeles différents

#calcul des beta pour chaques modèles, 17 variable + intercept + model number = 19

m_coefbeta = matrix(rep(0,19*NROW(gamma_b_unique)), ncol=19)

#calcul des coef grace a la fonction lm (on aurai pu aussi directement calculer avec la formule d inversion de matrices)

{
  Xpred = as.matrix(mut_nodup[,-c(1:6)])
for (i in 1:NROW(gamma_b_unique)) {
  Xtemp =  gamma_b_unique[i,]  * c(1:17,0) #numéroter les colonnes des modeles (retirer le mod number)
  Xtemp = Xtemp[Xtemp!=0]  #supression des 0
  coeftmp = lm(y~Xpred[,Xtemp])  #fit seulement sur variables du modele
  m_coefbeta[i,c(1,Xtemp+1)] = coeftmp$coefficients  # save les coef
  m_coefbeta[i,19] = gamma_b_unique[i,18]  #numéro du modèle
}
}



#remetre les proba sur les modèles

modelnumber_2e4iter_m1_noburn = as.data.frame(modelnumber_2e4iter_m1[(burnin+1):niter])
colnames(modelnumber_2e4iter_m1_noburn) = c("modnum")
proba = modelnumber_2e4iter_m1_noburn %>% group_by(modnum) %>% summarise(ndisct = n()/NROW(modelnumber_2e4iter_m1_noburn))
m_coefbeta = as.data.frame(m_coefbeta)
colnames(m_coefbeta)[19] = c("modnum")
m_coefbeta_prob = m_coefbeta %>% left_join(proba, by = "modnum")


#tirage aléatore des Beta

MCMC_Beta_sample = sample(1:NROW(m_coefbeta_prob), size = 1e4 , replace = T , prob = c(m_coefbeta_prob$ndisct) )


MCMC_Beta = m_coefbeta_prob[MCMC_Beta_sample,]

glimpse(MCMC_Beta)


#Distribution des B

moy2 = apply(gamma_2e4iter_m1[(burnin+1):niter,], 2, "mean")
moy3 = c(1,moy2) 
  
  par(mfrow = c(4,5))
par(mar = rep(2,4))
for (i in 1:18) {
    hist(MCMC_Beta[which( MCMC_Beta[,i] != 0),i], main = moy3[i] )
  
}
  
  
  
hist()
  

glo




burnin = 4000 # 500 itérations de burn-in
niter = 20000
gammab = modelnumber_2e4iter_m1[(burnin+1):niter] 
res = as.data.frame(table(gammab))
odo = order(res$Freq, decreasing=T)
modcho = res$gammab[odo]
probtop = res$Freq[odo]/(niter-burnin)




indices = match(modcho, modelnumber)
cbind(probtop50, gamma[indices, ])








glimpse(modelnumber_2e4iter_m1_noburn)

glimpse(gamma_b_unique)



gamma_2e4iter_m1 = gamma
modelnumber_2e4iter_m1 = modelnumber



burnin = 4000 # 500 itérations de burn-in
niter = 20000
gammab = modelnumber_2e4iter_m1[(burnin+1):niter] 
res = as.data.frame(table(gammab))
odo = order(res$Freq, decreasing=T)
modcho = res$gammab[odo]
probtop50 = res$Freq[odo]/(niter-burnin)

indices = match(modcho, modelnumber)
cbind(probtop50, gamma[indices, ])


glimpse(res)
glimpse(m_coefbeta)

summary(m_coefbeta)




summary(m_coefbeta)

gamma_2e4iter_m1[24,]  * c(1:17)



#jointure des coef avec les modeles tirés:
cbind() gamma_e4iter_m1_noburn

gamma_et_beta = gamma_e4iter_m1_noburn


as.matrix()











Xtemp =  gamma[1,]  * c(1:17)

Xtemp[Xtemp!=0]

vv = lm(y~X[,Xtemp[Xtemp!=0]])

vv$coefficients

glimpse(gammab)
 

gammab


ggg= gamma[(burnin+1):niter,]  %*% matrix( data = rep(c(1:17),8000) ,ncol=17, byrow = T)


sum(ggg)

gamma[1,]



res2 =  as.data.frame(table(as.factor(gammab)))


?intlike

2^17


matrix( data = rep(c(1:17),17) ,ncol=17, byrow = T)

?matrix


coef = X%*%

glimpse(gamma)

bet = 
  
  sample( x=1:(niter-burnin), replace = T)


sample( x=1:(niter-burnin), replace = T)




niter2 = 1e4
for (m in 1:niter2) {
  bet = sample(c(1:(niter-burnin)))
  
  
}

?sample




burnin = 2000 # 500 itérations de burn-in
gammab = modelnumber[(burnin+1):niter] 
res = as.data.frame(table(gammab))
odo = order(res$Freq, decreasing=T)[1:50]
modcho = res$gammab[odo]
probtop50 = res$Freq[odo]/(niter-burnin)

indices = match(modcho, modelnumber)
cbind(probtop50, gamma[indices, ])


#vérification de la convergence

#repetition des algo plusieurs fois






# poir comparer le nombre de modèles


ls_model1e4 = list()
ls_model1e5 = list()

for (m in 1:10) {
  
  niter = 1e4 # nombre d'iterations
  gamma = matrix(F,nrow=niter,ncol=NCOL(X)-1)
  gamma0 = sample(c(T,F),size=NCOL(X)-1, replace=TRUE) #valeur initiale aléatoire
  lkd = rep(0,niter)
  modelnumber = rep(0,niter)
  oldgamma = gamma0

    for(i in 1:niter){
      newgamma = oldgamma
      for(j in 1:(NCOL(X)-1)){
        g1 = newgamma; g1[j]=TRUE
        g2 = newgamma; g2[j]=FALSE
        ml1 = marglkd1(g1, X)
        ml2 = marglkd1(g2, X)
        p = c(ml1,ml2)-min(ml1,ml2)
        # On souhaite tirer depuis une Bernoulli, avec probabilité de tirer TRUE égale à exp(p[1])/(exp(p[1])+exp(p[2])).
        # C'est ce que fait la ligne suivante. Notons que la fonction sample() calcule la constante de normalisation.
        newgamma[j] = sample(c(T,F), size=1, prob=exp(p)) 
      }
      gamma[i,] = newgamma
      lkd[i] = marglkd1(newgamma, X )
      modelnumber[i] = sum(newgamma*2^(0:(NCOL(X)-2)))
      oldgamma = newgamma
    }
  
  ls_model1e4[[m]] = modelnumber
  
}


save(ls_model1e4, file = "ls_model1e4")

lapply(ls_model1e4, 2, "mean")

modsansburn = list()

for (i in 1:10) {
  modsansburn[[i]] = ls_model1e4[[i]][2000:NROW(ls_model1e4[[i]])] 
  
}

join10iter =   unlist(modsansburn)
  
as.data.frame(join10iter) %>% distinct() %>% summarise(nb = n())

as.data.frame(modelnumber_e5iter_m1) %>% distinct() %>% summarise(nb = n())

as.data.frame(c(join10iter,modelnumber_e5iter_m1[2000:NROW(modelnumber_e5iter_m1)])) %>% distinct() %>% summarise(nb = n())

#package BAS
install.packages("BAS")
library(BAS)


X_no_intercept = X[,-1]
modBASold = modBAS
modBAS = bas.lm(Barre~. ,data = mut_nodup[,-c(1:5)],
                prior = "g-prior",
                method="MCMC",
                MCMC.iterations=2e4,
               # n.models = 2e4,
                modelprior=uniform(),
                alpha = n
)

?bas.lm


plot(modBAS)

image(modBAS)
plot(confint(modBAS, parm = 2:17 ))

coef.BAS = coef(modBAS)
par(mfrow = c(4,5))
par(mar = rep(2,4))
plot(coef.BAS, subset = c(1:18), ask = F)

plot(confint(coef.BAS, parm = c(1:18)))

#recherche d un modele avec 1 var non nul

modBAS_f = bas.lm(Barre~. ,data = mut_nodup[,-c(1:5)],
                prior = "BIC",
                method="MCMC",
                MCMC.iterations=2e4,
                #n.models = 2e4,
                modelprior=uniform(),
                #alpha = n,
                #initprobs = "eplogp",
)


coef.BAS = coef(modBAS_f)
plot(confint(coef.BAS, parm = c(2:18)))



plot(confint(modBAS_f, parm = 2:17 ))

coef.BAS = coef(modBAS_f)
par(mfrow = c(4,5))
par(mar = rep(2,4))
plot(coef.BAS, subset = c(1:18), ask = F)

plot(confint(coef.BAS, parm = c(2:18)))

#recherche de meilleur param


modBAS_2 = bas.lm(Barre~  . ,
                  data = mut_nodup[,-c(1:5)],
                  prior = "hyper-g-laplace",
                  method="MCMC",
                  MCMC.iterations=2e4,
                  modelprior=beta.binomial(1,1)
)

coef.BAS2 = coef(modBAS_2)
par(mfrow = c(3,2))
par(mar = rep(2,4))
plot(coef.BAS2, subset = c(1:4), ask = F )
plot(confint(coef(modBAS_2), parm = c(2:4)),xaxt='n' )


coef.BAS2 = coef(modBAS_2)
par(mfrow = c(3,2))
par(mar = rep(2,4))
plot(coef.BAS2, subset = c(1:4), ask = F )

par(mfrow = c(4,5))
par(mar = rep(2,4))
plot(coef.BAS2, subset = c(1:18), ask = F )








#-----density plot

ggplot(mut_nodup, aes(Barre, colour = Matiere)) +
  geom_density() +
  facet_wrap(~Matiere)



ggplot(mut_nodup, aes(Barre, colour = code_etablissement)) +
  geom_density() +
  facet_wrap(~code_etablissement)

glimpse(mut_nodup)



?confint

BAS::

modBAS$probne0
moy3

comparre_prob = data.frame(
  Prob_Dm_procedure = moy3,
  Prob_Package_BAS = modBAS$probne0
  
)

comparre_prob2 = data.frame(
  Prob_Dm_procedure = moy3/(sum(moy3)-1),
  Prob_Package_BAS = modBAS$probne0/(sum(modBAS$probne0)-1),
  VarID = 0:17)

comparre_prob2$Prob_Dm_procedure[1] = 1
comparre_prob2$Prob_Package_BAS[1] = 1

sum(moy3)
sum(modBAS$probne0)


dataGrf = gather(comparre_prob2,"Prob_Dm_procedure","Prob_Package_BAS", key = "procedure" , value = "prob")


ggplot(dataGrf, aes(x = procedure , y = prob )) +
geom_bar(stat='identity', aes(fill=procedure)) +
facet_grid(~VarID)



#---- LASSO -----
install.packages("fastDummies")

lasso1 = regress(X, y, method="lasso")

Blasso1 = blasso(X, y, T = 5000, thin = 1000, 
       normalize = F)

plot(Blasso1, burnin=1000 , which="lambda2")
plot(Blasso1, burnin=1000 , which="s2")


plot(lasso1)

Blasso2 = blasso(X, y)

plot(Blasso2, burnin=100 , which="lambda")
plot(Blasso2, burnin=100,normalize = TRUE )


summary(Blasso2)

plot(Blasso2, burnin=100 , which="lambda2")
plot(Blasso2, burnin=100 , which="s2")
plot(Blasso2, burnin=100)


#creat dummy variable

X_cat = dummy_cols(fastDummies_example, select_columns = "Matiere")


#lasso standard
library(glmnet)
lass1 = glmnet(X,y,standardize=T)

plot(lass1,xvar="lambda")
vn=c("intercept", colnames(X)[-1])
par(mar=c(4.5,4.5,1,9))
plot(lass1)
vnat=coef(lass1)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=vn,las=1,tick=FALSE, cex.axis=0.6) 

colnames(X)[-1]

#Optimisation du paramètre lambd

cv.ridge2 <- cv.glmnet(X,y,
                       nfolds=10)

plot(cv.ridge2)
print(log(cv.ridge2$lambda.min))

glimpse(mut_nodup)

?cv.glmnet


cv.ridge3 <- cv.glmnet(X,y,
                       nfolds=10 ,standardize=T)



plot(cv.ridge3)
print(log(cv.ridge3$lambda.min))
plot(cv.ridge3,xvar="lambda")

coef(cv.ridge3, s = "lambda.min")


+ geom_bar(stat='identity')



 #+ facet_grid(~VarID)



#------------------- comparatifmodel 
#del

pAIC = predict(mods2, data = mut_nodup)
pAIC_s2 = sum((pAIC-y)^2)
pBAS = predict(modBAS_2, data = mut_nodup[,-c(1:5)])
pBAS_s2 = sum((pBAS$fit-y)^2)
pAIC_s2
pBAS_s2

#reste
# _____=================== Q 1.3 math et anglais===============


mut_AN =  mut_nodup[,-c(1:4)] %>% filter(Matiere == "ANGLAIS")
mut_AN = mut_AN[,-1]

modBAS_Ang = bas.lm(Barre~.  ,
                  data =mut_AN,
                  prior = "hyper-g-laplace",
                  method="MCMC+BAS",
                  MCMC.iterations=2e4,
                  modelprior=beta.binomial(1,1)
)

mut_MA =  mut_nodup[,-c(1:4)] %>% filter(Matiere == "MATHS")
mut_MA = mut_MA[,-1]

coef.BAS = coef(modBAS_Ang)
par(mfrow = c(4,5))
par(mar = rep(2,4))
plot(coef.BAS, subset = c(1:18), ask = F )

plot(confint(coef(modBAS_Ang), parm = c(1:18)))



modBAS_Ma = bas.lm(Barre~.,
                    data = mut_MA,
                    prior = "hyper-g-laplace",
                    method="MCMC+BAS",
                    MCMC.iterations=2e4,
                    modelprior=beta.binomial(1,1)
)

coef.BAS = coef(modBAS_Ma)
par(mfrow = c(4,5))
par(mar = rep(2,4))
plot(modBAS_Ma, subset = c(1:18), ask = F )
plot(confint(coef(modBAS_Ma), parm = c(1:18)))

par(mfrow = c(2,1))

plot(confint(coef(modBAS_Ma), parm = c(2:18)))
plot(confint(coef(modBAS_Ang), parm = c(2:18)))


coefMA = coef(modBAS_Ma)
coefAN = coef(modBAS_Ang)

coefMA$postmean
coefAN$postmean

df_graphcoef = data.frame(
  mean = c(coefMA$postmean,coefAN$postmean ),
  SD = c(coefMA$postsd,coefAN$postsd ),
  Matiere = c(rep("Math", NROW(coefMA$postmean)), rep("Anglais", NROW(coefAN$postmean))),
  VarID = rep(0:(NROW(coefMA$postmean)-1),2)
)

glimpse(df_graphcoef)

ggplot(df_graphcoef %>% filter(VarID !=0), aes(x=VarID, y=mean, fill=Matiere)) + 
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD),
                width=.2,                    
                position=position_dodge(.9))+
  theme_classic()+
  theme(panel.grid.major.x = element_line(colour = "grey"))+
  scale_x_continuous(breaks=c(1:17), labels =c(1:17))
  


ggplot(df_graphcoef 
       , aes(x= "", y=Beta, fill=Matiere)) + 
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD),
                width=.2,                    
                position=position_dodge(.9))+
  theme_bw()+
  facet_wrap(~VarID, scales = "free")

rownames(coefMA)

coefMA$namesx


coef.BAS2 = coef(modBAS_2)
par(mfrow = c(3,2))
par(mar = rep(2,4))
plot(coef.BAS2, subset = c(1:4), ask = F )
plot(confint(coef(modBAS_2), parm = c(2:4)),xaxt='n' )



#================== PARET0 ==================

install.packages("VGAM")
library(VGAM)

alpha = 1:10
m = 21
n = 1e4

p1 = rpareto(n, scale = alpha, shape = m )

ls_pareto = list()
ls_alpha  = list()


for (i in 1:10) {
  ls_pareto[[i]] = rpareto(n, scale = alpha[i], shape = 100 )
  ls_alpha[[i]] = rep(alpha[i],n)
  }


df_pareto = data.frame(
  tirage = unlist(ls_pareto),
  alpha = unlist(ls_alpha)
)
df_pareto$tiragecentré = df_pareto$tirage-df_pareto$alpha
df_pareto$alpha = as.character(df_pareto$alpha)


ggplot(df_pareto, aes(x=tirage, color = alpha)) +
  geom_density() +
  labs( title = "Densité tirage Pareto")+
  scale_x_continuous(breaks = 1:10)+
  xlim(c(1,12))

ggplot(df_pareto, aes(x=tiragecentré, color = alpha)) +
  stat_ecdf(geom = "step")+
  labs( title = "Densité tirage Pareto")



df_pareto$tirage-df_pareto$alpha


+
  scale_x_continuous(breaks = 1:10)+
  xlim(c(1,12))


#tirage alpha entre 0 et 10

v_alpha = seq(1e-3,30,.2)

ls_pareto = list()
ls_alpha  = list()
n=1000

for (i in 1:NROW(v_alpha)) {
  ls_pareto[[i]] = rpareto(n, scale = v_alpha[i], shape = m )
  ls_alpha[[i]] = rep(v_alpha[i],n)
}

df_pareto = data.frame(
  tirage = unlist(ls_pareto),
  alpha = unlist(ls_alpha)
)
#150k tirages

df_pareto$alpha = as.character(df_pareto$alpha)


ggplot(df_pareto, aes(x=tirage,)) +
  geom_density() +
  labs( title = "Densité tirage Pareto")


install.packages("loo")
library(loo)


gpdfit(mut_nodup$Barre, wip =F)


library(mcmc)

max(mut_nodup$Barre)# 2056
#prior uniforme sur 0-2056

logposterior = function(
  
  
  
)


  alpha = c(0.8,1,2,5,8,10)
m = 21
n = 2e4

ls_pareto = list()
ls_alpha  = list()

for (i in 1:NROW(alpha)) {
  ls_pareto[[i]] = rpareto(n, scale = alpha[i], shape = m )
  ls_alpha[[i]] = rep(alpha[i],n)
}

df_pareto = data.frame(
  tirage = unlist(ls_pareto),
  alpha = unlist(ls_alpha)
)

df_pareto$tiragecentr = df_pareto$tirage-df_pareto$alpha
df_pareto$alpha = as.factor(df_pareto$alpha)

ggplot(df_pareto, aes(x=tirage, color = alpha)) +
  geom_density() +
  labs( title = "Densité de la réalisation d'un loi de Pareto pour plusieurs alpha")+
  xlim(0,40)+
  xlab("x")


  
  #====================MH PARETO






lambda=1


m=21
a=1
b=1
#Prior loi exponentiele

?gamma


?digammma

prior = function(alpha,a,b){
  return(dexp(alpha,lambda))
}

dexp(1,1)
rexp(1,1)

?dexp

#pour gagner du temps:
?rgamma

?rnorm()

m=21
a=1
b=1



#Log Posterior
logposterior = function(alpha, y,m=m,a=a,b=b){
  n=length(y)
  logposterior = (a + n)*log(b+sum(log(y/m)))+(a+n-1)*log(alpha)-(b+sum(log(y/m)))*alpha-rgamma(n=1,shape = a+n)
  if(!is.finite(logposterior)) return(-Inf)
  #print("ok")
  return(logposterior)
}

 
require(mvtnorm)
MH = function(alpha0, y,niter, tau,a=a,b=b){
  alpha = rep(NA, nrow=niter)
  alpha[1] = alpha0
  acc = 0 # nombre d'acceptations
  
  for(i in 2:niter){
    proposal = rnorm(1, alpha[i-1], tau)
    logalpha = logposterior(proposal, y, m,a,b)-logposterior(alpha[i-1], y, m,a,b)
 
    
    if(log(runif(1)) < logalpha){
      alpha[i] = proposal
      acc = acc + 1
    }
    else{
      alpha[i] = alpha[i-1]
    }
  }
  cat("---Proportions d'acceptation pour tau=",tau,"\n")
  print(acc/niter) #proportion d'acceptations
  return(alpha)
}




  
y_trans=log(y/m)


niter = 2e4
cat("--------------------------------Proportions d'acceptations----------------------------------\n")
alpha1 = MH(alpha0=0.1, y,niter, tau=0.01)
alpha2 = MH(alpha0=0.1, y,niter, tau=0.1)
alpha3 = MH(alpha0=0.1, y,niter, tau=3)
alpha4 = MH(alpha0=0.1, y,niter, tau=0.275)


hist(alpha1[100:niter], breaks=50,xlab="",main=expression(paste(alpha,",",tau,"=0.1")))
hist(alpha2[100:niter], breaks=50,xlab="",main=expression(paste(alpha,",",tau,"=0.001")))
hist(alpha3[100:niter], breaks=50,xlab="",main=expression(paste(alpha,",",tau,"=2")))

df_alpha = data.frame(alpha = alpha4[1000:niter])


hdi(df_alpha)




vect_a = c(1,3,5,7,10)
vect_b = c(1,3,5,7,10)

ls_MH.alpha = list()
ls_MH.graphs = list()
countMH = 1
niter = 2e4


for (i in 1:NROW(vect_a)) {
    for (j in 1:NROW(vect_b)) {
      ls_MH.alpha[[countMH]] = MH(alpha0=0.1, y,niter, tau=0.275, a=vect_a[i], b=vect_b[j])
      df_alpha = data.frame(alpha = ls_MH.alpha[[countMH]][1000:niter])
      
      CI1 = round(hdi(df_alpha)[1],3)
      CI2 = round(hdi(df_alpha)[2],3)
      alphaM = round(mean(df_alpha$alpha),3) 
      
      
      ls_MH.graphs[[countMH]] = ggplot(df_alpha, aes(x = alpha)) +
                                geom_histogram(aes(y=..density..),na.rm=T,col="#113DAC",fill="#5267F7")+
                                geom_density(alpha=0.2,na.rm=T,fill="#FF6666")+
                                theme_classic()+
                                geom_vline(aes(xintercept=mean(alpha)),color="red", linetype="dashed", size=1)+
                                geom_segment(aes(x = hdi(df_alpha)[1], xend = hdi(df_alpha)[2], y = 1 , yend =1),linetype = 1, size = 2)+
                                labs(title = bquote("Prior"~Gamma~"("~.(vect_a[i])~","~.(vect_b[j])~")"),
                                     subtitle = bquote(alpha[m]==.(alphaM)~" , "~CI[0.95]==~"("~.(CI1)~","~.(CI2)~")"))
      countMH = countMH +1
    }
  
}


x = lapply(ls_MH.alpha, mean)

summary(unlist(x))

hdi(unlist(x))

hdi(ls_MH.alpha[[1]])
mean(ls_MH.alpha[[1]])
hdi(unlist(ls_MH.alpha))
mean(unlist(ls_MH.alpha))

ls_MH.graphs[[2]]

a=1
b=1
CI1 = round(hdi(df_alpha)[1],3)
CI2 = round(hdi(df_alpha)[2],3)
alphaM = round(mean(df_alpha$alpha),3)





ggplot(df_alpha, aes(x = alpha)) +
  geom_histogram(aes(y=..density..),na.rm=T,col="#113DAC",fill="#5267F7")+
  geom_density(alpha=0.2,na.rm=T,fill="#FF6666")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(alpha)),color="red", linetype="dashed", size=1)+
  geom_segment(aes(x = hdi(df_alpha)[1], xend = hdi(df_alpha)[2], y = 1 , yend =1),linetype = 1, size = 2)+
  labs(title = bquote("Prior"~Gamma~"("~.(a)~","~.(b)~")"),
       subtitle = bquote(alpha[m]==.(alphaM)~" , "~CI[0.95]==~"("~.(CI1)~","~.(CI2)~")"))
                         
                         
                       
#============ ANGLAIS VS MATHS



y_math = mut_nodup %>% filter(Matiere=="MATHS") %>% pull(Barre)
y_anglais = mut_nodup %>% filter(Matiere=="ANGLAIS") %>% pull(Barre)



alpha_math1 = MH(alpha0=0.1, y_math,niter, tau=0.01, a=1, b=1)
alpha_math2 = MH(alpha0=0.1, y_math,niter, tau=0.6, a=1, b=1)
alpha_math3 = MH(alpha0=0.1, y_math,niter, tau=3, a=1, b=1)

alpha_anglais1 = MH(alpha0=0.1, y_anglais,niter, tau=0.01, a=1, b=1)
alpha_anglais2 = MH(alpha0=0.1, y_anglais,niter, tau=0.6, a=1, b=1)
alpha_anglais3 = MH(alpha0=0.1, y_anglais,niter, tau=3, a=1, b=1)

?s.test


cmv.test



# Sortie de l'algorithme pour differentes valeurs de tau
par(mfcol=c(4,3))
par(mar = rep(2,4))
# trace
plot(alpha_math1, type="l",ylab=expression(alpha),xlab="")
plot(alpha_math2, type="l",ylab=expression(alpha),xlab="")
plot(alpha_math3, type="l",ylab=expression(alpha),xlab="")
plot(alpha4, type="l",ylab=expression(alpha),xlab="")

# autocorrélations
acf(alpha_math1[1000:niter],xlab="",main=expression(tau==0.01))
acf(alpha_math2[1000:niter],xlab="",main=expression(tau==0.1))
acf(alpha_math3[1000:niter],xlab="",main=expression(tau==3))
acf(alpha4[1000:niter],xlab="",main=expression(tau==0.27))

# histogrammes
hist(alpha_math1[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=0.01")))
hist(alpha_math2[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=0.1")))
hist(alpha_math3[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=3")))
hist(alpha4[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=0.27")))

cat("--------------------------------Tailles d'échantillons effectives ESS----------------------------------\n")
cat("ESS Tau=0.01\n",niter/(2*sum(acf(alpha_math1[1000:niter], plot=F)$acf) - 1),"\n")
cat("ESS Tau=0.1\n",niter/(2*sum(acf(alpha_math2[1000:niter], plot=F)$acf) - 1),"\n")
cat("ESS Tau=3\n",niter/(2*sum(acf(alpha_math3[1000:niter], plot=F)$acf) - 1),"\n")
cat("ESS Tau=0.275\n",niter/(2*sum(acf(alpha4[1000:niter], plot=F)$acf) - 1),"\n")



alpha_math = MH(alpha0=0.1, y_math,niter, tau=0.6, a=1, b=1)
alpha_anglais = MH(alpha0=0.1, y_anglais,niter, tau=0.6, a=1, b=1)

df_aan = data.frame(
  alpha = c(alpha_math,alpha_anglais),
  Matière = c(rep("MATHS",NROW(alpha_math)),rep("ANGLAIS",NROW(alpha_anglais)))
  )



ggplot() +
  geom_histogram(data = df_alpha, aes(x = alpha, y=..density..),na.rm=T,col="#113DAC",fill="#5267F7")+
  
  
  
  
  geom_density(alpha=0.2,na.rm=T,fill="#FF6666")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(alpha)),color="red", linetype="dashed", size=1)+
  geom_segment(aes(x = hdi(df_alpha)[1], xend = hdi(df_alpha)[2], y = 1 , yend =1),linetype = 1, size = 2)+
  labs(title = bquote("Prior"~Gamma~"("~.(a)~","~.(b)~")"),
       subtitle = bquote(alpha[m]==.(alphaM)~" , "~CI[0.95]==~"("~.(CI1)~","~.(CI2)~")"))




ggplot(df_aan, aes(x = alpha, fill = Matière )) +
  geom_density(alpha=0.5,na.rm=T)+
  theme_classic()



#--------------barre de m

a= n*0.548624 +0.1
b=1
alpha=0.548624
niter=1e2

if (j %in% c(1,100,1000,3000,5000,9000,15000,19500)) {
  #print(paste(alpha*log(b)*(a-alpha*n),log(m)*(-a+alpha-n-1),log(a-alpha*n))) 
}
t1=t1,t2,t3=t3,t4=t4
#Log Posterior
logposterior_m = function(y,m, alpha = alpha,a=a,b=b,j){
  n=length(y)
  logposterior_m = alpha*log(b)*(a-alpha*n)-log(m)*(a-alpha*n+1)+log(a-alpha*n)
 # print((a-alpha*n+1))
  #print(m)
  #print(paste(alpha*log(b)*(a-alpha*n),-log(m)*(a-alpha*n+1),log(a-alpha*n))) 
  
  if(!is.finite(logposterior_m)) return(-Inf)
  #print("ok")
  return(logposterior_m)
}

#alpha*log(b)*(a-alpha*n)-log(m)*(a-alpha+n+1)+log(a-alpha*n)
require(mvtnorm)
MH_m = function(m0, y,niter2, tau=tau,a=a,b=b, alpha = alpha){
  m = rep(NA, nrow=niter2)
  m[1] = m0
  acc = 0 # nombre d'acceptations
  
  for(i in 2:niter2){
    proposal = rnorm(1, m[i-1], tau)
    logm = logposterior_m(proposal, y, alpha,a,b,i)-logposterior_m(m[i-1], y, alpha,a,b,i)
 
    if(log(runif(1)) < logm){
      m[i] = proposal
      acc = acc + 1
    }
    else{
      m[i] = m[i-1]
    }
  }
  cat("---Proportions d'acceptation pour tau=",tau,"\n")
  print(acc/niter) #proportion d'acceptations
  return(m)
}


t1 = rep(0, niter)
t2 = rep(0, niter)
t3 = rep(0, niter)
t4 = rep(0, niter) 


MHm1 = MH_m(m0 = 100, y_anglais, niter = 2e4, tau = 1000, a= 1,b=1/22, alpha =0.548624 )
MHm2 = MH_m(m0 = 100, y_anglais, niter = 2e4, tau = .1, a= ,b=1/22, alpha =0.548624 )

MHm3 = MH_m(m0 = 01, y, niter = 1e4, tau = 1, a= 255 ,b=1, alpha =0.4681361 )

rnorm(1, 1, .001)

NROW(y)* 0.5

NROW(y_anglais)* 0.6

rmarkdown::render("markdwnDM3.Rmd" , output_file = "outDmBaye.html", quiet = F, encoding ="Windows-1252" )

setwd("C:/Users/jb/Google Drive/BIG DATA DAUPHINE/Bayésien")

par(mfcol=c(4,3))
par(mar = rep(2,4))
# trace
plot(MHm1, type="l",ylab=expression(alpha),xlab="")
plot(MHm2, type="l",ylab=expression(alpha),xlab="")
plot(MHm3, type="l",ylab=expression(alpha),xlab="")
plot(alpha4, type="l",ylab=expression(alpha),xlab="")

# autocorrélations
acf(MHm1[1000:niter],xlab="",main=expression(tau==0.01))
acf(MHm2[1000:niter],xlab="",main=expression(tau==0.1))
acf(MHm3[1000:niter],xlab="",main=expression(tau==3))
acf(alpha4[1000:niter],xlab="",main=expression(tau==0.27))

# histogrammes
hist(MHm1[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=0.01")))
hist(MHm2[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=0.1")))
hist(MHm3[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=3")))
hist(alpha4[1000:niter], breaks=30,xlab="",main=expression(paste(alpha,",",tau,"=0.27")))

cat("--------------------------------Tailles d'échantillons effectives ESS----------------------------------\n")
cat("ESS Tau=0.01\n",niter/(2*sum(acf(MHm3[1000:niter], plot=F)$acf) - 1),"\n")



#======================================PIRATEPLOT=======



G3 = pirateplot(formula = Number.of.rooms ~ RegionX5,
                data = as.data.frame(d2 %>% filter(is.na(RegionX5)==F,RegionX5 !="Reste de la France")),
                theme = 1,
                ylim = c(0,300),
                xlab = "Nombre d'hôtels",
                ylab = "Nombre de chambres",
                point.o = .00,
                point.pch = 16,
                avg.line.o = .8,
                quant = c(.25, .75),
                quant.col = "black",
                inf.f.o = 0,
                inf.b.o = 0,
                cex.lab = 1.2,
                cex.axis = 1.0,
                plot = T)



```{r ,fig.width=12, fig.height=10}
ggplot(mut_nodup, aes(Barre, colour = Matiere)) +
  geom_density() +
  facet_wrap(~Matiere)+
  labs( title = "Distribution de Barre en fonction des Matières")
```

pirateplot(formula = Barre ~ Matiere,
                data = mut_nodup,
                theme = 1,
                ylim = c(0,2100),
                xlab = "Matière",
                ylab = "Barre",
                point.o = .5,
                point.pch = 16,
                avg.line.o = .8,
                quant = c(.25, .75),
                quant.col = "black",
                inf.f.o = 0,
                inf.b.o = 0,
                cex.lab = 1.2,
                cex.axis =1,
                plot = T)








ggplot several 



df_alpha, aes(x = alpha)


       

       (hdi(df_alpha)[1] ))


?bquote

+
  geom_segment(data = cdat.dens, aes(x = ci.upp, xend = ci.upp, y = 0, yend = dens.ciupp, colour = cond),
               linetype = "dotted", size = 1)




?geom_hline
  
  
  geom_density(alpha=0.2,na.rm=T,fill="#FF6666")

?geom_density


#

+
  geom_hline(aes(xintercept=hdi(alpha)[1]),color="red", linetype="dashed", size=1)+
  
  geom_vline(aes(xintercept=hdi(alpha)[1]),color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=hdi(alpha)[2]),color="red", linetype="dashed", size=1)








confint(alpha1[100:niter])

?confint


min(y)
  
  
alpha=alpha4[1000:niter]

library(HDInterval)

hdi(alpha)


library(ggplot2)
dat <- data.frame(alpha = alpha[100:1e4])



ggplot(dat, aes(x = alpha)) +
  geom_histogram(aes(y=..density..),na.rm=T,col="red",fill="white")+
  geom_density(alpha=0.2,na.rm=T,fill="#FF6666")+
  geom_hline(aes(xintercept=hdi(alpha)[1]),color="red", linetype="dashed", size=1)+
  
  geom_vline(aes(xintercept=hdi(alpha)[1]),color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=hdi(alpha)[2]),color="red", linetype="dashed", size=1)













max(mut_nodup$Barre)# 2056

out = metrop(logposterior, c(2, 0), nbatch=1e4, 
             blen=1, scale=sqrtm(sigma), y=d$damage, x=d$tempF)










alpha <- 3; k <- exp(1); x <- seq(2.8, 8, len = 300)
## Not run: 
plot(x, dpareto(x, scale = alpha, shape = k), type = "l",
     main = "Pareto density split into 10 equal areas")
abline(h = 0, col = "blue", lty = 2)
qvec <- qpareto(seq(0.1, 0.9, by = 0.1), scale = alpha, shape = k)
lines(qvec, dpareto(qvec, scale = alpha, shape = k),
      col = "purple", lty = 3, type = "h")


p1

plot(p1)




?rpareto()


summarise(modBAS)

mut_nodup[,-c(1:6)]

glimpse(mut_nodup[,-c(1:5)])  

# calculons la log-vraisemblance marginale des 8 modèles=============================

X_restreint = X[,1:4]
logprob3 = c(
  marglkd1(c(F,F,F), X_restreint),
  marglkd1(c(F,F,T), X_restreint),
  marglkd1(c(F,T,F), X_restreint),
  marglkd1(c(F,T,T), X_restreint),
  marglkd1(c(T,F,F), X_restreint),
  marglkd1(c(T,F,T), X_restreint),
  marglkd1(c(T,T,F), X_restreint),
  marglkd1(c(T,T,T), X_restreint))

# on peut ajouter une constante, qui évitera les erreurs numériques
logprob3 = logprob3-max(logprob3)
# les probabilités des modèles sont donc
prob3 = exp(logprob3)/sum(exp(logprob3))
round(prob3, 3)
# c'est le modèle (T, F, F) qui est de loin le plus probable a posteriori


mods = step(lm(Barre~. , data = mutc ), direction = "both")
summary(mods)



lm()

?lm





?step



            
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))


#===========VRAC ELEVE==============0


#### Test avec des paramètres différents

```{r , echo=FALSE}
X_no_intercept = X[,-1]

modBAS_1 = bas.lm(Barre~. ,data = mut_nodup[,-c(1:5)],
                  prior = "g-prior",
                  method="MCMC",
                  MCMC.iterations=1e5,
                  # n.models = 2e4,
                  #modelprior=uniform(),
                  #alpha = n
)

sum(moy3)
sum(modBAS_1$probne0)

#Comparaison entre les selection de modèle de gamma et celles du package BAS
comparre_prob = data.frame(
  Prob_Dm_procedure = moy3,
  Prob_Package_BAS = modBAS_1$probne0)

#la somme des probabilité n'est pas la même, on regarde en renormalisant à 1 sur les varaibles (sans l'intercept)
comparre_prob2 = data.frame(
  Prob_Dm_procedure = moy3/(sum(moy3)-1),
  Prob_Package_BAS = modBAS$probne0/(sum(modBAS$probne0)-1),
  VarID = 0:17)

comparre_prob2$Prob_Dm_procedure[1] = 1
comparre_prob2$Prob_Package_BAS[1] = 1

dataGrf = gather(comparre_prob2,"Prob_Dm_procedure","Prob_Package_BAS", key = "Algorythme" , value = "prob") %>% filter(VarID!=0)

ggplot(dataGrf , aes(x = Algorythme , y = prob )) +
  geom_bar(stat='identity', aes(fill=Algorythme)) +
  facet_grid(~VarID) +
  theme(axis.text.x = element_blank()) +
  xlab("# Variable") +
  ylab("Probabilité de selection de la variable")


```


Tous d'abord lorsque l'on compare la somme des probabilité pour charque variables de modèles (hors intercepte):
  * Pour notre procédure on obtient 1,69 : donc en moyenne toujours une variable ce qui est logique et une dexieme ou plus de temps en temps
* Pour le package BAS on obtiens 0,55 : ainsi on comprends que l'algorythme fait de la selection de modèle et rejete dans de nombreux cas certains modèles et lui préfere un modèle nul (avec intercept)

Afin de comparer les férquences de selection pour chaques variables, on normalise les probabilité à 1 (hors intercept)

```{r , echo=FALSE}
#Comparaison entre les selection de modèle de gamma et celles du package BAS
comparre_prob = data.frame(
Prob_Dm_procedure = moy3,
Prob_Package_BAS = modBAS$probne0)

#la somme des probabilité n'est pas la même, on regarde en renormalisant à 1 sur les varaibles (sans l'intercept)
                                                                                                comparre_prob2 = data.frame(
                                                                                                Prob_Dm_procedure = moy3/(sum(moy3)-1),
                                                                                                Prob_Package_BAS = modBAS$probne0/(sum(modBAS$probne0)-1),
                                                                                                VarID = 0:17)
                                                                                                
                                                                                                comparre_prob2$Prob_Dm_procedure[1] = 1
                                                                                                comparre_prob2$Prob_Package_BAS[1] = 1
                                                                                                
                                                                                                dataGrf = gather(comparre_prob2,"Prob_Dm_procedure","Prob_Package_BAS", key = "Algorythme" , value = "prob") %>% filter(VarID!=0)
                                                                                                
                                                                                                ggplot(dataGrf , aes(x = Algorythme , y = prob )) +
                                                                                                geom_bar(stat='identity', aes(fill=Algorythme)) +
                                                                                                facet_grid(~VarID) +
                                                                                                theme(axis.text.x = element_blank()) +
                                                                                                xlab("# Variable") +
                                                                                                ylab("Probabilité de selection de la variable")
                                                                                                
                                                                                                ```
                                                                                                
                                                                                                Dans la figure ci-dessus, on à comparé les probabilités de selection des variable avec notre procédure MCMC et celle du package `BAS` :
                                                                                                * Globalement on constate que la selection de modèle pénalise les variables qui avaient deja une faible probabilité de séléchion avec notre algorythme. Notamment pour les variables 1 à 5, 10 et 11.
                                                                                                * Pour les variables qui avaient déja une probabilité importante d'être selectionné dans notre algorythme, le package favirose ces modeles et les selectionne plus souvent les variables: Notamment pour les variables 9,10,13 et 15
* Pour les autres variables la porabilité d'etre dans les modèles est plutot similaire, sauf pour la variables 7 (taux_reussite_attendu_serie_l) qui est plus de 2 fois moins souvent séléctionné avec le package qu'avec notre algorythme.

Pour conclure, nous avons exploré sous divers angles les résultats produit par notre algorythme MCMC et regression bayésienne.Cela nous à permi de comprendre la selectionde modèle et l'influance sur le calcul des $ \beta_{i} $ de la regression linéaire. Pour les question suivantes nous privilégions l'utilisation du package pour pouvoir aller plus loin dans nos analyses avec des algorythme plus puissance déja existants.

## 1.2 Choix des covariables significatives.









## 2.5 Choix d'une loi à priori

** Relation entre la loi exponentielle et Pareto **
  
  Les distributions de Pareto et exponentiel sont liés entre elle de la manière suivante :
  
  Si $Z$ suit une distribution de Pareto, alors $Y = log(X/{m})$ sui une distribution exponentiel. On à bien $\lambda > 0$ car $\alpha > 0$.

Ainsi on peu se place dans le cas de la loi exponentiel que l'ont connait et dont la loi congugué est une loi gamma.

On choisi donc la loi é priori suivante:

$$
\pi(\alpha)=\lambda e^{- \lambda\alpha}\quad ,   \lambda > 0
$$


## 2.5 Calcul de la loi à posteriorie 

$$
\begin{aligned}
\pi(\alpha|z)&\propto\lambda e^{-\lambda\alpha}L(\alpha|z_{1},z_{2}.....z_{n})\\
&\propto\lambda e^{-\lambda\alpha}\prod_{i=1}^{n}f_{Z}(z_{i};m;\alpha)\\
&\propto\lambda e^{-\lambda\alpha}\prod_{i=1}^{n}\alpha\frac{m^{\alpha}}{(z_{i})^{\alpha+1}}
=\lambda e^{-\lambda\alpha}\alpha^n\prod_{i=1}^{n}\frac{e^{\alpha\ln(m)}}{e^{(\alpha+1)\ln(z_{i})}}\\
&\propto\lambda\alpha^n\exp\left[-\lambda\alpha+\sum_{i=1}^{n}\bigg(\alpha\ln(m)-(\alpha+1)\ln(z_{i})\bigg)\right]\\
&\propto\lambda\alpha^n\exp\left[-\alpha\bigg(\lambda+\sum_{i=1}^{n}\ln(\frac{z_{i}}{m})\bigg)\right]\exp\left[-\sum_{i=1}^{n}\ln(z_{i})\right]\\
&\propto\alpha^n\exp\left[-\alpha\bigg(\lambda+\sum_{i=1}^{n}\ln(\frac{z_{i}}{m})\bigg)\right]\\
&\propto\Large\Gamma(\small n+1,\lambda+\sum_{i=1}^{n}\ln(\frac{z_{i}}{m})\Large)
\end{aligned}
$$

On retrouve bien notre changement de variable et uneloi gamma conjuguée. On remarque également que si les données suivent un modèle de loi de Paréto de paramètre $\alpha$, alors les données transformés suivent un cas particulier de la de la loi congugué gamma : $\Gamma (a,b)$ avec $a=1$ et $b=\alpha$

Finalement, la logpostérior est comme suit:

$$
\ln \pi(\alpha|z_{i}) = C + n\ln\alpha -\alpha\bigg(\lambda+\sum_{i=1}^{n}\ln(\frac{z_{i}}{m})\bigg)
$$
Avec $C$ une constante.

## 2.5 Tirage d'un echantillon de la loi à postériorie de $\alpha$
  
  
  
  
  
  
  
  ## 2.8 Bayesien sur le paramètre $m$ sur les données des matières Mathématiques et Anglais uniquement.
  
  On cherche à estimer la valeur de $m$ pour la distribution de Pareto suivantes:
  
  
  $$
  f_{Z}(z;m;\alpha)=\alpha\frac{m^{\alpha}}{z^{\alpha+1}}\mathbb{I}_{z\geq m}
$$
  
  On à vu précédement que la postérieur pour ce modèle de données est pour une prior $Pareto(a,b)$ et $\alpha$ calculé précédement:
  
  $$
  \pi(m|z)\propto\frac{(a-\alpha n).b^{(a-\alpha n)\alpha}}{m^{a-\alpha n+1}}; a>\alpha n , b> \frac{1}m
$$
  Ce qui donne la logposterieur:
  
  $$
  \alpha . log(b) .(a-\alpha n )-log(m).(a-\alpha*n+1)+log(a-\alpha n)
$$
  ne marche pas.



```{r , echo=FALSE}
\alpha
```

```{r , echo=FALSE}


```

```{r , echo=FALSE}


```





```{r , echo=FALSE}
#par(mfrow = c(2,2))
plot(confint(coef(modBAS_2), parm = c(1:4)),xaxt='n' )
#plot(confint(coef(modBAS_2,estimator = "HPM"), parm = c(1:4)))
#plot(confint(coef(modBAS_2, estimator = "MPM"), parm = c(1:4)))

```


Dans les question précédentes on à exclus les variables catégorielles, notamment `etablissement` et `matiere` alors que les statistiques descriptives nous ont données l'intuition de leur forte influence sur la variables `Barre`. 


```{r , echo=FALSE}

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
  
  
