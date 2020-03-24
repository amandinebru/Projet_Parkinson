#Lecture de la base de donnees Voice4PD
library(nnet)
library(AER)
library(MASS)
library(ggplot2)
D <- read.table("Voice4PD.csv",header=T,sep=";",dec=",",as.is=T)
D[,1] <- NULL
colnames(D) <- c("Classe","Atonie","Debit","Irreg","Puissance")
D[,1] <- as.factor(D[,1])
summary(D)

#On observe les effets des variables sur la reponse
hist(D$Atonie)
plot(D$Atonie ~ D$Classe)
plot(D$Debit ~ D$Classe)
plot(D$Irreg ~ D$Classe)
plot(D$Puissance ~ D$Classe)

ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Atonie>median(Atonie)))   
#On observe un effet de la variable Atonie sur la reponse

ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Debit>median(Debit)))
#On observe un effet de la variable Debit sur la reponse

ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Irreg>median(Irreg)))
#On observe un effet de la variable Irreg sur la reponse

ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Puissance>median(Puissance)))
#On observe un effet de la variable Puissance sur la reponse

ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Atonie>median(Atonie))+(Debit>median(Debit)))    
ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Atonie>median(Atonie))+(Irreg>median(Irreg)))    
ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Atonie>median(Atonie))+(Puissance>median(Puissance)))    
ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Debit>median(Debit))+(Irreg>median(Irreg)))    
ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Debit>median(Debit))+(Puissance>median(Puissance)))    
ggplot(data = D, mapping = aes(x = Classe, fill = Classe)) + geom_bar(position ="dodge") + facet_wrap(~(Irreg>median(Irreg))+(Puissance>median(Puissance)))    

#On regarde si les variables sont indépedantes
Atonie = D$Atonie
Debit = D$Debit
Irreg = D$Irreg
Puissance = D$Puissance


khi_test1 = chisq.test(table(Atonie,Debit)); khi_test1 #Atonie et Debit pas indep
cor(Atonie,Debit)
khi_test2 = chisq.test(table(Atonie,Irreg)); khi_test2 #Atonie et Irreg indep
cor(Atonie,Irreg)
khi_test3 = chisq.test(table(Atonie,Puissance)); khi_test3 #Atonie et Puissance indep
cor(Atonie,Puissance)
khi_test4 = chisq.test(table(Debit,Irreg)); khi_test4 #Debit et Irreg indep
cor(Debit,Irreg)
khi_test5 = chisq.test(table(Debit,Puissance)); khi_test5 #Debit et Puissance indep bof
cor(Debit,Puissance)
khi_test6 = chisq.test(table(Irreg,Puissance)); khi_test6 #Irreg et Puissance indep
cor(Irreg,Puissance)

#On teste un modele de regression multinomiale

test.ratio = 0.2 #part de l'echantillon test
npop = nrow(D) #nombre de lignes dans le dataframe
nvar = ncol(D) #nombre de colonnes
ntest = ceiling(npop*test.ratio) #taille de l'echantillon test
testi = sample(1:npop,ntest) # indices de l'échantillon test
appri=setdiff(1:npop,testi) # indices de l'échantillon d'apprentissage

datappr=D[appri,] # construction de l'échantillon d'apprentissage
datest=D[testi,-1] # construction de l'échantillon test

summary(datappr)



regMult <- multinom(Classe ~ .,data=D,Hess=T);
summary(regMult)

head(regMult$fitted.values)
pr = predict(regMult,D)
predictTab = table(D[,1],pr)
print(predictTab)
classRate <- sum(diag(predictTab))/sum(predictTab)
print(classRate)

#v1 <- c(3.11,2.37,1.19,1.10)
#v2 <- c(1,2.86,1.39,1.64)
#v3 <- c(1.73,2.02,0.87,1.45)

#test sur 3 nouvelles valeurs
new_val <- data.frame(Atonie=c(3.11,1,1.73), Debit=c(2.37,2.86,2.02),Irreg=c(1.19,1.39,0.87),Puissance=c(1.10,1.64,1.45))
summary(new_val)
pred <- predict(regMult, new_val)
print(pred) #MSA,MSA,MSA ou #MSA,PSP,MSA

#ON VA TESTER CE MODELE UNE 100AINE DE FOIS

vectClassRate = c()
for (i in 1:1000){
  test.ratio = 0.2 #part de l'echantillon test
  npop = nrow(D) #nombre de lignes dans le dataframe
  nvar = ncol(D) #nombre de colonnes
  ntest = ceiling(npop*test.ratio) #taille de l'echantillon test
  testi = sample(1:npop,ntest) # indices de l'échantillon test
  appri=setdiff(1:npop,testi) # indices de l'échantillon d'apprentissage
  
  datappr=D[appri,] # construction de l'échantillon d'apprentissage
  datest=D[testi,-1] # construction de l'échantillon test
  regMult <- multinom(Classe ~ .,data=datappr,Hess=T);
  
  pr = predict(regMult,datest)
  predictTab = table(D[testi,1],pr)
  print(predictTab)
  classRate <- sum(diag(predictTab))/sum(predictTab)
  print(classRate)
  vectClassRate <- c(vectClassRate,classRate)
}

hist(vectClassRate)
mean(vectClassRate) #0.445


###TEST 1 ATONIE=0 ?
#H0: Atonie = 0 ; H1: Atonie /= 0

regMult1 = multinom(Classe ~ Debit+Irreg+Puissance,data=D)
summary(regMult1)
rv1 = regMult1$deviance - regMult$deviance
ddl1 = regMult$edf - regMult1$edf
pvaleur1 = 1 - pchisq(rv1,ddl1)
print(c(rv1,ddl1,pvaleur1))

#pval = 0.01 < 0.05 donc on rejette H0. Il y a bien un effet de l'atonie sur la classification des malades.


###TEST 2 DEBIT=0 ?
#H0: Debit = 0 ; H1: Debit /= 0

regMult2 = multinom(Classe ~ Atonie+Irreg+Puissance,data=D)
rv2 = regMult2$deviance - regMult$deviance
ddl2 = regMult$edf - regMult2$edf
pvaleur2 = 1 - pchisq(rv2,ddl2)
print(c(rv2,ddl2,pvaleur2))

#pval = 0.0067 < 0.05 donc on rejette H0. Il y a bien un effet du débit sur la classification des malades.


###TEST 3 IRREG=0 ?
#H0: Irreg = 0 ; H1: Irreg /= 0

regMult3 = multinom(Classe ~ Atonie+Debit+Puissance,data=D)
rv3 = regMult3$deviance - regMult$deviance
ddl3 = regMult$edf - regMult3$edf
pvaleur3 = 1 - pchisq(rv3,ddl3)
print(c(rv3,ddl3,pvaleur3))

#pval = 0.0015 < 0.05 donc on rejette H0. Il y a bien un effet de l'irrégularité sur la classification des malades.


###TEST 4 PUISSANCE=0 ?
#H0: Puissance = 0 ; H1: Puissance /= 0

regMult4 = multinom(Classe ~ Atonie+Debit+Irreg,data=D)
rv4 = regMult4$deviance - regMult$deviance
ddl4 = regMult$edf - regMult4$edf
pvaleur4 = 1 - pchisq(rv4,ddl4)
print(c(rv4,ddl4,pvaleur4))

#pval = 0.0077 < 0.05 donc on rejette H0. Il y a bien un effet de la puissance sur la classification des malades.



#MODELE AVEC INTERACTIONS
regMultInter <- multinom(Classe ~ .^2,data=D,Hess=T);
summary(regMultInter)

head(regMultInter$fitted.values)
prInter = predict(regMultInter,D)
table(D[,1],prInter)

coeftest(regMultInter)
#Clairement pas terrible avec un AIC trop élevé


regMultStep = stepAIC(regMultInter,direction="backward")
#avec le critere AIC, toutes les interactions ont été enlevees on revient donc au modele de base


#ON VA TESTER LA CLASSIFICATION RATE SUR LE MODELE AVEC INTERCATIONS (inutile normalement)
vectClassRateInter = c()
for (i in 1:1000){
  test.ratio = 0.2 #part de l'echantillon test
  npop = nrow(D) #nombre de lignes dans le dataframe
  nvar = ncol(D) #nombre de colonnes
  ntest = ceiling(npop*test.ratio) #taille de l'echantillon test
  testi = sample(1:npop,ntest) # indices de l'échantillon test
  appri=setdiff(1:npop,testi) # indices de l'échantillon d'apprentissage
  
  datappr=D[appri,] # construction de l'échantillon d'apprentissage
  datest=D[testi,-1] # construction de l'échantillon test
  regMultInter <- multinom(Classe ~ .^2,data=datappr,Hess=T);
  
  pr = predict(regMultInter,datest)
  predictTab = table(D[testi,1],pr)
  print(predictTab)
  classRate <- sum(diag(predictTab))/sum(predictTab)
  print(classRate)
  vectClassRateInter <- c(vectClassRateInter,classRate)
}
hist(vectClassRateInter)
mean(vectClassRateInter)#0.434



#AVEC DES INTERACTIONS A L ORDRE 3
regOrdre3 <- multinom(Classe ~ .^3,data=D,Hess=T);
summary(regMultInter)
prInter = predict(regOrdre3,D)
table(D[,1],prInter)
classRate <- sum(diag(predictTab))/sum(predictTab)
print(classRate)
regMultStep3 = stepAIC(regOrdre3,direction="backward")
summary(regMultStep3)


vectClassRate3 = c()
for (i in 1:1000){
  test.ratio = 0.2 #part de l'echantillon test
  npop = nrow(D) #nombre de lignes dans le dataframe
  nvar = ncol(D) #nombre de colonnes
  ntest = ceiling(npop*test.ratio) #taille de l'echantillon test
  testi = sample(1:npop,ntest) # indices de l'échantillon test
  appri=setdiff(1:npop,testi) # indices de l'échantillon d'apprentissage
  
  datappr=D[appri,] # construction de l'échantillon d'apprentissage
  datest=D[testi,-1] # construction de l'échantillon test
  regMultStep3 <- multinom(Classe ~ Atonie + Debit + Irreg + Puissance + Atonie:Debit + 
                             Atonie:Irreg + Atonie:Puissance + Debit:Irreg + Debit:Puissance + 
                             Irreg:Puissance + Atonie:Debit:Puissance + Debit:Irreg:Puissance,data=datappr,Hess=T);
  
  pr = predict(regMultStep3,datest)
  predictTab = table(D[testi,1],pr)
  print(predictTab)
  classRate <- sum(diag(predictTab))/sum(predictTab)
  print(classRate)
  vectClassRate3 <- c(vectClassRate3,classRate)
}
hist(vectClassRate3)
mean(vectClassRate3)#0.443


#NAIVE BAYES CLASSIFIER
library(e1071)

Naive_Bayes_Model <- naiveBayes(Classe ~ .,data=D)
Naive_Bayes_Model
p = predict(Naive_Bayes_Model,D[,-1])
predictTab = table(D[,1],p)
predictTab
classRate <- sum(diag(predictTab))/sum(predictTab)
classRate


vectClassRateB = c()
for (i in 1:10000){
  test.ratio = 0.2 #part de l'echantillon test
  npop = nrow(D) #nombre de lignes dans le dataframe
  nvar = ncol(D) #nombre de colonnes
  ntest = ceiling(npop*test.ratio) #taille de l'echantillon test
  testi = sample(1:npop,ntest) # indices de l'échantillon test
  appri=setdiff(1:npop,testi) # indices de l'échantillon d'apprentissage
  
  datappr=D[appri,] # construction de l'échantillon d'apprentissage
  datest=D[testi,-1] # construction de l'échantillon test
  
  Naive_Bayes_Model <- naiveBayes(Classe ~ .,data=datappr)
  Naive_Bayes_Model
  p = predict(Naive_Bayes_Model,datest)
  predictTab = table(p,D[testi,1])
  predictTab
  classRate <- sum(diag(predictTab))/sum(predictTab)
  classRate 
  vectClassRateB <- c(vectClassRateB,classRate)
}
hist(vectClassRateB)
mean(vectClassRateB) #0.44
