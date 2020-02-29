#Lecture de la base de donnees Voice4PD
library(nnet)
library(AER)
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


#On teste un modele de regression multinomiale

regMult <- multinom(Classe ~ .,data=D,Hess=T);
summary(regMult)

head(regMult$fitted.values)
pr = predict(regMult,D)
table(D[,1],pr)

coeftest(regMult)


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

#Modele avec interactions
regMultInter <- multinom(Classe ~ .^2,data=D,Hess=T);
summary(regMultInter)

head(regMultInter$fitted.values)
prInter = predict(regMultInter,D)
table(D[,1],prInter)

coeftest(regMultInter)
#Clairement pas terrible avec un AIC trop élevé






###TEST AVEC UNE INTERPRETATION BINAIRE
D <- read.table("Voice4PD.csv",header=T,sep=";",dec=",",as.is=T)
D[,1] <- NULL
colnames(D) <- c("Classe","Atonie","Debit","Irreg","Puissance")
summary(D)
D$Classe[D$Classe %in% c('HC','PD')]<-0
D$Classe[D$Classe %in% c('MSA','PSP')]<-1
D[,1] <- as.factor(D[,1])
D$Classe
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

#Modele logistique
m2 <- glm(Classe ~ ., family = binomial, data = D)
summary(m2)
p <- predict(m2, type = "response")
predictTab <- table(p > 0.5, D$Classe)
print(predictTab)
classRate <- sum(diag(predictTab))/sum(predictTab)
print(classRate)
