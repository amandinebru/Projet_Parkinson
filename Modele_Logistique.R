library(ggplot2)
###TEST AVEC UNE INTERPRETATION BINAIRE
D <- read.table("Voice4PD.csv",header=T,sep=";",dec=",",as.is=T)
D[,1] <- NULL
colnames(D) <- c("Classe","Atonie","Debit","Irreg","Puissance")
D$Classe[D$Classe %in% c('HC','PD')]<-0
D$Classe[D$Classe %in% c('MSA','PSP')]<-1
D[,1] <- as.factor(D[,1])
summary(D)


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
RegLog <- glm(Classe ~ ., family = binomial, data = D)
summary(RegLog)
p <- predict(RegLog, type = "response")
predictTab <- table(p > 0.5, D$Classe)
print(predictTab)
classRate <- sum(diag(predictTab))/sum(predictTab)
print(classRate)


v1 <- c(3.11,2.37,1.19,1.10)
v2 <- c(1,2.86,1.39,1.64)
v3 <- c(1.73,2.02,0.87,1.45)

#test sur 3 nouvelles valeurs
new_val <- data.frame(Atonie=c(3.11,1,1.73), Debit=c(2.37,2.86,2.02),Irreg=c(1.19,1.39,0.87),Puissance=c(1.10,1.64,1.45))
summary(new_val)

pred <- c(predict(RegLog, new_val, type = "response") > 0.5)
print(pred) #on obtient trois "1"


RegLogStep = step(RegLog,direction="backward")
