## ----------
## Exercise 1 - Interaction with continuous variables and outputs.
# Visualize the effects of a single variable onto the response.
# Visualize the second-order interactions onto the response (using package lattice and ggplot2).
# Deduce from these plots a linear model with only one term, for which R^2 > 0.9 
## ----------
library(ggplot2)
library(lattice)
rm(list = ls())
df1 <- read.table("df1.txt")
str(df1)
m1 <- lm(y ~ ., data = df1)
summary(m1)

plot(y ~ X1 + X2, data = df1, ask = FALSE)

ggplot(data = df1, mapping = aes(x = X1, y = y, col = (X2>0.003692))) + geom_point() + geom_smooth(method = "lm")

ggplot(data = df1, mapping = aes(x = X2, y = y, col = (X3>0.10303))) + geom_point() + geom_smooth(method = "lm")#grosse inter entre x2 et x3 (on prend la mediane tmtc)
ggplot(data = df1, mapping = aes(x = X3, y = y, col = (X4>-0.003181))) + geom_point() + geom_smooth(method = "lm")

ggplot(data = df1, mapping = aes(x = X2, y = y, col = (X3>0.10303))) +geom_point() + geom_smooth(method = "lm") + facet_wrap(~(X3>0.10303))

mod1_2<- lm(y ~X1+X2*X3+X4,data=df1)
summary(mod1_2)

mod1_3<- lm(y ~X2*X3,data=df1)
summary(mod1_3)


# y= a + b*X2X3 + epsilon
mod1_4<- lm(y ~X2:X3,data=df1)
summary(mod1_4)


mod1_5 <- lm(y ~ .^2, data = df1)
summary(mod1_5)
#on garde bien que la bonne interaction hihihi


## ----------
## Exercise 2 - Interaction with a continuous inputs and a categorical output.
# Visualize the effect of X4 onto the response.
# Study graphically if there is a second-order interaction X1:X4 onto the response.
# Same question with X2:X4. Which one is the most important?
# Deduce from these plots a logistic model with only one term, 
# with a classification rate > 0.85 
## ----------
df2 <- read.table("df2.txt")
str(df2)
df2$y <- as.factor(df2$y)
m2 <- glm(y ~ ., family = binomial, data = df2)
summary(m2)
p <- predict(m2, type = "response")
predictTab <- table(p > 0.5, df2$y)
print(predictTab)
classRate <- sum(diag(predictTab))/sum(predictTab)
print(classRate)

ggplot(data = df2, mapping = aes(x = y, fill = y)) + geom_bar(position ="dodge") + facet_wrap(~(X4>median(df2$X4)))   #X4 has not a real influence on y

ggplot(data = df2, mapping = aes(x = y, fill = y)) + geom_bar(position ="dodge") + facet_wrap(~(X1>median(X1))+(X4>median(X4)))     #no interaction
ggplot(data = df2, mapping = aes(x = y, fill = y)) + geom_bar(position ="dodge") + facet_wrap(~(X2>median(X2))+(X4>median(X4)))     #interaction ok

mod2_2 <- glm(y ~ X2:X4, family = binomial, data = df2)
summary(mod2_2)

p2 <- predict(mod2_2, type = "response")
predictTab2 <- table(p2 > 0.5, df2$y)
print(predictTab2)
classRate2 <- sum(diag(predictTab2))/sum(predictTab2)
print(classRate2)


## ----------
## Exercise 3 - Interaction with a categorical input and a continuous output.
# Visualize the effects of a single variable onto the response.
# Visualize the second-order interactions onto the response (using package lattice and ggplot2).
# Deduce from these plots a linear model with only one term, for which R^2 > 0.9 
## ----------
df3 <- read.table("df3.txt")
str(df3)
m3 <- lm(y ~ ., data = df3)
summary(m3)
plot(y ~ X1, data = df3, ask = FALSE) 
plot(y ~ X2, data = df3, ask = FALSE) #pas d'effets visibles
plot(y ~ X3, data = df3, ask = FALSE) #Aya un effet
plot(y ~ X4, data = df3, ask = FALSE) #pas d'effets visibles

ggplot(data = df3, mapping = aes(x = X3, y = y, col = (X2>median(X2)))) + geom_point() + geom_smooth(method = "lm")
ggplot(data = df3, mapping = aes(x = X4, y = y, col = (X3>median(X3)))) + geom_point() + geom_smooth(method = "lm")
ggplot(data = df3, mapping = aes(x = X2, y = y, col = (X4>median(X4)))) + geom_point() + geom_smooth(method = "lm")

plot(X2~X1, data=df3)
plot(X3~X1, data=df3)
plot(X4~X1, data=df3)

ggplot(data = df3, mapping = aes(x = X3, y = y, col =X1 )) + geom_point() + geom_smooth(method = "lm")   #interaction flagrante
ggplot(data = df3, mapping = aes(x = X2, y = y, col =X1 )) + geom_point() + geom_smooth(method = "lm")
ggplot(data = df3, mapping = aes(x = X4, y = y, col =X1 )) + geom_point() + geom_smooth(method = "lm")

mod3_2 <- lm(y ~ X1:X3, data = df3)
summary(mod3_2)


## ----------
## Exercise 4 - Interaction with continuous variables and outputs.
# Similar to Exercise 1, but more subtile!
# Here you may need to condition on several regions (use 'cut').
# Deduce from these plots a linear model with only one term, for which R^2 > 0.9
## ----------
df4 <- read.table("df4.txt")
str(df4)
m4 <- lm(y ~ ., data = df4)
summary(m4)

plot(y~X1,data=df4)
plot(y~X2,data=df4)
plot(y~X3,data=df4)
plot(y~X4,data=df4)

ggplot(data = df4, mapping = aes(x = X1, y=y, col=cut(X3,4))) +geom_point()  + facet_wrap(~cut(X3,4))  #quand X3 grand en valeur absolue, courbe plus grande
ggplot(data = df4, mapping = aes(x = X3, y=y, col=cut(X1,4))) +geom_point()  + facet_wrap(~cut(X1,4)) 
# y=a+bX1²X2²

ggplot(data = df4, mapping = aes(x = X1^2, y=y, col=cut(X3^2,2))) +geom_smooth(method="lm") +geom_point() + facet_wrap(~cut(X3^2,2)) 

xyplot(y~I(X1^2)|cut(I(X3^2),2),data=df4)

mod4_2=lm(y~I(X1^2)+I(X3^2)+I(X1^2):I(X3^2),data=df4)
summary(mod4_2)

## ----------
## Exercise 5 - Interaction with continuous variables and outputs.
# Look also for an interaction.
# Deduce a linear model with only one term, for which R^2 > 0.9. 
## ----------
df5 <- read.table("df5.txt")
str(df5)
m5 <- lm(y ~ ., data = df5)
summary(m5)

plot(y~X1,data=df5)
plot(y~X2,data=df5)
plot(y~X3,data=df5)
plot(y~X4,data=df5)

ggplot(data = df5, mapping = aes(x = X1, y=y, col=cut(X2*X4,4))) +geom_point()  + facet_wrap(~cut(X2*X4,4))


#technique pour trouver direct l'interaction tester les differents niveaux d'interaction 
mod5_2<-lm(y~.,data=df5)
summary(mod5_2)
mod5_3<-lm(y~.^2,data=df5)
mod5_4<-lm(y~.^3,data=df5) #on rq que X1:X2:X4 est un terme significatif 
summary(mod5_4)
