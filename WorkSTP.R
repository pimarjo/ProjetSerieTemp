#### On en profite pour mettre en place les packages et les seeds.
library(knitr)
library(magrittr) 
library(dplyr)
library(corrplot)
library(forecast)
library(gridExtra)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(forecast)
library(tseries)

#fonctions utiles
ts.affichage <- function(ts, lag.max = 48,title="Mon titre"){
  split.screen(c(2,1))
  screen(1)
  plot(ts,main=title)
  
  split.screen(c(1,2),screen=2)
  screen(3)
  acf(ts,lag.max = lag.max,main="ACF",na.action = na.pass)
  
  screen(4)
  pacf(ts,lag.max = lag.max,main="pACF",na.action = na.pass)
  close.screen(all = TRUE)
}


data <- read.csv("Donnees.csv", sep = ";", header = T)


#Introduction

#L'objet de ce projet est l'?tude de 3 s?ries temporelles et la construction pour chaque s?rie d'un mod?le de pr?diction. Tous les outils d?velopp?s dans ce projet d?coulent du cours de S?ries Temporelles. 
#Nous commencerons par pr?senter les donn?es, puis nous r?serverons une partie d?di?e par s?rie temporelle a analyser. Enfin dans une derniere partie, nous r?sumerons les r?sultats des pr?dictions et les observations qu'elles appellent.




names(data) <- c("Periode",
                 "Production.Totale.Brute",
                 "Production.Totale.Nette",
                 "Production.Primaire.Brute",
                 "Production.Primaire.Nette",
                 "Production.Nucleaire.Brute",
                 "Production.Nucleaire.Nette",
                 "Production.Hydraulique.Brute",
                 "Production.Hydraulique.Nette",
                 "Production.Eolienne.Brute",
                 "Production.Eolienne.Nette",
                 "Production.Photovoltaique.Brute",
                 "Production.Photovoltaique.Nette",
                 "Production.Thermique.Brute",
                 "Production.Thermique.Nette",
                 "Electricite.absorbee.pour.les.pompages.(en.GWh)",
                 "Importations",
                 "Exportations",
                 "Energie.appelee.reelle.yc.pertes",
                 "Livraisons.BasseTension",
                 "Livraisons.MoyenneTension",
                 "Livraisons.HauteTension",
                 "Energie.appelee.reelle.yc.pertes.corrigee.du.climat",
                 "Livraisons.BasseTension.CorClimat",
                 "Livraisons.MoyenneTension.CorClimat",
                 "Consommation.Primaire",
                 "Consommation.Primaire.CorClimat",
                 "Puissance.maximale.appelee",
                 "Indice.Climatique"
)



ts <- list(prod.totale = ts(data$Production.Totale.Brute, frequency = 12, start = c(1981, 1), end = c(2017, 11)),
           import = ts(data$Importations, frequency = 12,  start = c(1981,1), end = c(2017, 11)),
           photo = ts(data$Production.Photovoltaique.Brute[data$Production.Photovoltaique.Brute!=0], frequency = 12, start = c(2011,1), end = c(2017,11)))


##############################################################################################
#############______   Partie 1: la production totale brute d'electricite   ______#############
##############################################################################################

##Representation.


plot(ts$prod.totale, ylab = "Production totale d'electricite brute", type = "l")
#Cette repr?sentation appelle d?ja plusieurs observations:
#  - Pr?sence d'une saisonnalit? d'amplitude constante: on se place alors de la cadre des mod?les additifs
#  - Le trend ne semble pas lin?aire
# EDIT: log? racine?

#Avant de se lancer brutalement dans l'estimation d'un mod?le pour cette s?rie temporelle, on peut essayer de s'int?resser aux valeurs qui composent cette production globale brute.
#Dans cet esprit, on remarque que cette s?rie est la somme des productions brutes. Cependant, les s?ries n'?tant pas ind?pendantes, il serait laborieux de d?velopper plus sur cette voie la.
#Nous d?cidons donc de malgr? tout nous lancer directement dans l'?tude de la s?rie brutalement.

#cor <- cor(data.production.nette[-1])
#corrplot(corr = cor)


##Stationnarit?

#La série n'est clairement pas stationnaire. Pour se rassurer on peut regardes l'acf et le pacf

par(mfrow = c(1,2))
acf(ts$prod.totale)
pacf(ts$prod.totale)


##Decompose()

decompose.prod.totale <- decompose(ts$prod.totale, type = "additive") %T>% plot(.)


acf(decompose.prod.totale$random, na.action = na.pass)
pacf(decompose.prod.totale$random, na.action = na.pass)
#Le bruit n'est pas stationnaire et présente une saisonnalité, le trend n'est pas linéaire

#############______  SARIMA  ______#############

#On différencie la saisonnalité


close.screen(all = T)
ts$prod.totale %>% diff(.,12) %T>% ts.affichage(title = "Production Brute Total d=0, D=1") %>% kpss.test(.)
#L'analyse de l'ACF et du pACF ne nous permet de conclure nettement en faveur de la stationnarité, le test KPSS nous le confirme

#On différencie alors encore et on test la stationnarité
ts$prod.totale %>% diff() %>% diff(.,12) %T>% ts.affichage(title = "Production Brute Total d=1, D=1") %>% kpss.test(.)
# p-value > 0.05, on accepte l'hypothèse nulle de stationnarité

#On enregistre alors la série différenciée
prod.totale.mod1 <- ts$prod.totale %>% diff() %>% diff(.,12)


#Maintenant on analyse l'ACF et le pACF
#Sur l'ACF on va chercher les MA et sur le pACF les AR

#Sur l'ACF: le premier pic après 0 est en faveur d'un MA(1), et le pique en 1 en faveur d'un SMA(1)
#Sur le pACF: le premier pic est en faveur d'un AR(1) et le pic en 1 en faveur d'un SAR(1)

#On fit donc un SARIMA(1,1,1)(1,1,1)_12
fit <- arima(ts$prod.totale, order=c(1,1,1), seasonal=c(1,1,1))
ts.affichage(residuals(fit), title = "Résidus SARIMA(1,1,1)(1,1,1)")
#L'ACF et le PACF sont plutôt satisfaisant

#On test la stationnarité des résidus
kpss.test(residuals(fit))
#on accpete l'hypothèse nulle de stationnarité


fit
#Tous les coefficients sont significatifs

#Il nous faut tester la blancheur des résidus
#Test Ljung-Box
x <- rep(0, 2, 48)
for (i in 1:48){
  x[i]<- Box.test(residuals(fit), lag=i, fitdf=4, type="Ljung")$p.value
}
plot(x)
#Nous avons un souci pour le lag qui ne passe pas le test. Sinon pour tous les autres, on accepte l'hypothèse d'absence d'autocorrélation



### Test de normalité
#on centre et réduit les résidus:
prod.totale.fit.res.norm <- (residuals(fit)-mean(residuals(fit)))/sd(residuals(fit))

#qq plot test: il faut que ce soit aligné sur la première bissectrice du plan
qqnorm(prod.totale.fit.res.norm)
abline(0,1, col = "red")
#Test de Kolmogorov Smirnov
ks.test(prod.totale.fit.res.norm, 'pnorm') #on accepte (p-value > 0.05)

#Test de Shapiro-Wilk
shapiro.test(prod.totale.fit.res.norm) #on refuse (p-value < 0.05). Cela est probablement du aux queues de distribution 


#On projette sur 12 mois
predict.arima <- forecast(fit, h=12)
#On plot
plot(predict.arima)


#############______  2e méthode: lissage Holt-Winters  ______#############

#On fit un Holt-Winters basique sur la série
HW.model <- HoltWinters(ts$prod.totale)

#On projette
HW.predict <- predict(object = HW.model, 36, level = 0.95, prediction.interval = F) #Mettre prediction.interval = T si on veut l'intervalle de confiance

#On plot
plot(HW.model, HW.predict)



#On plot les prédictions du SARIMA et du Holt-Winters qur le même graphe
########>>>>>>>>>>>>> Je n'y arrive pas

#On compare les 2 méthodes en comparant les SSE, c'est le SARIMA qui gagne à ce jeu là
sum(fit$residuals^2) < HW.model$SSE


##############################################################################################
#############______   Partie 2: IMPORT   ______#############
##############################################################################################

#------- plot de la s?rie --------
plot(ts$import)       #il semble que la s?rie soit de type multiplicative car l'amplitude de la saisonalit? n'est pas constante
plot(log(ts$import))  #On passe au log pour la rendre additive : OK

ts <- c(ts,list(logimpor = log(ts$import)))
#------- ACF , PACF ----------
acf(ts$logimpor)        #pas stationnaire en l'?tat
pacf(ts$logimpor)

#------- Decompose ---------

logimport.decompose <- decompose(ts$logimpor,type = "additive") 
acf(logimport.decompose$random, na.action = na.pass)  # A d?faut que la composante al?atoire ne soit pas un bruit blanc, elle n'est m?me pas stationnaire
pacf(logimport.decompose$random, na.action = na.pass)

