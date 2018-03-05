#### On en profite pour mettre en place les packages et les seeds.
library(knitr)
library(magrittr) 
library(dplyr)
library(corrplot)
library(forecast)

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



#Partie 1: la production totale brute d'electricite


##Representation.


plot(ts$prod.totale, ylab = "Production totale d'electricite brute", type = "l")
#Cette repr?sentation appelle d?ja plusieurs observations:
#  - Pr?sence d'une saisonnalit? d'amplitude constante: on se place alors de la cadre des mod?les additifs
#  - Le trend ne semble pas lin?aire

#Avant de se lancer brutalement dans l'estimation d'un mod?le pour cette s?rie temporelle, on peut essayer de s'int?resser aux valeurs qui composent cette production globale brute.
#Dans cet esprit, on remarque que cette s?rie est la somme des productions brutes. Cependant, les s?ries n'?tant pas ind?pendantes, il serait laborieux de d?velopper plus sur cette voie la. Nous d?cidons donc de malgr? tout nous lancer directement dans l'?tude de la s?rie brutalement.

#cor <- cor(data.production.nette[-1])
#corrplot(corr = cor)


##Stationnarit?

#La série n'est clairement pas stationnaire. Pour se rassurer on peut regardes l'acf et le pacf

par(mfrow = c(1,2))
acf(ts$prod.totale)
pacf(ts$prod.totale)


##Decompose()

decompose.prod.totale <- decompose(ts$prod.totale, type = "additive")
plot(decompose(ts$prod.totale, type = "additive"))


acf(decompose.prod.totale$random, na.action = na.pass)
pacf(decompose.prod.totale$random, na.action = na.pass)
#Le bruit n'est pas stationnaire et présente une saisonnalité

#On différencie la saisonnalité

ts$prod.totale %>% diff(.,12) %>% ts.affichage(title = "Production Brute Total d=0, D=1")
#L'analyse de l'ACF et du pACF ne nous permet de conclure nettement en faveur de la stationnarité

#On différencie alors encore
ts$prod.totale %>% diff() %>% diff(.,12) %>% ts.affichage(title = "Production Brute Total d=1, D=1")

##Maintenant on analyse l'ACF et le pACF
#Sur l'ACF on va chercher les MA et sur le pACF les AR

#Sur l'ACF: le premier pic après 0 est en faveur d'un MA(1), et le pique en 1 en faveur d'un SMA(1)
#Sur le pACF: le premier pic est en faveur d'un AR(1) et le pic en 1 en faveur d'un SAR(1)

#On fit donc un SARIMA(1,1,1)(1,1,1)_12
fit <- Arima(ts$prod.totale, order=c(1,1,1), seasonal=c(1,1,1))
ts.affichage(residuals(fit), title = "Résidus SARIMA(1,1,1)(1,1,1)")

#L'ACF et le PACF sont plutôt satisfaisant

fit
#Tous les coefficients sont significatifs

#Il nous faut tester la blancheur des résidus
#Test Ljung-Box


#on plot les prévisions sur 3 années
plot(forecast(fit, h=36))

#Partie 2: la production photovoltaique brute










#fonctions utiles
ts.affichage <- function(ts, lag.max = 48,title="Mon titre"){
  split.screen(c(2,1))
  split.screen(c(1,2),screen=2)
  screen(1)
  plot(ts,main=title)
  screen(3)
  acf(ts,lag.max = lag.max,main="ACF")
  screen(4)
  pacf(ts,lag.max = lag.max,main="pACF")
  close.screen(all = TRUE)
}


x <- rep(0, 2, 48)
for (i in 2:48){
  x[i]<- Box.test(residuals(auto.arima(ts$prod.totale)), lag=i, fitdf=2, type="Ljung")$p.value
}
plot(x)

qqnorm((residuals(fit)-mean(residuals(fit)))/sd(residuals(fit)))
abline(0,1)
