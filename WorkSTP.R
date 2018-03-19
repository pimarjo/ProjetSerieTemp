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
#L'analyse de l'ACF et du pACF ne nous permet de conclure nettement en faveur de la stationnarité, le test KPSS nous confirme que ce n'est pas stationnaire
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
mod.prod <- list(mod1 = arima(ts$prod.totale, order=c(1,1,1), seasonal=c(1,1,1)))
ts.affichage(residuals(mod.prod$mod1), title = "Résidus SARIMA(1,1,1)(1,1,1)")
#L'ACF et le PACF sont plutôt satisfaisant

#On test la stationnarité des résidus
kpss.test(residuals(mod.prod$mod1))
#on accpete l'hypothèse nulle de stationnarité


mod.prod$mod1
#Seul le sar1 est vraiment � la limite de la significativit�, mais l'est quand m�me

#Il nous faut tester la blancheur des résidus
#Test Ljung-Box
x <- rep(0, 2, 48)
for (i in 1:48){
  x[i]<- Box.test(residuals(mod.prod$mod1), lag=i, fitdf=4, type="Ljung")$p.value
}
plot(x)
#Nous avons un souci pour le lag qui ne passe pas le test. Sinon pour tous les autres, on accepte l'hypothèse d'absence d'autocorrélation



### Test de normalité
#on centre et réduit les résidus:
prod.totale.fit.res.norm <- (residuals(mod.prod$mod1)-mean(residuals(mod.prod$mod1)))/sd(residuals(mod.prod$mod1))

#qq plot test: il faut que ce soit aligné sur la première bissectrice du plan
qqnorm(prod.totale.fit.res.norm)
abline(0,1, col = "red")
#Test de Kolmogorov Smirnov
ks.test(prod.totale.fit.res.norm, 'pnorm') #on accepte (p-value > 0.05)

#Test de Shapiro-Wilk
shapiro.test(prod.totale.fit.res.norm) #on refuse (p-value < 0.05). Cela est probablement du aux queues de distribution 


#On projette sur 12 mois
predict.arima <- forecast(mod.prod$mod1, h=12)
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



##### Back testing

ts <- c(ts,list(shortProd = ts(data$Production.Totale.Brute, frequency = 12,  start = c(1981,1), end = c(2016, 11))))

fit <- arima(ts$shortProd, order=c(1,1,1), seasonal=c(1,1,1))

ts.affichage(residuals(fit), title = paste0("Résidus SARIMA(",1,",",1, ",", 1, ")("
                                            ,1,",",1, ",", 1, ")"))
#On projette sur 12 mois
back.predict.arima <- forecast(fit, h=12)


plot(back.predict.arima)
#On rajoute la série totale
lines(ts$prod.totale)



##############################################################################################
#############______   Partie 2: IMPORT   ______#############
##############################################################################################

#------- plot de la s?rie --------
plot(ts$import)       #il semble que la s?rie soit de type multiplicative car l'amplitude de la saisonalit? n'est pas constante
plot(log(ts$import))  #On passe au log pour la rendre additive : OK

#On a maintenant le log de notre série. La composition par le log est légale parce que la fonction log est strictement croissante
ts <- c(ts,list(logimpor = log(ts$import)))

#------- ACF , PACF ----------
acf(ts$logimpor)        #pas stationnaire en l'?tat
pacf(ts$logimpor)

#------- Decompose ---------

logimport.decompose <- decompose(ts$logimpor,type = "additive") 
acf(logimport.decompose$random, na.action = na.pass)  # A d?faut que la composante al?atoire ne soit pas un bruit blanc, elle n'est m?me pas stationnaire
pacf(logimport.decompose$random, na.action = na.pass)

#Le bruit n'est pas stationnaire et présente une saisonnalité, le trend n'est pas linéaire!!!
#Il va falloir réfléchir autrement du coup - un trend non linéaire -> bof avec moyenne mobiles

#------- SARIMA  ----------

#Essayons de trouver la différenciation la plus représentative
close.screen(all = T)
stck <- ts$logimpor %>% diff(.,12) %>% kpss.test(.)

logimport_kpss_test <- data.frame(matrix(0, 11, 4))

names(logimport_kpss_test) <- c("diff_12_diff_i_kpss", "diff_12_diff_i_p_value", "diff_i_kpss", "diff_i_p_value")
for(i in 1:11)
{
  suppressWarnings(tmp_12 <- ts$logimpor %>% diff(.,12) %>% diff(., i) %>% kpss.test(.))
  suppressWarnings(tmp <- ts$logimpor %>% diff(., i) %>% kpss.test(.))
  logimport_kpss_test[i, 1] <- tmp_12$statistic %>% unname()
  logimport_kpss_test[i, 2] <- tmp_12$p.value %>% unname()
  logimport_kpss_test[i, 3] <- tmp$statistic %>% unname()
  logimport_kpss_test[i, 4] <- tmp$p.value %>% unname()
}

print(logimport_kpss_test)

#Clairement, d = 1 et D = 12 nous apporte la série la plus stationnaire; trouvons les parametres maximums pour les sarimas que nous allons fitter ainsi

logimport_parameters <- list(d = 1, D = 12)

ts$logimpor %>% diff() %>% diff(.,12) %>% ts.affichage(title = "Imports d=1, D=1")

#Un processus AR d’ordre p se caractérise par sa fonction d’autocorrélation partielle qui s’annule à partir de l’ordre p + 1.

#Un processus MA d’ordre q se caractérise par sa fonction d’autocorrélation qui s’annule à partir de l’ordre q + 1.

#Donc arma(p, q) -> arima(p, q, d)

#ici p = 2 # pacf s'annule en 3
#ici q = 1 # acf s'annule en 2
#ici P = 3 # saisonnalité pacf jusqu'à au moins 4
#ici Q = 1 # saisonalité acf jusqu'à 2

logimport_parameters$p <- 2
logimport_parameters$q <- 1
logimport_parameters$P <- 3
logimport_parameters$Q <- 1

fit <- arima(ts$logimpor, order=c(logimport_parameters$p,logimport_parameters$q,logimport_parameters$d),
             seasonal=c(logimport_parameters$P,logimport_parameters$Q,1)
)
ts.affichage(residuals(fit), title = paste0("Résidus SARIMA(",logimport_parameters$p,",",logimport_parameters$q, ",", logimport_parameters$d, ")("
                                            ,logimport_parameters$P,",",logimport_parameters$Q, ",", 1, ")"))

#test de stationnarité des résidus
kpss.test(residuals(fit))

fit
#ar2, sar2 et sar3 non significatifs

#--------------- virons ar2


logimport_parameters$p = 1

fit <- arima(ts$logimpor, order=c(logimport_parameters$p,logimport_parameters$q,logimport_parameters$d),
             seasonal=c(logimport_parameters$P,logimport_parameters$Q,1)
)
ts.affichage(residuals(fit), title = paste0("Résidus SARIMA(",logimport_parameters$p,",",logimport_parameters$q, ",", logimport_parameters$d, ")("
                                            ,logimport_parameters$P,",",logimport_parameters$Q, ",", 1, ")"))

#test de stationnarité des résidus
kpss.test(residuals(fit))

fit

#--------------- virons sar3

logimport_parameters$P =2

fit <- arima(ts$logimpor, order=c(logimport_parameters$p,logimport_parameters$q,logimport_parameters$d),
             seasonal=c(logimport_parameters$P,logimport_parameters$Q,1)
)

ts.affichage(residuals(fit), title = paste0("Résidus SARIMA(",logimport_parameters$p,",",logimport_parameters$q, ",", logimport_parameters$d, ")("
                                            ,logimport_parameters$P,",",logimport_parameters$Q, ",", 1, ")"))

#test de stationnarité des résidus
kpss.test(residuals(fit))

fit

#--------------- virons sar2

logimport_parameters$P = 1

fit <- arima(ts$logimpor, order=c(logimport_parameters$p,logimport_parameters$q,logimport_parameters$d),
             seasonal=c(logimport_parameters$P,logimport_parameters$Q,1)
)

ts.affichage(residuals(fit), title = paste0("Résidus SARIMA(",logimport_parameters$p,",",logimport_parameters$q, ",", logimport_parameters$d, ")("
                                            ,logimport_parameters$P,",",logimport_parameters$Q, ",", 1, ")"))

#test de stationnarité des résidus
kpss.test(residuals(fit))

fit
#Jai un joli sarima qui fonctionne sa mère

#!!!!!!!!!!!

#CHERCHONS MIEUX

aic_from_model <- function(logimport_parameters, ts_to_study)
{
  fit <- arima(ts_to_study, order=c(logimport_parameters$p,logimport_parameters$q,logimport_parameters$d),
               seasonal=c(logimport_parameters$P,logimport_parameters$Q,1)
  )
  print(fit)
  return(fit$aic)
}

#Voici le model que j'ai trouvé tout à l'heure, il me va bien, voyons voir si on a mieux en baissant les parametres
aic_from_model(ts_to_study = ts$logimpor, logimport_parameters = list(p = 1, q = 1, d = 1, P = 1, Q = 1, D = 1))

aic_from_model(ts_to_study = ts$logimpor, logimport_parameters = list(p = 1, q = 1, d = 1, P = 1, Q = 0, D = 1))

#Ce modele est meilleur, je n'arrive pas à trouver mieux en bruteforçant...
logimport_parameters = list(p = 1, q = 1, d = 1, P = 1, Q = 0, D = 1)

fit <- arima(ts$logimpor, order=c(logimport_parameters$p,logimport_parameters$q,logimport_parameters$d),
             seasonal=c(logimport_parameters$P,logimport_parameters$Q,1)
)

ts.affichage(residuals(fit), title = paste0("Résidus SARIMA(",logimport_parameters$p,",",logimport_parameters$q, ",", logimport_parameters$d, ")("
                                            ,logimport_parameters$P,",",logimport_parameters$Q, ",", 1, ")"))

#test de stationnarité des résidus
kpss.test(residuals(fit))

#Il nous faut tester la blancheur des résidus
#Test Ljung-Box
x <- rep(0, 2, 48)
for (i in 1:48){
  x[i]<- Box.test(residuals(fit), lag=i, fitdf=4, type="Ljung")$p.value
}
plot(x)

#WARNING
#WARNING

#Je sais pas comment interpréter ce truc.... Je dirais que c'est loin d'être une droite, que c'est moche
#ET donc présence d'autocorrelations ? 

#on centre et réduit les résidus:
logimport.fit.res.norm <- (residuals(fit)-mean(residuals(fit)))/sd(residuals(fit))

#qq plot test: il faut que ce soit aligné sur la première bissectrice du plan
qqnorm(logimport.fit.res.norm)
abline(0,1, col = "red")

#Test de Kolmogorov Smirnov
ks.test(logimport.fit.res.norm, 'pnorm') #on accepte (p-value > 0.05)

#Test de Shapiro-Wilk
shapiro.test(logimport.fit.res.norm) #on refuse (p-value < 0.05). Cela est probablement du aux queues de distribution 

#On projette sur 12 mois
predict.arima <- forecast(fit, h=12)
#On plot
plot(predict.arima)

#Because exponentiel is strictement croissante
#alors je peu composer pour avoir mon interval de confiance

predict.arima$lower <- exp(predict.arima$lower)
predict.arima$upper <- exp(predict.arima$upper)
predict.arima$x <- exp(predict.arima$x)
predict.arima$fitted <- exp(predict.arima$fitted)
predict.arima$mean <- exp(predict.arima$mean)

plot(predict.arima)

#On garde ce joli sarima

#------- HOLT WINTERS -------

#On fit un Holt-Winters basique sur la série
HW.model <- HoltWinters(ts$logimpor)

#On projette
HW.predict_80 <- predict(object = HW.model, 12, level = 0.80, prediction.interval = T) #Mettre prediction.interval = T si on veut l'intervalle de confiance
HW.predict_95 <- predict(object = HW.model, 12, level = 0.95, prediction.interval = T) #Mettre prediction.interval = T si on veut l'intervalle de confiance

#On plot
HW.predict_80 %<>% exp
HW.predict_95 %<>% exp

HW.model$x %<>% exp

plot(HW.model, HW.predict_80)
plot(HW.model, HW.predict_95)

sum(fit$residuals^2) < HW.model$SSE

#SARIMA qui gagne!

#BACKTESTS IMPORT--------------------
#Now time to give some graphs
#Backtestons nos prédictions sur l'année précédente de la fin des observations
ts <- c(ts,list(shortlogimport =  ts(log(data$Importations), frequency = 12,  start = c(1981,1), end = c(2016, 11))))

fit <- arima(ts$shortlogimport, order=c(logimport_parameters$p,logimport_parameters$q,logimport_parameters$d),
             seasonal=c(logimport_parameters$P,logimport_parameters$Q,1)
)

ts.affichage(residuals(fit), title = paste0("Résidus SARIMA(",logimport_parameters$p,",",logimport_parameters$q, ",", logimport_parameters$d, ")("
                                            ,logimport_parameters$P,",",logimport_parameters$Q, ",", 1, ")"))
#On projette sur 12 mois
back.predict.arima <- forecast(fit, h=12)
#On passe à l'exponentiel
back.predict.arima$lower <- exp(back.predict.arima$lower)
back.predict.arima$upper <- exp(back.predict.arima$upper)
back.predict.arima$x <- exp(back.predict.arima$x)
back.predict.arima$fitted <- exp(back.predict.arima$fitted)
back.predict.arima$mean <- exp(back.predict.arima$mean)


plot(back.predict.arima)
#On rajoute la série totale
lines(ts$import)

##############################################################################################
#############______   Partie 2: PHOTOVOLTAIQUE  ______#############
##############################################################################################

plot(ts$photo)            # Mod�le � priori multiplicatif
logphoto <- log(ts$photo) # On passe au log pour homogeneiser la variance et passer dans un mod�le additif
plot(logphoto)            # Ok , ca semble additif

logphoto.decompose<-decompose(logphoto)
plot(logphoto.decompose)  # Saisonnalit� 12

acf(logphoto) #Pas stationnaire
pacf(logphoto)

kpss.test(logphoto.decompose$random) #la partie aleatoire de la est stationnaire

#On diff�rencie une fois avec une p�riode de 12
logphoto %>% diff(.,lag=12) %T>% ts.affichage() %>% kpss.test(.)
#On identifie alors les composantes:
# ordre de diff�rentiation: d = 1
# composante AR : q = 1 - En effet, l'ACF decroit exponentiellement vite et les autocorrelations sont nulles apr�s 1
# composante MA : p = 0 - Il n'y a apparemment pas de composante en MA � retenir (A v�rifier - tester)
# Y a-t-il des composantes saisonni�res ? TODO

fit<-arima(logphoto,order = c(1,1,0), seasonal = c(2,1,0)) 
fit.significiant <- (1-pnorm(abs(fit$coef)/sqrt(diag(fit$var.coef))))*2
fit.significiant <= 0.05 #Significatifs avec SAR = 2, pas avec SAR = 1

fit.R2 <- 1 - ((fit$sigma2 / (fit$nobs-length(fit$coef))) / var(logphoto)/fit$nobs ) #PARFAIT

predictlogphoto<-forecast(fit)

acf(predictlogphoto$residuals) #les r�sidus ont l'air stationnaires
pacf(predictlogphoto$residuals)

logphotopredict.norm <- (residuals(fit)-mean(residuals(fit)))/sd(residuals(fit) )

#qq plot test: il faut que ce soit aligné sur la première bissectrice du plan
qqnorm(logphotopredict.norm)
abline(0,1, col = "red")
#Test de Kolmogorov Smirnov
ks.test(logphotopredict.norm, 'pnorm') #on accepte (p-value > 0.05)

Box.test(logphotopredict.norm)
kpss.test(logphotopredict.norm) #stationnarit� ok
