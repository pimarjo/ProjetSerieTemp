#### On en profite pour mettre en place les packages et les seeds.
library(knitr)
library(magrittr) 
library(dplyr)
library(corrplot)
library(forecast)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(forecast)
library(tseries)
library(broom)
library(lmtest)

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


ljung.box.pvalues <- function(fit,fitdf=0){
  x <- rep(0, 48)
  for (i in fitdf+1:48){
    x[i]<- Box.test(residuals(fit), lag=i, fitdf=fitdf, type="Ljung")$p.value
  }
  return(x)
}

normalize <- function(x){
  return((x-mean(x))/sd(x))
}


tidy_sign <- function(x) 
{
  x <- tidy(x)
  std <- x$std.error
  estimation <- x$estimate
  x$is_strong <- abs(estimation) > 2*std
  return(x)
}

signif <- function(model)
{
  is_signif <- tidy_sign(model)$is_strong
  return(length(is_signif) == sum(is_signif))
}

aic_bic_r2_signif <- function(model, serie)
{
  R_2_Adj <- 1 - (model$sigma2 / (model$nobs-length(model$coef))) / (var(serie)/(model$nobs-1))
  AIC <- glance(model)$AIC
  BIC <- glance(model)$BIC
  return(list(AIC = AIC,BIC = BIC,R_2_Adj = R_2_Adj, All_signif = signif(model)))
}

make_arima <- function(serie, param_list = .param_list)
{
  arima(x= serie, order = c(param_list$p, param_list$d, param_list$q), seasonal = c(param_list$P, param_list$D, param_list$Q)) -> new_model
  return(new_model)
}

LogToExp<-function(predict.logModel)
{
  predict.logModel$mean <- exp(predict.logModel$mean)
  predict.logModel$x <- exp(predict.logModel$x)            #NO PARTICULAR CHANGE
  predict.logModel$fitted <- exp(predict.logModel$fitted)  #NO PARTICULAR CHANGE
  predict.logModel$upper <- exp(predict.logModel$upper) #cas particulier de formule dans le cas log->exp
  predict.logModel$lower <- exp(predict.logModel$lower)
  return (predict.logModel)
}

make_param <- function(p, d, q, P, D, Q)
{
  return(list(p = p, d = d, q = q, P = P, D = D, Q = Q))
}

kpss_shapi_qq_ks_Ljung <- function(model, parm_list = .param_list)
{
  par("mfrow" = c(3,1))
  kpss_ <- suppressWarnings(kpss.test(model$residuals, lshort = F))
  
  dl  <- (parm_list$p+parm_list$q+parm_list$P+parm_list$Q)
  x <- rep(0, 48 - dl - 1)
  for (i in 1:(48- dl)){
    x[i]<- Box.test(residuals(model), lag=i + dl,fitdf = dl, type="Ljung")$p.value
  }
  plot(x, main = "Test de LJung-Box", ylim = c(0, 1))
  abline(h = 0.05, col = "red")
  
  x <- rep(0, 48)
  for (i in 1:48){
    x[i]<- suppressWarnings(adf.test(residuals(model), k=i)$p.value)
  }
  plot(x, main = "Test de l'ADF pour plusieurs lags", ylim = c(0, 1))
  abline(h = 0.05, col = "red")
  
  #on centre et réduit les résidus:
  fit.res.norm <- residuals(model)/sd(model$residuals)
  # fit.res.norm <- (residuals(model) - mean(model$residuals))/sd(residuals(model))
  qqnorm((residuals(model) - mean(model$residuals))/sd(model$residuals))
  abline(0,1, col = "red")
  
  #Test de Kolmogorov Smirnov
  ks_ <- ks.test(fit.res.norm, 'pnorm') #on accepte (p-value > 0.05)
  
  #Test de Shapiro-Wilk
  sh_ <- shapiro.test(fit.res.norm) #on refuse (p-value < 0.05). Cela est probablement du aux queues de distribution 
  
  t_ <- t.test(x = model$residuals)
  
  par("mfrow" = c(1,1))
  return(list(kpss_stat = unname(kpss_$statistic), kpss_pvalue = unname(kpss_$p.value)
              , ks_statistic = unname(ks_$statistic), ks_pvalue = unname(ks_$p.value)
              , shapi_statistic = unname(sh_$statistic), shapi_pvalue = unname(sh_$p.value)
              , center_student = unname(t_$statistic), center_pvalue = unname(t_$p.value)))
}

make_rendu <- function(serie, param_list)
{
  new_model <- make_arima(serie, param_list = param_list)
  abrs <- aic_bic_r2_signif(new_model, serie)
  ksqs <- kpss_shapi_qq_ks_Ljung(new_model, param_list)
  print(new_model)
  return(do.call(what = cbind, c(n_obs = length(serie), param_list, abrs, ksqs)))
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
ts$prod.totale %>% diff(., differences = 1) %>% diff(.,12) %T>% ts.affichage(title = "Production Brute Total d=1, D=1") %>% kpss.test(.)
# p-value > 0.05, on accepte l'hypothèse nulle de stationnarité


#Maintenant on analyse l'ACF et le pACF
#Sur l'ACF on va chercher les MA et sur le pACF les AR

#Sur l'ACF: le premier pic après 0 est en faveur d'un MA(1), et le pique en 1 en faveur d'un SMA(1)
#Sur le pACF: le premier pic est en faveur d'un AR(1) et le pic en 1 en faveur d'un SAR(1)

#On fit donc un SARIMA(1,1,1)(1,1,1)_12
prod.totale <- list(mod.111_111 = arima(ts$prod.totale, order=c(1,1,1), seasonal=c(1,1,1))) #order=c(4,2,1), seasonal=c(0,1,1)
prod.totale$mod.111_111%>% residuals(.)%T>%ts.affichage(., title = "Résidus SARIMA(1,1,1)(1,1,1)") %>% kpss.test(., lshort = F)


#L'ACF et le PACF sont plutôt satisfaisant

#On test la stationnarité des résidus
prod.totale$mod.111_111 %>% residuals() %>% kpss.test()
#on accpete l'hypothèse nulle de stationnarité


prod.totale$mod.111_111
#Tous les coefficients sont significatifs

#Il nous faut tester la blancheur des résidus
#Test Ljung-Box pour les auto corrélations

prod.totale$mod.111_111 %>% ljung.box.pvalues(fit = . , fitdf = 4) %>% plot(.,ylim = c(0,1))
abline(a= 0.05, b =0, col = 'red')
close.screen(all = T)
#Le test ne passe pas pour les 2 premiers points -> présence d'autocorrélation

#Test de Student pour vérifier que la moyenne est centrée
prod.totale$mod.111_111 %>% residuals() %>% t.test(., mu = 0)
#Le test est franchi


### Test de normalité

#On cherche à savoir si le bruit blanc est gaussien, de moyenne nulle et de variance donnée
#qq plot test des résidus réduits: il faut que ce soit aligné sur la première bissectrice du plan,
prod.totale.fit.res.norm <- residuals(prod.totale$mod.111_111)/sd(residuals(prod.totale$mod.111_111))

prod.totale.fit.res.norm %>% qqnorm()
abline(0,1, col = "red")

#Test de Kolmogorov Smirnov
prod.totale.fit.res.norm %>% ks.test(. , 'pnorm') #on refuse (p-value < 0.05)

#Test de Shapiro-Wilk
prod.totale.fit.res.norm %>% shapiro.test() #on refuse (p-value < 0.05). Cela est probablement du aux queues de distribution 


#on résume tout ça là-dedans
make_rendu(ts$prod.totale, make_param(1,1,1,1,1,1))
fit <- arima(ts$prod.totale, order = c(1,1,1), seasonal = c(1,1,1))




#On va chercher un meilleur modèle selon le critère du BIC.
#La première piste est de s'intéresser à la composante SAR(1) qui est à la limite de la significativité.
#Voyons ce que donne un SARIMA(1,1,1)(0,1,1)

make_rendu(ts$prod.totale, make_param(1,1,1,0,1,1))



#le BIC est inférieur de 2 points ce qui n'est pas négligeable
#les résidus sont bien stationnaires
#le test de Ljung-Box n'est clairement pas passé -> présence d'autocorrélation
#les résidus passent le test student pour leur caractère centré
#ils ne sont pas gaussiens d'après les tests de Kolmogorov-Smirnov et de Shapiro-Wilk


          # prod.totale <- c(prod.totale, list(mod.111_011 = arima(ts$prod.totale, order = c(1,1,1), seasonal = c(0,1,1))))
          # prod.totale$mod.111_011 %>% residuals() %T>% ts.affichage(.,title = "SARIMA(1,1,1)(0,1,1)")
          # 
          # 
          # #L'ACF et le PACF sont plutôt satisfaisant
          # 
          # #On test la stationnarité des résidus
          # prod.totale$mod.111_011 %>% residuals() %>% kpss.test()
          # #on accpete l'hypothèse nulle de stationnarité
          # 
          # 
          # prod.totale$mod.111_011
          # #Tous les coefficients sont significatifs
          # 
          # #Il nous faut tester la blancheur des résidus
          # #Test Ljung-Box pour les auto corrélations
          # 
          # prod.totale$mod.111_011 %>% ljung.box.pvalues(fit = . , fitdf = 0) %>% plot(.,xlim = c(0,50))
          # abline(a= 0.05, b =0, col = 'red')
          # close.screen(all = T)
          # #Le test ne passe pas pour les 2 premiers points -> présence d'autocorrélation
          # 
          # #Test de Student pour vérifier que la moyenne est centrée
          # prod.totale$mod.111_011 %>% residuals() %>% t.test(., mu = 0)
          # #Le test est franchi
          # 
          # 
          # ### Test de normalité
          # 
          # #On cherche à savoir si le bruit blanc est gaussien, de moyenne nulle et de variance donnée
          # #qq plot test des résidus réduits: il faut que ce soit aligné sur la première bissectrice du plan,
          # prod.totale.fit.res.norm <- residuals(prod.totale$mod.111_011)/sd(residuals(prod.totale$mod.111_011))
          # 
          # prod.totale.fit.res.norm %>% qqnorm()
          # abline(0,1, col = "red")
          # 
          # #Test de Kolmogorov Smirnov
          # prod.totale.fit.res.norm %>% ks.test(. , 'pnorm') #on refuse (p-value < 0.05)
          # 
          # #Test de Shapiro-Wilk
          # prod.totale.fit.res.norm %>% shapiro.test() #on refuse (p-value < 0.05). Cela est probablement du aux queues de distribution 



#######___________    Conclusion:
#On retient notre premier modèle qui présente toutefois des défauts dans la validation des hypothèses

#On projette sur 12 mois
predict.arima <- forecast(fit, h=12)
#On plot
plot(predict.arima)


#############______  2e méthode: lissage Holt-Winters  ______#############

#On fit un Holt-Winters basique sur la série
HW.model <- HoltWinters(ts$prod.totale)

#On projette
HW.predict <- predict(object = HW.model, 12, level = 0.95, prediction.interval = F) #Mettre prediction.interval = T si on veut l'intervalle de confiance

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


arima.111_011 <- arima.sim()

ts.sim <- arima.sim(list(order = c(1,0,1), ar = 0.7, ma = 0.7), n = 100000)
ts.plot(ts.sim)
ts.sim %>% ts.affichage(., title = "ARIMA(1,0,1)")
ts.sim %>% diff() %>% ts.affichage(., title = "ARIMA(1,1,0) diff")

fit <- arima(ts.sim, order=c(1,0,1))
fit <- auto.arima(ts$prod.totale)
fit %>% residuals() %>% ts.affichage(., title = "Fit")
fit %>% ljung.box.pvalues(fit = . , fitdf = 2) %>% plot(.,ylim = c(0,1))
abline(h = 0.05, col = "red")


