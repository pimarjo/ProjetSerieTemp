#### On en profite pour mettre en place les packages et les seeds.
library(knitr)
library(magrittr) 
library(dplyr)
library(corrplot)

data <- read.csv("Donnees.csv", sep = ";", header = T)

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

ts.production.Totale.Brute <- ts(data$Production.Totale.Brute, frequency = 12, start = c(1981, 1), end = c(2017, 11))

ts.import <- ts(data$Importations, frequency = 12,  start = c(1981,1), end = c(2017, 11))

ts.photo <- ts(data$Production.Photovoltaique.Brute, frequency = 12, start = c(1981,1), end = c(2017,11))

plot(decompose(ts.import, type = "additive"))

plot(ts.production.Totale.Brute, ylab = "Production totale d'electricite brute", type = "l")

