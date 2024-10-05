library(tidyverse)
library(readxl)

#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##--------------------
## Import des données
##--------------------
don = read_excel("C:/Users/hp270/Documents/Scolaire/M1 Stats/Data challenge/Enedis/Diagnostic_train.xlsx")

don$Nb_of_incident[is.na(don$Nb_of_incident)] = 0   #donnée manquante => 0
don$Nb_of_anomaly[is.na(don$Nb_of_anomaly)] = 0   #idem
don$Service_date = as.Date(don$Service_date)  #conversion en date
don$`Last treatment PR immobilized`[is.na(don$`Last treatment PR immobilized`)] = 0
#choix de 0 pour le test d'inégalité juste après

Y = as.integer((don$`Last treatment PR immobilized` != 0) & don$Year_helicopter_flight <= don$`Last treatment PR immobilized`)
#vaut 1 s'il y a eu une maintenance (date non nulle) et si elle a eu lieu après le dernier passage de l'hélicoptère

#xi : variables explicatives
x1 = don$Length_climate_hazard_plan
x2 = don$Length_fragile_section
x3 = 2024 - year(don$Service_date)
x4 = don$Nb_of_anomaly
x5 = don$Nb_of_incident

enedis_data = tibble(true_anomaly = as.factor(Y), lg_clim = x1, lg_frag = x2, age = as.integer(x3), nb_anomaly = as.integer(x4), nb_incidents = as.integer(x5))


##--------------------
## Analyse des données
##--------------------
summary(enedis_data)
table(enedis_data$true_anomaly)

create_report(enedis_data, output_file = 'enedis_data_summary', output_dir = getwd(), report_title = 'Enedis data summary')


##--------------------
## Algorithme RF
##--------------------
library(ranger)
library(caret)

std_data = mutate_all(enedis_data[,-1], ~(as.vector(scale(.))))
std_data$true_anomaly = enedis_data$true_anomaly

under_data = downSample(std_data[,-6], std_data$true_anomaly)

set.seed(123)
ind_u = sample(c(1:dim(under_data)[1]), 5000)
under_train = under_data[ind_u,]
foret_u3 = ranger(Class~., data = under_train, mtry = 3)
foret_u3

pred_u3 = predict(foret_u3, under_data[-ind_u,])

#table(pred_u3$predictions, under_data$Class[-ind_u])


##-------------------------
## Application aux données
##-------------------------

don_etu = read_excel("Diagnostic_test_etu.xlsx")

don_etu$Nb_of_incident[is.na(don_etu$Nb_of_incident)] = 0
don_etu$Nb_of_anomaly[is.na(don_etu$Nb_of_anomaly)] = 0
don_etu$Service_date = as.Date(don_etu$Service_date)

x1 = don_etu$Length_climate_hazard_plan
x2 = don_etu$Length_fragile_section
x3 = 2024 - year(don_etu$Service_date)
x4 = don_etu$Nb_of_anomaly
x5 = don_etu$Nb_of_incident

Y = rep(0, length(x5))

ultimate_data = tibble(true_anomaly = as.factor(Y), lg_clim = x1, lg_frag = x2, age = as.integer(x3), nb_anomaly = as.integer(x4), nb_incidents = as.integer(x5))
ulti_std = mutate_all(ultimate_data[,-1], ~(as.vector(scale(.))))

P = predict(foret_u3, ulti_std)

library(writexl)

export = tibble(ID_t = don_etu$ID_t, `Last treatment PR immobilized` = P$predictions)

write_xlsx(export, path = "C:/Users/hp270/Documents/Scolaire/M1 Stats/Data challenge/Enedis/Diagnostic_pred_etu.xlsx")