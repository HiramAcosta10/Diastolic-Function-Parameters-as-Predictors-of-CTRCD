library(purrr)
library(broom)
library(dplyr)
library(purrr)
library(tidyverse)
library(rstatix)
library(gtsummary)
library(ggsankey)
library(ggplot2) 
library(geepack)
library(WeightIt)
library(survival)



## 1.1 Demographic tables####
tabla1<- df%>%
  select(hormonal, anti_her2, triple_neg, estadío_clinical, diabetes, htn, smoking, hyperchol, hypertri, antecedente_de_ca_de_mama_0, familiar_0, Grupos_edad)%>%
  tbl_summary( 
    by= Grupos_edad, missing = "no",
    label = c(hormonal ~ "Hormone receptors",
              anti_her2 ~ "HER2 receptor",  triple_neg~"Triple negative", estadío_clinical~ "Breast cancer stage", diabetes~ "Diabetes", htn~ "Arterial hypertension", smoking~ "Smoking history", 
              hyperchol~ "Hypercholesterolemia", hypertri~"Hypertriglyceridemia", antecedente_de_ca_de_mama_0~ "History of breast cancer", familiar_0~ "Familiar history of breast cancer")) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels() %>%
  bold_p() 
print(tabla1)


tabla2<- df %>% 
  select (relación_ea_0, relación_ee___medial_0, e_0, velocidad__peak_it_0, volumen_auricular_indexado_izquierdo_0, tiempo_de_desaceleración_0, Grupos_edad) %>%
  tbl_summary(
    by= Grupos_edad, missing = "no",
    label = c(relación_ea_0~"Mitral E/A ratio", relación_ee___medial_0~"E/é ratio", e_0~"é septal", velocidad__peak_it_0~"Peak TR velocity", 
              volumen_auricular_indexado_izquierdo_0~"Indexed left atrial volume", tiempo_de_desaceleración_0~"Deceleration time")
  ) %>%
  add_overall() %>%
  add_p() %>% 
  bold_labels() %>%
  bold_p() 

print(tabla2)



tabla3<- df %>% 
  select (fevi_3d_0,sgl_vi_0, Grupos_edad) %>%
  tbl_summary(
    by= Grupos_edad, missing = "no",
    label = c(fevi_3d_0~ "LVEF",sgl_vi_0~ "LV-GLS")
  ) %>%
  add_overall() %>%
  add_p() %>% 
  bold_labels() %>%
  bold_p() 
print(tabla3)

tablacom1<- 
  tbl_stack(
    list(tabla1,tabla2, tabla3), 
    group_header = c("Baseline Demographic and Clinical Variables", "Baseline Diastolic Function", "Baseline Systolic Function")
  ) 

print(tablacom1)


##1.2 Smooth line changes during time template #####
DDVI1_long <- SK %>%
  pivot_longer(cols = c(relación_ea_0, relación_ea_3, relación_ea_6, relación_ea_12),
               names_to = "variable", 
               values_to = "valor")

DDVI1_long$tiempo <- rep(c("Basal", "3-months", "6-months", "12-months"), length.out = nrow(DDVI1_long))

EA <- ggplot(DDVI1_long, aes(x = tiempo, y = valor, group = 1)) +  
  geom_smooth(method = "loess", aes(color = "red"), fill = "lightcoral", se = TRUE) +  
  scale_x_discrete(limits = c("Basal", "3-months", "6-months", "12-months")) +
  labs(title = "Mitral E/A ratio during time",
       x = "Time",
       y = "Mitral E/A ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

EA

## 1.3 Changes during time template####

tabla10 <- SK %>% 
  select(relación_ea_3, `EAB3`, relación_ee_3, `EEB3`, velocidad__peak_it_3, 
         `PB3`, e_3, `ESB3`, volumen_auricular_indexado_izquierdo_3, `VB3`, 
         tiempo_de_desaceleración_3, `DB3`) %>%
  rename(
    "Mitral E/A ratio" = relación_ea_3,
    "Delta E/A" = `EAB3`,
    "E/é ratio" = relación_ee_3,
    "Delta E/é" = `EEB3`,
    "Peak TR velocity" = velocidad__peak_it_3,
    "Delta Peak" = `PB3`,
    "é septal" = e_3,
    "Delta é septal" = `ESB3`,
    "Indexed left atrial volume" = volumen_auricular_indexado_izquierdo_3,
    "Delta volume" = `VB3`,
    "Deceleration time" = tiempo_de_desaceleración_3,
    "Delta Deceleration" = `DB3`
  ) %>%
  tbl_summary(missing = "no")
tabla10


tabla11 <- SK %>%
  select(relación_ea_6, `EAB6`, relación_ee_6, `EEB6`, velocidad__peak_it_6, 
         `PB6`, e_6, `ESB6`, volumen_auricular_indexado_izquierdo_6, `VB6`, 
         tiempo_de_desaceleración_6, `DB6`) %>%
  rename(
    "Mitral E/A ratio" = relación_ea_6,
    "Delta E/A" = `EAB6`,
    "E/é ratio" =  relación_ee_6,
    "Delta E/é" = `EEB6`,
    "Peak TR velocity" = velocidad__peak_it_6,
    "Delta Peak" = `PB6`,
    "é septal" = e_6,
    "Delta é septal" = `ESB6`,
    "Indexed left atrial volume" = volumen_auricular_indexado_izquierdo_6,
    "Delta volume" = `VB6`,
    "Deceleration time" = tiempo_de_desaceleración_6,
    "Delta Deceleration" = `DB6`
  ) %>%
  tbl_summary(missing = "no")
tabla11

tabla12 <- SK %>% 
  select(relación_ea_12, `EAB12`, relación_ee_12, `EEB12`, velocidad__peak_it_12, 
         `PB12`, e_12, `ESB12`, volumen_auricular_indexado_izquierdo_12, `VB12`, 
         tiempo_de_desaceleración_12, `DB12`) %>%
  rename(
    "Mitral E/A ratio" = relación_ea_12,
    "Delta E/A" = `EAB12`,
    "E/é ratio" = relación_ee_12,
    "Delta E/é" = `EEB12`,
    "Peak TR velocity" = velocidad__peak_it_12,
    "Delta Peak" = `PB12`,
    "é septal" = e_12,
    "Delta é septal" = `ESB12`,
    "Indexed left atrial volume" = volumen_auricular_indexado_izquierdo_12,
    "Delta volume" = `VB12`,
    "Deceleration time" = tiempo_de_desaceleración_12,
    "Delta Deceleration" = `DB12`
  ) %>%
  tbl_summary(missing = "no")


tablacom2 <- tbl_merge(
  list(tabla10, tabla11, tabla12), 
  tab_spanner = c("3-months", "6-months", "12-months")
)


## 1.4 Changes hypothesis tests template ####
#b vs 3
p_value <- wilcox.test(SK$relación_ea_0, SK$relación_ea_3) $p.value
p_value

#b vs 6
p_value <- wilcox.test(SK$relación_ea_0, SK$relación_ea_6) $p.value
p_value

#b vs 12
p_value <- wilcox.test(SK$relación_ea_0, SK$relación_ea_12) $p.value
p_value

#deltas
#3 vs 6

p_value <- wilcox.test(SK$EAB3, SK$EAB6) $p.value
p_value

#6 vs 12

p_value <- wilcox.test(SK$EAB6, SK$EAB12) $p.value
p_value


## 2.1 Sanketplots models ####
df1 <- SK %>%
  dplyr::select(edad_0, ddvif_0, ddvif_3, ddvif_6, ddvif_12) %>%
  filter(ddvif_0 != "Excluded", 
         ddvif_3 != "Excluded", 
         ddvif_6 != "Excluded", 
         ddvif_12 != "Excluded")



nombres_nuevos<- c("Basal","3 Months", "6 Months", "12 Months")
nombres_originales<- c("ddvif_0","ddvif_3","ddvif_6", "ddvif_12")


df1l<-df1 %>%
  make_long(ddvif_0,ddvif_3,ddvif_6,ddvif_12)
nivel_orden <- factor(c("Grade II", "Grade I", "Diastolic Dysfunction", "Indeterminated", "Normal"),
                      levels = c("Normal","Indeterminated","Diastolic Dysfunction","Grade I", "Grade II"))
df1l$node <- factor(df1l$node, levels = nivel_orden)

SPG<-ggplot(df1l,aes(x = x
                     , next_x = next_x
                     , node = node
                     ,next_node = next_node
                     , fill = factor(node)
                     , label = node))
SPG<- SPG+geom_sankey(flow.alpha = 0.5,
                      node.color = "black",
                      show.legend = TRUE)
SPG<- SPG+geom_sankey_label(size = 3, color = "black", fill = "white", hjust = -0.5)
SPG<- SPG+ theme(legend.position = "none")
SPG<- SPG + theme(axis.title = element_blank(),
                  axis.text.y = element_blank())
SPG<- SPG+scale_x_discrete(labels = setNames(nombres_nuevos, nombres_originales))

SPG


## 3.1 GEE database creation ####

SKgee <- SK %>%
  pivot_longer(cols = c(relación_ea_0, relación_ea_3,relación_ea_6 , relación_ea_12), 
               names_to = "visita", 
               values_to = "E_AT") %>%
  mutate(visita = case_when(
    visita == "relación_ea_0" ~ 1,
    visita == "relación_ea_3" ~ 2,
    visita == "relación_ea_6" ~ 3,
    visita == "relación_ea_12" ~ 4,
    TRUE ~ NA_real_
  )) %>%
  
  
  left_join(SK %>%
              pivot_longer(cols = c(relación_ee___medial_0, relación_ee_3, relación_ee_6, relación_ee_12),
                           names_to = "visita_Eé",
                           values_to = "E_éT") %>%
              mutate(visita_Eé = case_when(
                visita_Eé == "relación_ee___medial_0" ~ 1,
                visita_Eé == "relación_ee_3" ~ 2,
                visita_Eé == "relación_ee_6" ~ 3,
                visita_Eé == "relación_ee_12" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_Eé), 
            by = c("id", "visita")) %>%
  
  
  left_join(SK %>%
              pivot_longer(cols = c(e_0, e_3, e_6, e_12),
                           names_to = "visita_ES",
                           values_to = "EST") %>%
              mutate(visita_ES = case_when(
                visita_ES == "e_0" ~ 1,
                visita_ES == "e_3" ~ 2,
                visita_ES == "e_6" ~ 3,
                visita_ES == "e_12" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_ES),  
            by = c("id", "visita")) %>%
  
  
  left_join(SK %>%
              pivot_longer(cols = c(velocidad__peak_it_0, velocidad__peak_it_3, velocidad__peak_it_6, velocidad__peak_it_12),
                           names_to = "visita_P",
                           values_to = "PT") %>%
              mutate(visita_P = case_when(
                visita_P == "velocidad__peak_it_0" ~ 1,
                visita_P == "velocidad__peak_it_3" ~ 2,
                visita_P == "velocidad__peak_it_6" ~ 3,
                visita_P == "velocidad__peak_it_12" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_P),  
            by = c("id", "visita")) %>%
  
  
  left_join(SK %>%
              pivot_longer(cols = c(volumen_auricular_indexado_izquierdo_0, volumen_auricular_indexado_izquierdo_3, volumen_auricular_indexado_izquierdo_6, volumen_auricular_indexado_izquierdo_12),
                           names_to = "visita_V",
                           values_to = "VT") %>%
              mutate(visita_V = case_when(
                visita_V == "volumen_auricular_indexado_izquierdo_0" ~ 1,
                visita_V == "volumen_auricular_indexado_izquierdo_3" ~ 2,
                visita_V == "volumen_auricular_indexado_izquierdo_6" ~ 3,
                visita_V == "volumen_auricular_indexado_izquierdo_12" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_V), 
            by = c("id", "visita")) %>%
  
  left_join(SK %>%
              pivot_longer(cols = c(tiempo_de_desaceleración_0, tiempo_de_desaceleración_3, tiempo_de_desaceleración_6, tiempo_de_desaceleración_12),
                           names_to = "visita_D",
                           values_to = "DT") %>%
              mutate(visita_D = case_when(
                visita_D == "tiempo_de_desaceleración_0" ~ 1,
                visita_D == "tiempo_de_desaceleración_3" ~ 2,
                visita_D == "tiempo_de_desaceleración_6" ~ 3,
                visita_D == "tiempo_de_desaceleración_12" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_D),  
            by = c("id", "visita")) %>%
  
  left_join(SK %>%
              pivot_longer(cols = c(sgl_vi_0, sgl_vi_3, sgl_vi_6, sgl_vi_12),
                           names_to = "visita_SGL",
                           values_to = "SGLT") %>%
              mutate(visita_SGL = case_when(
                visita_SGL == "sgl_vi_0" ~ 1,
                visita_SGL == "sgl_vi_3" ~ 2,
                visita_SGL == "sgl_vi_6" ~ 3,
                visita_SGL == "sgl_vi_12" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_SGL),  
            by = c("id", "visita")) %>%
  
  left_join(SK %>%
              pivot_longer(cols = c(cardiotoxDich_0, cardiotoxDich_three, cardiotoxDich_six, cardiotoxDich_twelve),
                           names_to = "visita_CT",
                           values_to = "CTT") %>%
              mutate(visita_CT = case_when(
                visita_CT == "cardiotoxDich_0" ~ 1,
                visita_CT == "cardiotoxDich_three" ~ 2,
                visita_CT == "cardiotoxDich_six" ~ 3,
                visita_CT == "cardiotoxDich_twelve" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_CT),  
            by = c("id", "visita")) %>%
  
  left_join(SK %>%
              pivot_longer(cols = c(fevi_3d_0, fevi_3d_3, fevi_3d_6, fevi_3d_12),
                           names_to = "visita_FEVI",
                           values_to = "FEVIT") %>%
              mutate(visita_FEVI = case_when(
                visita_FEVI == "fevi_3d_0" ~ 1,
                visita_FEVI == "fevi_3d_3" ~ 2,
                visita_FEVI == "fevi_3d_6" ~ 3,
                visita_FEVI == "fevi_3d_12" ~ 4,
                TRUE ~ NA_real_
              )) %>%
              rename(visita = visita_FEVI),  
            by = c("id", "visita")) %>%
  
  select(`id`, `visita`, `E_AT`, `E_éT`, `EST`, `PT`, `VT`, `DT`, `FEVIT`, `CTT`, `SGLT`, diabetes, htn, hyperchol, hypertri, smoking, anti_her2, triple_neg, edad_0)

SKgee$id<-gsub("INCAN","",SKgee$id)
SKgee$diabetes<-as.factor(SKgee$diabetes)
SKgee$htn<-as.factor(SKgee$htn)
SKgee$hyperchol<-as.factor(SKgee$hyperchol)
SKgee$hypertri<-as.factor(SKgee$hypertri)
SKgee$smoking<-as.factor(SKgee$smoking)
SKgee$anti_her2<-as.factor(SKgee$anti_her2)
SKgee$triple_neg<-as.factor(SKgee$triple_neg)
SKgee$visita<-as.factor(SKgee$visita)
SKgee$id<-as.factor(SKgee$id)
SKgee$CTT<-as.factor(SKgee$CTT)

SKgee<- SKgee %>%
  group_by(id) %>%
  filter(n() == 4) %>%
  ungroup() 
SKgee <- SKgee %>%
  filter(!is.na(FEVIT) & !is.na(PT) & !is.na(E_AT) & !is.na(E_éT) & !is.na(diabetes) & 
           !is.na(htn) & !is.na(hypertri) & !is.na(hyperchol) & !is.na(EST) & !is.na(VT) & !is.na(DT) &
           !is.na(smoking) & !is.na(anti_her2) & !is.na(triple_neg))

SKgee<- SKgee %>%
  group_by(id) %>%
  filter(n() == 4) %>%
  ungroup()

## 3.2 GEE models for LVEF ####
gee1 <- geeglm(FEVIT ~ E_AT + diabetes+ htn + hypertri + hyperchol + smoking + anti_her2 +  triple_neg,  
               id = id, 
               family = gaussian(), 
               data = SKgee,
               corstr = "exchangeable")
jtools::summ(gee1, confint= T)

gee1con<-summary(gee1)
gee1con
coefficients <- gee1con$coefficients[, "Estimate"]
std_errors <- gee1con$coefficients[, "Std.err"]
z_value <- qnorm(0.975)
lower_bound <- coefficients - z_value * std_errors
upper_bound <- coefficients + z_value * std_errors
cbind(coefficients, lower_bound, upper_bound)


## 3.3 GEE models for LV-GLS ####
gee1 <- geeglm(SGLT ~ E_AT + diabetes+ htn + hypertri + hyperchol + smoking + anti_her2 +  triple_neg , 
               id = id, 
               family = gaussian(), 
               data = SKgee,
               corstr = "exchangeable")
jtools::summ(gee1, confint = T)


gee1con<-summary(gee1)
gee1con
coefficients <- gee1con$coefficients[, "Estimate"]
std_errors <- gee1con$coefficients[, "Std.err"]
z_value <- qnorm(0.975)
lower_bound <- coefficients - z_value * std_errors
upper_bound <- coefficients + z_value * std_errors
cbind(coefficients, lower_bound, upper_bound) 


## 3.4 GEE models for >50 years group ####
SKgee2<-SKgee %>% 
  filter(edad_0>=50)
gee1 <- geeglm(SGLT ~ DT + diabetes+ htn + hypertri + hyperchol + smoking + anti_her2 +  triple_neg , 
               id = id, 
               family = gaussian(), 
               data = SKgee2,
               corstr = "exchangeable")
jtools::summ(gee1, confint = T)
gee1con<-summary(gee1)
gee1con
coefficients <- gee1con$coefficients[, "Estimate"]
std_errors <- gee1con$coefficients[, "Std.err"]
z_value <- qnorm(0.975)
lower_bound <- coefficients - z_value * std_errors
upper_bound <- coefficients + z_value * std_errors
cbind(coefficients, lower_bound, upper_bound) 

##3.5 GEE models for <50 years group ####
SKgee2<-SKgee %>% 
  filter(edad_0<50)
gee1 <- geeglm(SGLT ~ DT + diabetes+ htn + hypertri + hyperchol + smoking + anti_her2 +  triple_neg , 
               id = id, 
               family = gaussian(), 
               data = SKgee2,
               corstr = "exchangeable")
jtools::summ(gee1, confint = T)
gee1con<-summary(gee1)
gee1con
coefficients <- gee1con$coefficients[, "Estimate"]
std_errors <- gee1con$coefficients[, "Std.err"]
z_value <- qnorm(0.975)
lower_bound <- coefficients - z_value * std_errors
upper_bound <- coefficients + z_value * std_errors
cbind(coefficients, lower_bound, upper_bound) 


##4.1 COX IPW database creation ####
SK$dd_0 <- ifelse(SK$ddvif_0 == "Normal", 0, 1)
SK$dd_3 <- ifelse(SK$ddvif_3 == "Normal", 0, 1)
SK$dd_6 <- ifelse(SK$ddvif_6 == "Normal", 0, 1)
SK$dd_12 <- ifelse(SK$ddvif_12== "Normal", 0, 1)

SK$dd_0<-factor(SK$dd_0)
SK$dd_3<-factor(SK$dd_3)
SK$dd_6<-factor(SK$dd_6)
SK$dd_12<-factor(SK$dd_12)
SK$htn<-factor(SK$htn)
SK$diabetes<-factor(SK$diabetes)
SK$hyperchol<-factor(SK$hyperchol)
SK$hypertri<-factor(SK$hypertri)
SK$triple_neg<-factor(SK$triple_neg)
SK$smoking<-factor(SK$smoking)

n_filas <- nrow(SK) * 3  
SKcox <- data.frame(id = rep(NA, n_filas),
                    visita = rep(NA, n_filas),
                    dd = rep(NA, n_filas),
                    cardiotox = rep(NA, n_filas),
                    start = rep(NA, n_filas),
                    stop = rep(NA, n_filas),
                    relación_ea = rep(NA, n_filas),
                    relación_ee = rep(NA, n_filas),
                    e = rep(NA, n_filas),
                    peak = rep(NA, n_filas),
                    volumen = rep(NA, n_filas),
                    tiempo_de = rep(NA, n_filas),
                    edad_0 = rep(NA, n_filas),
                    htn = rep(NA, n_filas),
                    diabetes = rep(NA, n_filas),
                    hypertri = rep(NA, n_filas),
                    hyperchol = rep(NA, n_filas),
                    smoking = rep(NA, n_filas),
                    triple_neg = rep(NA, n_filas))
ids_repetidos <- rep(SK$id, each = 3)
visitas <- rep(c(1, 2, 3), times = nrow(SK))
dd_values <- c(SK$dd_0, SK$dd_3, SK$dd_6)
cardiotox_values <- c(SK$cardiotoxDich_three, SK$cardiotoxDich_six, SK$cardiotoxDich_twelve)
relacion_ea_values <- c(SK$relación_ea_0, SK$relación_ea_3, SK$relación_ea_6)
relacion_ee_values<- c(SK$relación_ee___medial_0, SK$relación_ee_3, SK$relación_ee_6)
es_values<- c(SK$e_0, SK$e_3, SK$e_6)
peak_values<-c(SK$velocidad__peak_it_0, SK$velocidad__peak_it_3, SK$velocidad__peak_it_6)
volume_values<-c(SK$volumen_auricular_indexado_izquierdo_0, SK$volumen_auricular_indexado_izquierdo_3, SK$volumen_auricular_indexado_izquierdo_6)
tiempo_de_values<-c(SK$tiempo_de_desaceleración_0, SK$tiempo_de_desaceleración_3, SK$tiempo_de_desaceleración_6)

start_values <- rep(c(0, 3, 6), times = nrow(SK))
stop_values <- rep(c(3, 6, 12), times = nrow(SK))

SKcox$id <- ids_repetidos
SKcox$visita <- visitas
SKcox$dd <- dd_values
SKcox$cardiotox <- cardiotox_values
SKcox$start <- start_values
SKcox$stop <- stop_values
SKcox$relación_ea <- relacion_ea_values
SKcox$relación_ee<-relacion_ee_values
SKcox$e<-es_values
SKcox$peak<-peak_values
SKcox$volumen<-volume_values
SKcox$tiempo_de<-tiempo_de_values

for (var in c("htn", "diabetes", "hypertri", "hyperchol", "smoking", "triple_neg", "edad_0")) {
  SKcox[[var]] <- rep(SK[[var]], each = 3)
}


SKcoxf <- data.frame()

for(id in unique(SKcox$id)) {
  
  subset_id <- SKcox[SKcox$id == id, ]
  
  visita_cardio <- which(subset_id$cardiotox == 1)
  
  if(length(visita_cardio) == 0) {
    SKcoxf <- rbind(SKcoxf, subset_id)
  } else {
    if(1 %in% visita_cardio) {
      subset_id <- subset_id[1, , drop = FALSE]  
    }
    if(2 %in% visita_cardio) {
      subset_id <- subset_id[1:2, , drop = FALSE]  
    }
    
    SKcoxf <- rbind(SKcoxf, subset_id)
  }
}


SKcoxf<-na.omit(SKcoxf)

## 4.2 Weight calculation ####

weights_df<-weightit(relación_ea ~ edad_0 + htn + diabetes + hypertri+ hyperchol + smoking + triple_neg ,data = SKcoxf)


SKcoxf$ipw_v1<-weights_df$weights


max_weight_v1 <- quantile(SKcoxf$ipw_v1, 0.99)
SKcoxf$ipw_v1 <- ifelse(SKcoxf$ipw_v1 > max_weight_v1, max_weight_v1, SKcoxf$ipw_v1)

## 4.3 IPW Cox models template####

model_v1<- modelo_cox <- coxph(Surv(start, stop, cardiotox) ~ tiempo_de + cluster(id), data = SKcoxf, weights = ipw_v1)
summary(model_v1)


## 4.4 IPW Cox models in >50 years template####
SKcoxf2<-SKcoxf %>% 
  filter(edad_0>=50)

weights_df<-weightit(tiempo_de ~ edad_0 + htn + diabetes + hypertri+ hyperchol + smoking + triple_neg ,data = SKcoxf2)


SKcoxf2$ipw_v1<-weights_df$weights


max_weight_v1 <- quantile(SKcoxf2$ipw_v1, 0.99)
SKcoxf2$ipw_v1 <- ifelse(SKcoxf2$ipw_v1 > max_weight_v1, max_weight_v1, SKcoxf2$ipw_v1)



model_v2<- modelo_cox <- coxph(Surv(start, stop, cardiotox) ~ tiempo_de + cluster(id), data = SKcoxf2, weights = ipw_v1)
summary(model_v2)

## 4.5 IPW Cox models in <50 years template####
SKcoxf2<-SKcoxf %>% 
  filter(edad_0<50)

weights_df<-weightit(tiempo_de ~ edad_0 + htn + diabetes + hypertri+ hyperchol + smoking + triple_neg ,data = SKcoxf2)


SKcoxf2$ipw_v1<-weights_df$weights


max_weight_v1 <- quantile(SKcoxf2$ipw_v1, 0.99)
SKcoxf2$ipw_v1 <- ifelse(SKcoxf2$ipw_v1 > max_weight_v1, max_weight_v1, SKcoxf2$ipw_v1)



model_v2<- modelo_cox <- coxph(Surv(start, stop, cardiotox) ~ tiempo_de + cluster(id), data = SKcoxf2, weights = ipw_v1)
summary(model_v2)

 
