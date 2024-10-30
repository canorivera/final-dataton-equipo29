#base de datos para dataton

# Desactivamos la notación científica
options(scipen = 999999)

# Cargamos los paquetes que requerimos
if (!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(tidyverse, fixest, ggplot2, sf, janitor, readxl, broom, purrr)

#enighs
concentrado_hogar_2022 = read_csv("C:/Users/luism/Documents/final_dataton/data/enigh/2022/concentradohogar.csv")
concentrado_hogar_2020 = read_csv("C:/Users/luism/Documents/final_dataton/data/enigh/2020/concentradohogar.csv")
concentrado_hogar_2018 = read_csv("C:/Users/luism/Documents/final_dataton/data/enigh/2018/concentradohogar.csv")
concentrado_hogar_2016 = read_csv("C:/Users/luism/Documents/final_dataton/data/enigh/2016/concentradohogar.csv")

#creamos folios por hogar y añadimos decil a los datos
lista_var=c("gasto_mon", "alimentos", "vivienda", "limpieza", "salud", "transporte", "ano","ubica_geo", "clase_hog",
            "educa_espa", "personales", "transf_gas", "ing_cor", "vesti_calz", "folioviv","foliohog","decil")
concentrado_hogar_2022 = concentrado_hogar_2022 %>%
  mutate(foliohog = paste(folioviv, foliohog, sep=""),
         decil = ntile(ing_cor,10), ano=2022)   %>%
  select(any_of(lista_var)) 
concentrado_hogar_2020 = concentrado_hogar_2020 %>%
  mutate(foliohog = paste(folioviv, foliohog, sep=""),
         decil = ntile(ing_cor,10), ano=2020) %>%
  select(any_of(lista_var)) 
concentrado_hogar_2018 = concentrado_hogar_2018 %>%
  mutate(foliohog = paste(folioviv, foliohog, sep=""),
         decil = ntile(ing_cor,10), ano=2018) %>%
  select(any_of(lista_var)) 
concentrado_hogar_2016 = concentrado_hogar_2016 %>%
  mutate(foliohog = paste(folioviv, foliohog, sep=""),
         decil = ntile(ing_cor,10), ano=2016,
         ubica_geo = substr(ubica_geo,1,5)) %>%
  select(any_of(lista_var)) 
conc_total = rbind(concentrado_hogar_2016, concentrado_hogar_2018,concentrado_hogar_2020, concentrado_hogar_2022)
conc_total = conc_total %>% mutate(ag_rep=paste(as.character(ano), ubica_geo, as.character(clase_hog),as.character(decil), sep=""))
conc_total = conc_total %>% relocate(ag_rep)


#agregamos
agg_conc_2022 = concentrado_hogar_2022 %>%
  group_by(ubica_geo, clase_hog, decil) %>%
  summarise(gasto=mean(gasto_mon)+1,alimentos=mean(alimentos)+1, vivienda=mean(vivienda)+1,limpieza=mean(limpieza)+1,
            salud=mean(salud)+1, transporte=mean(transporte)+1, educa_espa=mean(educa_espa)+1, personales=mean(personales)+1,
            transf_gas=mean(transf_gas)+1, ingreso=mean(ing_cor)+1, vesti_calz=mean(vesti_calz)+1)%>%
  ungroup()

agg_conc_2020 = concentrado_hogar_2020 %>%
  group_by(ubica_geo, clase_hog, decil) %>%
  summarise(gasto=mean(gasto_mon)+1,alimentos=mean(alimentos)+1, vivienda=mean(vivienda)+1,limpieza=mean(limpieza)+1,
            salud=mean(salud)+1, transporte=mean(transporte)+1, educa_espa=mean(educa_espa)+1, personales=mean(personales)+1,
            transf_gas=mean(transf_gas)+1, ingreso=mean(ing_cor)+1, vesti_calz=mean(vesti_calz)+1)%>%
  ungroup()

agg_conc_2018 = concentrado_hogar_2018 %>%
  group_by(ubica_geo, clase_hog, decil) %>%
  summarise(gasto=mean(gasto_mon)+1,alimentos=mean(alimentos)+1, vivienda=mean(vivienda)+1,limpieza=mean(limpieza)+1,
            salud=mean(salud)+1, transporte=mean(transporte)+1, educa_espa=mean(educa_espa)+1, personales=mean(personales)+1,
            transf_gas=mean(transf_gas)+1, ingreso=mean(ing_cor)+1, vesti_calz=mean(vesti_calz)+1)%>%
  ungroup()

agg_conc_2016 = concentrado_hogar_2016 %>%
  group_by(ubica_geo, clase_hog, decil) %>%
  summarise(gasto=mean(gasto_mon)+1,alimentos=mean(alimentos)+1, vivienda=mean(vivienda)+1,limpieza=mean(limpieza)+1,
            salud=mean(salud)+1, transporte=mean(transporte)+1, educa_espa=mean(educa_espa)+1, personales=mean(personales)+1,
            transf_gas=mean(transf_gas)+1, ingreso=mean(ing_cor)+1, vesti_calz=mean(vesti_calz)+1)%>%
  ungroup()
agg_conc_2016$ano=2016
agg_conc_2018$ano=2018
agg_conc_2020$ano=2020
agg_conc_2022$ano=2022

agg_conc = bind_rows(agg_conc_2016, agg_conc_2018, agg_conc_2020, agg_conc_2022)
agg_conc = agg_conc %>% relocate(ano)
agg_conc = agg_conc %>% mutate(ag_rep=paste(as.character(ano), ubica_geo, as.character(clase_hog),as.character(decil), sep=""))
agg_conc = agg_conc %>% relocate(ag_rep)
#guardar
save(agg_conc,file = "C:/Users/luism/Documents/final_dataton/output/agg_enigh.csv")

#regresiones
reg1=feols(gasto_mon ~ clase_hog | ubica_geo+decil, 
           data=concentrado_hogar_2022)
reg2=feols(personales ~ decil | ubica_geo+clase_hog, 
           data=concentrado_hogar_2022)
reg3=feols(personales ~ clase_hog | ubica_geo+decil, 
           data=concentrado_hogar_2022)
reg4=feols(educa_espa ~ decil | ubica_geo+clase_hog, 
            data=concentrado_hogar_2022)
reg5=feols(educa_espa ~ clase_hog | ubica_geo+decil, 
           data=concentrado_hogar_2022)

etable(reg1, reg2, reg3, reg4, reg5)
reg_cd=feols(log(ing_cor) ~ log(alimentos)+log(vivienda)+log(limpieza)+log(salud)+
               log(transporte)+log(educa_espa)+log(personales | ano+ubica_geo+clase_hog+decil ), 
             data=conc_total, split=~ubica_geo)

coeficientes <- coeftable(reg_cd)
filt = coeficientes %>%
  filter(Estimate<1 & Estimate>0 & Estimate < `Std. Error`& is.na(`Std. Error`)==F & coefficient!="(Intercept)")

#guardar 
save(coeficientes,file = "C:/Users/luism/Documents/final_dataton/output/coef_cd.csv")
save(filt,file = "C:/Users/luism/Documents/final_dataton/output/coef_cd_filt.csv")
