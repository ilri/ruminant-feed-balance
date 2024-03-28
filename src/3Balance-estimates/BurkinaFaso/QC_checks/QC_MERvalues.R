library(dplyr)
library(tidyr)

param_ME <- read.csv('../LivestockParams/Livestock_energy_requirement.csv', stringsAsFactors = F)
param_ME <- pivot_longer(param_ME, cols = c("Bull", "Steer", "Calf", "Heifer", "Cow", "Lamb", "Sheep", "Kid", "Goat"))

ECM <- 3.054 #energy content of milk MJ/kg (CSIRO,2007)
EC <- 20 #energy content of the tissue=MJ/kg
#MERl <- (DMY*ECM)/((0.02*M.D)+0.04)
#MERg_Positive=(DWG*0.92*EC)/(0.043*M.D)
#MERg_Negative=(DWG*0.92*EC)/(0.8)

MERl_WS_Sah_cow <- (param_ME$value[param_ME$Variable=="DMY" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                      ECM)/
                  ((0.02*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"])
                  +0.04)

MERg_WS_Sah_cow <- ifelse(param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"] >= 0,
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 
                            (0.043*param_ME$value[param_ME$Variable == "M.D" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]),
                          (param_ME$value[param_ME$Variable=="DWG" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]*
                             0.92*EC) / 0.8)

MERl_WS_Sah_cow / param_ME$value[param_ME$Variable == "MERl" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]
MERg_WS_Sah_cow / param_ME$value[param_ME$Variable == "MERg" & param_ME$name == "Cow" & param_ME$Season == "WS" & param_ME$Region == "Sah"]


#####
###Sudanian
(MERm_DS_Sud_cow * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"]+
MERm_WS_Sud_cow * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"])/365

(MERg_DS_Sud_cow * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"]+
MERg_WS_Sud_cow * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"])/365

((MERt_DS_Sud_cow)*param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"] +
(MERt_WS_Sud_cow) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sud"])/365

MERp_cow_fullPreg * param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/365

((MERl_DS_Sud_cow* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
    param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2)+
(MERl_WS_Sud_cow* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
     param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2))/365

#Sheep
((MERm_DS_Sud_sheep) *  param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"] +
    (MERm_WS_Sud_sheep) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"])/365

((MERg_DS_Sud_sheep) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"] +
    (MERg_WS_Sud_sheep) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"])/365

((MERt_DS_Sud_sheep) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"] +
    (MERt_WS_Sud_sheep) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sud"])/365

MERp_shoat_fullPreg *  param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/365

((MERl_DS_Sud_sheep* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
    param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2)+
    (MERl_WS_Sud_sheep* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
       param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2))/365

#Goats
((MERm_DS_Sud_goat) *  param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"] +
    (MERm_WS_Sud_goat) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"])/365

((MERg_DS_Sud_goat) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"] +
    (MERg_WS_Sud_goat) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"])/365

((MERt_DS_Sud_goat) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"] +
    (MERt_WS_Sud_goat) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sud"])/365

MERp_shoat_fullPreg *  param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/365

((MERl_DS_Sud_goat* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
    param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2)+
    (MERl_WS_Sud_goat* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
       param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2))/365

###Sahelian
((MERm_DS_Sah_cow) *  param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"] +
(MERm_WS_Sah_cow) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"])/365

((MERg_DS_Sah_cow) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"] +
(MERg_WS_Sah_cow) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"])/365

((MERt_DS_Sah_cow) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"] +
(MERt_WS_Sah_cow) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Cow" & param_ME$Region == "Sah"])/365

MERp_cow_fullPreg *  param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/365

((MERl_DS_Sah_cow* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
    param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2)+
(MERl_WS_Sah_cow* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Cow"]*
       param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Cow"]/2))/365


#Sheep
((MERm_DS_Sah_sheep) *  param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"] +
    (MERm_WS_Sah_sheep) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"])/365

((MERg_DS_Sah_sheep) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"] +
    (MERg_WS_Sah_sheep) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"])/365

((MERt_DS_Sah_sheep) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"] +
    (MERt_WS_Sah_sheep) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Sheep" & param_ME$Region == "Sah"])/365

MERp_shoat_fullPreg *  param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/365

((MERl_DS_Sah_sheep* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
    param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2)+
    (MERl_WS_Sah_sheep* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Sheep"]*
       param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Sheep"]/2))/365

#Goats
((MERm_DS_Sah_goat) *  param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"] +
    (MERm_WS_Sah_goat) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"])/365

((MERg_DS_Sah_goat) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"] +
    (MERg_WS_Sah_goat) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"])/365

((MERt_DS_Sah_goat) * param_ME$value[param_ME$Variable=="DSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"] +
    (MERt_WS_Sah_goat) * param_ME$value[param_ME$Variable=="WSdays" & param_ME$name == "Goat" & param_ME$Region == "Sah"])/365

MERp_shoat_fullPreg *  param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/365

((MERl_DS_Sah_goat* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
    param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2)+
    (MERl_WS_Sah_goat* param_ME$value[param_ME$Variable=="LL" & param_ME$name == "Goat"]*
       param_ME$value[param_ME$Variable=="Fertility" & param_ME$name == "Goat"]/2))/365
