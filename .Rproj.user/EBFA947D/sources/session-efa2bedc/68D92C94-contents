
rm(list = ls(all = TRUE))

# libraries needed
library(tidyverse)  
library(tidyselect) 
library(haven)      
library(labelled)   
library(xlsx)     
library(sjlabelled) 
library(srvyr)
library(survey)  
library(tidyr)
library(lme4)
library(svylme)


## Importation des deux base

togo_grd2 <- read_dta("PASEC2019_GRADE2/PASEC2019_GRADE2.dta") 

togo_grd6 <- read_dta("PASEC2019_GRADE6/PASEC2019_GRADE6.dta") 


# Création des design pour la pondération
# En début de scolarité

dstrata2 <- togo_grd2 |> 
  as_survey_design(ids = ID_TOT, strata = ID_STRATE, weights = rwgt0) |>
  filter(PAYS == 'TOGO') |> ## garder uniquement les données du togo
  filter(qd17 != 3,qd17 != 9) ## enlever les données du communautaire(3) et autre (9)

## calcul des scores
dstrata2 <- dstrata2 |> 
  mutate(lng2 = (LECT_PV1 + LECT_PV2 + LECT_PV3 + LECT_PV4 + LECT_PV5)/5,
         math2 = (MATHS_PV1 + MATHS_PV2 + MATHS_PV3 + MATHS_PV4 + MATHS_PV5)/5) 

# En fin de scolarité
dstrata6 <- togo_grd6 |> 
  as_survey_design(strata = ID_STRATE, weights = rwgt0) |> 
  filter(PAYS == 'TOGO') |> ## garder uniquement les données du togo
  filter(qd17 != 3,qd17 != 9)## enlever les données du communautaire(3) et autre (9)

## calcul des scores
dstrata6 <- dstrata6 |> 
  mutate(lng6 = (LECT_PV1 + LECT_PV2 + LECT_PV3 + LECT_PV4 + LECT_PV5)/5,
         math6 = (MATHS_PV1 + MATHS_PV2 + MATHS_PV3 + MATHS_PV4 + MATHS_PV5)/5)



