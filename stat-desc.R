rm(list = ls(all = TRUE))

# libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)    # for creating tables with Haven labeled data
library(xlsx)     # for exporting to excel
library(naniar)   # to use replace_with_na function
library(here)       # to get R project path
library(sjlabelled) # to set variables label
library(srvyr)
library(survey)  # to calculate weighted ratio for GAR
library(tidyr)
library(labelled)
library(gtsummary)

# Importation des données
togo_grd2 <- read_dta("PASEC2019_GRADE2/PASEC2019_GRADE2.dta")|>
  filter(PAYS == 'TOGO')  

togo_grd6 <- read_dta("PASEC2019_GRADE6/PASEC2019_GRADE6.dta") |> 
  filter(PAYS == 'TOGO')

# Création des design pour la pondération
# En début de scolarité
dstrata2 <- togo_grd2 %>%
  as_survey_design(ids = ID_TOT, strata = ID_STRATE, weights = rwgt0) |>  
  filter(qd17 != 3,qd17 != 9)

dstrata2 <- dstrata2 |> 
  mutate(lng2 = (LECT_PV1 + LECT_PV2 + LECT_PV3 + LECT_PV4 + LECT_PV5)/5,
         math2 = (MATHS_PV1 + MATHS_PV2 + MATHS_PV3 + MATHS_PV4 + MATHS_PV5)/5)

dstrata2 <- dstrata2 |> 
  mutate(
    pe = ifelse(qd70 == 1 ,1,0),
    doub_flu = ifelse(qd23 == 1 ,1,0),
    multigrad = ifelse(qd21 == 1 ,1,0),
    coop = ifelse(qd45d == 3 ,1,0),
    COGEP = ifelse(qd45e == 3 ,1,0),
    AME = ifelse(qd45c == 3,1,0), 
    APEE = ifelse(qd45b == 3,1,0),
    APE = ifelse(qd45a == 3,1,0), 
    cloture = ifelse(qd78i == 1,1,0), 
    urbain = ifelse(qd31 == 1,1,0),
    ecole_pub = ifelse(qd17 == 1,1,0),
    fille_cp = ifelse(qe23 == 2,1,0),
    redouble_cp = ifelse(qe210 == 1,1,0),
    presco = ifelse(qe25 == 1,1,0),
    fr_msn_cp = ifelse(qe28 %in% c(1,2),1,0), # toujours ou souvent
    liv_msn = ifelse(qe211 == 1,1,0),
    tab_msn = ifelse(qe212 == 1,1,0),
    lect_msn = ifelse(qe213 == 1,1,0),
    lect_class = ifelse(qe214 == 1,1,0),
    math_class = ifelse(qe216 == 1,1,0),
    dip_academie = case_when(
      qm3 == 2 ~'premiere',
      qm3 == 4 ~'terminale',
      qm3 == 6 ~'BAC+1',
      qm3 == 8 ~'BAC+2',
      qm3 == 9 ~'3ieme',
      qm3 == 10 ~'BAC+3',
      qm3 == 11 ~'seconde'),
    dip_pro = case_when(
        qm4== 1 | qm4== 99 ~ 'Aucun',
        qm4== 2 ~ 'CAM',
        qm4== 5 ~ 'CAP-CFEN',
        qm4== 6 ~ 'Aucun')
    
  ) 

### Regression en langue

glm_lng_cp <- svyglm(lng2 ~ fille_cp + redouble_cp + presco+ fr_msn_cp+liv_msn+tab_msn +
                       lect_msn + lect_class + math_class + doub_flu + multigrad + pe +
                       coop + COGEP + AME + APEE + APE + cloture +urbain + ecole_pub +
                       qd69a + qd3 + qd4 + qm5 + dip_academie + dip_pro,
                     design = dstrata2)

glm_lng_cp2 <- step(glm_lng_cp)

summary(glm_lng_cp2)

reg_lang_cp <- broom::tidy(glm_lng_cp2) |>
  writexl::write_xlsx('export/reg_lang_cp.xlsx')

## modele langue : publique

dstrata2 |> 
  filter(qd17 == 1) -> dstrata2_pub

glm_lng_cp_pub <- svyglm(lng2 ~ fille_cp + redouble_cp + presco + fr_msn_cp + liv_msn + tab_msn +
                       lect_msn + lect_class + math_class + doub_flu + multigrad + pe +
                       coop +COGEP + AME + APEE + APE + cloture +urbain + ecole_pub +
                       qd69a + qd3 + qd4 + qm5 + dip_academie + dip_pro,
                     design = dstrata2_pub)

glm_lng_cp_pub <- step(glm_lng_cp_pub)

summary(glm_lng_cp_pub)

reg_lang_cp_pub <- broom::tidy(glm_lng_cp_pub) |>
  writexl::write_xlsx('export/reg_lang_cp_pub.xlsx')



## modele privé

dstrata2 |> 
  filter(qd17 == 2) |> 
  srvyr::drop_na(dip_academie) -> dstrata2_priv

dstrata2_priv |> 
  group_by(dip_pro) |> 
  summarise(survey_mean())

glm_lng_cp_priv <- svyglm(lng2 ~ fille_cp + redouble_cp + presco+ fr_msn_cp+liv_msn+tab_msn +
                           lect_msn + lect_class + math_class + doub_flu + multigrad + pe +
                           COGEP + AME + APEE + APE + cloture +urbain  + dip_academie +
                           qd69a + qd3 + qd4,design = dstrata2_priv)

  

glm_lng_cp_priv <- step(glm_lng_cp_priv)

summary(glm_lng_cp_priv)

reg_lang_cp_priv <- broom::tidy(glm_lng_cp_priv) |>
  writexl::write_xlsx('export/reg_lang_cp_priv.xlsx')




### Regression en math

# modèle complet
glm_math_cp <- svyglm(math2 ~ fille_cp + redouble_cp + presco+ fr_msn_cp+liv_msn+tab_msn +
                       lect_msn + lect_class + math_class + doub_flu + multigrad + pe +
                       coop + COGEP + AME + APEE + APE + cloture +urbain + ecole_pub +
                       qd69a + qd3 + qd4 + qm5 + dip_academie + dip_pro,
                     design = dstrata2)

glm_math_cp2 <- step(glm_math_cp)

summary(glm_math_cp2)

broom::tidy(glm_math_cp2) |>
  writexl::write_xlsx('export/reg_math_cp.xlsx')

# modèle public

dstrata2 |> 
  filter(qd17 == 1) -> dstrata2_pub

glm_math_cp_pub <- svyglm(math2 ~ fille_cp + redouble_cp + presco+ fr_msn_cp+liv_msn+tab_msn +
                        lect_msn + lect_class + math_class + doub_flu + multigrad + pe +
                        coop + COGEP + AME + APEE + APE + cloture +urbain + ecole_pub +
                        qd69a + qd3 + qd4 + qm5 + dip_academie + dip_pro,
                      design = dstrata2_pub)

glm_math_cp_pub <- step(glm_math_cp_pub)

summary(glm_math_cp_pub)

broom::tidy(glm_math_cp_pub) |>
  writexl::write_xlsx('export/reg_math_cp_pub.xlsx')



# Privé

dstrata2 |> 
  filter(qd17 == 2) |> 
  srvyr::drop_na(dip_academie) -> dstrata2_priv

glm_math_cp_priv <- svyglm(math2 ~ fille_cp + redouble_cp + presco+ fr_msn_cp+liv_msn+tab_msn +
                            lect_msn + lect_class + math_class + doub_flu + multigrad + pe +
                            coop + COGEP + AME + APEE + APE + cloture +urbain + ecole_pub +
                            qd69a + qd3 + qd4 + qm5 + dip_academie,
                          design = dstrata2_priv)

glm_math_cp_priv <- step(glm_math_cp_priv)

summary(glm_math_cp_priv)

broom::tidy(glm_math_cp_priv) |>
  writexl::write_xlsx('export/glm_math_cp_priv.xlsx')



dstrata2 |> 
  group_by(qd17) |> 
  summarise(
    fille_cp = survey_mean(fille_cp),
    age_cp = survey_mean(qe22),
    redouble_cp = survey_mean(redouble_cp),
    presco = survey_mean(presco),
    fr_msn_cp = survey_mean(fr_msn_cp),
    liv_msn = survey_mean(liv_msn),
    tab_msn = survey_mean(tab_msn),
    lect_msn = survey_mean(lect_msn),
    lect_class = survey_mean(lect_class),
    math_class = survey_mean(math_class)
  ) |> 
  writexl::write_xlsx('export/dsc_cp_nat.xlsx')


dstrata6 <- togo_grd6 |> 
  as_survey_design(strata = ID_STRATE, weights = rwgt0) |> 
  filter(qd17 != 3,qd17 != 9)

dstrata6 <- dstrata6 |> 
  mutate(lng6 = (LECT_PV1 + LECT_PV2 + LECT_PV3 + LECT_PV4 + LECT_PV5)/5,
         math6 = (MATHS_PV1 + MATHS_PV2 + MATHS_PV3 + MATHS_PV4 + MATHS_PV5)/5)


dstrata6 <- dstrata6 |> 
  mutate(
    pe = ifelse(qd70 == 1 ,1,0),
    doub_flu = ifelse(qd23 == 1 ,1,0),
    multigrad = ifelse(qd21 == 1 ,1,0),
    coop = ifelse(qd45d == 3 ,1,0),
    COGEP = ifelse(qd45e == 3 ,1,0),
    AME = ifelse(qd45c == 3,1,0), 
    APEE = ifelse(qd45b == 3,1,0),
    APE = ifelse(qd45a == 3,1,0), 
    cloture = ifelse(qd78i == 1,1,0), 
    urbain = ifelse(qd31 == 1,1,0),
    ecole_pub = ifelse(qd17 == 1,1,0),
    fille_cm = ifelse(qe63 == 2,1,0),
    presco = ifelse(qe66 == 1,1,0),
    redouble_cm = ifelse(qe68 == 1,1,0),
    manger_msn = ifelse(qe612 %in% c(1,2),1,0),
    faim_class = ifelse(qe613 %in% c(1,2),1,0),
    manger_midi = ifelse(qe614 == 1,1,0),
    trv_dom = ifelse(qe616 %in% c(1,2),1,0),
    trv_agri = ifelse(qe617 %in% c(1,2),1,0),
    trv_com = ifelse(qe618 %in% c(1,2),1,0),
    fr_msn = ifelse(qe620 %in% c(1,2),1,0),
    dvr_msn = ifelse(qe622 == 1,1,0),
    lect_msn = ifelse(qe626 == 1,1,0),
    livr_lect_class = ifelse(qe627 == 1,1,0),
    livre_math_class = ifelse(qe629 == 1,1,0),
    livre_msn = ifelse(qe631 == 1,1,0),
    soutien_cp = ifelse(qd58 == 1,1,0),
    soutien_cm = ifelse(qd60 == 1,1,0),
    cantine = ifelse(qd85 == 1,1,0),
    ens_fem = ifelse(qm1 == 2,1,0),
    dir_fem = ifelse(qd2 == 2,1,0),
    inspect = ifelse(qd35 == 1,1,0),
    dip_academie_dir = case_when(
      qd11 == 1 ~'inf_6eme',
      qd11 == 2 ~'premiere',
      qd11 == 3 ~'6eme',
      qd11 == 4 ~'terminale',
      qd11 == 5 ~'5eme',
      qd11 == 6 ~'BAC+1',
      qd11 == 7 ~'4eme',
      qd11 == 8 ~'BAC+2',
      qd11 == 9 ~'3ieme',
      qd11 == 10 ~'BAC+3',
      qd11 == 11 ~'seconde',
      qd11 == 12 ~'BAC+4 ou plus'),
    dip_pro_dir = case_when(
      qd12== 1 | qd12==99 | qd12==96  ~ 'Aucun',
      qd12== 4 ~ 'CAP',
      qd12== 5 ~ 'BSEN',
      qd12== 7 ~ 'BSC1',
      qd12== 8 ~ 'BSC2',
      ),
    dip_academie = case_when(
      qm3 == 2 ~'premiere',
      qm3 == 4 ~'terminale',
      qm3 == 5 ~'5eme',
      qm3 == 6 ~'BAC+1',
      qm3 == 8 ~'BAC+2',
      qm3 == 9 ~'3ieme',
      qm3 == 10 ~'BAC+3',
      qm3 == 11 ~'seconde',
      qm3 == 12 ~'BAC+4 ou plus'),
    dip_pro = case_when(
      qm4== 1 | qm4== 99 ~ 'Aucun',
      qm4== 2 ~ 'CAM',
      qm4== 3 ~ 'CEAP-CFEN',
      qm4== 4 ~ 'CEAP',
      qm4== 5 ~ 'CAP-CFEN',
      qm4== 6 ~ 'CAP'),
    ecole_pub = ifelse(qd17 == 1,1,0)
    
  )


### CM Regression en langue

glm_lng_cm <- svyglm(lng6 ~ pe + ecole_pub + doub_flu + multigrad + coop + COGEP + AME + APEE + APE +
                       cloture + urbain + ecole_pub + fille_cm + presco + redouble_cm + manger_msn +
                       faim_class + manger_midi + trv_dom + trv_agri + trv_com + fr_msn + dvr_msn + lect_msn +
                       livr_lect_class + livre_math_class + livre_msn + soutien_cp + soutien_cm + cantine + ens_fem +
                       inspect + dip_academie + dip_pro + qd1 + dip_academie_dir + dip_pro_dir + dip_academie + dip_pro,
                     design = dstrata6)

glm_lng_cm <- step(glm_lng_cm)

summary(glm_lng_cm)

broom::tidy(glm_lng_cm) |>
  writexl::write_xlsx('export/glm_lng_cm.xlsx')

## modele langue : publique

dstrata6 |> 
  filter(qd17 == 1) -> dstrata6_pub

glm_lng_cm_pub <- svyglm(lng6 ~ pe + ecole_pub + doub_flu + multigrad + coop + COGEP + AME + APEE + APE +
                           cloture + urbain + ecole_pub + fille_cm + presco + redouble_cm + manger_msn +
                           faim_class + manger_midi + trv_dom + trv_agri + trv_com + fr_msn + dvr_msn + lect_msn +
                           livr_lect_class + livre_math_class + livre_msn + soutien_cp + soutien_cm + cantine + ens_fem +
                           inspect + dip_academie + dip_pro + qd1 + dip_academie_dir + dip_pro_dir + dip_academie + dip_pro,
                         design = dstrata6_pub)

glm_lng_cm_pub <- step(glm_lng_cm_pub)

summary(glm_lng_cm_pub)

 broom::tidy(glm_lng_cm_pub) |>
  writexl::write_xlsx('export/glm_lng_cm_pub.xlsx')



## modele privé

dstrata6 |> 
  filter(qd17 == 2) |> 
  srvyr::drop_na(dip_academie_dir) |> 
  srvyr::drop_na(dip_academie) |> 
  srvyr::drop_na(dip_pro) |> 
  srvyr::drop_na(dip_pro_dir) -> dstrata6_priv

dstrata6_priv |> 
  group_by(dip_academie) |> 
  summarise(survey_mean())

glm_lng_cm_priv <- svyglm(lng6 ~ pe + ecole_pub + doub_flu + multigrad + coop + COGEP + AME + APEE + APE +
                            cloture + urbain + fille_cm + presco + redouble_cm + manger_msn +
                            faim_class + manger_midi + trv_dom + trv_agri + trv_com + fr_msn + dvr_msn + lect_msn +
                            livr_lect_class + livre_math_class + livre_msn + soutien_cp + soutien_cm + cantine + ens_fem +
                            inspect + dip_academie + dip_pro + qd1 + dip_academie_dir + dip_pro_dir ,
                          design = dstrata6_priv)



glm_lng_cm_priv <- step(glm_lng_cm_priv)

summary(glm_lng_cm_priv)

broom::tidy(glm_lng_cm_priv) |>
  writexl::write_xlsx('export/glm_lng_cm_priv.xlsx')




### Regression en math

# modèle complet
glm_math_cm <- svyglm(math6 ~ pe + ecole_pub + doub_flu + multigrad + coop + COGEP + AME + APEE + APE +
                        cloture + urbain + ecole_pub + fille_cm + presco + redouble_cm + manger_msn +
                        faim_class + manger_midi + trv_dom + trv_agri + trv_com + fr_msn + dvr_msn + lect_msn +
                        livr_lect_class + livre_math_class + livre_msn + soutien_cp + soutien_cm + cantine + ens_fem +
                        inspect + dip_academie + dip_pro + qd1 + dip_academie_dir + dip_pro_dir,
                      design = dstrata6)

glm_math_cm <- step(glm_math_cm)

summary(glm_math_cm)

broom::tidy(glm_math_cm) |>
  writexl::write_xlsx('export/glm_math_cm.xlsx')

# modèle public

glm_math_cm_pub <- svyglm(math6 ~ pe + ecole_pub + doub_flu + multigrad + coop + COGEP + AME + APEE + APE +
                            cloture + urbain + ecole_pub + fille_cm + presco + redouble_cm + manger_msn +
                            faim_class + manger_midi + trv_dom + trv_agri + trv_com + fr_msn + dvr_msn + lect_msn +
                            livr_lect_class + livre_math_class + livre_msn + soutien_cp + soutien_cm + cantine + ens_fem +
                            inspect + dip_academie + dip_pro + qd1 + dip_academie_dir + dip_pro_dir,
                          design = dstrata6_pub)

glm_math_cm_pub <- step(glm_math_cm_pub)

summary(glm_math_cm_pub)

broom::tidy(glm_math_cm_pub) |>
  writexl::write_xlsx('export/glm_math_cm_pub.xlsx')



# Privé


glm_math_cm_priv <- svyglm(math6 ~ pe + ecole_pub + doub_flu + multigrad + coop + COGEP + AME + APEE + APE +
                             cloture + urbain + fille_cm + presco + redouble_cm + manger_msn +
                             faim_class + manger_midi + trv_dom + trv_agri + trv_com + fr_msn + dvr_msn + lect_msn +
                             livr_lect_class + livre_math_class + livre_msn + soutien_cp + soutien_cm + cantine + ens_fem +
                             inspect + dip_academie + dip_pro + qd1 + dip_academie_dir + dip_pro_dir ,
                           design = dstrata6_priv)

glm_math_cm_priv <- step(glm_math_cm_priv)

summary(glm_math_cm_priv)

broom::tidy(glm_math_cm_priv) |>
  writexl::write_xlsx('export/glm_math_cm_priv.xlsx')



dstrata6 |> 
  #group_by(qd17) |> 
  summarise( fille_cm = survey_mean(fille_cm),
            age = survey_mean(qe62),
            presco = survey_mean(presco),
            redouble_cm = survey_mean(redouble_cm),
            manger_msn = survey_mean(manger_msn),
            faim_class = survey_mean(faim_class),
            manger_midi = survey_mean(manger_midi),
            trv_dom = survey_mean(trv_dom),
            trv_agri = survey_mean(trv_agri),
            trv_com = survey_mean(trv_com),
            fr_msn = survey_mean(fr_msn),
            dvr_msn = survey_mean(dvr_msn),
            lect_msn = survey_mean(lect_msn),
            livr_lect_class = survey_mean(livr_lect_class),
            livre_math_class = survey_mean(livre_math_class),
            livre_msn = survey_mean(livre_msn),
            soutien_cp = survey_mean(soutien_cp),
            soutien_cm = survey_mean(soutien_cm),
            cantine = survey_mean(cantine),
            inspect = survey_mean(inspect)
            ) |> 
  writexl::write_xlsx('export/dsc_cm_nat.xlsx')
  
  
dstrata6 |> 
  group_by(qd17) |> 
  summarise(survey_mean(qm2,na.rm=TRUE))


glm <- svyglm(lng2 ~ fille_cp + redouble_cp + presco+ fr_msn_cp+liv_msn+tab_msn+lect_msn + lect_class + math_class+INDICE_AMENAG_TERRI+INDICE_EQUIP_CLASSE+INDICE_IMPLI_COMMUNAU+INDICE_PERCEPT_MT+INDICE_INFRASTRUCTURES, design = dstrata2)
glm1 <- svyglm(lng2 ~ fille_cp + redouble_cp + presco+ fr_msn_cp+liv_msn+tab_msn+lect_msn + lect_class + math_class, design = dstrata2)


summary(glm1)























  
  
  
  
  
  
  
  
  

