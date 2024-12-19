source("importation.R")

#library(lme4)
#library(svylme)


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
    cantine = ifelse(qd85 == 1,1,0),
    ens_femm = ifelse(qm1 == 2,1,0),
    form_ini = ifelse(qm5 %in% c(2,3,5,6),1,0),
    form_comp = ifelse(qm7 == 1,1,0),
    rem_math = qm21 / qm23b,
    rem_fr = qm21 / qm23a,
    guide_maitre_math = ifelse(qm26c == 1,1,0),
    guide_maitre_fr = ifelse(qm26d == 1,1,0),
    elect_class = ifelse(qm28 == 1,1,0),
    elect_ecole = ifelse(qd78o == 1,1,0),
    eau_ecole = ifelse(qd78m == 1,1,0),
    ecole_en_dur = ifelse(qd77a == 1,1,0),
    biblio_ecole = ifelse(qd78d == 1,1,0),
    sall_info = ifelse(qd78e == 1,1,0),
    toilette = ifelse(qd79a == 1,1,0),
    soutien_cp = ifelse(qd58 == 1,1,0),
    felicitation = ifelse(qd57 == 1,1,0),
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
      qd11 == 12 ~'BAC+4 ou plus',
      qd11 == 96 | qd11 == 99 ~'Aucun'),
    dip_pro_dir = case_when(
      qd12== 1 | qd12==99 | qd12==96  ~ 'Aucun',
      qd12== 4 ~ 'CAP',
      qd12== 5 ~ 'BSEN',
      qd12== 7 ~ 'BSC1',
      qd12== 8 ~ 'BSC2'
    ),
    dip_academie_ens = case_when(
      qm3 == 1 ~'inf_6eme',
      qm3 == 2 ~'premiere',
      qm3 == 3 ~'sixieme',
      qm3 == 4 ~'terminale',
      qm3 == 5 ~'5eme',
      qm3 == 6 ~'BAC+1',
      qm3 == 7 ~'quatrieme',
      qm3 == 8 ~'BAC+2',
      qm3 == 9 ~'3ieme',
      qm3 == 10 ~'BAC+3',
      qm3 == 11 ~'seconde',
      qm3 == 12 ~'BAC+4 ou plus'),
    dip_pro_ens = case_when(
      qm4== 1 | qm4== 99 ~ 'Aucun',
      qm4== 2 ~ 'CAM',
      qm4== 3 ~ 'CEAP-CFENI',
      qm4== 4 ~ 'CEAP',
      qm4== 5 ~ 'CAP-CFENI',
      qm4== 6 ~ 'CAP')
    
  ) 

glm_lng_cp <- svyglm(lng2 ~ fille_cp + qe22 + redouble_cp + presco + fr_msn_cp+liv_msn+tab_msn + lect_msn + lect_class + math_class +dip_academie_ens  + dip_pro_ens + qm2 + ens_femm + form_ini + form_comp +
                       rem_math +rem_fr + guide_maitre_math + guide_maitre_fr + elect_class + doub_flu + multigrad + pe + coop + COGEP + AME + APEE + APE + cloture + urbain + ecole_pub +dip_academie_dir + dip_pro_dir+
                       qd69a + qd3 + qd4 + qm5 + cantine + elect_ecole + eau_ecole + ecole_en_dur + biblio_ecole + sall_info + qd76 + toilette + soutien_cp + felicitation,design = dstrata2)



#glm_lng_cp2 <- step(glm_lng_cp)


#summary(glm_lng_cp2)


#svy2lme(lng2~(1|ID_ECOLE) + (1|ID_ENSEIGNANT), design = dstrata2,method="nested")

#lme4::lmer(lng2~ (1|ID_ECOLE), data = dstrata2)


fit1 = lme4::lmer(lng2 ~ fille_cp + qe22 + redouble_cp + presco + fr_msn_cp+liv_msn+tab_msn + lect_msn + lect_class + math_class +dip_academie_ens  + dip_pro_ens + qm2 + ens_femm + form_ini + form_comp +
             rem_math +rem_fr + guide_maitre_math + guide_maitre_fr + elect_class + doub_flu + multigrad + pe + coop + COGEP + AME + APEE + APE + cloture + urbain + ecole_pub +dip_academie_dir + dip_pro_dir+
             qd69a + qd3 + qd4 + qm5 + cantine + elect_ecole + eau_ecole + ecole_en_dur + biblio_ecole + sall_info + qd76 + toilette + soutien_cp + felicitation + (1|ID_ECOLE), data = dstrata6)

summary(fit)




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













