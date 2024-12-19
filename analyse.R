
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


# Importation des données
togo_grd2 <- read_dta("PASEC2019_GRADE2/PASEC2019_GRADE2.dta") 

togo_grd6 <- read_dta("PASEC2019_GRADE6/PASEC2019_GRADE6.dta") 

# Création des design pour la pondération
# En début de scolarité
dstrata2 <- togo_grd2 %>%
  as_survey_design(ids = ID_TOT, strata = ID_STRATE, weights = rwgt0) |>
  filter(PAYS == 'TOGO') |> 
  filter(qd17 != 3,qd17 != 9)

dstrata2 <- dstrata2 |> 
  mutate(lng2 = (LECT_PV1 + LECT_PV2 + LECT_PV3 + LECT_PV4 + LECT_PV5)/5,
         math2 = (MATHS_PV1 + MATHS_PV2 + MATHS_PV3 + MATHS_PV4 + MATHS_PV5)/5) 

# En fin de scolarité
dstrata6 <- togo_grd6 %>%
  as_survey_design(strata = ID_STRATE, weights = rwgt0)|> 
  filter(PAYS == 'TOGO') |> 
  filter(qd17 != 3,qd17 != 9)

dstrata6 <- dstrata6 %>%
  mutate(lng6 = (LECT_PV1 + LECT_PV2 + LECT_PV3 + LECT_PV4 + LECT_PV5)/5,
         math6 = (MATHS_PV1 + MATHS_PV2 + MATHS_PV3 + MATHS_PV4 + MATHS_PV5)/5)

# Seuil en début de scolarisation

dstrata2 %>%
  group_by(ID_STRATE,qd17,qe23) %>%
  summarise(lng2 = survey_mean(lng2, vartype = "ci")) |> 
  writexl::write_xlsx('lang_cp.xlsx')

dstrata2 %>%
  group_by(ID_STRATE,qd17) %>%
  summarise(lng2 = survey_mean(lng2)) |> 
  pivot_wider(
    names_from = qd17, 
    values_from = lng2) |>
  writexl::write_xlsx('export/lang_cp_statut.xlsx')

dstrata2 %>%
  group_by(qd17) %>%
  summarise(lng2 = survey_mean(lng2))|> 
  writexl::write_xlsx('export/lang_cp_nat.xlsx')

dstrata2 %>%
  group_by(qd17,qe23) %>%
  summarise(lng2 = survey_mean(lng2, vartype = "ci")) |> 
  writexl::write_xlsx('lang_cp_nat.xlsx')

dstrata2 %>%
  group_by(ID_STRATE,qd17,qe23) %>%
  summarise(math2 = survey_mean(math2, vartype = "ci")) |> 
  writexl::write_xlsx('math_cp.xlsx')

dstrata2 %>%
  group_by(qd17,qe23) %>%
  summarise(math2 = survey_mean(math2, vartype = "ci")) |> 
  writexl::write_xlsx('math_cp_nat.xlsx')

dstrata2 %>%
  group_by(ID_STRATE,qd17) %>%
  summarise(math2 = survey_mean(math2)) |> 
  pivot_wider(
    names_from = qd17, 
    values_from = math2) |>
  writexl::write_xlsx('export/math_cp_statut.xlsx')

dstrata2 %>%
  group_by(qd17) %>%
  summarise(math2 = survey_mean(math2)) |>
  writexl::write_xlsx('export/math_cp_nat.xlsx')

####Proportion par niveau en début de scolarité
## Langue

grd2_rep <- togo_grd2 |> 
  clean_names() |> 
  as_tibble() |> 
  as_survey_rep(weights = rwgt0,repweights = rwgt1:rwgt45,type = "JK1",mse = TRUE,combined.weights = TRUE, scale = (45-1)/45) |> 
  mutate(lng2 = (lect_pv1 + lect_pv2 + lect_pv3 + lect_pv4 + lect_pv5)/5,
         math2 = (maths_pv1 + maths_pv2 + maths_pv3 + maths_pv4 + maths_pv5)/5)

grd2_rep |> 
  mutate(niveau = case_when(
    lng2 <= 399.12 ~ "Ss_Niveau_1",
    lng2 > 399.12 & lng2 <= 469.54 ~ "Niveau_1",
    lng2 > 469.54 & lng2 <= 539.96 ~ "Niveau_2",
    lng2 > 539.96 & lng2 <= 610.38 ~ "Niveau_3",
    lng2 > 610.38 ~ "Niveau_4",
    TRUE ~ NA_character_)) |> 
  group_by(niveau) |> 
  summarise(p = survey_mean()) |> 
  writexl::write_xlsx('export/lang_niv_statu.xlsx')


## Mathématiques

grd2_rep |> 
  mutate(niveau = case_when(
    math2 < 400 ~ "Ss_Niveau_1",
    math2 >= 400 & math2 < 489 ~ "Niveau_1",
    math2 >= 489 & math2 < 577 ~ "Niveau_2",
    math2 >= 577 ~ "Niveau_3",
    TRUE ~ NA_character_)) |> 
  group_by(qd17,niveau) |> 
  summarise(p = survey_mean()) |> 
  writexl::write_xlsx('export/math_niv_statu.xlsx')

#### Fin de scolarité

grd6_rep <- togo_grd6 |> 
  clean_names() |> 
  as_tibble() |> 
  as_survey_rep(weights = rwgt0,repweights = rwgt1:rwgt90,type = "JK1",mse = TRUE,combined.weights = FALSE, scale = (45-1)/45) |> 
  mutate(lng6 = (lect_pv1 + lect_pv2 + lect_pv3 + lect_pv4 + lect_pv5)/5,
         math6 = (maths_pv1 + maths_pv2 + maths_pv3 + maths_pv4 + maths_pv5)/5)


grd6_rep |> 
  mutate(niveau = case_when(
    lng6 < 365.01 ~ "Ss_Niveau_1",
    lng6 > 365.01 & lng6 <= 441.69 ~ "Niveau_1",
    lng6 > 441.69 & lng6 <= 518.37 ~ "Niveau_2",
    lng6 > 518.37 & lng6 <= 595.05 ~ "Niveau_3",
    lng6 > 595.05 ~ "Niveau_4",
    TRUE ~ NA_character_)) |> 
  group_by(niveau) |> 
  summarise(p = survey_mean()) |> 
  writexl::write_xlsx('export/lang_niv_statu_6.xlsx')


grd6_rep |> 
  mutate(niveau = case_when(
    math6 < 433 ~ "Ss_Niveau_1",
    math6 >= 433 & math6 < 521 ~ "Niveau_1",
    math6 >= 521 & math6 < 609 ~ "Niveau_2",
    math6 >= 609 ~ "Niveau_3",
    TRUE ~ NA_character_)) |> 
  group_by(niveau) |> 
  summarise(p = survey_mean()) |> 
  writexl::write_xlsx('export/math_niv_statu_6.xlsx')


##############################






grd6_rep |> 
  group_by(ID_STRATE,qd17) |> 
  summarise(lng6 = survey_mean(lng6)) |> 
  pivot_wider(
    names_from = qd17, 
    values_from = lng6) |>
  writexl::write_xlsx('export/lang_cm_statut.xlsx')

dstrata6 |> 
  group_by(qd17) |> 
  summarise(lng6 = survey_mean(lng6))|> 
  writexl::write_xlsx('export/lang_cm_nat.xlsx')



dstrata6 |> 
  group_by(ID_STRATE,qd17) |> 
  summarise(math6 = survey_mean(math6)) |> 
  pivot_wider(
    names_from = qd17, 
    values_from = math6) |>
  writexl::write_xlsx('export/math_cm_statut.xlsx')

dstrata6 |> 
  group_by(qd17) |> 
  summarise(math6 = survey_mean(math6)) |> 
  writexl::write_xlsx('export/math_cm_nat.xlsx')



####Proportion en fin de scolarité
## Langue

dstrata6 |> 
  mutate(niveau = case_when(
    lng6 < 365.01 ~ "Ss_Niveau_1",
    lng6 > 365.01 & lng6 <= 441.69 ~ "Niveau_1",
    lng6 > 441.69 & lng6 <= 518.37 ~ "Niveau_2",
    lng6 > 518.37 & lng6 <= 595.05 ~ "Niveau_3",
    lng6 > 595.05 ~ "Niveau_4",
    TRUE ~ NA_character_)) |> 
  group_by(niveau) |> 
  summarise(p = survey_mean()) |> 
  writexl::write_xlsx('export/lang_niv_statu_cm.xlsx')



## Mathématiques

dstrata6 |> 
  mutate(niveau = case_when(
    math6 < 433 ~ "Ss_Niveau_1",
    math6 >= 433 & math6 < 521 ~ "Niveau_1",
    math6 >= 521 & math6 < 609 ~ "Niveau_2",
    math6 >= 609 ~ "Niveau_3",
    TRUE ~ NA_character_)) |> 
  group_by(qd17,niveau) |> 
  summarise(p = survey_mean()) |> 
  writexl::write_xlsx('export/math_niv_statu_cm.xlsx')



dstrata2 %>%
  group_by(ID_STRATE,qd17) %>%
  summarise(lng2 = survey_mean(lng2, vartype = "ci"))

dstrata2 %>%
  group_by(ID_STRATE,qe23) %>%
  summarise(lng2 = survey_mean(lng2, vartype = "ci"))

dstrata2 %>%
  #group_by(qd17) %>%
  summarise(math2 = survey_mean(math2, vartype = "ci"))

# Seuil en fin de scolarisation

dstrata6 %>%
  group_by(ID_STRATE) %>%
  summarise(lng6 = survey_mean(lng6, vartype = "ci"))

dstrata2 %>%
  group_by(ID_ECOLE) %>%
  summarise(math2 = survey_mean(math2),
            COMM_ECOLE = survey_mean(INDICE_IMPLI_COMMUNAU)) -> df


togo_grd6 |> glimpse()
