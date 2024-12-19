
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
togo_grd2 <- read_dta("PASEC2019_GRADE2/PASEC2019_GRADE2.dta") |> 
  filter(PAYS == 'TOGO') |> 
  filter(qd17 != 3,qd17 != 9)

togo_grd6 <- read_dta("PASEC2019_GRADE6/PASEC2019_GRADE6.dta") |> 
  filter(PAYS == 'TOGO') |> 
  filter(qd17 != 3,qd17 != 9)

## Echantillon

togo_grd2 |> 
  group_by(ID_STRATE,qd17) |> 
  summarise(eff = n()) |> 
  pivot_wider(
    names_from = ID_STRATE, 
    values_from = eff,
    values_fn = sum
  ) |> 
  writexl::write_xlsx('cp2_eff.xlsx')


togo_grd2 |> 
  group_by(ID_STRATE,qd17,ID_ECOLE) |> 
  count() -> df

df |> 
  group_by(ID_STRATE,qd17) |> 
  summarise(nombre=n()) |> 
  pivot_wider(
    names_from = ID_STRATE, 
    values_from = nombre,
    values_fn = sum
  ) -> df1

writexl::write_xlsx(df1,'cp2.xlsx')


togo_grd6 |> 
  group_by(ID_STRATE,qd17,ID_ECOLE) |> 
  count() -> df

df |> 
  group_by(ID_STRATE,qd17) |> 
  summarise(nombre=n()) |> 
  pivot_wider(
    names_from = ID_STRATE, 
    values_from = nombre,
    values_fn = sum
  ) -> df1

writexl::write_xlsx(df1,'cm2.xlsx')

togo_grd6 |> 
  group_by(ID_STRATE,qd17) |> 
  summarise(eff = n()) |> 
  pivot_wider(
    names_from = ID_STRATE, 
    values_from = eff,
    values_fn = sum
  ) |> 
  writexl::write_xlsx('cm2_eff.xlsx')



