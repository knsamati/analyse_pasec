source("importation.R")

dstrata2 %>%
  group_by(ID_ECOLE) %>%
  summarise(math2 = survey_mean(math2),
            lang2 = survey_mean(lng2),
            engage_comm = survey_mean(INDICE_IMPLI_COMMUNAU),
            infra_ecole = survey_mean(INDICE_INFRASTRUCTURES),
            context_ecole = survey_mean(INDICE_AMENAG_TERRI),
            moyen_context = (engage_comm+infra_ecole+context_ecole)/3) -> df2

df2 <- df2 |> 
  inner_join(togo_grd2,by = join_by(ID_ECOLE)) |>
  select(ID_ECOLE,math2,lang2,engage_comm,infra_ecole,context_ecole,moyen_context,qd17) |> 
  distinct(ID_ECOLE,.keep_all = TRUE) |> 
  filter(qd17 %in% c(1,2) ) |> 
  mutate(qd17 = as.factor(qd17)) |>
  labelled::remove_val_labels() 
  

ggplot(df2,aes(x = moyen_context , y = math2 ,color = qd17)) +
  geom_point(alpha = .5,size = 2) +
  scale_color_manual(name = NULL,labels = c("Publique","Privée"),values = c("#00B0F0","#33658A")) +
  geom_hline(yintercept = mean(df2$math2,na.rm = TRUE),linetype = "dashed", color = "skyblue") +
  geom_vline(xintercept = mean(df2$moyen_context,na.rm = TRUE),linetype = "dashed", color = "darkblue") +
  geom_smooth(method = "lm") +
  labs(x = "Contexte et moyens de l'école", y = "Score moyen de l'école en mathématiques") +
  theme_light() -> p1

ggplot(df2,aes(x = moyen_context, y = lang2,color = qd17)) +
  geom_point(alpha = .5,size = 2) +
  scale_color_manual(name = NULL,labels = c("Publique","Privée"),values = c("#00B0F0","#33658A")) +
  geom_hline(yintercept= mean(df2$lang2),linetype = "dashed", color = "skyblue") +
  geom_vline(xintercept= mean(df2$moyen_context),linetype = "dashed", color = "darkblue") +
  geom_smooth(method = "lm") +
  labs(x = "Contexte et moyens de l'école", y = "Score moyen de l'école en français") +
  theme_light() -> p2

library(patchwork)

p1+p2+plot_layout(guides = 'collect')

#########################
########## Fin de scolarité

## INDICE_PERCEPT_MT
## INDICE_EQUIP_CLASSE

dstrata6 %>%
  group_by(ID_ECOLE) %>%
  summarise(math6 = survey_mean(math6),
            lang6 = survey_mean(lng6),
            engage_comm = survey_mean(INDICE_IMPLI_COMMUNAU),
            infra_ecole = survey_mean(INDICE_INFRASTRUCTURES),
            context_ecole = survey_mean(INDICE_AMENAG_TERRI),
            equip_class = survey_mean(INDICE_EQUIP_CLASSE),
            percept_mt = survey_mean(INDICE_PERCEPT_MT),
            moyen_context = (engage_comm + infra_ecole + context_ecole + equip_class)/4) -> df6

df6 <- df6 |> 
  inner_join(togo_grd6,by = join_by(ID_ECOLE)) |>
  select(ID_ECOLE,math6,lang6,engage_comm,infra_ecole,context_ecole,moyen_context,equip_class,percept_mt,qd17) |> 
  distinct(ID_ECOLE,.keep_all = TRUE) |> 
  filter(qd17 %in% c(1,2) ) |> 
  mutate(qd17 = as.factor(qd17)) |>
  labelled::remove_val_labels() 


ggplot(df6,aes(x = moyen_context, y = math6,color = qd17)) +
  geom_point(alpha = .5,size = 2) +
  scale_color_manual(name = NULL,labels = c("Publique","Privée"),values = c("#00B0F0","#33658A")) +
  geom_hline(yintercept = mean(df6$math6,na.rm = TRUE),linetype = "dashed", color = "skyblue") +
  geom_vline(xintercept = mean(df6$moyen_context,na.rm = TRUE),linetype = "dashed", color = "darkblue") +
  geom_smooth(method = "lm") +
  labs(x = "Contexte et moyens de l'école", y = "Score moyen de l'école en mathématiques")+
  theme_light() -> p3

ggplot(df6,aes(x = moyen_context, y = lang6 ,color = qd17)) +
  geom_point(alpha = .5,size = 2) +
  scale_color_manual(name = NULL,labels = c("Publique","Privée"),values = c("#00B0F0","#33658A")) +
  geom_hline(yintercept = mean(df6$lang6,na.rm = TRUE),linetype = "dashed", color = "skyblue") +
  geom_vline(xintercept = mean(df6$moyen_context,na.rm = TRUE),linetype = "dashed", color = "darkblue") +
  geom_smooth(method = "lm") +
  labs(x = "Contexte et moyens de l'école", y = "Score moyen de l'école en français") +
  theme_light() -> p4

p3+p4+plot_layout(guides = 'collect')




