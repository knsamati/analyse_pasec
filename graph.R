library(tidyverse)

df = readxl::read_xlsx("data_perf.xlsx")

df %>% 
  select(ID_STRATE,mat_pub,mat_pri) %>%
  pivot_longer(cols = c(mat_pub,mat_pri)) %>%
  rename(perf = name, 
         taux = value) -> dat


ggplot(dat, aes(x = taux, y = ID_STRATE)) +
  geom_line() +
  geom_point(aes(color = perf),size = 3) +
  geom_vline(xintercept = 489,linetype = "solid",size = 0.5,alpha = 0.4) +
  scale_color_manual(name = NULL,labels = c("Privée", "Publique"),values = c("#00B0F0","#33658A")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8.5)
  )


df %>% 
  select(ID_STRATE,lang_pub,lang_pri) %>%
  pivot_longer(cols = c(lang_pub,lang_pri)) %>%
  rename(perf = name, 
         taux = value) -> dat2


ggplot(dat2, aes(x = taux, y = ID_STRATE)) +
  geom_line() +
  geom_point(aes(color = perf),size = 3) +
  geom_vline(xintercept = 540,linetype = "solid",size = 0.5,alpha = 0.4) +
  scale_color_manual(name = NULL,labels = c("Privée", "Publique"),values = c("#00B0F0","#33658A")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8.5)
  )

