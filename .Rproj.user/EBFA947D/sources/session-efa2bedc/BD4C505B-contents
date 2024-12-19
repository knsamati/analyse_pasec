
library(tidyverse)

df = readxl::read_xlsx("data_perf_cm.xlsx")

df %>% 
  select(Region,math_pub,math_priv) %>%
  pivot_longer(cols = c(math_pub,math_priv)) %>%
  rename(perf = name, 
         taux = value) -> dat


ggplot(dat, aes(x = taux, y = Region)) +
  geom_line() +
  geom_point(aes(color = perf),size = 3) +
  geom_vline(xintercept = 521,linetype = "solid",size = 0.5,alpha = 0.4) +
  scale_color_manual(name = NULL,labels = c("Privée", "Publique"),values = c("#00B0F0","#33658A")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8.5)
  )


df %>% 
  select(Region,lang_pub,lang_priv) %>%
  pivot_longer(cols = c(lang_pub,lang_priv)) %>%
  rename(perf = name, 
         taux = value) -> dat2


ggplot(dat2, aes(x = taux, y = Region)) +
  geom_line() +
  geom_point(aes(color = perf),size = 3) +
  geom_vline(xintercept = 518,linetype = "solid",size = 0.5,alpha = 0.4) +
  scale_color_manual(name = NULL,labels = c("Privée", "Publique"),values = c("#00B0F0","#33658A")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8.5)
  )


