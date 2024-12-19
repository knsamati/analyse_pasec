library(sf)
library(tidyverse)
library(ggtext)
library(rcartocolor)
library(patchwork)
library(cowplot)
library(sf)
library(hrbrthemes)
library(ggthemes)


pref = st_read("donnees_geo/tgo_admbnda_adm2_inseed_itos_20210107.shp")
df = readxl::read_xlsx('proportion_privee.xlsx')

prive = readxl::read_xlsx('prive.xlsx') |> 
  st_as_sf( coords = c("Longitude", "Latitude")) |> 
  st_set_crs(st_crs(pref))

ggplot() +
  geom_sf( # plot the base map
    data = pref, size = 0.05,fill="white"
  ) +
  geom_sf( # plot the points
    data = prive, color='#00B0F0',
    color = "white", size = 1, alpha = 2/3, shape = 20,
    show.legend = FALSE # no legend since we're using facets and it'd just be redundant
  ) + 
  scale_color_tableau() +
  theme_void() +
  theme(text = element_text(size = 12, family = 'Ubuntu', color = "grey40"),
        plot.title = element_markdown(family = 'Ubuntu', size = 28, hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = 'Ubuntu', size = 11),
        plot.caption.position = "plot",
        plot.caption = element_markdown(hjust = 0.5),
        #legend.direction = "horizontal",
        legend.key.width = unit(2, "lines")#,
        # legend.position = "top"
  )



plot(pref)


temp <- pref %>% 
  select(ADM2_FR,ADM2_PCODE,ADM1_FR,ADM1_PCODE)%>% 
  filter(ADM2_FR=='Agoe-Nyive'| ADM2_FR=='Golfe' | ADM2_FR=='Lome Commune') %>% 
  sf::st_union() 

temp <- sf::st_sf(geometry = temp) 

old_pref <- pref %>%
  filter(!(ADM2_FR=='Agoe-Nyive'| ADM2_FR=='Golfe' | ADM2_FR=='Lome Commune'))

temp <- bind_rows(old_pref, temp) %>%
  mutate(ADM2_FR = ifelse(is.na(ADM2_FR), 'Lomé',ADM2_FR),
         ADM2_FR = ifelse(ADM2_FR=='Plaine du Mo', 'Mo',ADM2_FR),
         ADM1_FR = ifelse(is.na(ADM1_FR), 'Lomé', ADM1_FR)) %>%
  select(ADM2_FR,ADM1_FR)

temp |> 
  left_join(df,by = join_by(ADM2_FR == Region)) -> map


ggplot() +
  geom_sf(data = map, aes(fill = prop_ecole),colour = NA,size = 0.25) + 
  scale_fill_carto_c(type = "quantitative", palette = "SunsetDark",
                     na.value = "white", direction = 1,name = '') +
  guides(fill = guide_colourbar(title.position = "top")) +
  theme_void() +
  theme(text = element_text(size = 12, family = 'Ubuntu', color = "grey40"),
        plot.title = element_markdown(family = 'Ubuntu', size = 28, hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = 'Ubuntu', size = 11),
        plot.caption.position = "plot",
        plot.caption = element_markdown(hjust = 0.5),
        #legend.direction = "horizontal",
        legend.key.width = unit(0.5, "lines"),
        legend.text = element_text(size = 11)
       # legend.position = "top"
  ) #+
 # labs(title = "Proportion des élèves des écoles privées",caption = " Source des Données: <span style='color: #E34F6F;'><b>SIGE | MEPST</b></span> | Réalisation: Consultant") 
  



ggsave("carte1.png", bg = "white", dpi = 600, height = 15, width = 10)

