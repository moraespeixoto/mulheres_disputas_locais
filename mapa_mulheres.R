library(geobr)
library(ggthemes)
library(viridis)

load("~/dados_eleicoes_tse/resultados_finais_1998_2020.Rda")

mulheres_vereadoras <- resultados_finais_1998_2020 %>% 
  filter(eleito == 1,
        CD_CARGO == 13,
        CD_GENERO %in% c(2, 4)) %>% 
  group_by(DS_GENERO, ANO_ELEICAO, CD_MUNICIPIO) %>% 
  summarise(n = n()) %>% 
  group_by(ANO_ELEICAO, CD_MUNICIPIO) %>% 
  mutate(total = sum(n),
         perc_total = n/total*100) 


mulheres_mun <- mulheres_vereadoras %>% 
  select(-4, -5) %>% 
  pivot_wider(values_from = perc_total, names_from = DS_GENERO) %>% 
  mutate(id_municipio_tse = as.numeric(CD_MUNICIPIO))


#### abrir shape 


mun <- read_municipality(code_muni = "all", year = 2010)
estado <- read_state(code_state = "all", year = 2010)

codigos <- read_csv("~/dados_eleicoes_tse/MULHERES/codigos.csv")


mapa_mulheres <- full_join(mulheres_mun, codigos)

mapa_mulheres <- left_join(mun, mapa_mulheres, by = c("code_muni" = "id_municipio"))

save(mapa_mulheres, file = "~/dados_eleicoes_tse/MULHERES/mapa_mulheres.Rda")

load("~/dados_eleicoes_tse/MULHERES/mapa_mulheres.Rda")


mapa_mulheres %>% 
  filter(ANO_ELEICAO != "NA") %>%
  mutate(FEMININO = ifelse(is.na(FEMININO), 0, FEMININO)) %>% 
ggplot() +
  geom_sf(aes(fill = FEMININO), color = NA)+
 geom_sf(data = estado, alpha = 0, color = "#999999")+
  facet_wrap(~ANO_ELEICAO)+
  scale_fill_continuous(high = "#3a2244", low = "white")+
  theme_fivethirtyeight()+
  
  theme(panel.background = element_rect(fill = "white", colour = "white", color = "white"),
        plot.background = element_rect(fill = "white", colour = "white", color = "white"),
        strip.background=element_rect(fill="white", colour="white"),
        legend.background = element_rect(fill = "white", colour = "white", color = "white"),
        legend.key = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 18, margin = margin(0, 0, 30, 0) ),
        axis.title.x = element_text(size = 6),
        axis.text.x =  element_text(size = 10),
        axis.text.y =  element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.6, "cm"),
        panel.grid.major.x = element_line(colour = "#f2f2f2", size = 1.2),
        panel.grid.major.y = element_line(colour = "#f2f2f2", size = 2.0),
        strip.text = element_text(size = 12, colour = "black"))+
  
  labs(title = "Proporção de vereadoras nas Câmaras Municipais (2000-2020)",
       col = "",
       fill = "")+
  xlab("")

  