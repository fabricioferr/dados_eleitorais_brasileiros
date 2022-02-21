# Importação de pacotes ---------------------------------------------------
library(tidyverse)
library(basedosdados)
library(scales)
library(geobr)
library(RColorBrewer)
library(ggthemes)
library(Cairo)


# Importação e manipulação de dados ---------------------------------------
set_billing_id('projeto-cidades-medias')

dados_eleicoes <- bdplyr('basedosdados.br_tse_eleicoes.candidatos') %>% 
  select(ano, tipo_eleicao, sigla_uf, id_municipio, id_candidato_bd, nome, 
         ocupacao, cargo, idade, genero, instrucao, raca, sigla_partido) %>% 
  bd_collect()

eleicoes_brasileiras <- dados_eleicoes %>% 
  filter(tipo_eleicao == "eleicao ordinaria") %>% 
  mutate(sigla_uf = factor(sigla_uf),
         id_candidato_bd = factor(id_candidato_bd),
         ocupacao = factor(ocupacao),
         cargo = factor(cargo),
         genero = factor(genero),
         instrucao = factor(instrucao),
         raca = factor(raca),
         sigla_partido = factor(sigla_partido),
         instrucao = factor(case_when(instrucao == "analfabeto" ~ "Analfabeto",
                                      instrucao == "le e escreve" ~ "Lê e Escreve",
                                      instrucao == "ensino fundamental incompleto" ~ "Ensino Fundamental Incompleto",
                                      instrucao == "ensino fundamental completo" ~ "Ensino Fundamental Completo",
                                      instrucao == "ensino medio incompleto" ~ "Ensino Médio Incompleto",
                                      instrucao == "ensino medio completo" ~ "Ensino Médio Completo",
                                      instrucao == "ensino superior incompleto" ~ "Ensino Superior Incompleto",
                                      instrucao == "ensino superior completo" ~ "Ensino Superior Completo"),
                            levels = c("Analfabeto",
                                       "Lê e Escreve",
                                       "Ensino Fundamental Incompleto",
                                       "Ensino Fundamental Completo",
                                       "Ensino Médio Incompleto",
                                       "Ensino Médio Completo",
                                       "Ensino Superior Incompleto",
                                       "Ensino Superior Completo"),
                            ordered = TRUE))


# Percentual de candidatos por gênero nas eleições brasileiras ------------
eleicoes_genero_ano <- eleicoes_brasileiras %>% 
  group_by(ano, genero) %>% 
  summarise(total_candidatos = n()) %>% 
  drop_na() %>% 
  mutate(prop_candidatos = total_candidatos/sum(total_candidatos),
         genero = stringr::str_to_title(genero))

plot_eleicoes_genero <- ggplot(data = eleicoes_genero_ano, aes(x = ano, y = prop_candidatos, color = genero))+
  geom_line(size = 1.2)+
  scale_y_continuous(labels = scales::percent_format())+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Ano",
       y = "%",
       color = "Gênero",
       title = "Percentual dos candidatos às eleições brasileiras por gênero entre 1994 e 2020")

plot(plot_eleicoes_genero)
ggsave(plot = plot_eleicoes_genero, filename = "perc_eleicoes_genero.pdf", width = 12, height = 5, dpi = 120, units = "in")


# Qual é o percentual de candidatas mulheres entre os municípios brasileiros --------
perc_mulheres_por_municipio <- eleicoes_brasileiras %>% 
  group_by(id_municipio, genero) %>% 
  summarise(total_candidaturas = n()) %>% 
  drop_na() %>% 
  mutate(percentual_candidaturas = total_candidaturas/sum(total_candidaturas)*100,
         id_municipio = as.numeric(id_municipio)) %>% 
  filter(genero == "feminino")

municipios_brasileiros <- geobr::read_municipality()
perc_mulheres_municipios_shp <- inner_join(municipios_brasileiros, perc_mulheres_por_municipio,
                                           by = c("code_muni" = "id_municipio"))

plot_perc_mulheres_map <- ggplot(data = perc_mulheres_municipios_shp)+
  geom_sf(aes(fill = percentual_candidaturas), color = NA, size = .15)+
  theme_map()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8,"Blues"), na.value = "#A9A9A9")+
  labs(fill = "%",
       title = "Percentual médio de candidatas mulheres (1994-2020)")

plot(plot_perc_mulheres_map)
ggsave(plot = plot_perc_mulheres_map, filename = "plot_perc_mulheres_map.pdf", width = 12, height = 5, dpi = 120, units = "in")


# Qual é o nível de instrução dos candidatos às eleições brasileiras --------
instrucao_candidatos_ano <- eleicoes_brasileiras %>% 
  group_by(ano, instrucao) %>% 
  summarise(quantidade_candidatos = n()) %>% 
  drop_na() %>% 
  mutate(prop_candidatos = quantidade_candidatos/sum(quantidade_candidatos),
         tipo_de_eleicao = ifelse(ano %in% c(seq(1994, 2018, 4)), "Eleições Presidenciais", "Eleições Municipais"))


plot_instrucao_candidatos <- ggplot(data = instrucao_candidatos_ano, aes(x = ano, y = prop_candidatos, color = instrucao))+
  geom_line(size = .9)+
  facet_wrap(~tipo_de_eleicao, scales = "free_y")+
  scale_y_continuous(labels = scales::percent_format())+
  theme_light()+theme(strip.background = element_blank(),
                      strip.text = element_text(colour = "black"))+
  labs(x = "Ano",
       y = "%",
       title = "Nível de instrução dos candidatos às eleições brasileiras em (%) do total de candidatos",
       subtitle = "Eleições presidenciais e municipais no período entre 1998 e 2020",
       color = "Escolaridade")

plot(plot_instrucao_candidatos)
ggsave(plot = plot_instrucao_candidatos, filename = "plot_instrucao_candidatos.pdf", width = 12, height = 5, dpi = 120, units = "in")

