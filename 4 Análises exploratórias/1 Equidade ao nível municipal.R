# Pacotes

library(tidyverse)
library(openxlsx)
library(here)
library(readxl)

# ── 2. Distrito-município ───────────────────────────────────────────────────────────────

here("3 Produtos finais", "bancos", "bd_distritos_municipal.xlsx")

ie_data <- openxlsx::read.xlsx(
  here("3 Produtos finais", "bancos", "bd_distritos_municipal.xlsx")
)

names(ie_data)
summary(ie_data)

ie_data <- openxlsx::read.xlsx(
  here("3 Produtos finais", "bancos", "bd_distritos_municipal.xlsx")
)

names(ie_data)
summary(ie_data)


# __ Correlações

## Candidaturas

ie_data %>%
  ggplot(aes(x = indice_genero_candidatos_vereanca, y = indice_genero_receitas_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm")

ie_data %>%
  ggplot(aes(x = indice_genero_candidatos_vereanca, y = indice_genero_receitas_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm") + facet_wrap(~ano)

ie_data %>%
  ggplot(aes(x = indice_genero_candidatos_vereanca, y = indice_genero_receitas_partidarias_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm")

ie_data %>%
  ggplot(aes(x = indice_genero_candidatos_vereanca, y = indice_genero_receitas_partidarias_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm") + facet_wrap(~ano)


## Eleitos

ie_data %>%
  ggplot(aes(x = indice_genero_eleitos_vereanca, y = indice_genero_receitas_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm")

ie_data %>%
  ggplot(aes(x = indice_genero_eleitos_vereanca, y = indice_genero_receitas_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm") + facet_wrap(~ano)

ie_data %>%
  ggplot(aes(x = indice_genero_eleitos_vereanca, y = indice_genero_receitas_partidarias_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm")

ie_data %>%
  ggplot(aes(x = indice_genero_eleitos_vereanca, y = indice_genero_receitas_partidarias_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm") + facet_wrap(~ano)


ie_data %>%
  ggplot(aes(x = indice_racial_eleitos_vereanca, y = indice_racial_receitas_partidarias_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm") + facet_wrap(~ano)


## Maior equidade onde



## Preditores

## Candidaturas

names(ie_data)

md1 <- lm(indice_genero_candidatos_vereanca ~ prop_mulheres_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo, data = ie_data)

summary(md1)

md2 <- lm(indice_genero_candidatos_vereanca ~ prop_mulheres_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo +
            tem_mulher_eleita_prefeitura + partidos_prop_mulheres_decisao
            , data = ie_data)

summary(md2)

## Receitas 

md3 <- lm(indice_genero_receitas_partidarias_vereanca ~ prop_mulheres_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo, data = ie_data)

summary(md3)

md4 <- lm(indice_genero_receitas_partidarias_vereanca ~ prop_mulheres_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo +
            tem_mulher_eleita_prefeitura + partidos_prop_mulheres_decisao
          , data = ie_data)

summary(md4)

## Eleitas 

md5 <- lm(indice_genero_eleitos_vereanca ~ prop_mulheres_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo, data = ie_data)

summary(md5)

md6 <- lm(indice_genero_eleitos_vereanca ~ prop_mulheres_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo +
            tem_mulher_eleita_prefeitura + partidos_prop_mulheres_decisao
          , data = ie_data)

summary(md6)

md6 <- lm(indice_genero_eleitos_vereanca ~ prop_mulheres_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo +
            tem_mulher_eleita_prefeitura + partidos_prop_mulheres_decisao*distrito_prop_sup_completo
          , data = ie_data)

summary(md6)


ie_data %>%
  ggplot(aes(x = partidos_prop_mulheres_decisao, y = indice_genero_receitas_partidarias_vereanca)) +
  geom_point() +
  geom_smooth(method = "lm")

names(ie_data)

md6 <- lm(indice_racial_eleitos_vereanca ~ prop_negros_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo +
            tem_negro_eleito_prefeitura 
          , data = ie_data)

summary(md6)

md6 <- lm(indice_racial_eleitos_vereanca ~ prop_negros_populacao + pib_per_capita +
            populacao + distrito_prop_sup_completo +
            tem_negro_eleito_prefeitura*pib_per_capita
          , data = ie_data)

summary(md6)

