---
title: "10_2 Comparecimento, Vagas e Eleitorado Municipais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `base_mun.rds` (código 10_2, feito com a basedosdados) com os arquivos do Portal de Dados Abertos do TSE, para uma amostra: **eleição de 2016**.

1. **Vagas** por município e cargo (todo o Brasil), contra `consulta_vagas_2016`.
2. **Votos nominais** por município e cargo (1º turno), contra a soma dos votos por candidato em `votacao_candidato_munzona_2016` para o Acre.

Critério: igualdade exata das vagas em pelo menos 99% dos municípios; diferença de até 0,5% nos votos.

## Pacotes e amostra


``` r
library(tidyverse)
```

```
## Warning: pacote 'ggplot2' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'tibble' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'tidyr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'readr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'purrr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'dplyr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'stringr' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'forcats' foi compilado no R versão 4.5.2
```

```
## Warning: pacote 'lubridate' foi compilado no R versão 4.5.2
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.2.0     ✔ readr     2.1.6
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.1     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(here)
```

```
## Warning: pacote 'here' foi compilado no R versão 4.5.2
```

```
## here() starts at <raiz do projeto>
```

``` r
source(here("0 Funções, estrutura e passo-a-passo", "0_funcoes_auxiliares.R"))
```

```
## Warning: pacote 'data.table' foi compilado no R versão 4.5.3
```

```
## 
## Anexando pacote: 'data.table'
## 
## Os seguintes objetos são mascarados por 'package:lubridate':
## 
##     hour, isoweek, isoyear, mday, minute, month, quarter, second, wday,
##     week, yday, year
## 
## Os seguintes objetos são mascarados por 'package:dplyr':
## 
##     between, first, last
## 
## O seguinte objeto é mascarado por 'package:purrr':
## 
##     transpose
```

``` r
ano_amostra <- 2016

produzido <- readRDS(file.path(pasta$prod_distritos, "base_mun.rds")) |>
  filter(ano == ano_amostra) |>
  mutate(id_municipio_tse = as.integer(id_municipio_tse))
```

## 1. Vagas


``` r
vagas_tse <- ler_zip_tse(
  file.path(pasta$brutos_distritos, paste0("consulta_vagas_", ano_amostra, ".zip")),
  paste0("consulta_vagas_", ano_amostra, "_BRASIL.csv")
) |>
  as_tibble() |>
  filter(CD_TIPO_ELEICAO == "2") |>
  transmute(id_municipio_tse = as.integer(SG_UE), cargo = texto_bd(DS_CARGO),
            vagas_tse = as.numeric(QT_VAGAS)) |>
  filter(cargo %in% c("prefeito", "vereador"))

comparar_valores(
  full_join(select(produzido, id_municipio_tse, cargo, vagas), vagas_tse,
            by = c("id_municipio_tse", "cargo")) |>
    transmute(id_municipio_tse, cargo, saida = vagas, tse = vagas_tse),
  "Vagas por município e cargo", tolerancia = 0
) |> head(10)
```

```
## 
## --- Vagas por município e cargo ---
## Casos nos dois bancos: 11136 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 0%: 11136 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 63.510 | total TSE: 63.510 | diferença: 0 %
## Veredito: OK
```

```
##     id_municipio_tse    cargo saida   tse dif_rel
##                <int>   <char> <int> <num>   <num>
##  1:             1120 prefeito     1     1       0
##  2:             1120 vereador     9     9       0
##  3:             1570 prefeito     1     1       0
##  4:             1570 vereador     9     9       0
##  5:             1058 prefeito     1     1       0
##  6:             1058 vereador    11    11       0
##  7:             1007 prefeito     1     1       0
##  8:             1007 vereador     9     9       0
##  9:             1015 prefeito     1     1       0
## 10:             1015 vereador     9     9       0
```

## 2. Votos nominais (Acre)


``` r
uf_amostra <- "AC"

votos_tse <- ler_zip_tse(
  file.path(pasta$brutos_resultados, paste0("votacao_candidato_munzona_", ano_amostra, ".zip")),
  paste0("votacao_candidato_munzona_", ano_amostra, "_", uf_amostra, ".csv"),
  select = c("CD_TIPO_ELEICAO", "NR_TURNO", "SG_UE", "DS_CARGO", "QT_VOTOS_NOMINAIS_VALIDOS")
) |>
  as_tibble() |>
  filter(NR_TURNO == "1", CD_TIPO_ELEICAO == "2") |>
  group_by(id_municipio_tse = as.integer(SG_UE), cargo = texto_bd(DS_CARGO)) |>
  summarise(votos_nominais_tse = sum(as.numeric(QT_VOTOS_NOMINAIS_VALIDOS)), .groups = "drop")

comparar_valores(
  inner_join(filter(produzido, sigla_uf == uf_amostra) |> select(id_municipio_tse, cargo, votos_nominais),
             votos_tse, by = c("id_municipio_tse", "cargo")) |>
    transmute(id_municipio_tse, cargo, saida = votos_nominais, tse = votos_nominais_tse),
  "Votos nominais válidos por município e cargo (Acre, 1º turno)", tolerancia = 0.005
) |> head(10)
```

```
## 
## --- Votos nominais válidos por município e cargo (Acre, 1º turno) ---
## Casos nos dois bancos: 44 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 0.5%: 44 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 797.235 | total TSE: 797.235 | diferença: 0 %
## Veredito: OK
```

```
##     id_municipio_tse    cargo saida   tse dif_rel
##                <int>   <char> <num> <num>   <num>
##  1:             1120 prefeito  7169  7169       0
##  2:             1120 vereador  6778  6778       0
##  3:             1570 prefeito  4419  4419       0
##  4:             1570 vereador  4141  4141       0
##  5:             1058 prefeito 12832 12832       0
##  6:             1058 vereador 12190 12190       0
##  7:             1007 prefeito  6204  6204       0
##  8:             1007 vereador  5832  5832       0
##  9:             1015 prefeito  6155  6155       0
## 10:             1015 vereador  5777  5777       0
```
