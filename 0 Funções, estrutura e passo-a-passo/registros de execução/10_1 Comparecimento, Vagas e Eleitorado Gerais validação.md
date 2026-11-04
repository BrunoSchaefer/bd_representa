---
title: "10_1 Comparecimento, Vagas e Eleitorado Gerais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `base_uf.xlsx` (código 10_1, feito com a basedosdados) com os arquivos do Portal de Dados Abertos do TSE, para uma amostra: **eleição de 2018**.

1. **Vagas** por UF e cargo, contra `consulta_vagas_2018`.
2. **Votos nominais** por UF e cargo (1º turno), contra a soma dos votos por candidato em `votacao_candidato_munzona_2018` para o Acre (o arquivo nacional tem 4 GB).

Critério: igualdade exata das vagas; diferença de até 0,5% nos votos.

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
library(openxlsx)
```

```
## Warning: pacote 'openxlsx' foi compilado no R versão 4.5.2
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
ano_amostra <- 2018

produzido <- read.xlsx(file.path(pasta$prod_distritos, "base_uf.xlsx")) |>
  filter(ano == ano_amostra)
```

## 1. Vagas


``` r
vagas_tse <- ler_zip_tse(
  file.path(pasta$brutos_distritos, paste0("consulta_vagas_", ano_amostra, ".zip")),
  paste0("consulta_vagas_", ano_amostra, "_BRASIL.csv")
) |>
  as_tibble() |>
  filter(CD_TIPO_ELEICAO == "2") |>
  transmute(sigla_uf = SG_UF, cargo = texto_bd(DS_CARGO), vagas_tse = as.numeric(QT_VAGA))

comparar_valores(
  full_join(select(produzido, sigla_uf, cargo, vagas),
            filter(vagas_tse, cargo %in% unique(produzido$cargo)),
            by = c("sigla_uf", "cargo")) |>
    transmute(sigla_uf, cargo, saida = vagas, tse = vagas_tse),
  "Vagas por UF e cargo", tolerancia = 0
) |> head(10)
```

```
## 
## --- Vagas por UF e cargo ---
## Casos nos dois bancos: 108 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 0%: 108 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 1.653 | total TSE: 1.653 | diferença: 0 %
## Veredito: OK
```

```
##     sigla_uf             cargo saida   tse dif_rel
##       <char>            <char> <num> <num>   <num>
##  1:       AC deputado estadual    24    24       0
##  2:       AC  deputado federal     8     8       0
##  3:       AC        governador     1     1       0
##  4:       AC           senador     2     2       0
##  5:       AL deputado estadual    27    27       0
##  6:       AL  deputado federal     9     9       0
##  7:       AL        governador     1     1       0
##  8:       AL           senador     2     2       0
##  9:       AM deputado estadual    24    24       0
## 10:       AM  deputado federal     8     8       0
```

## 2. Votos nominais (Acre)


``` r
uf_amostra <- "AC"

votos_tse <- ler_zip_tse(
  file.path(pasta$brutos_resultados, paste0("votacao_candidato_munzona_", ano_amostra, ".zip")),
  paste0("votacao_candidato_munzona_", ano_amostra, "_", uf_amostra, ".csv"),
  select = c("CD_TIPO_ELEICAO", "NR_TURNO", "DS_CARGO", "QT_VOTOS_NOMINAIS_VALIDOS")
) |>
  as_tibble() |>
  filter(NR_TURNO == "1", CD_TIPO_ELEICAO == "2") |>
  group_by(cargo = texto_bd(DS_CARGO)) |>
  summarise(votos_nominais_tse = sum(as.numeric(QT_VOTOS_NOMINAIS_VALIDOS)), .groups = "drop")

comparar_valores(
  inner_join(filter(produzido, sigla_uf == uf_amostra) |> select(cargo, votos_nominais),
             votos_tse, by = "cargo") |>
    transmute(cargo, saida = votos_nominais, tse = votos_nominais_tse),
  "Votos nominais válidos por cargo (Acre, 1º turno)", tolerancia = 0.005
)
```

```
## 
## --- Votos nominais válidos por cargo (Acre, 1º turno) ---
## Casos nos dois bancos: 4 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 0.5%: 4 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 1.984.894 | total TSE: 1.984.894 | diferença: 0 %
## Veredito: OK
```
