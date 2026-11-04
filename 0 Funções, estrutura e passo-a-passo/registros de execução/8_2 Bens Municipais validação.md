---
title: "8_2 Bens Municipais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `bens_candidatos_municipais.csv` (código 8_2, feito com a basedosdados) com o arquivo de bens declarados do Portal de Dados Abertos do TSE (`bem_candidato`), para uma amostra: **eleição de 2020, Tocantins**.

Mesmo procedimento do 8_1: fator de deflação estimado pela mediana, comparação em valores nominais do patrimônio total, do número de bens e dos imóveis por candidato. Critério: diferença de até 1%, com pelo menos 99% dos candidatos dentro da tolerância.

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
ano_amostra <- 2020
uf_amostra  <- "TO"

arquivo_produzido <- file.path(pasta$prod_candidatos, "bens_candidatos_municipais.csv")
if (!file.exists(arquivo_produzido)) stop("Rode o código 8_2 antes da validação: ", arquivo_produzido)
```

## Dados


``` r
produzido <- ler_banco(arquivo_produzido) |>
  filter(ano == ano_amostra, sigla_uf == uf_amostra) |>
  mutate(sequencial_candidato = as.character(sequencial_candidato))

tse <- data.table::fread(
  file.path(pasta$brutos_bens, paste0("bem_candidato_", ano_amostra, "_BRASIL.csv")),
  sep = ";", encoding = "Latin-1", colClasses = "character",
  select = c("SG_UF", "SQ_CANDIDATO", "DS_TIPO_BEM_CANDIDATO", "VR_BEM_CANDIDATO")
) |>
  as_tibble() |>
  filter(SG_UF == uf_amostra) |>
  mutate(valor = converter_valor_tse(VR_BEM_CANDIDATO),
         tipo  = texto_bd(DS_TIPO_BEM_CANDIDATO))

imoveis <- c("terreno", "casa", "apartamento", "outros bens imoveis", "terra nua",
             "sala ou conjunto", "predio comercial", "loja", "construcao",
             "predio residencial", "benfeitorias", "galpao")

tse_cand <- tse |>
  group_by(sequencial_candidato = SQ_CANDIDATO) |>
  summarise(n_bens_tse     = n(),
            patrimonio_tse = sum(valor, na.rm = TRUE),
            imoveis_tse    = sum(valor[tipo %in% imoveis], na.rm = TRUE),
            .groups = "drop")

comp <- full_join(
  select(produzido, sequencial_candidato, n_bens, patrimonio_total, soma_bens_imoveis),
  tse_cand, by = "sequencial_candidato"
)
```

## Comparação


``` r
fator <- comp |>
  filter(patrimonio_total > 0, patrimonio_tse > 0) |>
  summarise(fator = median(patrimonio_total / patrimonio_tse)) |>
  pull(fator)

fator
```

```
## [1] 1.34936
```

``` r
comparar_valores(
  comp |> transmute(sequencial_candidato, saida = n_bens, tse = n_bens_tse),
  "Número de bens por candidato", tolerancia = 0
) |> head(10)
```

```
## 
## --- Número de bens por candidato ---
## Casos nos dois bancos: 6320 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 0%: 6320 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 16.200 | total TSE: 16.200 | diferença: 0 %
## Veredito: OK
```

```
##     sequencial_candidato saida   tse dif_rel
##                   <char> <int> <int>   <num>
##  1:         270000632075     2     2       0
##  2:         270000632076    15    15       0
##  3:         270000632179     1     1       0
##  4:         270000632180     1     1       0
##  5:         270000632182     3     3       0
##  6:         270000632184     4     4       0
##  7:         270000632185     1     1       0
##  8:         270000632187     3     3       0
##  9:         270000632188     2     2       0
## 10:         270000632189     3     3       0
```

``` r
comparar_valores(
  comp |> transmute(sequencial_candidato, saida = patrimonio_total / fator, tse = patrimonio_tse),
  "Patrimônio total por candidato (valores nominais)"
) |> head(10)
```

```
## 
## --- Patrimônio total por candidato (valores nominais) ---
## Casos nos dois bancos: 6320 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 1%: 6320 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 2.834.350.342 | total TSE: 2.834.350.342 | diferença: 0 %
## Veredito: OK
```

```
##     sequencial_candidato  saida    tse      dif_rel
##                   <char>  <num>  <num>        <num>
##  1:         270000865131  74658  74658 4.872859e-15
##  2:         270001027130  76800  76800 4.736952e-15
##  3:         270000647085  74500  74500 4.687865e-15
##  4:         270001075930  74500  74500 4.687865e-15
##  5:         270001210724  74500  74500 4.687865e-15
##  6:         270001179314 765000 765000 4.413130e-15
##  7:         270000758309  76143  76143 4.395598e-15
##  8:         270001184789  76500  76500 4.375086e-15
##  9:         270000646912  80000  80000 4.365575e-15
## 10:         270000648294  80000  80000 4.365575e-15
```

``` r
comparar_valores(
  comp |> transmute(sequencial_candidato, saida = soma_bens_imoveis / fator, tse = imoveis_tse),
  "Imóveis por candidato (valores nominais)"
) |> head(10)
```

```
## 
## --- Imóveis por candidato (valores nominais) ---
## Casos nos dois bancos: 6320 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 1%: 6320 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 1.534.656.415 | total TSE: 1.534.656.415 | diferença: 0 %
## Veredito: OK
```

```
##     sequencial_candidato saida   tse      dif_rel
##                   <char> <num> <num>        <num>
##  1:         270001089500 76800 76800 4.736952e-15
##  2:         270000634917 80000 80000 4.365575e-15
##  3:         270000638870 80000 80000 4.365575e-15
##  4:         270000638877 80000 80000 4.365575e-15
##  5:         270000639902 80000 80000 4.365575e-15
##  6:         270000639904 80000 80000 4.365575e-15
##  7:         270000640025 80000 80000 4.365575e-15
##  8:         270000640311 80000 80000 4.365575e-15
##  9:         270000646912 80000 80000 4.365575e-15
## 10:         270000648294 80000 80000 4.365575e-15
```
