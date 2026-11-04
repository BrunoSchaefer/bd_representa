---
title: "8_1 Bens Gerais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `bens_candidatos_gerais.xlsx` (código 8_1, feito com a basedosdados) com o arquivo de bens declarados do Portal de Dados Abertos do TSE (`bem_candidato`), para uma amostra: **eleição de 2018** (todas as UFs).

Os valores produzidos estão deflacionados; o fator de deflação é estimado pela mediana da razão produzido/TSE e a comparação é feita em valores nominais. Compara-se o patrimônio total, o número de bens e o valor em imóveis por candidato. Critério: diferença de até 1%, com pelo menos 99% dos candidatos dentro da tolerância.

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
```

## Dados


``` r
produzido <- read.xlsx(file.path(pasta$prod_candidatos, "bens_candidatos_gerais.xlsx")) |>
  filter(ano == ano_amostra) |>
  mutate(sequencial_candidato = as.character(sequencial_candidato))

tse <- data.table::fread(
  file.path(pasta$brutos_bens, paste0("bem_candidato_", ano_amostra, "_BRASIL.csv")),
  sep = ";", encoding = "Latin-1", colClasses = "character"
) |>
  as_tibble() |>
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
## [1] 1.453629
```

``` r
tryCatch(
  deflateBR::ipca(1, as.Date(paste0(ano_amostra, "-12-31")), "12/2025"),
  error = function(e) "deflateBR indisponível"
)
```

```
## 
## Downloading necessary data from IPEA's API
## ...
```

```
## [1] "deflateBR indisponível"
```

``` r
comp_n <- comparar_valores(
  comp |> transmute(sequencial_candidato, saida = n_bens, tse = n_bens_tse),
  "Número de bens por candidato", tolerancia = 0
)
```

```
## 
## --- Número de bens por candidato ---
## Casos nos dois bancos: 17612 | só no banco produzido: 0 | só no TSE: 34 
## Casos com diferença <= 0%: 17612 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 93.196 | total TSE: 93.196 | diferença: 0 %
## Veredito: OK
```

``` r
head(comp_n, 10)
```

```
##     sequencial_candidato saida   tse dif_rel
##                   <char> <num> <int>   <num>
##  1:          10000600001     3     3       0
##  2:          10000600002     7     7       0
##  3:          10000600003     2     2       0
##  4:          10000600004     1     1       0
##  5:          10000600005     4     4       0
##  6:          10000600117     2     2       0
##  7:          10000600119     1     1       0
##  8:          10000600123     1     1       0
##  9:          10000600129     1     1       0
## 10:          10000600132     6     6       0
```

``` r
comp_pat <- comparar_valores(
  comp |> transmute(sequencial_candidato, saida = patrimonio_total / fator, tse = patrimonio_tse),
  "Patrimônio total por candidato (valores nominais)"
)
```

```
## 
## --- Patrimônio total por candidato (valores nominais) ---
## Casos nos dois bancos: 17612 | só no banco produzido: 0 | só no TSE: 34 
## Casos com diferença <= 1%: 17612 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 23.961.796.263 | total TSE: 23.961.796.263 | diferença: 0 %
## Veredito: OK
```

``` r
head(comp_pat, 10)
```

```
##     sequencial_candidato      saida        tse      dif_rel
##                   <char>      <num>      <num>        <num>
##  1:          60000605090  693447.05  693447.05 5.036375e-15
##  2:         220000627674   70580.00   70580.00 4.948228e-15
##  3:          80000624524  689649.00  689649.00 4.895308e-15
##  4:         250000618592   70317.04   70317.04 4.759786e-15
##  5:         200000603537   70629.14   70629.14 4.738753e-15
##  6:          70000625533 7290000.00 7290000.00 4.726877e-15
##  7:         250000615210    7217.79    7217.79 4.662273e-15
##  8:         190000614674  724262.80  724262.80 4.661353e-15
##  9:         210000605320  731364.34  731364.34 4.616091e-15
## 10:          70000625618  737740.00  737740.00 4.576198e-15
```

``` r
comp_imo <- comparar_valores(
  comp |> transmute(sequencial_candidato, saida = soma_bens_imoveis / fator, tse = imoveis_tse),
  "Imóveis por candidato (valores nominais)"
)
```

```
## 
## --- Imóveis por candidato (valores nominais) ---
## Casos nos dois bancos: 17612 | só no banco produzido: 0 | só no TSE: 34 
## Casos com diferença <= 1%: 17612 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 13.264.163.537 | total TSE: 13.264.163.537 | diferença: 0 %
## Veredito: OK
```

``` r
head(comp_imo, 10)
```

```
##     sequencial_candidato     saida       tse      dif_rel
##                   <char>     <num>     <num>        <num>
##  1:         140000614282  691415.9  691415.9 4.882798e-15
##  2:         240000627288  713420.8  713420.8 4.732192e-15
##  3:         250000619515  724229.5  724229.5 4.661567e-15
##  4:         250000623989  729000.0  729000.0 4.631062e-15
##  5:         260000616178  705990.0  705990.0 4.617104e-15
##  6:         250000606211  706298.5  706298.5 4.615087e-15
##  7:          90000610089 7157801.3 7157801.3 4.553953e-15
##  8:         220000618467  751087.7  751087.7 4.494874e-15
##  9:         170000610367  699742.9  699742.9 4.491955e-15
## 10:          60000611222  759000.0  759000.0 4.448016e-15
```
