---
title: "9_1 Votos e Resultados Gerais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `resultados_gerais.xlsx` (código 9_1, feito com a basedosdados) com os votos por município e zona do Portal de Dados Abertos do TSE (`votacao_candidato_munzona`), para uma amostra: **eleição de 2018, Acre, 1º turno** (governador, senador, deputados federais e estaduais). Presidente fica de fora porque o arquivo da UF traz só os votos do presidente naquela UF.

Os votos do TSE são somados por candidato. Compara-se também a situação final no turno (eleito / não eleito). Critério: votos idênticos em pelo menos 99% dos candidatos e concordância da situação de pelo menos 99%.

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
uf_amostra  <- "AC"
cargos      <- c("governador", "senador", "deputado federal", "deputado estadual")
```

## Dados


``` r
produzido <- read.xlsx(file.path(pasta$prod_candidatos, "resultados_gerais.xlsx")) |>
  filter(ano == ano_amostra, sigla_uf == uf_amostra, turno == 1,
         cargo %in% cargos, tipo_eleicao == "eleicao ordinaria") |>
  mutate(numero_candidato = as.character(numero_candidato))

tse <- ler_zip_tse(
  file.path(pasta$brutos_resultados, paste0("votacao_candidato_munzona_", ano_amostra, ".zip")),
  paste0("votacao_candidato_munzona_", ano_amostra, "_", uf_amostra, ".csv")
) |>
  as_tibble() |>
  filter(NR_TURNO == "1", CD_TIPO_ELEICAO == "2") |>
  mutate(cargo = texto_bd(DS_CARGO)) |>
  filter(cargo %in% cargos)

tse_cand <- tse |>
  group_by(cargo, numero_candidato = NR_CANDIDATO) |>
  summarise(
    votos_tse         = sum(as.numeric(QT_VOTOS_NOMINAIS), na.rm = TRUE),
    votos_validos_tse = sum(as.numeric(QT_VOTOS_NOMINAIS_VALIDOS), na.rm = TRUE),
    situacao_tse      = first(texto_bd(DS_SIT_TOT_TURNO)),
    .groups = "drop"
  ) |>
  mutate(eleito_tse = if_else(str_detect(situacao_tse, "^eleito"), "eleito", "nao eleito"))

comp <- full_join(
  select(produzido, cargo, numero_candidato, votos, eleito),
  tse_cand, by = c("cargo", "numero_candidato")
)
```

## Comparação


``` r
comparar_valores(
  comp |> transmute(cargo, numero_candidato, saida = votos, tse = votos_tse),
  "Votos nominais por candidato (1º turno)", tolerancia = 0
) |> head(10)
```

```
## 
## --- Votos nominais por candidato (1º turno) ---
## Casos nos dois bancos: 528 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 0%: 528 (100%)
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

```
##                 cargo numero_candidato saida   tse dif_rel
##                <char>           <char> <num> <num>   <num>
##  1: deputado estadual            10333    41    41       0
##  2: deputado estadual            10810     4     4       0
##  3: deputado estadual            10444   151   151       0
##  4: deputado estadual            10101    65    65       0
##  5: deputado estadual            10111    81    81       0
##  6: deputado estadual            10777  5827  5827       0
##  7: deputado estadual            10510     6     6       0
##  8: deputado estadual            10310     1     1       0
##  9: deputado estadual            10910   214   214       0
## 10: deputado estadual            10555    83    83       0
```

``` r
comparar_valores(
  comp |> transmute(cargo, numero_candidato, saida = votos, tse = votos_validos_tse),
  "Votos nominais válidos por candidato (1º turno)", tolerancia = 0
) |> head(10)
```

```
## 
## --- Votos nominais válidos por candidato (1º turno) ---
## Casos nos dois bancos: 528 | só no banco produzido: 0 | só no TSE: 0 
## Casos com diferença <= 0%: 528 (100%)
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

```
##                 cargo numero_candidato saida   tse dif_rel
##                <char>           <char> <num> <num>   <num>
##  1: deputado estadual            10333    41    41       0
##  2: deputado estadual            10810     4     4       0
##  3: deputado estadual            10444   151   151       0
##  4: deputado estadual            10101    65    65       0
##  5: deputado estadual            10111    81    81       0
##  6: deputado estadual            10777  5827  5827       0
##  7: deputado estadual            10510     6     6       0
##  8: deputado estadual            10310     1     1       0
##  9: deputado estadual            10910   214   214       0
## 10: deputado estadual            10555    83    83       0
```

``` r
comp |>
  filter(!is.na(eleito), !is.na(eleito_tse)) |>
  summarise(concordancia_eleito = round(100 * mean(eleito == eleito_tse), 2),
            eleitos_produzido   = sum(eleito == "eleito"),
            eleitos_tse         = sum(eleito_tse == "eleito"))
```

```
##   concordancia_eleito eleitos_produzido eleitos_tse
## 1                 100                35          35
```

``` r
comp |> filter(eleito != eleito_tse) |> head(10)
```

```
## [1] cargo             numero_candidato  votos             eleito           
## [5] votos_tse         votos_validos_tse situacao_tse      eleito_tse       
## <0 linhas> (ou row.names de comprimento 0)
```
