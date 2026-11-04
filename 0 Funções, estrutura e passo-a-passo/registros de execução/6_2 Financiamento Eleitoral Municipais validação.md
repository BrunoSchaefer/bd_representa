---
title: "6_2 Financiamento Eleitoral Municipais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `financiamento_municipais.csv` (código 6_2, feito com a basedosdados) com os arquivos de prestação de contas do Portal de Dados Abertos do TSE, para uma amostra: **eleição de 2020, candidatos de Tocantins** (prefeitos e vereadores).

Como no 6_1, os valores produzidos estão deflacionados; o fator de deflação é estimado pela mediana da razão produzido/TSE e os valores são comparados em termos nominais. Critério: diferença de até 1% por candidato, com pelo menos 99% dos candidatos dentro da tolerância.

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
zip_tse     <- file.path(pasta$brutos_financ_eleitoral,
                    "prestacao_de_contas_eleitorais_candidatos_2020.zip")
```

## Banco produzido


``` r
produzido <- ler_banco(file.path(pasta$prod_candidatos, "financiamento_municipais.csv")) |>
  filter(ano == ano_amostra, sigla_uf == uf_amostra) |>
  mutate(numero_candidato = as.character(numero_candidato),
         id_municipio_tse = as.integer(id_municipio_tse))

count(produzido, cargo)
```

```
##            cargo     n
##           <char> <int>
## 1:      prefeito   427
## 2:      vereador  7737
## 3: vice-prefeito     1
```

## Receitas


``` r
rec_tse <- ler_zip_tse(zip_tse, paste0("receitas_candidatos_", ano_amostra, "_", uf_amostra, ".csv"))

count(rec_tse, NM_TIPO_ELEICAO, ST_TURNO)
```

```
##    NM_TIPO_ELEICAO ST_TURNO     n
##             <char>   <char> <int>
## 1:       ORDINÁRIA        1 37618
```

``` r
rec_tse_cand <- rec_tse |>
  filter(texto_bd(NM_TIPO_ELEICAO) %in% c("ordinaria", "eleicao ordinaria")) |>
  mutate(
    cargo            = texto_bd(DS_CARGO),
    id_municipio_tse = as.integer(SG_UE),
    numero_candidato = NR_CANDIDATO,
    valor            = converter_valor_tse(VR_RECEITA),
    publico          = texto_bd(DS_FONTE_RECEITA) %in% c("fundo especial", "fundo partidario")
  ) |>
  group_by(cargo, id_municipio_tse, numero_candidato) |>
  summarise(receitas_tse = sum(valor, na.rm = TRUE),
            publicos_tse = sum(valor[publico], na.rm = TRUE),
            .groups = "drop")

chaves <- c("cargo", "id_municipio_tse", "numero_candidato")

comp_rec <- full_join(
  select(produzido, all_of(chaves), soma_receitas, soma_recursos_publicos),
  rec_tse_cand, by = chaves
)

fator <- comp_rec |>
  filter(soma_receitas > 0, receitas_tse > 0) |>
  summarise(fator = median(soma_receitas / receitas_tse)) |>
  pull(fator)

fator
```

```
## [1] 1.34936
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
comp_total <- comparar_valores(
  comp_rec |> transmute(across(all_of(chaves)), saida = soma_receitas / fator, tse = receitas_tse),
  "Receita total por candidato (valores nominais)"
)
```

```
## 
## --- Receita total por candidato (valores nominais) ---
## Casos nos dois bancos: 8165 | só no banco produzido: 0 | só no TSE: 2 
## Casos com diferença <= 1%: 8162 (99.96%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 75.448.597 | total TSE: 75.449.823 | diferença: -0.002 %
## Veredito: OK
```

``` r
head(comp_total, 10)
```

```
##        cargo id_municipio_tse numero_candidato    saida      tse      dif_rel
##       <char>            <int>           <char>    <num>    <num>        <num>
##  1: vereador            92533            13000     0.00   930.00 1.000000e+00
##  2: vereador            92436            11151  9800.00 10051.25 2.499689e-02
##  3: vereador            92436            77111  1800.00  1845.00 2.439024e-02
##  4: prefeito            73148               15 78087.60 78087.60 4.658843e-15
##  5: prefeito            92193               55 78250.00 78250.00 4.649174e-15
##  6: vereador            93858            77100  7958.00  7958.00 4.571474e-15
##  7: prefeito            96792               14 76499.30 76499.30 4.565349e-15
##  8: vereador            95591            14234  7589.51  7589.51 4.553759e-15
##  9: vereador            73440            23222 74740.00 74740.00 4.478111e-15
## 10: prefeito            92142               55 82300.00 82300.00 4.420387e-15
```

``` r
comp_publico <- comparar_valores(
  comp_rec |> transmute(across(all_of(chaves)), saida = soma_recursos_publicos / fator, tse = publicos_tse),
  "Recursos públicos (FEFC + Fundo Partidário) por candidato"
)
```

```
## 
## --- Recursos públicos (FEFC + Fundo Partidário) por candidato ---
## Casos nos dois bancos: 8165 | só no banco produzido: 0 | só no TSE: 2 
## Casos com diferença <= 1%: 8165 (100%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 43.250.109 | total TSE: 43.250.109 | diferença: 0 %
## Veredito: OK
```

``` r
head(comp_publico, 10)
```

```
##        cargo id_municipio_tse numero_candidato   saida     tse      dif_rel
##       <char>            <int>           <char>   <num>   <num>        <num>
##  1: vereador            92738            25500   768.6   768.6 4.733254e-15
##  2: prefeito            73075               11 75950.0 75950.0 4.406768e-15
##  3: vereador            96032            15000  7650.0  7650.0 4.398863e-15
##  4: prefeito            73040               11 80000.0 80000.0 4.365575e-15
##  5: prefeito            73245               55 80000.0 80000.0 4.365575e-15
##  6: vereador            73440            10259 80000.0 80000.0 4.365575e-15
##  7: vereador            93238            11123  7740.0  7740.0 4.347714e-15
##  8: prefeito            73466               10  8000.0  8000.0 4.320100e-15
##  9: prefeito            73318               23  8000.0  8000.0 4.320100e-15
## 10: vereador            93653            11007  8000.0  8000.0 4.320100e-15
```

## Despesas


``` r
desp_tse <- ler_zip_tse(zip_tse, paste0("despesas_contratadas_candidatos_", ano_amostra, "_", uf_amostra, ".csv"))

desp_tse_cand <- desp_tse |>
  filter(texto_bd(NM_TIPO_ELEICAO) %in% c("ordinaria", "eleicao ordinaria")) |>
  mutate(cargo            = texto_bd(DS_CARGO),
         id_municipio_tse = as.integer(SG_UE),
         numero_candidato = NR_CANDIDATO,
         valor            = converter_valor_tse(VR_DESPESA_CONTRATADA)) |>
  group_by(cargo, id_municipio_tse, numero_candidato) |>
  summarise(despesas_tse = sum(valor, na.rm = TRUE), .groups = "drop")

comp_desp <- comparar_valores(
  full_join(select(produzido, all_of(chaves), despesas_total_can), desp_tse_cand, by = chaves) |>
    transmute(across(all_of(chaves)), saida = despesas_total_can / fator, tse = despesas_tse),
  "Despesas contratadas por candidato"
)
```

```
## 
## --- Despesas contratadas por candidato ---
## Casos nos dois bancos: 8165 | só no banco produzido: 0 | só no TSE: 2 
## Casos com diferença <= 1%: 8161 (99.95%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 61.148.238 | total TSE: 61.147.198 | diferença: 0.002 %
## Veredito: OK
```

``` r
head(comp_desp, 10)
```

```
##        cargo id_municipio_tse numero_candidato    saida      tse      dif_rel
##       <char>            <int>           <char>    <num>    <num>        <num>
##  1: vereador            92436            11151  1650.00  9035.25 8.173819e-01
##  2: prefeito            92681               77 19000.00 10500.00 4.473684e-01
##  3: vereador            73300            22022   120.00   150.00 2.000000e-01
##  4: vereador            92436            77111   800.00   845.00 5.325444e-02
##  5: vereador            95273            15999  7414.00  7414.00 4.906904e-15
##  6: vereador            73440            43147   754.70   754.70 4.820430e-15
##  7: vereador            92185            15000   741.40   741.40 4.753563e-15
##  8: vereador            95591            55590  7499.30  7499.30 4.729814e-15
##  9: vereador            92410            19111 74333.27 74333.27 4.698380e-15
## 10: vereador            73210            13111   760.45   760.45 4.634482e-15
```
