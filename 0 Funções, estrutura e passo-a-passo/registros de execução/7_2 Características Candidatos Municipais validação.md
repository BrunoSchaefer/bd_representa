---
title: "7_2 Características Candidatos Municipais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `candidaturas_municipais.csv` (código 7_2, feito com a basedosdados) com o arquivo de candidaturas do Portal de Dados Abertos do TSE (`consulta_cand`), para uma amostra: **eleição de 2020, Tocantins** (prefeitos e vereadores).

Verificações: correspondência das candidaturas (município, cargo e número), contagem por cargo contra as candidaturas aptas no TSE e concordância de gênero, cor/raça, data de nascimento e título eleitoral. Critério: pelo menos 99%.

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
cargos      <- c("prefeito", "vereador")
```

## Dados


``` r
produzido <- ler_banco(file.path(pasta$prod_candidatos, "candidaturas_municipais.csv")) |>
  filter(ano == ano_amostra, sigla_uf == uf_amostra, cargo %in% cargos) |>
  mutate(
    id_municipio_tse = as.integer(id_municipio_tse),
    numero           = as.character(numero),
    data_nascimento  = if (is.numeric(data_nascimento)) convertToDate(data_nascimento) else as.Date(data_nascimento)
  )

tse <- ler_zip_tse(
  file.path(pasta$brutos_candidatos, paste0("consulta_cand_", ano_amostra, ".zip")),
  paste0("consulta_cand_", ano_amostra, "_", uf_amostra, ".csv")
) |>
  as_tibble() |>
  filter(NR_TURNO == "1", CD_TIPO_ELEICAO == "2") |>
  mutate(
    cargo            = texto_bd(DS_CARGO),
    id_municipio_tse = as.integer(SG_UE),
    numero           = NR_CANDIDATO,
    genero_tse       = texto_bd(DS_GENERO),
    raca_tse         = texto_bd(DS_COR_RACA),
    nascimento_tse   = dmy(DT_NASCIMENTO, quiet = TRUE),
    titulo_tse       = NR_TITULO_ELEITORAL_CANDIDATO,
    apto             = DS_SITUACAO_CANDIDATURA == "APTO"
  ) |>
  filter(cargo %in% cargos) |>
  distinct(id_municipio_tse, cargo, numero, .keep_all = TRUE)
```

## 1. Correspondência


``` r
chaves <- c("id_municipio_tse", "cargo", "numero")

comp <- produzido |>
  left_join(select(tse, all_of(chaves), genero_tse, raca_tse, nascimento_tse, titulo_tse, apto),
            by = chaves)

comp |>
  summarise(candidaturas = n(), encontradas_no_tse = 100 * mean(!is.na(titulo_tse)))
```

```
##   candidaturas encontradas_no_tse
## 1         7878                100
```

``` r
comp |> filter(is.na(titulo_tse)) |> select(id_municipio_tse, cargo, numero, nome) |> head(20)
```

```
## data.table vazia (0 linhas e 4 colunas): id_municipio_tse,cargo,numero,nome
```

## 2. Contagem por cargo


``` r
full_join(
  count(produzido, cargo, name = "produzido"),
  tse |> filter(apto) |> count(cargo, name = "tse_aptos"),
  by = "cargo"
) |>
  mutate(dif_pct = round(100 * (produzido - tse_aptos) / tse_aptos, 2))
```

```
##       cargo produzido tse_aptos dif_pct
##      <char>     <int>     <int>   <num>
## 1: prefeito       414       408    1.47
## 2: vereador      7464      7465   -0.01
```

## 3. Concordância das características


``` r
concordancia <- comp |>
  filter(!is.na(titulo_tse)) |>
  summarise(
    genero     = mean(genero == genero_tse, na.rm = TRUE),
    raca       = mean(raca == raca_tse, na.rm = TRUE),
    nascimento = mean(data_nascimento == nascimento_tse, na.rm = TRUE),
    titulo     = mean(as.numeric(titulo_eleitoral) == as.numeric(titulo_tse), na.rm = TRUE)
  ) |>
  mutate(across(everything(), ~ round(100 * .x, 2)))

concordancia
```

```
##   genero  raca nascimento titulo
## 1  99.92 99.83      99.77  99.75
```

``` r
cat("Veredito:",
    if (all(unlist(concordancia) >= 99, na.rm = TRUE)) "OK" else "ATENÇÃO — verificar discordâncias",
    "\n")
```

```
## Veredito: OK
```
