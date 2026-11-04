---
title: "7_1 Características Candidatos Gerais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `candidaturas_gerais.xlsx` (código 7_1, feito com a basedosdados) com o arquivo de candidaturas do Portal de Dados Abertos do TSE (`consulta_cand`), para uma amostra: **eleição de 2018** (todas as UFs).

Verificações:

1. toda candidatura do banco produzido existe no TSE (UF, cargo e número, eleição ordinária);
2. número de candidaturas por cargo, comparado com as candidaturas aptas no TSE;
3. concordância de gênero, cor/raça, data de nascimento e título eleitoral.

Critério: pelo menos 99% de correspondência e de concordância.

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
cargos      <- c("presidente", "governador", "senador", "deputado federal",
                 "deputado estadual", "deputado distrital")
```

## Dados


``` r
produzido <- read.xlsx(file.path(pasta$prod_candidatos, "candidaturas_gerais.xlsx")) |>
  filter(ano == ano_amostra, cargo %in% cargos, tipo_eleicao == "eleicao ordinaria") |>
  mutate(
    sigla_uf        = if_else(cargo == "presidente", "BR", sigla_uf),
    numero          = as.character(numero),
    data_nascimento = if (is.numeric(data_nascimento)) convertToDate(data_nascimento) else as.Date(data_nascimento)
  )

tse <- data.table::fread(
  file.path(pasta$brutos_candidatos, paste0("consulta_cand_", ano_amostra, "_BRASIL.csv")),
  sep = ";", encoding = "Latin-1", colClasses = "character"
) |>
  as_tibble() |>
  filter(NR_TURNO == "1", CD_TIPO_ELEICAO == "2") |>
  mutate(
    cargo           = texto_bd(DS_CARGO),
    sigla_uf        = SG_UF,
    numero          = NR_CANDIDATO,
    genero_tse      = texto_bd(DS_GENERO),
    raca_tse        = texto_bd(DS_COR_RACA),
    nascimento_tse  = dmy(DT_NASCIMENTO, quiet = TRUE),
    titulo_tse      = NR_TITULO_ELEITORAL_CANDIDATO,
    apto            = DS_SITUACAO_CANDIDATURA == "APTO"
  ) |>
  filter(cargo %in% cargos) |>
  distinct(sigla_uf, cargo, numero, .keep_all = TRUE)
```

## 1. Correspondência


``` r
chaves <- c("sigla_uf", "cargo", "numero")

comp <- produzido |>
  left_join(select(tse, all_of(chaves), genero_tse, raca_tse, nascimento_tse, titulo_tse, apto),
            by = chaves)

comp |>
  summarise(candidaturas = n(),
            encontradas_no_tse = mean(!is.na(genero_tse) | !is.na(titulo_tse)) * 100)
```

```
##   candidaturas encontradas_no_tse
## 1        25074                100
```

``` r
# Candidaturas não encontradas
comp |> filter(is.na(titulo_tse)) |> select(sigla_uf, cargo, numero, nome) |> head(20)
```

```
## [1] sigla_uf cargo    numero   nome    
## <0 linhas> (ou row.names de comprimento 0)
```

## 2. Contagem por cargo

As candidaturas do banco produzido são as deferidas; no TSE, a situação comparável é "APTO". Diferenças pequenas vêm de candidaturas deferidas e depois canceladas ou substituídas.


``` r
full_join(
  count(produzido, cargo, name = "produzido"),
  tse |> filter(apto) |> count(cargo, name = "tse_aptos"),
  by = "cargo"
) |>
  mutate(dif_pct = round(100 * (produzido - tse_aptos) / tse_aptos, 2))
```

```
##                cargo produzido tse_aptos dif_pct
## 1 deputado distrital       917       915    0.22
## 2  deputado estadual     16024     16008    0.10
## 3   deputado federal      7628      7606    0.29
## 4         governador       183       182    0.55
## 5         presidente        13        13    0.00
## 6            senador       309       312   -0.96
```

## 3. Concordância das características


``` r
concordancia <- comp |>
  filter(!is.na(titulo_tse)) |>
  summarise(
    genero     = mean(genero == genero_tse, na.rm = TRUE),
    raca       = mean(raca == raca_tse, na.rm = TRUE),
    nascimento = mean(data_nascimento == nascimento_tse, na.rm = TRUE),
    titulo     = mean(as.numeric(titulo_eleitoral) == as.numeric(titulo_tse), na.rm = TRUE),
    genero_na_produzido = mean(is.na(genero)),
    raca_na_produzido   = mean(is.na(raca))
  ) |>
  mutate(across(everything(), ~ round(100 * .x, 2)))

concordancia
```

```
##   genero  raca nascimento titulo genero_na_produzido raca_na_produzido
## 1  99.94 99.84       99.8  99.76                   0                 0
```

``` r
cat("Veredito:",
    if (all(unlist(concordancia[1:4]) >= 99, na.rm = TRUE)) "OK" else "ATENÇÃO — verificar discordâncias",
    "\n")
```

```
## Veredito: OK
```

``` r
# Exemplos de discordância de gênero ou raça
comp |>
  filter(genero != genero_tse | raca != raca_tse) |>
  select(sigla_uf, cargo, numero, nome, genero, genero_tse, raca, raca_tse) |>
  head(20)
```

```
##    sigla_uf              cargo numero                               nome
## 1        SP  deputado estadual  19511          Gildaecio Soares De Sousa
## 2        RJ  deputado estadual  36611   Neilton Virgilio De Souza Junior
## 3        MG   deputado federal   4057               Vilson Luiz Da Silva
## 4        DF deputado distrital  70133 Francisco Celio Ribeiro De Santana
## 5        MG  deputado estadual  70378    Thayanne Felizardo Duarte Alves
## 6        MG  deputado estadual  70369            Edvania Maria Fernandes
## 7        DF   deputado federal   2711               Gildete Santana Neto
## 8        AM  deputado estadual  25123          Márcia Regina Gondim Dôce
## 9        CE   deputado federal   1522                Gisele Felipe Braga
## 10       AP  deputado estadual  44289              Aldine Alves Sarmento
## 11       MG  deputado estadual  28019            Jaqueline Alves Rezende
## 12       MG   deputado federal   2801                 Glauciene Nogueira
## 13       PI   deputado federal   4041      Marcos Daniel Soares De Souza
## 14       MA  deputado estadual  45123            Geanir Maria Dos Santos
## 15       PE   deputado federal   5023         Aldierize Barbosa Da Silva
## 16       RO  deputado estadual  13123        Vandeir Pereira De Sa Leite
## 17       RO  deputado estadual  13789 Fabio Janilson Pereira De Oliveira
## 18       RJ   deputado federal   3636               Georgiton Luis Ramos
## 19       RR  deputado estadual  36777          Tania Nubia Lucena Coelho
## 20       ES  deputado estadual  43222  Daniela Mellissa De Matos Quezado
##       genero     genero_tse   raca       raca_tse
## 1  masculino      masculino  parda         branca
## 2  masculino      masculino branca          parda
## 3  masculino nao divulgavel branca nao divulgavel
## 4  masculino       feminino  parda          parda
## 5   feminino       feminino branca          parda
## 6   feminino       feminino  parda         branca
## 7   feminino       feminino branca          parda
## 8   feminino       feminino branca          parda
## 9   feminino       feminino  parda         branca
## 10  feminino       feminino  parda          preta
## 11  feminino       feminino branca        amarela
## 12  feminino nao divulgavel  parda nao divulgavel
## 13 masculino      masculino  parda         branca
## 14  feminino      masculino  parda         branca
## 15  feminino      masculino branca          parda
## 16 masculino      masculino  parda         branca
## 17 masculino      masculino  parda          preta
## 18 masculino      masculino  parda         branca
## 19  feminino       feminino  parda         branca
## 20  feminino      masculino  parda          parda
```
