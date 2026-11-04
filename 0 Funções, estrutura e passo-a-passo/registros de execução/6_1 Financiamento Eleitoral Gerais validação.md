---
title: "6_1 Financiamento Eleitoral Gerais - validação"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-09-30"
output: html_document
---



## Objetivo

Comparar o banco `financiamento_gerais.xlsx` (código 6_1, feito com a basedosdados) com os arquivos de prestação de contas do Portal de Dados Abertos do TSE, para uma amostra: **eleição de 2022, candidatos do Rio de Janeiro**.

Os valores do banco produzido estão deflacionados (IPCA de 12/2025, com referência em dezembro do ano da eleição). Como o fator de deflação é o mesmo para todos os candidatos de um ano, a razão entre o valor produzido e o valor nominal do TSE deve ser igual para todos. A validação estima esse fator pela mediana das razões, converte os valores produzidos de volta para valores nominais e compara candidato a candidato.

Critério: diferença de até 1% por candidato; o banco é considerado OK se pelo menos 99% dos candidatos estão dentro da tolerância e o total difere menos de 1%.

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
ano_amostra <- 2022
uf_amostra  <- "RJ"
zip_tse     <- file.path(pasta$brutos_financ_eleitoral,
                    "prestacao_de_contas_eleitorais_candidatos_2022.zip")
```

## Banco produzido


``` r
produzido <- read.xlsx(file.path(pasta$prod_candidatos, "financiamento_gerais.xlsx")) |>
  filter(ano == ano_amostra, sigla_uf == uf_amostra) |>
  mutate(numero_candidato = as.character(numero_candidato))

count(produzido, cargo)
```

```
##               cargo    n
## 1 deputado estadual 1608
## 2  deputado federal 1068
## 3        governador    9
## 4           senador   13
```

## Receitas


``` r
rec_tse <- ler_zip_tse(zip_tse, paste0("receitas_candidatos_", ano_amostra, "_", uf_amostra, ".csv"))

count(rec_tse, NM_TIPO_ELEICAO, ST_TURNO)
```

```
##    NM_TIPO_ELEICAO ST_TURNO     n
##             <char>   <char> <int>
## 1:       ORDINÁRIA        1 20718
```

``` r
rec_tse_cand <- rec_tse |>
  mutate(
    cargo            = texto_bd(DS_CARGO),
    numero_candidato = NR_CANDIDATO,
    valor            = converter_valor_tse(VR_RECEITA),
    publico          = texto_bd(DS_FONTE_RECEITA) %in% c("fundo especial", "fundo partidario")
  ) |>
  group_by(cargo, numero_candidato) |>
  summarise(receitas_tse  = sum(valor, na.rm = TRUE),
            publicos_tse  = sum(valor[publico], na.rm = TRUE),
            .groups = "drop")

comp_rec <- full_join(
  select(produzido, cargo, numero_candidato, soma_receitas, soma_recursos_publicos),
  rec_tse_cand,
  by = c("cargo", "numero_candidato")
)

# Fator de deflação estimado (mediana da razão produzido / TSE)
fator <- comp_rec |>
  filter(soma_receitas > 0, receitas_tse > 0) |>
  summarise(fator = median(soma_receitas / receitas_tse)) |>
  pull(fator)

fator
```

```
## [1] 1.150615
```

``` r
# Fator esperado pelo deflateBR (requer internet)
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
  comp_rec |> transmute(cargo, numero_candidato, saida = soma_receitas / fator, tse = receitas_tse),
  "Receita total por candidato (valores nominais)"
)
```

```
## 
## --- Receita total por candidato (valores nominais) ---
## Casos nos dois bancos: 2698 | só no banco produzido: 0 | só no TSE: 5 
## Casos com diferença <= 1%: 2688 (99.63%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 440.918.769 | total TSE: 441.166.989 | diferença: -0.056 %
## Veredito: OK
```

``` r
head(comp_total, 10)
```

```
##                 cargo numero_candidato  saida    tse    dif_rel
##                <char>           <char>  <num>  <num>      <num>
##  1: deputado estadual            51555      0    500 1.00000000
##  2: deputado estadual            55233      0   5060 1.00000000
##  3:  deputado federal             1217      0  20000 1.00000000
##  4:  deputado federal             7749      0  25000 1.00000000
##  5: deputado estadual            44567  25000  71800 0.65181058
##  6: deputado estadual            12369  15000  35000 0.57142857
##  7:  deputado federal             5111  69470 128300 0.45853468
##  8: deputado estadual            14122 300000 372300 0.19419823
##  9: deputado estadual            10008  21095  20000 0.05190804
## 10:  deputado federal             7074  25000  25540 0.02114330
```

``` r
comp_publico <- comparar_valores(
  comp_rec |> transmute(cargo, numero_candidato, saida = soma_recursos_publicos / fator, tse = publicos_tse),
  "Recursos públicos (FEFC + Fundo Partidário) por candidato"
)
```

```
## 
## --- Recursos públicos (FEFC + Fundo Partidário) por candidato ---
## Casos nos dois bancos: 2698 | só no banco produzido: 0 | só no TSE: 5 
## Casos com diferença <= 1%: 2693 (99.81%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 371.014.870 | total TSE: 371.182.070 | diferença: -0.045 %
## Veredito: OK
```

``` r
head(comp_publico, 10)
```

```
##                 cargo numero_candidato      saida        tse      dif_rel
##                <char>           <char>      <num>      <num>        <num>
##  1:  deputado federal             1217       0.00   20000.00 1.000000e+00
##  2:  deputado federal             7749       0.00   25000.00 1.000000e+00
##  3: deputado estadual            44567   25000.00   71200.00 6.488764e-01
##  4: deputado estadual            12369   15000.00   35000.00 5.714286e-01
##  5:  deputado federal             5111   50000.00  106000.00 5.283019e-01
##  6: deputado estadual            10123  911230.00  911230.00 4.726981e-15
##  7:  deputado federal             3524     877.28     877.28 4.406065e-15
##  8:  deputado federal             3533     877.28     877.28 4.406065e-15
##  9:  deputado federal             1066   95730.00   95730.00 4.256279e-15
## 10: deputado estadual            12345 1041410.00 1041410.00 4.136091e-15
```

## Despesas

O TSE divulga despesas contratadas e despesas pagas; o arquivo de despesas pagas não identifica o candidato. A comparação é feita com as despesas contratadas.


``` r
desp_tse <- ler_zip_tse(zip_tse, paste0("despesas_contratadas_candidatos_", ano_amostra, "_", uf_amostra, ".csv"))

desp_tse_cand <- desp_tse |>
  mutate(cargo            = texto_bd(DS_CARGO),
         numero_candidato = NR_CANDIDATO,
         valor            = converter_valor_tse(VR_DESPESA_CONTRATADA)) |>
  group_by(cargo, numero_candidato) |>
  summarise(despesas_tse = sum(valor, na.rm = TRUE), .groups = "drop")

comp_desp <- comparar_valores(
  full_join(select(produzido, cargo, numero_candidato, despesas_total_can),
            desp_tse_cand, by = c("cargo", "numero_candidato")) |>
    transmute(cargo, numero_candidato, saida = despesas_total_can / fator, tse = despesas_tse),
  "Despesas contratadas por candidato"
)
```

```
## 
## --- Despesas contratadas por candidato ---
## Casos nos dois bancos: 2698 | só no banco produzido: 0 | só no TSE: 5 
## Casos com diferença <= 1%: 2661 (98.63%)
```

```
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
## Warning in prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, :
## 'big.mark' e 'decimal.mark' são ambos '.', o que pode ser confuso
```

```
## Total produzido: 439.885.236 | total TSE: 440.633.716 | diferença: -0.17 %
## Veredito: ATENÇÃO — verificar os casos discrepantes
```

``` r
head(comp_desp, 10)
```

```
##                 cargo numero_candidato    saida       tse   dif_rel
##                <char>           <char>    <num>     <num>     <num>
##  1: deputado estadual            36022     0.00  30000.00 1.0000000
##  2: deputado estadual            51315   500.00      0.00 1.0000000
##  3: deputado estadual            55233     0.00   5060.00 1.0000000
##  4:  deputado federal             1217     0.00  20000.00 1.0000000
##  5:  deputado federal             3366     0.00  39957.69 1.0000000
##  6:  deputado federal             7749     0.00  24940.00 1.0000000
##  7:  deputado federal             7767   892.50  32187.50 0.9722718
##  8: deputado estadual            44567  4420.00  71800.00 0.9384401
##  9: deputado estadual            11235  1584.35  13779.35 0.8850200
## 10:  deputado federal             5111 17470.00 126286.95 0.8616642
```
