---
title: "9_1 Votos e Resultados Gerais"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-11-04"
output: html_document
---



Dados dos resultados dos candidatos das eleições gerais (2010, 2014, 2018, 2022, 2026)

Por candidato

Retirados da basedosdados


Posteriormente, validar a partir das informaçoes retiradas diretamente do Portal de Dados Abertos do TSE 

https://dadosabertos.tse.jus.br/dataset/ 


## Pacotes e configuração


``` r
library(tidyverse)
```

```
## Warning: pacote 'ggplot2' foi compilado no R versão 4.5.3
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
## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.1     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(basedosdados)
```

```
## Warning: pacote 'basedosdados' foi compilado no R versão 4.5.2
```

``` r
library(stringr)
library(stringi)
library(openxlsx)
```

```
## Warning: pacote 'openxlsx' foi compilado no R versão 4.5.2
```

``` r
library(scales)
```

```
## 
## Anexando pacote: 'scales'
## 
## O seguinte objeto é mascarado por 'package:purrr':
## 
##     discard
## 
## O seguinte objeto é mascarado por 'package:readr':
## 
##     col_factor
```

``` r
library(deflateBR)
```

```
## 
## To cite deflateBR in publications, please use: citation('deflateBR')
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
source(here("0 Funções, estrutura e passo-a-passo", "0_pastas.R"))

set_billing_id("seu-projeto") ## substituir por seu projeto
```

```
## Project keys set successfully
```

``` r
# Somente eleições ordinárias. "eleicoes municipais 2020 - ap" é a eleição
# ordinária de Macapá (AP) de 2020, adiada para dezembro por causa do apagão
# no estado; as demais categorias são eleições suplementares e novas eleições.
tipos_ordinarios <- c("eleicao ordinaria", "eleicoes municipais 2020 - ap")
```


## Coleta dos dados



``` r
bd_res <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.resultados_candidato") %>%
  dplyr::filter(ano %in% c(2002, 2006, 2010, 2014, 2018, 2022, 2026), tipo_eleicao %in% tipos_ordinarios) %>%
  dplyr::select(
    ano, cargo, turno, sigla_uf,
    sequencial_candidato, titulo_eleitoral_candidato,
    numero_partido, sigla_partido,
    numero_candidato, resultado, votos, tipo_eleicao
  )
```

```
## ! Using an auto-discovered, cached token.
```

```
##   To suppress this message, modify your code or options to clearly consent to
##   the use of a cached token.
```

```
##   See gargle's "Non-interactive auth" vignette for more details:
```

```
##   <https://gargle.r-lib.org/articles/non-interactive-auth.html>
```

```
## ℹ The bigrquery package is using a cached token for '<e-mail>'.
```

```
## Successfully connected to table
## `basedosdados.br_tse_eleicoes.resultados_candidato`.
```

``` r
resultados <- basedosdados::bd_collect(bd_res)
rm(bd_res); gc()
```

```
##           used  (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 1958663 104.7    3965872 211.9  2377981 127.0
## Vcells 5141600  39.3   10146329  77.5  8353988  63.8
```

``` r
names(resultados)
```

```
##  [1] "ano"                        "cargo"                     
##  [3] "turno"                      "sigla_uf"                  
##  [5] "sequencial_candidato"       "titulo_eleitoral_candidato"
##  [7] "numero_partido"             "sigla_partido"             
##  [9] "numero_candidato"           "resultado"                 
## [11] "votos"                      "tipo_eleicao"
```

``` r
# --- Classificar resultados

table(resultados$resultado)
```

```
## 
##                      2º turno                        eleito 
##                           168                          4585 
##              eleito por media                 eleito por qp 
##                          1814                          5096 
##                    nao eleito renuncia/falecimento/cassacao 
##                         34251                             2 
##                      suplente 
##                         98496
```


## Criar variável eleito



``` r
# eleito = "eleito" quando resultado %in% c("eleito","eleito por media","eleito por qp")
# caso contrário = "nao eleito"

resultados_gerais <- resultados %>%
  mutate(
    eleito = ifelse(
      resultado %in% c("eleito", "eleito por media", "eleito por qp"),
      "eleito",
      "nao eleito"
    )
  )

table(resultados_gerais$eleito, useNA = "ifany")
```

```
## 
##     eleito nao eleito 
##      11495     132930
```

``` r
table(resultados_gerais$eleito, resultados_gerais$ano, useNA = "ifany")
```

```
##             
##               2002  2006  2010  2014  2018  2022  2026
##   eleito      1654  1626  1653  1628  1653  1627  1654
##   nao eleito 14787 15816 16229 20787 23462 24647 17202
```

## Incumbência

Um candidato é **incumbente** quando foi eleito para o mesmo cargo, no mesmo distrito (UF; o Brasil para presidente), na eleição anterior. A pessoa é identificada pelo título eleitoral (`titulo_eleitoral_candidato`). No Senado o mandato é de 8 anos: o senador é incumbente se foi eleito 4 ou 8 anos antes (mandato em curso). Para calcular a incumbência em 2010, as eleições de 2002 e 2006 são baixadas junto e descartadas depois deste passo. Nos 2º turnos em que o resultado não vem preenchido (presidente em 2010), o eleito é o mais votado. `incumbente` fica NA quando o título está ausente.


``` r
resultados_gerais <- resultados_gerais %>%
  mutate(
    titulo_eleitoral_candidato = if_else(
      titulo_eleitoral_candidato %in% c("", "-1", "-3", "#NULO", "#NULO#", "#NE"),
      NA_character_, titulo_eleitoral_candidato),
    distrito = coalesce(sigla_uf, "BR")
  )

# Quem foi eleito em cada disputa (ano, cargo, distrito)
eleitos_anteriores <- resultados_gerais %>%
  group_by(ano, cargo, distrito, turno) %>%
  mutate(eleito_disputa = eleito == "eleito" |
           (turno == 2 & is.na(resultado) & votos == max(votos))) %>%
  ungroup() %>%
  filter(eleito_disputa, !is.na(titulo_eleitoral_candidato)) %>%
  distinct(ano, cargo, distrito, titulo_eleitoral_candidato)

# Mandato em curso: eleição anterior (4 anos); no Senado, também 8 anos antes
eleitos_anteriores <- bind_rows(
  eleitos_anteriores %>% mutate(ano_seguinte = ano + 4),
  eleitos_anteriores %>% filter(cargo == "senador") %>% mutate(ano_seguinte = ano + 8)
) %>%
  distinct(ano_seguinte, cargo, distrito, titulo_eleitoral_candidato) %>%
  mutate(incumbente = 1L)

resultados_gerais <- resultados_gerais %>%
  left_join(eleitos_anteriores,
            by = c("ano" = "ano_seguinte", "cargo", "distrito", "titulo_eleitoral_candidato")) %>%
  mutate(incumbente = if_else(is.na(titulo_eleitoral_candidato), NA_integer_,
                              coalesce(incumbente, 0L))) %>%
  select(-distrito) %>%
  filter(ano >= 2010)

# Cobertura do título e número de incumbentes (1º turno)
resultados_gerais %>%
  filter(turno == 1) %>%
  group_by(ano, cargo) %>%
  summarise(com_titulo  = round(100 * mean(!is.na(titulo_eleitoral_candidato)), 1),
            candidatos  = n(),
            incumbentes = sum(incumbente, na.rm = TRUE),
            eleitos     = sum(eleito == "eleito"),
            .groups = "drop") %>%
  print(n = Inf)
```

```
## # A tibble: 30 × 6
##      ano cargo              com_titulo candidatos incumbentes eleitos
##    <dbl> <chr>                   <dbl>      <int>       <int>   <int>
##  1  2010 deputado distrital       99.7        798          14      24
##  2  2010 deputado estadual        99.9      11806         755    1035
##  3  2010 deputado federal         99.8       4887         375     513
##  4  2010 governador               99.3        149          10      18
##  5  2010 presidente              100            9           0       0
##  6  2010 senador                 100          213          25      54
##  7  2014 deputado distrital       99.8        979          14      24
##  8  2014 deputado estadual        99.8      14883         748    1035
##  9  2014 deputado federal         99.7       6178         353     513
## 10  2014 governador              100          165          12      14
## 11  2014 presidente              100           11           1       0
## 12  2014 senador                  99.4        169           8      27
## 13  2018 deputado distrital      100          917          16      24
## 14  2018 deputado estadual       100        16036         732    1035
## 15  2018 deputado federal         99.9       7630         377     513
## 16  2018 governador               99.5        184          14      13
## 17  2018 presidente              100           13           0       0
## 18  2018 senador                  99          305          29      53
## 19  2022 deputado distrital       99.8        578          19      24
## 20  2022 deputado estadual        99.9      15552         766    1035
## 21  2022 deputado federal         99.7       9683         423     513
## 22  2022 governador              100          211          15      15
## 23  2022 presidente              100           11           1       0
## 24  2022 senador                  99.5        213          12      27
## 25  2026 deputado distrital      100          415          20      24
## 26  2026 deputado estadual        99.9      10671         808    1035
## 27  2026 deputado federal         99.7       7262         403     513
## 28  2026 governador               99.5        182           9      21
## 29  2026 presidente              100           12           1       0
## 30  2026 senador                  99.7        300          29      54
```

## Exportação



``` r
dir.create(pasta$prod_candidatos, showWarnings = FALSE, recursive = TRUE)



write.xlsx(
  resultados_gerais,
  file      = file.path(pasta$prod_candidatos, "resultados_gerais.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)
```




