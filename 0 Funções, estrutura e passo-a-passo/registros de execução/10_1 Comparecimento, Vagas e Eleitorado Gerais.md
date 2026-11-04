---
title: "10 Comparecimento, Vagas e Eleitorado Gerais"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-11-04"
output: html_document
---



Dados das eleições gerais (2010, 2014, 2018, 2022, 2026)

Por distrito

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


## Coleta dos dados


### Perfil do eleitorado



``` r
bd_eleitorado <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.perfil_eleitorado_municipio_zona") %>%
  dplyr::filter(
    ano %in% c(2010, 2014, 2018, 2022, 2026)
  ) %>%
  dplyr::select(
    ano, sigla_uf,
    genero, estado_civil, instrucao, grupo_idade, eleitores
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
## `basedosdados.br_tse_eleicoes.perfil_eleitorado_municipio_zona`.
```

``` r
caracteristicas_eleitorado <- basedosdados::bd_collect(bd_eleitorado)
rm(bd_eleitorado); gc()
```

```
##             used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells   1731622  92.5    3420164  182.7   3420164  182.7
## Vcells 127728844 974.5  216613613 1652.7 169486560 1293.1
```


#### Codificação



``` r
## Categorizar o dado

summary(caracteristicas_eleitorado)
```

```
##       ano         sigla_uf            genero          estado_civil      
##  Min.   :2010   Length:20788506    Length:20788506    Length:20788506   
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2018                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##   instrucao         grupo_idade          eleitores      
##  Length:20788506    Length:20788506    Min.   :   1.00  
##  Class :character   Class :character   1st Qu.:   2.00  
##  Mode  :character   Mode  :character   Median :   5.00  
##                                        Mean   :  35.65  
##                                        3rd Qu.:  20.00  
##                                        Max.   :7187.00
```

``` r
table(caracteristicas_eleitorado$estado_civil)
```

```
## 
##       0       1       3       5       7       9 
##  327678 7048729 6236767 2516292 1918068 2740972
```

``` r
table(caracteristicas_eleitorado$genero)
```

```
## 
##        0        2        4 
##   199105  9643245 10946156
```

``` r
table(caracteristicas_eleitorado$instrucao)
```

```
## 
##       0       1       2       3       4       5       6       7       8 
##  169907 2488923 2989229 3571228 2561839 2353796 2916856 1637273 2099455
```

``` r
table(caracteristicas_eleitorado$grupo_idade)
```

```
## 
##      -3    1600    1700    1800    1900    2000    2124    2529    3034    3539 
##   16268  186516  266632  350657  395162  431844  725279  969722 1169649 1337989 
##    4044    4549    5054    5559    6064    6569    7074    7579    8084    8589 
## 1501719 1635216 1714865 1734069 1679095 1548860 1380310 1173136  957098  734785 
##    9094    9599    9999 
##  483958  261265  134412
```

``` r
## Agrupar por município

chaves <- c("ano", "sigla_uf")

## 1. Total de eleitores
eleitorado_total <- caracteristicas_eleitorado %>%
  group_by(across(all_of(chaves))) %>%
  summarise(total_eleitores = sum(eleitores, na.rm = TRUE), .groups = "drop")

## 2. Gênero (2 = masculino, 4 = feminino, 0 = não informado)
eleitorado_genero <- caracteristicas_eleitorado %>%
  mutate(genero = recode(genero,
                         "2" = "homens",
                         "4" = "mulheres",
                         "0" = "genero_ni")) %>%
  count(across(all_of(chaves)), genero, wt = eleitores, name = "n") %>%
  pivot_wider(names_from = genero, values_from = n,
              names_prefix = "eleitores_", values_fill = 0)

## 3. Instrução
eleitorado_instrucao <- caracteristicas_eleitorado %>%
  mutate(instrucao = recode(instrucao,
                            "0" = "instr_ni",
                            "1" = "analfabeto",
                            "2" = "le_escreve",
                            "3" = "fund_incompleto",
                            "4" = "fund_completo",
                            "5" = "medio_incompleto",
                            "6" = "medio_completo",
                            "7" = "sup_incompleto",
                            "8" = "sup_completo")) %>%
  count(across(all_of(chaves)), instrucao, wt = eleitores, name = "n") %>%
  pivot_wider(names_from = instrucao, values_from = n,
              names_prefix = "eleitores_", values_fill = 0)

## 4. Juntar e calcular proporções
eleitorado_uf <- eleitorado_total %>%
  left_join(eleitorado_genero,    by = chaves) %>%
  left_join(eleitorado_instrucao, by = chaves) %>%
  mutate(across(starts_with("eleitores_"),
                ~ .x / total_eleitores,
                .names = "prop_{sub('eleitores_', '', .col)}"))


eleitorado_uf <- eleitorado_uf %>%
  filter(sigla_uf != "ZZ")
```





### Vagas



``` r
vagas <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.vagas") %>%
  dplyr::filter(ano %in% c(2010, 2014, 2018, 2022, 2026)) %>%
  basedosdados::bd_collect()
```

```
## Successfully connected to table `basedosdados.br_tse_eleicoes.vagas`.
```

``` r
names(vagas)                  # confirmar se existe tipo_eleicao (ou similar)
```

```
## [1] "ano"              "id_eleicao"       "tipo_eleicao"     "data_eleicao"    
## [5] "sigla_uf"         "id_municipio"     "id_municipio_tse" "cargo"           
## [9] "vagas"
```

``` r
count(vagas, tipo_eleicao)
```

```
## # A tibble: 5 × 2
##   tipo_eleicao                            n
##   <chr>                               <int>
## 1 eleicao ordinaria                     702
## 2 eleicao suplementar governador - to     2
## 3 eleicao suplementar governador am       2
## 4 eleicao suplementar senador - mt        3
## 5 senador                                 3
```

``` r
# Vagas da eleição ordinária, por UF e cargo (presidente fica de fora: a
# unidade de análise é a UF). Os cargos são restringidos aos de votação na
# etapa de junção.
vagas_uf <- vagas %>%
  filter(tipo_eleicao %in% "eleicao ordinaria",
         sigla_uf != "ZZ",
         cargo != "presidente") %>%
  group_by(ano, sigla_uf, cargo) %>%
  summarise(vagas = max(vagas), .groups = "drop")
```







### Comparecimento



``` r
bd_eleitorado <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.detalhes_votacao_municipio") %>%
  dplyr::filter(
    ano %in% c(2010, 2014, 2018, 2022, 2026)
  ) %>%
  dplyr::select(
    ano, sigla_uf,
    id_municipio, turno, 
    cargo, aptos, votos_validos,
    votos_brancos, votos_nulos, 
    votos_legenda, votos_nominais,
    abstencoes, tipo_eleicao
  )
```

```
## Successfully connected to table
## `basedosdados.br_tse_eleicoes.detalhes_votacao_municipio`.
```

``` r
comparecimento <- basedosdados::bd_collect(bd_eleitorado)
rm(bd_eleitorado); gc()
```

```
##             used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells   1864710  99.6    3420164  182.7   3420164  182.7
## Vcells 129538206 988.3  296754512 2264.1 296754512 2264.1
```

#### Codificação


``` r
vars_votos <- c("aptos", "votos_validos", "votos_brancos", "votos_nulos",
                "votos_legenda", "votos_nominais", "abstencoes")

comparecimento_uf <- comparecimento %>%
  filter(tipo_eleicao %in% "eleicao ordinaria") %>%
  group_by(ano, sigla_uf, cargo, turno) %>%
  summarise(
    n_municipios = n_distinct(id_municipio),
    across(all_of(vars_votos), ~ sum(as.numeric(.x), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    comparecimento     = aptos - abstencoes,
    votos_totais       = votos_validos + votos_brancos + votos_nulos,
    votos_por_eleitor  = votos_totais / comparecimento,   # ≈ 1; ≈ 2 no Senado com 2 vagas

    pct_comparecimento = 100 * comparecimento / aptos,
    pct_abstencao      = 100 * abstencoes / aptos,
    pct_validos        = 100 * votos_validos / votos_totais,
    pct_brancos        = 100 * votos_brancos / votos_totais,
    pct_nulos          = 100 * votos_nulos / votos_totais,
    pct_legenda        = if_else(votos_validos > 0, 100 * votos_legenda / votos_validos, NA_real_)
  )


# 1. O filtro pegou o rótulo certo? (confira a grafia exata)
count(comparecimento, ano, tipo_eleicao)
```

```
## # A tibble: 11 × 3
##      ano tipo_eleicao                                                 n
##    <int> <chr>                                                    <int>
##  1  2010 eleicao ordinaria                                        34787
##  2  2014 eleicao ordinaria                                        35556
##  3  2014 eleicao suplementar governador - to                        278
##  4  2014 eleicao suplementar governador am                          124
##  5  2018 eleicao conselho distrital 2018 fn                           1
##  6  2018 eleicao ordinaria                                        36755
##  7  2018 eleicao suplementar senador - mt                           141
##  8  2022 eleicao conselho distrital 2022                              1
##  9  2022 eleicao ordinaria                                        36492
## 10  2026 eleicao ordinaria                                        34267
## 11  2026 eleicao ordinaria municipal - 2026 - 04/10/2026 1º turno     1
```

``` r
# 2. Votos por eleitor: ~1 em todos os cargos, ~2 no Senado de 2010 e 2018
comparecimento_uf %>%
  group_by(ano, cargo, turno) %>%
  summarise(votos_por_eleitor = round(sum(votos_totais) / sum(comparecimento), 2),
            .groups = "drop") %>%
  print(n = Inf)
```

```
## # A tibble: 40 × 4
##      ano cargo              turno votos_por_eleitor
##    <int> <chr>              <int>             <dbl>
##  1  2010 deputado distrital     1              1   
##  2  2010 deputado estadual      1              1   
##  3  2010 deputado federal       1              1   
##  4  2010 governador             1              1   
##  5  2010 governador             2              1   
##  6  2010 presidente             1              1   
##  7  2010 presidente             2              1   
##  8  2010 senador                1              2   
##  9  2014 deputado distrital     1              1   
## 10  2014 deputado estadual      1              1   
## 11  2014 deputado federal       1              1   
## 12  2014 governador             1              1   
## 13  2014 governador             2              1   
## 14  2014 presidente             1              1   
## 15  2014 presidente             2              1   
## 16  2014 senador                1              1   
## 17  2018 deputado distrital     1              1   
## 18  2018 deputado estadual      1              1   
## 19  2018 deputado federal       1              1   
## 20  2018 governador             1              1   
## 21  2018 governador             2              1   
## 22  2018 presidente             1              1   
## 23  2018 presidente             2              1   
## 24  2018 senador                1              1.99
## 25  2022 deputado distrital     1              1   
## 26  2022 deputado estadual      1              1   
## 27  2022 deputado federal       1              0.99
## 28  2022 governador             1              1   
## 29  2022 governador             2              1   
## 30  2022 presidente             1              1   
## 31  2022 presidente             2              1   
## 32  2022 senador                1              1   
## 33  2026 deputado distrital     1              1   
## 34  2026 deputado estadual      1              1   
## 35  2026 deputado federal       1              1   
## 36  2026 governador             1              1   
## 37  2026 governador             2              1   
## 38  2026 presidente             1              1   
## 39  2026 presidente             2              1   
## 40  2026 senador                1              2
```

``` r
# 3. Nos cargos proporcionais, válidos = legenda + nominais
comparecimento_uf %>%
  filter(votos_legenda > 0, votos_validos != votos_legenda + votos_nominais)
```

```
## # A tibble: 1 × 21
##     ano sigla_uf cargo     turno n_municipios  aptos votos_validos votos_brancos
##   <int> <chr>    <chr>     <int>        <int>  <dbl>         <dbl>         <dbl>
## 1  2018 MA       deputado…     1          217 4.54e6       3262551        139441
## # ℹ 13 more variables: votos_nulos <dbl>, votos_legenda <dbl>,
## #   votos_nominais <dbl>, abstencoes <dbl>, comparecimento <dbl>,
## #   votos_totais <dbl>, votos_por_eleitor <dbl>, pct_comparecimento <dbl>,
## #   pct_abstencao <dbl>, pct_validos <dbl>, pct_brancos <dbl>, pct_nulos <dbl>,
## #   pct_legenda <dbl>
```

``` r
# 4. 2º turno só deve aparecer para presidente e governador
count(comparecimento_uf, ano, turno, cargo)
```

```
## # A tibble: 40 × 4
##      ano turno cargo                  n
##    <int> <int> <chr>              <int>
##  1  2010     1 deputado distrital     1
##  2  2010     1 deputado estadual     26
##  3  2010     1 deputado federal      27
##  4  2010     1 governador            27
##  5  2010     1 presidente            29
##  6  2010     1 senador               27
##  7  2010     2 governador             9
##  8  2010     2 presidente            29
##  9  2014     1 deputado distrital     1
## 10  2014     1 deputado estadual     26
## # ℹ 30 more rows
```

``` r
comparecimento_uf <- comparecimento_uf %>%
  filter(sigla_uf != "ZZ")

comparecimento_uf %>%
  filter((ano == 2014 & sigla_uf %in% c("AM", "TO") & cargo == "governador") |
         (ano == 2018 & sigla_uf == "MT" & cargo == "senador")) %>%
  select(ano, sigla_uf, cargo, turno, aptos, comparecimento, votos_totais, votos_por_eleitor)
```

```
## # A tibble: 4 × 8
##     ano sigla_uf cargo      turno   aptos comparecimento votos_totais
##   <int> <chr>    <chr>      <int>   <dbl>          <dbl>        <dbl>
## 1  2014 AM       governador     1 2225630        1791817      1791817
## 2  2014 AM       governador     2 2225691        1722286      1722286
## 3  2014 TO       governador     1  996379         801084       801084
## 4  2018 MT       senador        1 2329374        1757368      2836194
## # ℹ 1 more variable: votos_por_eleitor <dbl>
```

``` r
count(comparecimento, ano, tipo_eleicao)
```

```
## # A tibble: 11 × 3
##      ano tipo_eleicao                                                 n
##    <int> <chr>                                                    <int>
##  1  2010 eleicao ordinaria                                        34787
##  2  2014 eleicao ordinaria                                        35556
##  3  2014 eleicao suplementar governador - to                        278
##  4  2014 eleicao suplementar governador am                          124
##  5  2018 eleicao conselho distrital 2018 fn                           1
##  6  2018 eleicao ordinaria                                        36755
##  7  2018 eleicao suplementar senador - mt                           141
##  8  2022 eleicao conselho distrital 2022                              1
##  9  2022 eleicao ordinaria                                        36492
## 10  2026 eleicao ordinaria                                        34267
## 11  2026 eleicao ordinaria municipal - 2026 - 04/10/2026 1º turno     1
```




## Juntar bancos de dados


Juntar bancos de dados por ano, distrito e cargo



``` r
chaves_cargo <- c("ano", "sigla_uf", "cargo")

# 1. Vagas: só os cargos com votação ------------------------------------------
vagas_uf <- vagas_uf %>%
  filter(cargo %in% unique(comparecimento_uf$cargo))

# Uma linha por chave? (deve voltar vazio)
vagas_uf %>% count(across(all_of(chaves_cargo))) %>% filter(n > 1)
```

```
## # A tibble: 0 × 4
## # ℹ 4 variables: ano <int>, sigla_uf <chr>, cargo <chr>, n <int>
```

``` r
# Senado: 2 em 2010/2018/2026, 1 em 2014/2022
vagas_uf %>% filter(cargo == "senador") %>% count(ano, vagas)
```

```
## # A tibble: 5 × 3
##     ano vagas     n
##   <int> <int> <int>
## 1  2010     2    27
## 2  2014     1    27
## 3  2018     2    27
## 4  2022     1    27
## 5  2026     2    27
```

``` r
# 2. Votação: 1º turno, sem presidente ------------------------------------------
comp_t1 <- comparecimento_uf %>%
  filter(turno == 1, cargo != "presidente") %>%
  select(-turno)

# 3. Junção ---------------------------------------------------------------------
base_uf <- comp_t1 %>%
  left_join(vagas_uf, by = chaves_cargo, relationship = "one-to-one") %>%
  left_join(eleitorado_uf, by = c("ano", "sigla_uf"), relationship = "many-to-one") %>%
  mutate(
    eleitores_por_vaga = aptos / vagas,
    votos_por_vaga     = votos_validos / vagas,
    razao_aptos_perfil = aptos / total_eleitores
  )

nrow(base_uf)   # 432
```

```
## [1] 540
```








## Exportação



``` r
dir.create(pasta$prod_distritos, showWarnings = FALSE, recursive = TRUE)



write.xlsx(
  base_uf,
  file      = file.path(pasta$prod_distritos, "base_uf.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)
```



