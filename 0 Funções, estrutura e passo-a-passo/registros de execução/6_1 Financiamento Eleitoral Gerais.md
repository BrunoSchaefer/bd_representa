---
title: "6_1 Financiamento Eleitoral Gerais "
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-06-11"
output: pdf_document
---




Dados de financiamento das eleições gerais (2010, 2014, 2018, 2022 e 2026)

Por candidato

Em relação às receitas e as despesas

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
tipos_ordinarios <- c("eleicao ordinaria", "eleicoes municipais 2020 - ap",
                      # 2026: receitas e despesas vêm rotuladas por esfera
                      "eleicao geral federal 2026", "eleicoes gerais estaduais 2026")
```


## Dados Receitas

### Carregar dados


``` r
bd_rec <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.receitas_candidato") %>%
  dplyr::filter(ano %in% c(2010, 2014, 2018, 2022, 2026), tipo_eleicao %in% tipos_ordinarios) %>%
  dplyr::select(
    ano, cargo, sigla_uf, tipo_eleicao,
    numero_partido, sigla_partido, numero_candidato, sequencial_candidato,
    fonte_receita, origem_receita, especie_receita,
    valor_receita, nome_doador,
    cargo_candidato_doador, numero_partido_doador, numero_candidato_doador,
    descricao_receita
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
## `basedosdados.br_tse_eleicoes.receitas_candidato`.
```

``` r
financiamento <- basedosdados::bd_collect(bd_rec)
rm(bd_rec); gc()
```

```
##            used  (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells  3434000 183.4    6118972 326.8  5472012 292.3
## Vcells 49711109 379.3   74727771 570.2 52064383 397.3
```

``` r
names(financiamento)
```

```
##  [1] "ano"                     "cargo"                  
##  [3] "sigla_uf"                "tipo_eleicao"           
##  [5] "numero_partido"          "sigla_partido"          
##  [7] "numero_candidato"        "sequencial_candidato"   
##  [9] "fonte_receita"           "origem_receita"         
## [11] "especie_receita"         "valor_receita"          
## [13] "nome_doador"             "cargo_candidato_doador" 
## [15] "numero_partido_doador"   "numero_candidato_doador"
## [17] "descricao_receita"
```

``` r
summary(financiamento)
```

```
##       ano          cargo             sigla_uf         tipo_eleicao      
##  Min.   :2010   Length:2112017     Length:2112017     Length:2112017    
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2018                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##  numero_partido     sigla_partido      numero_candidato   sequencial_candidato
##  Length:2112017     Length:2112017     Length:2112017     Length:2112017      
##  Class :character   Class :character   Class :character   Class :character    
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character    
##                                                                               
##                                                                               
##                                                                               
##  fonte_receita      origem_receita     especie_receita    valor_receita     
##  Length:2112017     Length:2112017     Length:2112017     Min.   :       0  
##  Class :character   Class :character   Class :character   1st Qu.:      79  
##  Mode  :character   Mode  :character   Mode  :character   Median :     600  
##                                                           Mean   :   11634  
##                                                           3rd Qu.:    2600  
##                                                           Max.   :66708023  
##  nome_doador        cargo_candidato_doador numero_partido_doador
##  Length:2112017     Length:2112017         Length:2112017       
##  Class :character   Class :character       Class :character     
##  Mode  :character   Mode  :character       Mode  :character     
##                                                                 
##                                                                 
##                                                                 
##  numero_candidato_doador descricao_receita 
##  Length:2112017          Length:2112017    
##  Class :character        Class :character  
##  Mode  :character        Mode  :character  
##                                            
##                                            
## 
```

``` r
table(financiamento$origem_receita, useNA = "always")
```

```
## 
##                    comercializacao de bens com dc 
##                                                10 
##                  comercializacao de bens com fefc 
##                                                 4 
##                    comercializacao de bens com or 
##                                               138 
##  comercializacao de bens ou realizacao de eventos 
##                                               104 
## descricao das doacoes relativas a comercializacao 
##                                             30659 
##                             doacoes para campanha 
##                                                57 
##                             doacoes pela internet 
##                                              5259 
##       fundo especial de financiamento de campanha 
##                                               152 
##                                  fundo partidario 
##                                                14 
##                recursos de financiamento coletivo 
##                                             12835 
##             recursos de origens nao identificadas 
##                                              1732 
##                     recursos de outros candidatos 
##                                            110920 
##             recursos de outros candidatos/comites 
##                                            220655 
##                      recursos de partido politico 
##                                            299218 
##                       recursos de pessoas fisicas 
##                                           1200861 
##                     recursos de pessoas juridicas 
##                                             82294 
##                                 recursos proprios 
##                                            138244 
##             rendimentos de aplicacoes financeiras 
##                                              1785 
##                                              <NA> 
##                                              7076
```

``` r
table(financiamento$fonte_receita, useNA = "always")
```

```
## 
##                fundo especial                fundo partidao 
##                        211568                          1859 
##              fundo partidario              nao especificado 
##                         76921                        584330 
##                 nao informado               outros recursos 
##                           328                        977162 
## outros recursos nao descritos                          <NA> 
##                        258087                          1762
```

``` r
doadores <- financiamento %>%
  count(nome_doador, name = "n_doacoes", sort = TRUE)

head(doadores, 50)
```

```
## # A tibble: 50 × 2
##    nome_doador                                             n_doacoes
##    <chr>                                                       <int>
##  1 Direção Estadual/Distrital                                 148174
##  2 Comitê Financeiro Único                                     73846
##  3 Direção Nacional                                            45660
##  4 <NA>                                                        10424
##  5 Diretório Estadual/Distrital                                 7891
##  6 Comitê Financeiro Nacional para Presidente da República      6801
##  7 Direção Estadual/Distrital - PSD - RIO DE JANEIRO            6137
##  8 DIREÇÃO ESTADUAL/DISTRITAL                                   4038
##  9 Diretório Nacional                                           3600
## 10 QueroApoiar.com.br                                           3540
## # ℹ 40 more rows
```




### Categorização 

Por origem

Por fonte 

Por doador



``` r
financiamento <- financiamento %>%
  mutate(
    recursos_pessoa_fisica = if_else(
      origem_receita %in% "recursos de pessoas fisicas", 1, 0),
    recursos_pessoa_juridica = if_else(
      origem_receita %in% "recursos de pessoas juridicas", 1, 0),
        recursos_proprios = if_else(
      origem_receita %in% "recursos proprios", 1, 0),
    recursos_publicos = if_else(
      fonte_receita %in% c("fundo especial", "fundo partidao", "fundo partidario"), 1, 0),
    recursos_partidos = if_else(
      origem_receita %in% "recursos de partido politico", 1, 0),
    recursos_outros_candidatos = if_else(
      origem_receita %in% c("recursos de outros candidatos",
                            "recursos de outros candidatos/comites"), 1, 0),
    recursos_partidos_candidatos = pmax(recursos_partidos, recursos_outros_candidatos)
  )


table(financiamento$recursos_publicos, financiamento$recursos_partidos, useNA = "always")
```

```
##       
##              0       1    <NA>
##   0    1735414   86255       0
##   1      77385  212963       0
##   <NA>       0       0       0
```

``` r
table(financiamento$recursos_publicos, financiamento$recursos_outros_candidatos, useNA = "always")
```

```
##       
##              0       1    <NA>
##   0    1566765  254904       0
##   1     213677   76671       0
##   <NA>       0       0       0
```

``` r
# --- 7.2 Direções partidárias (a partir do nome do doador) --------------------

padrao_nacional <- "\\bDIR(ETORIO|ECAO)?\\.? ?NAC|EXECUTIVA NACIONAL|COMISSAO PROVISORIA NACIONAL"
padrao_estadual <- "\\bDIR(ETORIO|ECAO)?\\.? ?(REG|EST)|EXECUTIVA (REGIONAL|ESTADUAL)|COMISSAO PROVISORIA (REGIONAL|ESTADUAL)"

financiamento <- financiamento %>%
  mutate(
    doador_norm = nome_doador %>%
      stri_trans_general("Latin-ASCII") %>% str_to_upper() %>% str_squish(),
    recursos_direcao_nacional  = if_else(str_detect(doador_norm, padrao_nacional) %in% TRUE, 1, 0),
    recursos_direcao_estadual  = if_else(str_detect(doador_norm, padrao_estadual) %in% TRUE, 1, 0),
    recursos_direcao_partidaria = pmax(recursos_direcao_nacional, recursos_direcao_estadual)
  )

table(financiamento$recursos_direcao_nacional, useNA = "always")
```

```
## 
##       0       1    <NA> 
## 2029804   82213       0
```

``` r
table(financiamento$recursos_direcao_estadual, useNA = "always")
```

```
## 
##       0       1    <NA> 
## 1900215  211802       0
```

``` r
table(financiamento$recursos_publicos, financiamento$recursos_direcao_nacional, useNA = "always")
```

```
##       
##              0       1    <NA>
##   0    1811208   10461       0
##   1     218596   71752       0
##   <NA>       0       0       0
```

``` r
table(financiamento$recursos_partidos, financiamento$recursos_direcao_nacional, useNA = "always")
```

```
##       
##              0       1    <NA>
##   0    1812762      37       0
##   1     217042   82176       0
##   <NA>       0       0       0
```

``` r
table(financiamento$recursos_publicos, financiamento$recursos_direcao_estadual, useNA = "always")
```

```
##       
##              0       1    <NA>
##   0    1748734   72935       0
##   1     151481  138867       0
##   <NA>       0       0       0
```

``` r
table(financiamento$recursos_partidos, financiamento$recursos_direcao_estadual, useNA = "always")
```

```
##       
##              0       1    <NA>
##   0    1812642     157       0
##   1      87573  211645       0
##   <NA>       0       0       0
```

``` r
# --- 7.3 Recursos públicos repassados por cada direção ------------------------
# Do total de recursos públicos (FEFC + Fundo Partidário), quanto chega pela
# direção nacional e quanto pela direção estadual. Usado no código 8
# (características da lista).

financiamento <- financiamento %>%
  mutate(
    recursos_publicos_nacional = recursos_publicos * recursos_direcao_nacional,
    recursos_publicos_estadual = recursos_publicos * recursos_direcao_estadual
  )

table(financiamento$ano, financiamento$recursos_publicos_nacional)
```

```
##       
##             0      1
##   2010 426561    456
##   2014 426216   1273
##   2018 312961  16531
##   2022 650670  23742
##   2026 223857  29750
```

``` r
table(financiamento$ano, financiamento$recursos_publicos_estadual)
```

```
##       
##             0      1
##   2010 426281    736
##   2014 424362   3127
##   2018 284628  44864
##   2022 613439  60973
##   2026 224440  29167
```

``` r
names(financiamento)
```

```
##  [1] "ano"                          "cargo"                       
##  [3] "sigla_uf"                     "tipo_eleicao"                
##  [5] "numero_partido"               "sigla_partido"               
##  [7] "numero_candidato"             "sequencial_candidato"        
##  [9] "fonte_receita"                "origem_receita"              
## [11] "especie_receita"              "valor_receita"               
## [13] "nome_doador"                  "cargo_candidato_doador"      
## [15] "numero_partido_doador"        "numero_candidato_doador"     
## [17] "descricao_receita"            "recursos_pessoa_fisica"      
## [19] "recursos_pessoa_juridica"     "recursos_proprios"           
## [21] "recursos_publicos"            "recursos_partidos"           
## [23] "recursos_outros_candidatos"   "recursos_partidos_candidatos"
## [25] "doador_norm"                  "recursos_direcao_nacional"   
## [27] "recursos_direcao_estadual"    "recursos_direcao_partidaria" 
## [29] "recursos_publicos_nacional"   "recursos_publicos_estadual"
```






### Unificação ao nível do candidato



``` r
names(financiamento)
```

```
##  [1] "ano"                          "cargo"                       
##  [3] "sigla_uf"                     "tipo_eleicao"                
##  [5] "numero_partido"               "sigla_partido"               
##  [7] "numero_candidato"             "sequencial_candidato"        
##  [9] "fonte_receita"                "origem_receita"              
## [11] "especie_receita"              "valor_receita"               
## [13] "nome_doador"                  "cargo_candidato_doador"      
## [15] "numero_partido_doador"        "numero_candidato_doador"     
## [17] "descricao_receita"            "recursos_pessoa_fisica"      
## [19] "recursos_pessoa_juridica"     "recursos_proprios"           
## [21] "recursos_publicos"            "recursos_partidos"           
## [23] "recursos_outros_candidatos"   "recursos_partidos_candidatos"
## [25] "doador_norm"                  "recursos_direcao_nacional"   
## [27] "recursos_direcao_estadual"    "recursos_direcao_partidaria" 
## [29] "recursos_publicos_nacional"   "recursos_publicos_estadual"
```

``` r
vars_recursos <- c(
  "recursos_publicos", "recursos_pessoa_fisica", "recursos_pessoa_juridica",
  "recursos_proprios",
  "recursos_partidos", "recursos_outros_candidatos", "recursos_partidos_candidatos",
  "recursos_direcao_nacional", "recursos_direcao_estadual", "recursos_direcao_partidaria",
  "recursos_publicos_nacional", "recursos_publicos_estadual"
)

# Chave do candidato SEM a sigla (ver topo). O número do partido também sai
# da chave: dentro de ano x cargo x UF o número do candidato já é único.
dados_cand_fin <- financiamento %>%
  mutate(valor_receita = as.numeric(valor_receita)) %>%
  group_by(ano, cargo, sigla_uf, numero_candidato) %>%
  summarise(
    sequencial_candidato = dplyr::first(na.omit(sequencial_candidato)),
    n_receitas    = n(),
    soma_receitas = sum(valor_receita, na.rm = TRUE),
    across(all_of(vars_recursos),
           ~ sum(valor_receita[.x == 1], na.rm = TRUE),
           .names = "soma_{.col}"),
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("soma_recursos_"),
           ~ if_else(soma_receitas > 0, 100 * .x / soma_receitas, NA_real_),
           .names = "{sub('soma_', 'pct_', .col)}")
  )

glimpse(dados_cand_fin)
```

```
## Rows: 110,041
## Columns: 31
## $ ano                               <int> 2010, 2010, 2010, 2010, 2010, 2010, …
## $ cargo                             <chr> "1º suplente senador", "1º suplente …
## $ sigla_uf                          <chr> "AL", "AL", "AL", "AM", "BA", "BA", …
## $ numero_candidato                  <chr> "111", "151", "654", "211", "400", "…
## $ sequencial_candidato              <chr> "20000000641", "20000000277", "20000…
## $ n_receitas                        <int> 14, 1, 1, 1, 2, 6, 1, 1, 3, 190, 4, …
## $ soma_receitas                     <dbl> 58510.60, 100.00, 300.00, 500.00, 50…
## $ soma_recursos_publicos            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ soma_recursos_pessoa_fisica       <dbl> 300, 100, 0, 0, 0, 5200, 0, 0, 320, …
## $ soma_recursos_pessoa_juridica     <dbl> 58210.60, 0.00, 0.00, 0.00, 50000.00…
## $ soma_recursos_proprios            <dbl> 0.00, 0.00, 0.00, 0.00, 70.00, 0.00,…
## $ soma_recursos_partidos            <dbl> 0, 0, 0, 0, 0, 0, 0, 200000, 0, 0, 0…
## $ soma_recursos_outros_candidatos   <dbl> 0.00, 0.00, 300.00, 500.00, 0.00, 0.…
## $ soma_recursos_partidos_candidatos <dbl> 0.00, 0.00, 300.00, 500.00, 0.00, 0.…
## $ soma_recursos_direcao_nacional    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ soma_recursos_direcao_estadual    <dbl> 0e+00, 0e+00, 0e+00, 0e+00, 0e+00, 0…
## $ soma_recursos_direcao_partidaria  <dbl> 0, 0, 0, 0, 0, 0, 0, 200000, 0, 0, 0…
## $ soma_recursos_publicos_nacional   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ soma_recursos_publicos_estadual   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ pct_recursos_publicos             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ pct_recursos_pessoa_fisica        <dbl> 0.5127276, 100.0000000, 0.0000000, 0…
## $ pct_recursos_pessoa_juridica      <dbl> 99.48727, 0.00000, 0.00000, 0.00000,…
## $ pct_recursos_proprios             <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0…
## $ pct_recursos_partidos             <dbl> 0.00000, 0.00000, 0.00000, 0.00000, …
## $ pct_recursos_outros_candidatos    <dbl> 0.000000, 0.000000, 100.000000, 100.…
## $ pct_recursos_partidos_candidatos  <dbl> 0.000000, 0.000000, 100.000000, 100.…
## $ pct_recursos_direcao_nacional     <dbl> 0.00000, 0.00000, 0.00000, 0.00000, …
## $ pct_recursos_direcao_estadual     <dbl> 0, 0, 0, 0, 0, 0, 0, 100, 0, 0, 0, 0…
## $ pct_recursos_direcao_partidaria   <dbl> 0.00000, 0.00000, 0.00000, 0.00000, …
## $ pct_recursos_publicos_nacional    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
## $ pct_recursos_publicos_estadual    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

``` r
dados_cand_fin %>%
  group_by(ano) %>%
  summarise(across(starts_with("pct_"), ~ round(mean(.x, na.rm = TRUE), 1)))
```

```
## # A tibble: 5 × 13
##     ano pct_recursos_publicos pct_recursos_pessoa_fisica pct_recursos_pessoa_j…¹
##   <int>                 <dbl>                      <dbl>                   <dbl>
## 1  2010                   2.3                       19.7                    12.7
## 2  2014                   4.7                       28.8                     8.5
## 3  2018                  56.7                       20.5                     0  
## 4  2022                  77.6                       10.7                     0  
## 5  2026                  74.2                       11.9                     0  
## # ℹ abbreviated name: ¹​pct_recursos_pessoa_juridica
## # ℹ 9 more variables: pct_recursos_proprios <dbl>, pct_recursos_partidos <dbl>,
## #   pct_recursos_outros_candidatos <dbl>,
## #   pct_recursos_partidos_candidatos <dbl>,
## #   pct_recursos_direcao_nacional <dbl>, pct_recursos_direcao_estadual <dbl>,
## #   pct_recursos_direcao_partidaria <dbl>,
## #   pct_recursos_publicos_nacional <dbl>, …
```

``` r
# Nenhum percentual deveria passar de 100
dados_cand_fin %>% filter(if_any(starts_with("pct_"), ~ .x > 100))
```

```
## # A tibble: 359 × 31
##      ano cargo         sigla_uf numero_candidato sequencial_candidato n_receitas
##    <int> <chr>         <chr>    <chr>            <chr>                     <int>
##  1  2010 deputado dis… DF       14514            70000001016                  11
##  2  2010 deputado est… BA       36010            50000000712                   6
##  3  2010 deputado est… ES       31831            80000000578                   3
##  4  2010 deputado est… MG       22226            130000000860                  9
##  5  2010 deputado est… PB       10000            150000000285                  2
##  6  2010 deputado est… PB       20013            150000000382                  2
##  7  2010 deputado est… PB       33151            150000000234                  3
##  8  2010 deputado est… PB       65222            150000000209                  3
##  9  2010 deputado est… RJ       22125            190000003951                  3
## 10  2010 deputado est… RJ       22213            190000004051                  2
## # ℹ 349 more rows
## # ℹ 25 more variables: soma_receitas <dbl>, soma_recursos_publicos <dbl>,
## #   soma_recursos_pessoa_fisica <dbl>, soma_recursos_pessoa_juridica <dbl>,
## #   soma_recursos_proprios <dbl>, soma_recursos_partidos <dbl>,
## #   soma_recursos_outros_candidatos <dbl>,
## #   soma_recursos_partidos_candidatos <dbl>,
## #   soma_recursos_direcao_nacional <dbl>, …
```





### Deflação


``` r
names(dados_cand_fin)
```

```
##  [1] "ano"                               "cargo"                            
##  [3] "sigla_uf"                          "numero_candidato"                 
##  [5] "sequencial_candidato"              "n_receitas"                       
##  [7] "soma_receitas"                     "soma_recursos_publicos"           
##  [9] "soma_recursos_pessoa_fisica"       "soma_recursos_pessoa_juridica"    
## [11] "soma_recursos_proprios"            "soma_recursos_partidos"           
## [13] "soma_recursos_outros_candidatos"   "soma_recursos_partidos_candidatos"
## [15] "soma_recursos_direcao_nacional"    "soma_recursos_direcao_estadual"   
## [17] "soma_recursos_direcao_partidaria"  "soma_recursos_publicos_nacional"  
## [19] "soma_recursos_publicos_estadual"   "pct_recursos_publicos"            
## [21] "pct_recursos_pessoa_fisica"        "pct_recursos_pessoa_juridica"     
## [23] "pct_recursos_proprios"             "pct_recursos_partidos"            
## [25] "pct_recursos_outros_candidatos"    "pct_recursos_partidos_candidatos" 
## [27] "pct_recursos_direcao_nacional"     "pct_recursos_direcao_estadual"    
## [29] "pct_recursos_direcao_partidaria"   "pct_recursos_publicos_nacional"   
## [31] "pct_recursos_publicos_estadual"
```

``` r
dados_cand_fin <- dados_cand_fin %>%
  # Deflaciona variáveis pelo IPCA de 12/2025 (2026: a partir do IPCA de 09/2026, último divulgado)
  mutate(across(c(soma_receitas, starts_with("soma_recursos_")),   # selecionar variáveis
                ~ ipca(.x, pmin(as.Date(paste0(ano, "-12-31")), as.Date("2026-09-01")), "12/2025")))
```

```
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
```

``` r
summary(dados_cand_fin)
```

```
##       ano          cargo             sigla_uf         numero_candidato  
##  Min.   :2010   Length:110041      Length:110041      Length:110041     
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2019                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##                                                                         
##  sequencial_candidato   n_receitas        soma_receitas      
##  Length:110041        Min.   :     1.00   Min.   :        0  
##  Class :character     1st Qu.:     3.00   1st Qu.:     5408  
##  Mode  :character     Median :     6.00   Median :    27810  
##                       Mean   :    19.19   Mean   :   316654  
##                       3rd Qu.:    14.00   3rd Qu.:   153715  
##                       Max.   :392138.00   Max.   :644121371  
##                                                              
##  soma_recursos_publicos soma_recursos_pessoa_fisica
##  Min.   :        0      Min.   :        0          
##  1st Qu.:        0      1st Qu.:        0          
##  Median :     2123      Median :      436          
##  Mean   :   138378      Mean   :    38609          
##  3rd Qu.:    48331      3rd Qu.:    11046          
##  Max.   :144242421      Max.   :101433371          
##                                                    
##  soma_recursos_pessoa_juridica soma_recursos_proprios soma_recursos_partidos
##  Min.   :        0             Min.   :       0       Min.   :        0     
##  1st Qu.:        0             1st Qu.:       0       1st Qu.:        0     
##  Median :        0             Median :       0       Median :     4361     
##  Mean   :    45282             Mean   :   23142       Mean   :   168645     
##  3rd Qu.:        0             3rd Qu.:    3701       3rd Qu.:    57531     
##  Max.   :540918267             Max.   :82900474       Max.   :144781638     
##                                                                             
##  soma_recursos_outros_candidatos soma_recursos_partidos_candidatos
##  Min.   :        0               Min.   :        0                
##  1st Qu.:        0               1st Qu.:     1485                
##  Median :        0               Median :    13437                
##  Mean   :    39722               Mean   :   208367                
##  3rd Qu.:     5275               3rd Qu.:    82070                
##  Max.   :349273504               Max.   :376242359                
##                                                                   
##  soma_recursos_direcao_nacional soma_recursos_direcao_estadual
##  Min.   :        0              Min.   :       0              
##  1st Qu.:        0              1st Qu.:       0              
##  Median :        0              Median :       0              
##  Mean   :   126183              Mean   :   41735              
##  3rd Qu.:    17259              3rd Qu.:    7351              
##  Max.   :143053597              Max.   :50290194              
##                                                               
##  soma_recursos_direcao_partidaria soma_recursos_publicos_nacional
##  Min.   :        0                Min.   :        0              
##  1st Qu.:        0                1st Qu.:        0              
##  Median :     4226                Median :        0              
##  Mean   :   167918                Mean   :   110867              
##  3rd Qu.:    57531                3rd Qu.:    13083              
##  Max.   :144533319                Max.   :143053597              
##                                                                  
##  soma_recursos_publicos_estadual pct_recursos_publicos
##  Min.   :       0                Min.   :  0.00       
##  1st Qu.:       0                1st Qu.:  0.00       
##  Median :       0                Median : 41.41       
##  Mean   :   21396                Mean   : 46.33       
##  3rd Qu.:    1999                3rd Qu.: 97.68       
##  Max.   :17139743                Max.   :100.00       
##                                  NA's   :6888         
##  pct_recursos_pessoa_fisica pct_recursos_pessoa_juridica pct_recursos_proprios
##  Min.   :  0.000            Min.   :  0.000              Min.   :  0.000      
##  1st Qu.:  0.000            1st Qu.:  0.000              1st Qu.:  0.000      
##  Median :  1.995            Median :  0.000              Median :  0.000      
##  Mean   : 18.090            Mean   :  3.607              Mean   : 11.934      
##  3rd Qu.: 25.485            3rd Qu.:  0.000              3rd Qu.:  9.783      
##  Max.   :100.000            Max.   :100.000              Max.   :100.000      
##  NA's   :6888               NA's   :6888                 NA's   :6888         
##  pct_recursos_partidos pct_recursos_outros_candidatos
##  Min.   :  0.00        Min.   :  0.00                
##  1st Qu.:  0.00        1st Qu.:  0.00                
##  Median : 46.64        Median :  0.00                
##  Mean   : 47.76        Mean   : 18.04                
##  3rd Qu.: 96.98        3rd Qu.: 22.35                
##  Max.   :100.00        Max.   :100.00                
##  NA's   :6888          NA's   :6888                  
##  pct_recursos_partidos_candidatos pct_recursos_direcao_nacional
##  Min.   :  0.00                   Min.   :  0.00               
##  1st Qu.: 28.57                   1st Qu.:  0.00               
##  Median : 86.25                   Median :  0.00               
##  Mean   : 65.80                   Mean   : 24.93               
##  3rd Qu.:100.00                   3rd Qu.: 56.34               
##  Max.   :100.00                   Max.   :100.00               
##  NA's   :6888                     NA's   :6888                 
##  pct_recursos_direcao_estadual pct_recursos_direcao_partidaria
##  Min.   :  0.00                Min.   :  0.00                 
##  1st Qu.:  0.00                1st Qu.:  0.00                 
##  Median :  0.00                Median : 45.54                 
##  Mean   : 22.48                Mean   : 47.41                 
##  3rd Qu.: 35.03                3rd Qu.: 96.75                 
##  Max.   :100.00                Max.   :100.00                 
##  NA's   :6888                  NA's   :6888                   
##  pct_recursos_publicos_nacional pct_recursos_publicos_estadual
##  Min.   :  0.00                 Min.   :  0.000               
##  1st Qu.:  0.00                 1st Qu.:  0.000               
##  Median :  0.00                 Median :  0.000               
##  Mean   : 23.93                 Mean   : 16.644               
##  3rd Qu.: 52.63                 3rd Qu.:  9.608               
##  Max.   :100.00                 Max.   :100.000               
##  NA's   :6888                   NA's   :6888
```

``` r
dados_cand_fin <- dados_cand_fin %>%
  select(-n_receitas)

names(dados_cand_fin)
```

```
##  [1] "ano"                               "cargo"                            
##  [3] "sigla_uf"                          "numero_candidato"                 
##  [5] "sequencial_candidato"              "soma_receitas"                    
##  [7] "soma_recursos_publicos"            "soma_recursos_pessoa_fisica"      
##  [9] "soma_recursos_pessoa_juridica"     "soma_recursos_proprios"           
## [11] "soma_recursos_partidos"            "soma_recursos_outros_candidatos"  
## [13] "soma_recursos_partidos_candidatos" "soma_recursos_direcao_nacional"   
## [15] "soma_recursos_direcao_estadual"    "soma_recursos_direcao_partidaria" 
## [17] "soma_recursos_publicos_nacional"   "soma_recursos_publicos_estadual"  
## [19] "pct_recursos_publicos"             "pct_recursos_pessoa_fisica"       
## [21] "pct_recursos_pessoa_juridica"      "pct_recursos_proprios"            
## [23] "pct_recursos_partidos"             "pct_recursos_outros_candidatos"   
## [25] "pct_recursos_partidos_candidatos"  "pct_recursos_direcao_nacional"    
## [27] "pct_recursos_direcao_estadual"     "pct_recursos_direcao_partidaria"  
## [29] "pct_recursos_publicos_nacional"    "pct_recursos_publicos_estadual"
```





## Dados Despesas


### Carregar dados



``` r
bd_rec <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.despesas_candidato") %>%
  dplyr::filter(ano %in% c(2010, 2014, 2018, 2022, 2026), tipo_eleicao %in% tipos_ordinarios) %>%
  dplyr::select(
    ano, cargo, sigla_uf, tipo_eleicao,
    numero_partido, sigla_partido, numero_candidato, sequencial_candidato,
    valor_despesa, tipo_despesa, origem_despesa, 
    descricao_despesa
  )
```

```
## Successfully connected to table
## `basedosdados.br_tse_eleicoes.despesas_candidato`.
```

``` r
despesas <- basedosdados::bd_collect(bd_rec)
rm(bd_rec); gc()
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells   5684574  303.6    8982178  479.8   8982178  479.8
## Vcells 357151076 2724.9  581085530 4433.4 366351218 2795.1
```

``` r
names(despesas)
```

```
##  [1] "ano"                  "cargo"                "sigla_uf"            
##  [4] "tipo_eleicao"         "numero_partido"       "sigla_partido"       
##  [7] "numero_candidato"     "sequencial_candidato" "valor_despesa"       
## [10] "tipo_despesa"         "origem_despesa"       "descricao_despesa"
```

``` r
summary(despesas)
```

```
##       ano          cargo             sigla_uf         tipo_eleicao      
##  Min.   :2010   Length:9014974     Length:9014974     Length:9014974    
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2017                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##  numero_partido     sigla_partido      numero_candidato   sequencial_candidato
##  Length:9014974     Length:9014974     Length:9014974     Length:9014974      
##  Class :character   Class :character   Class :character   Class :character    
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character    
##                                                                               
##                                                                               
##                                                                               
##  valor_despesa      tipo_despesa       origem_despesa     descricao_despesa 
##  Min.   :       0   Length:9014974     Length:9014974     Length:9014974    
##  1st Qu.:     210   Class :character   Class :character   Class :character  
##  Median :     561   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :    2416                                                           
##  3rd Qu.:    1500                                                           
##  Max.   :31000000
```

``` r
table(despesas$origem_despesa, useNA = "always")
```

```
## 
##                                                           #nulo# 
##                                                            12533 
##                                                             agua 
##                                                             8298 
##                                                      alimentacao 
##                                                            94483 
##                       aquisicao/doacao de bens moveis ou imoveis 
##                                                             1336 
##                    atividades de militancia e mobilizacao de rua 
##                                                          1326272 
##                                    cessao ou locacao de veiculos 
##                                                           102773 
##                                     combustiveis e lubrificantes 
##                                                           283328 
##                                                         comicios 
##                                                             2708 
##                              correspondencias e despesas postais 
##                                                            35205 
##                        criacao e inclusao de paginas na internet 
##                                                             9874 
##                                 despesa com geradores de energia 
##                                                              230 
##                         despesa com impulsionamento de conteudos 
##                                                            90942 
##                                          despesas com hospedagem 
##                                                            11221 
##                                             despesas com pessoal 
##                                                           993178 
##                          despesas com transporte ou deslocamento 
##                                                            28577 
##                                           diversas a especificar 
##                                                            62508 
##                 doacoes financeiras a outros candidatos/partidos 
##                                                            15688 
##   encargos financeiros taxas bancarias e/ou op cartao de credito 
##                                                           411771 
## encargos financeiros, taxas bancarias e/ou op. cartao de credito 
##                                                            74571 
##                                                 encargos sociais 
##                                                              850 
##                                                 energia eletrica 
##                                                             2463 
##                               eventos de promocao da candidatura 
##                                                            11251 
##                                   impostos contribuicoes e taxas 
##                                                             3641 
##                                  impostos, contribuicoes e taxas 
##                                                              728 
##                                   locacao/cessao de bens imoveis 
##                                                            28896 
##                  locacao/cessao de bens moveis (exceto veiculos) 
##                                                            15068 
##                                          materiais de expediente 
##                                                            80947 
##                                                multas eleitorais 
##                                                               62 
##                                                   passagem aerea 
##                                                             2284 
##                                   pesquisas ou testes eleitorais 
##                                                             1824 
##                      pre-instalacao fisica de comite de campanha 
##                                                             5717 
##                           producao de jingles vinhetas e slogans 
##                                                            14389 
##                          producao de jingles, vinhetas e slogans 
##                                                             3128 
##                producao de programas de radio televisao ou video 
##                                                            18784 
##               producao de programas de radio, televisao ou video 
##                                                             4152 
##                                         publicidade por adesivos 
##                                                           214936 
##                                    publicidade por carros de som 
##                                                            10113 
##                               publicidade por jornais e revistas 
##                                                            27386 
##                              publicidade por materiais impressos 
##                                                           585305 
##                    reembolsos de gastos realizados por eleitores 
##                                                              658 
##  seguranca e prevencao, repressao e combate a violencia politica 
##                                                              131 
##                                            servicos advocaticios 
##                                                            18891 
##                                               servicos contabeis 
##                                                            19077 
##                                 servicos prestados por terceiros 
##                                                           277643 
##                        servicos proprios prestados por terceiros 
##                                                            19520 
##                  taxa de administracao de financiamento coletivo 
##                                                            12267 
##                                                         telefone 
##                                                             1858 
##                                                             <NA> 
##                                                          4067509
```

``` r
table(despesas$tipo_despesa, useNA = "always")
```

```
## 
##                                                                                    agua 
##                                                                                    7591 
##                                                                             alimentacao 
##                                                                                   50737 
##                                              aquisicao/doacao de bens moveis ou imoveis 
##                                                                                    1585 
##                                           atividades de militancia e mobilizacao de rua 
##                                                                                  137908 
##                                                              baixa de estimaveis - agua 
##                                                                                     413 
##                                                       baixa de estimaveis - alimentacao 
##                                                                                     705 
##                     baixa de estimaveis - atividades de militancia e mobilizacao de rua 
##                                                                                    7593 
##                                     baixa de estimaveis - cessao ou locacao de veiculos 
##                                                                                   33800 
##                                      baixa de estimaveis - combustiveis e lubrificantes 
##                                                                                    5546 
##                                                          baixa de estimaveis - comicios 
##                                                                                      86 
##                               baixa de estimaveis - correspondencias e despesas postais 
##                                                                                     227 
##                         baixa de estimaveis - criacao e inclusao de paginas na internet 
##                                                                                     401 
##                                              baixa de estimaveis - despesas com pessoal 
##                                                                                   16707 
##                           baixa de estimaveis - despesas com transporte ou deslocamento 
##                                                                                    1604 
##                                            baixa de estimaveis - diversas a especificar 
##                                                                                    4965 
##    baixa de estimaveis - encargos financeiros taxas bancarias e/ou op cartao de credito 
##                                                                                      74 
##                                                  baixa de estimaveis - encargos sociais 
##                                                                                      16 
##                                                  baixa de estimaveis - energia eletrica 
##                                                                                     250 
##                                baixa de estimaveis - eventos de promocao da candidatura 
##                                                                                     646 
##                                    baixa de estimaveis - impostos contribuicoes e taxas 
##                                                                                     107 
##                                    baixa de estimaveis - locacao/cessao de bens imoveis 
##                                                                                    7603 
##                                     baixa de estimaveis - locacao/cessao de bens moveis 
##                                                                                    7083 
##                                           baixa de estimaveis - materiais de expediente 
##                                                                                    1007 
##                                                 baixa de estimaveis - multas eleitorais 
##                                                                                       2 
##                                    baixa de estimaveis - pesquisas ou testes eleitorais 
##                                                                                     134 
## baixa de estimaveis - preparacao de campanha e instalacao fisica de comites de campanha 
##                                                                                     438 
##                            baixa de estimaveis - producao de jingles vinhetas e slogans 
##                                                                                    1389 
##                 baixa de estimaveis - producao de programas de radio televisao ou video 
##                                                                                   14443 
##                                     baixa de estimaveis - publicidade por carros de som 
##                                                                                    1911 
##                                baixa de estimaveis - publicidade por jornais e revistas 
##                                                                                    2656 
##                               baixa de estimaveis - publicidade por materiais impressos 
##                                                                                   80305 
##                       baixa de estimaveis - publicidade por placas estandartes e faixas 
##                                                                                   25685 
##                                     baixa de estimaveis - publicidade por telemarketing 
##                                                                                      85 
##                             baixa de estimaveis - recursos de outros candidatos/comites 
##                                                                                    3193 
##                                      baixa de estimaveis - recursos de partido politico 
##                                                                                      52 
##                                       baixa de estimaveis - recursos de pessoas fisicas 
##                                                                                       6 
##                                     baixa de estimaveis - recursos de pessoas juridicas 
##                                                                                      18 
##                                                 baixa de estimaveis - recursos proprios 
##                                                                                       1 
##                                  baixa de estimaveis - servicos prestados por terceiros 
##                                                                                   32568 
##                                                          baixa de estimaveis - telefone 
##                                                                                     870 
##                                                baixa de recursos estimaveis em dinheiro 
##                                                                                  253634 
##                                                                        bens permanentes 
##                                                                                     794 
##                                                           cessao ou locacao de veiculos 
##                                                                                   98363 
##                                                            combustiveis e lubrificantes 
##                                                                                  177508 
##                                                                                comicios 
##                                                                                    2061 
##                                                     correspondencias e despesas postais 
##                                                                                   15136 
##                                               criacao e inclusao de paginas na internet 
##                                                                                    6241 
##                                                                    despesas com pessoal 
##                                                                                 1843045 
##                                                 despesas com transporte ou deslocamento 
##                                                                                   48972 
##                                                                        despesas postais 
##                                                                                    9525 
##                                                                  diversas a especificar 
##                                                                                   60647 
##                        doacoes financeiras a outros candidatos e/ou comites financeiros 
##                                                                                    7090 
##                    doacoes financeiras a outros candidatos/comites financeiros/partidos 
##                                                                                    6958 
##                                                  encargos financeiros e taxas bancarias 
##                                                                                   56742 
##                          encargos financeiros taxas bancarias e/ou op cartao de credito 
##                                                                                   89136 
##                                                                        encargos sociais 
##                                                                                    3933 
##                                                                        energia eletrica 
##                                                                                    7944 
##                                                      eventos de promocao da candidatura 
##                                                                                    6983 
##                                                          impostos contribuicoes e taxas 
##                                                                                    6167 
##                                                          locacao/cessao de bens imoveis 
##                                                                                   23579 
##                                                           locacao/cessao de bens moveis 
##                                                                                   17772 
##                                                                 materiais de expediente 
##                                                                                   48292 
##                                                                       multas eleitorais 
##                                                                                     229 
##                                                          pesquisas ou testes eleitorais 
##                                                                                    1292 
##                                pre-instalacao fisica de comite de campanha de candidato 
##                                                                                    1747 
##                       preparacao de campanha e instalacao fisica de comites de campanha 
##                                                                                    5403 
##                                                  producao de jingles vinhetas e slogans 
##                                                                                    8793 
##                                       producao de programas de radio televisao ou video 
##                                                                                    7943 
##                                                           publicidade por carros de som 
##                                                                                   58149 
##                                                      publicidade por jornais e revistas 
##                                                                                   47156 
##                                                     publicidade por materiais impressos 
##                                                                                  282888 
##                                             publicidade por placas estandartes e faixas 
##                                                                                  102019 
##                                                           publicidade por telemarketing 
##                                                                                    3386 
##                                           reembolsos de gastos realizados por eleitores 
##                                                                                    1405 
##                                                        servicos prestados por terceiros 
##                                                                                  284583 
##                                                                                telefone 
##                                                                                   17207 
##                                                                                    <NA> 
##                                                                                 4951842
```





### Categorização 

Por tipo 


``` r
table(despesas$origem_despesa)
```

```
## 
##                                                           #nulo# 
##                                                            12533 
##                                                             agua 
##                                                             8298 
##                                                      alimentacao 
##                                                            94483 
##                       aquisicao/doacao de bens moveis ou imoveis 
##                                                             1336 
##                    atividades de militancia e mobilizacao de rua 
##                                                          1326272 
##                                    cessao ou locacao de veiculos 
##                                                           102773 
##                                     combustiveis e lubrificantes 
##                                                           283328 
##                                                         comicios 
##                                                             2708 
##                              correspondencias e despesas postais 
##                                                            35205 
##                        criacao e inclusao de paginas na internet 
##                                                             9874 
##                                 despesa com geradores de energia 
##                                                              230 
##                         despesa com impulsionamento de conteudos 
##                                                            90942 
##                                          despesas com hospedagem 
##                                                            11221 
##                                             despesas com pessoal 
##                                                           993178 
##                          despesas com transporte ou deslocamento 
##                                                            28577 
##                                           diversas a especificar 
##                                                            62508 
##                 doacoes financeiras a outros candidatos/partidos 
##                                                            15688 
##   encargos financeiros taxas bancarias e/ou op cartao de credito 
##                                                           411771 
## encargos financeiros, taxas bancarias e/ou op. cartao de credito 
##                                                            74571 
##                                                 encargos sociais 
##                                                              850 
##                                                 energia eletrica 
##                                                             2463 
##                               eventos de promocao da candidatura 
##                                                            11251 
##                                   impostos contribuicoes e taxas 
##                                                             3641 
##                                  impostos, contribuicoes e taxas 
##                                                              728 
##                                   locacao/cessao de bens imoveis 
##                                                            28896 
##                  locacao/cessao de bens moveis (exceto veiculos) 
##                                                            15068 
##                                          materiais de expediente 
##                                                            80947 
##                                                multas eleitorais 
##                                                               62 
##                                                   passagem aerea 
##                                                             2284 
##                                   pesquisas ou testes eleitorais 
##                                                             1824 
##                      pre-instalacao fisica de comite de campanha 
##                                                             5717 
##                           producao de jingles vinhetas e slogans 
##                                                            14389 
##                          producao de jingles, vinhetas e slogans 
##                                                             3128 
##                producao de programas de radio televisao ou video 
##                                                            18784 
##               producao de programas de radio, televisao ou video 
##                                                             4152 
##                                         publicidade por adesivos 
##                                                           214936 
##                                    publicidade por carros de som 
##                                                            10113 
##                               publicidade por jornais e revistas 
##                                                            27386 
##                              publicidade por materiais impressos 
##                                                           585305 
##                    reembolsos de gastos realizados por eleitores 
##                                                              658 
##  seguranca e prevencao, repressao e combate a violencia politica 
##                                                              131 
##                                            servicos advocaticios 
##                                                            18891 
##                                               servicos contabeis 
##                                                            19077 
##                                 servicos prestados por terceiros 
##                                                           277643 
##                        servicos proprios prestados por terceiros 
##                                                            19520 
##                  taxa de administracao de financiamento coletivo 
##                                                            12267 
##                                                         telefone 
##                                                             1858
```

``` r
despesas <- despesas %>%
  mutate(tipo_gasto = case_when(
    origem_despesa %in% c(
      "atividades de militancia e mobilizacao de rua", 
      "cessao ou locacao de veiculos", 
      "combustiveis e lubrificantes", 
      "comicios", 
      "despesas com pessoal", 
      "despesas com transporte ou deslocamento", 
      "eventos de promocao da candidatura", 
      "publicidade por adesivos", 
      "publicidade por carros de som", 
      "publicidade por materiais impressos"
    ) ~ "Tradicional",
    
    origem_despesa %in% c(
      "criacao e inclusao de paginas na internet", 
      "pesquisas ou testes eleitorais", 
      "producao de jingles vinhetas e slogans", 
      "producao de programas de radio televisao ou video", 
      "publicidade por jornais e revistas", 
      "servicos prestados por terceiros", 
      "servicos proprios prestados por terceiros",
      "despesa com impulsionamento de conteudos" # Adicionado aqui
    ) ~ "Moderno",
    
    origem_despesa %in% c(
      "agua", 
      "alimentacao", 
      "aquisicao/doacao de bens moveis ou imoveis", 
      "correspondencias e despesas postais", 
      "energia eletrica", 
      "despesas com hospedagem", 
      "locacao/cessao de bens imoveis", 
      "locacao/cessao de bens moveis (exceto veiculos)", 
      "materiais de expediente", 
      "pre-instalacao fisica de comite de campanha", 
      "telefone"
    ) ~ "Organização",
    
    origem_despesa %in% c(
      "diversas a especificar", 
      "doacoes financeiras a outros candidatos/partidos", 
      "encargos financeiros taxas bancarias e/ou op cartao de credito", 
      "encargos sociais", 
      "impostos contribuicoes e taxas", 
      "multas eleitorais", 
      "reembolsos de gastos realizados por eleitores"
    ) ~ "Outras",
    
    TRUE ~ "Outras"
  ))


despesas <- despesas %>%
  mutate(gastos_internet = case_when(
    origem_despesa %in% c("criacao e inclusao de paginas na internet", "despesa com impulsionamento de conteudos") ~ 1,
    TRUE ~ 0
  ))
```




### Unificação ao nível do candidato



``` r
names(despesas)
```

```
##  [1] "ano"                  "cargo"                "sigla_uf"            
##  [4] "tipo_eleicao"         "numero_partido"       "sigla_partido"       
##  [7] "numero_candidato"     "sequencial_candidato" "valor_despesa"       
## [10] "tipo_despesa"         "origem_despesa"       "descricao_despesa"   
## [13] "tipo_gasto"           "gastos_internet"
```

``` r
despesas_candidato <- despesas %>%
  group_by(ano, cargo, sigla_uf, numero_candidato,
           numero_partido) %>%
  summarise(sequencial_candidato_desp = dplyr::first(na.omit(sequencial_candidato)),
            despesas_total_can = sum(valor_despesa, na.rm = TRUE),
            despesas_internet_can = sum(valor_despesa[gastos_internet == 1], na.rm = TRUE),
            despesas_tradicional_can = sum(valor_despesa[tipo_gasto == "Tradicional"], na.rm = TRUE),
            despesas_moderno_can = sum(valor_despesa[tipo_gasto == "Moderno"], na.rm = TRUE),
            despesas_organizacao_can = sum(valor_despesa[tipo_gasto == "Organização"], na.rm = TRUE),
            despesas_outras_can = sum(valor_despesa[tipo_gasto == "Outras"], na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(pct_despesas_internet = (despesas_internet_can/despesas_total_can)*100,
         pct_despesas_tradicional = (despesas_tradicional_can/despesas_total_can)*100,
         pct_despesas_moderno = (despesas_moderno_can/despesas_total_can)*100,
         pct_despesas_organizacao = (despesas_organizacao_can/despesas_total_can)*100)
```

```
## `summarise()` has regrouped the output.
## ℹ Summaries were computed grouped by ano, cargo, sigla_uf, numero_candidato,
##   and numero_partido.
## ℹ Output is grouped by ano, cargo, sigla_uf, and numero_candidato.
## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
## ℹ Use `summarise(.by = c(ano, cargo, sigla_uf, numero_candidato,
##   numero_partido))` for per-operation grouping (`?dplyr::dplyr_by`) instead.
```

``` r
summary(despesas_candidato)
```

```
##       ano          cargo             sigla_uf         numero_candidato  
##  Min.   :2010   Length:107876      Length:107876      Length:107876     
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2019                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##                                                                         
##  numero_partido     sequencial_candidato_desp despesas_total_can 
##  Length:107876      Length:107876             Min.   :        0  
##  Class :character   Class :character          1st Qu.:     1562  
##  Mode  :character   Mode  :character          Median :    14279  
##                                               Mean   :   201923  
##                                               3rd Qu.:    95943  
##                                               Max.   :350232164  
##                                                                  
##  despesas_internet_can despesas_tradicional_can despesas_moderno_can
##  Min.   :       0      Min.   :       0         Min.   :       0    
##  1st Qu.:       0      1st Qu.:       0         1st Qu.:       0    
##  Median :       0      Median :       0         Median :       0    
##  Mean   :    8668      Mean   :   74908         Mean   :   36438    
##  3rd Qu.:       0      3rd Qu.:   24995         3rd Qu.:    6040    
##  Max.   :33011339      Max.   :43206607         Max.   :73346766    
##                                                                     
##  despesas_organizacao_can despesas_outras_can pct_despesas_internet
##  Min.   :      0          Min.   :        0   Min.   :  0.0000     
##  1st Qu.:      0          1st Qu.:        0   1st Qu.:  0.0000     
##  Median :      0          Median :     1212   Median :  0.0000     
##  Mean   :   4096          Mean   :    86482   Mean   :  3.7490     
##  3rd Qu.:      0          3rd Qu.:    15331   3rd Qu.:  0.5589     
##  Max.   :2296299          Max.   :350232164   Max.   :100.0000     
##                                               NA's   :16663        
##  pct_despesas_tradicional pct_despesas_moderno pct_despesas_organizacao
##  Min.   :  0.00           Min.   :  0.00       Min.   :  0.000         
##  1st Qu.:  0.00           1st Qu.:  0.00       1st Qu.:  0.000         
##  Median : 33.03           Median :  0.00       Median :  0.000         
##  Mean   : 39.31           Mean   : 15.26       Mean   :  1.716         
##  3rd Qu.: 77.95           3rd Qu.: 21.10       3rd Qu.:  0.068         
##  Max.   :100.00           Max.   :100.00       Max.   :100.000         
##  NA's   :16663            NA's   :16663        NA's   :16663
```





### Deflação


``` r
names(despesas_candidato)
```

```
##  [1] "ano"                       "cargo"                    
##  [3] "sigla_uf"                  "numero_candidato"         
##  [5] "numero_partido"            "sequencial_candidato_desp"
##  [7] "despesas_total_can"        "despesas_internet_can"    
##  [9] "despesas_tradicional_can"  "despesas_moderno_can"     
## [11] "despesas_organizacao_can"  "despesas_outras_can"      
## [13] "pct_despesas_internet"     "pct_despesas_tradicional" 
## [15] "pct_despesas_moderno"      "pct_despesas_organizacao"
```

``` r
despesas_candidato <- despesas_candidato %>%
  # Deflaciona variáveis pelo IPCA de 12/2025 (2026: a partir do IPCA de 09/2026, último divulgado)
  mutate(across(c(despesas_total_can, starts_with("despesas_")),   # selecionar variáveis
                ~ ipca(.x, pmin(as.Date(paste0(ano, "-12-31")), as.Date("2026-09-01")), "12/2025")))
```

```
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
## 
## 
## Downloading necessary data from IPEA's API
## ...
```

``` r
summary(despesas_candidato)
```

```
##       ano          cargo             sigla_uf         numero_candidato  
##  Min.   :2010   Length:107876      Length:107876      Length:107876     
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2019                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##                                                                         
##  numero_partido     sequencial_candidato_desp despesas_total_can 
##  Length:107876      Length:107876             Min.   :        0  
##  Class :character   Class :character          1st Qu.:     2600  
##  Mode  :character   Mode  :character          Median :    20682  
##                                               Mean   :   297026  
##                                               3rd Qu.:   126542  
##                                               Max.   :643641279  
##                                                                  
##  despesas_internet_can despesas_tradicional_can despesas_moderno_can
##  Min.   :       0      Min.   :       0         Min.   :       0    
##  1st Qu.:       0      1st Qu.:       0         1st Qu.:       0    
##  Median :       0      Median :       0         Median :       0    
##  Mean   :    9545      Mean   :   87182         Mean   :   42725    
##  3rd Qu.:       0      3rd Qu.:   29174         3rd Qu.:    7391    
##  Max.   :37983357      Max.   :49714189         Max.   :84393924    
##                                                                     
##  despesas_organizacao_can despesas_outras_can pct_despesas_internet
##  Min.   :      0          Min.   :        0   Min.   :  0.0000     
##  1st Qu.:      0          1st Qu.:        0   1st Qu.:  0.0000     
##  Median :      0          Median :     1940   Median :  0.0000     
##  Mean   :   4790          Mean   :   162328   Mean   :  3.7490     
##  3rd Qu.:      0          3rd Qu.:    23088   3rd Qu.:  0.5589     
##  Max.   :3179556          Max.   :643641279   Max.   :100.0000     
##                                               NA's   :16663        
##  pct_despesas_tradicional pct_despesas_moderno pct_despesas_organizacao
##  Min.   :  0.00           Min.   :  0.00       Min.   :  0.000         
##  1st Qu.:  0.00           1st Qu.:  0.00       1st Qu.:  0.000         
##  Median : 33.03           Median :  0.00       Median :  0.000         
##  Mean   : 39.31           Mean   : 15.26       Mean   :  1.716         
##  3rd Qu.: 77.95           3rd Qu.: 21.10       3rd Qu.:  0.068         
##  Max.   :100.00           Max.   :100.00       Max.   :100.000         
##  NA's   :16663            NA's   :16663        NA's   :16663
```

``` r
names(despesas_candidato)
```

```
##  [1] "ano"                       "cargo"                    
##  [3] "sigla_uf"                  "numero_candidato"         
##  [5] "numero_partido"            "sequencial_candidato_desp"
##  [7] "despesas_total_can"        "despesas_internet_can"    
##  [9] "despesas_tradicional_can"  "despesas_moderno_can"     
## [11] "despesas_organizacao_can"  "despesas_outras_can"      
## [13] "pct_despesas_internet"     "pct_despesas_tradicional" 
## [15] "pct_despesas_moderno"      "pct_despesas_organizacao"
```





## Juntar receitas e despesas



``` r
names(despesas_candidato)
```

```
##  [1] "ano"                       "cargo"                    
##  [3] "sigla_uf"                  "numero_candidato"         
##  [5] "numero_partido"            "sequencial_candidato_desp"
##  [7] "despesas_total_can"        "despesas_internet_can"    
##  [9] "despesas_tradicional_can"  "despesas_moderno_can"     
## [11] "despesas_organizacao_can"  "despesas_outras_can"      
## [13] "pct_despesas_internet"     "pct_despesas_tradicional" 
## [15] "pct_despesas_moderno"      "pct_despesas_organizacao"
```

``` r
names(dados_cand_fin)
```

```
##  [1] "ano"                               "cargo"                            
##  [3] "sigla_uf"                          "numero_candidato"                 
##  [5] "sequencial_candidato"              "soma_receitas"                    
##  [7] "soma_recursos_publicos"            "soma_recursos_pessoa_fisica"      
##  [9] "soma_recursos_pessoa_juridica"     "soma_recursos_proprios"           
## [11] "soma_recursos_partidos"            "soma_recursos_outros_candidatos"  
## [13] "soma_recursos_partidos_candidatos" "soma_recursos_direcao_nacional"   
## [15] "soma_recursos_direcao_estadual"    "soma_recursos_direcao_partidaria" 
## [17] "soma_recursos_publicos_nacional"   "soma_recursos_publicos_estadual"  
## [19] "pct_recursos_publicos"             "pct_recursos_pessoa_fisica"       
## [21] "pct_recursos_pessoa_juridica"      "pct_recursos_proprios"            
## [23] "pct_recursos_partidos"             "pct_recursos_outros_candidatos"   
## [25] "pct_recursos_partidos_candidatos"  "pct_recursos_direcao_nacional"    
## [27] "pct_recursos_direcao_estadual"     "pct_recursos_direcao_partidaria"  
## [29] "pct_recursos_publicos_nacional"    "pct_recursos_publicos_estadual"
```

``` r
financiamento_gerais <- left_join(dados_cand_fin, despesas_candidato,
                                  by = c("ano", "cargo",
                                         "sigla_uf", "numero_candidato")) %>%
  # Sequencial do candidato: das receitas; se ausente, das despesas
  mutate(sequencial_candidato = coalesce(sequencial_candidato, sequencial_candidato_desp)) %>%
  select(-sequencial_candidato_desp)

summary(financiamento_gerais)
```

```
##       ano          cargo             sigla_uf         numero_candidato  
##  Min.   :2010   Length:110041      Length:110041      Length:110041     
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2019                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##                                                                         
##  sequencial_candidato soma_receitas       soma_recursos_publicos
##  Length:110041        Min.   :        0   Min.   :        0     
##  Class :character     1st Qu.:     5408   1st Qu.:        0     
##  Mode  :character     Median :    27810   Median :     2123     
##                       Mean   :   316654   Mean   :   138378     
##                       3rd Qu.:   153715   3rd Qu.:    48331     
##                       Max.   :644121371   Max.   :144242421     
##                                                                 
##  soma_recursos_pessoa_fisica soma_recursos_pessoa_juridica
##  Min.   :        0           Min.   :        0            
##  1st Qu.:        0           1st Qu.:        0            
##  Median :      436           Median :        0            
##  Mean   :    38609           Mean   :    45282            
##  3rd Qu.:    11046           3rd Qu.:        0            
##  Max.   :101433371           Max.   :540918267            
##                                                           
##  soma_recursos_proprios soma_recursos_partidos soma_recursos_outros_candidatos
##  Min.   :       0       Min.   :        0      Min.   :        0              
##  1st Qu.:       0       1st Qu.:        0      1st Qu.:        0              
##  Median :       0       Median :     4361      Median :        0              
##  Mean   :   23142       Mean   :   168645      Mean   :    39722              
##  3rd Qu.:    3701       3rd Qu.:    57531      3rd Qu.:     5275              
##  Max.   :82900474       Max.   :144781638      Max.   :349273504              
##                                                                               
##  soma_recursos_partidos_candidatos soma_recursos_direcao_nacional
##  Min.   :        0                 Min.   :        0             
##  1st Qu.:     1485                 1st Qu.:        0             
##  Median :    13437                 Median :        0             
##  Mean   :   208367                 Mean   :   126183             
##  3rd Qu.:    82070                 3rd Qu.:    17259             
##  Max.   :376242359                 Max.   :143053597             
##                                                                  
##  soma_recursos_direcao_estadual soma_recursos_direcao_partidaria
##  Min.   :       0               Min.   :        0               
##  1st Qu.:       0               1st Qu.:        0               
##  Median :       0               Median :     4226               
##  Mean   :   41735               Mean   :   167918               
##  3rd Qu.:    7351               3rd Qu.:    57531               
##  Max.   :50290194               Max.   :144533319               
##                                                                 
##  soma_recursos_publicos_nacional soma_recursos_publicos_estadual
##  Min.   :        0               Min.   :       0               
##  1st Qu.:        0               1st Qu.:       0               
##  Median :        0               Median :       0               
##  Mean   :   110867               Mean   :   21396               
##  3rd Qu.:    13083               3rd Qu.:    1999               
##  Max.   :143053597               Max.   :17139743               
##                                                                 
##  pct_recursos_publicos pct_recursos_pessoa_fisica pct_recursos_pessoa_juridica
##  Min.   :  0.00        Min.   :  0.000            Min.   :  0.000             
##  1st Qu.:  0.00        1st Qu.:  0.000            1st Qu.:  0.000             
##  Median : 41.41        Median :  1.995            Median :  0.000             
##  Mean   : 46.33        Mean   : 18.090            Mean   :  3.607             
##  3rd Qu.: 97.68        3rd Qu.: 25.485            3rd Qu.:  0.000             
##  Max.   :100.00        Max.   :100.000            Max.   :100.000             
##  NA's   :6888          NA's   :6888               NA's   :6888                
##  pct_recursos_proprios pct_recursos_partidos pct_recursos_outros_candidatos
##  Min.   :  0.000       Min.   :  0.00        Min.   :  0.00                
##  1st Qu.:  0.000       1st Qu.:  0.00        1st Qu.:  0.00                
##  Median :  0.000       Median : 46.64        Median :  0.00                
##  Mean   : 11.934       Mean   : 47.76        Mean   : 18.04                
##  3rd Qu.:  9.783       3rd Qu.: 96.98        3rd Qu.: 22.35                
##  Max.   :100.000       Max.   :100.00        Max.   :100.00                
##  NA's   :6888          NA's   :6888          NA's   :6888                  
##  pct_recursos_partidos_candidatos pct_recursos_direcao_nacional
##  Min.   :  0.00                   Min.   :  0.00               
##  1st Qu.: 28.57                   1st Qu.:  0.00               
##  Median : 86.25                   Median :  0.00               
##  Mean   : 65.80                   Mean   : 24.93               
##  3rd Qu.:100.00                   3rd Qu.: 56.34               
##  Max.   :100.00                   Max.   :100.00               
##  NA's   :6888                     NA's   :6888                 
##  pct_recursos_direcao_estadual pct_recursos_direcao_partidaria
##  Min.   :  0.00                Min.   :  0.00                 
##  1st Qu.:  0.00                1st Qu.:  0.00                 
##  Median :  0.00                Median : 45.54                 
##  Mean   : 22.48                Mean   : 47.41                 
##  3rd Qu.: 35.03                3rd Qu.: 96.75                 
##  Max.   :100.00                Max.   :100.00                 
##  NA's   :6888                  NA's   :6888                   
##  pct_recursos_publicos_nacional pct_recursos_publicos_estadual
##  Min.   :  0.00                 Min.   :  0.000               
##  1st Qu.:  0.00                 1st Qu.:  0.000               
##  Median :  0.00                 Median :  0.000               
##  Mean   : 23.93                 Mean   : 16.644               
##  3rd Qu.: 52.63                 3rd Qu.:  9.608               
##  Max.   :100.00                 Max.   :100.000               
##  NA's   :6888                   NA's   :6888                  
##  numero_partido     despesas_total_can  despesas_internet_can
##  Length:110041      Min.   :        0   Min.   :       0     
##  Class :character   1st Qu.:     2619   1st Qu.:       0     
##  Mode  :character   Median :    20777   Median :       0     
##                     Mean   :   297456   Mean   :    9557     
##                     3rd Qu.:   126691   3rd Qu.:       0     
##                     Max.   :643641279   Max.   :37983357     
##                     NA's   :2330        NA's   :2330         
##  despesas_tradicional_can despesas_moderno_can despesas_organizacao_can
##  Min.   :       0         Min.   :       0     Min.   :      0         
##  1st Qu.:       0         1st Qu.:       0     1st Qu.:      0         
##  Median :       0         Median :       0     Median :      0         
##  Mean   :   87312         Mean   :   42784     Mean   :   4797         
##  3rd Qu.:   29306         3rd Qu.:    7462     3rd Qu.:      0         
##  Max.   :49714189         Max.   :84393924     Max.   :3179556         
##  NA's   :2330             NA's   :2330         NA's   :2330            
##  despesas_outras_can pct_despesas_internet pct_despesas_tradicional
##  Min.   :        0   Min.   :  0.0000      Min.   :  0.00          
##  1st Qu.:        0   1st Qu.:  0.0000      1st Qu.:  0.00          
##  Median :     1940   Median :  0.0000      Median : 33.25          
##  Mean   :   162562   Mean   :  3.7546      Mean   : 39.38          
##  3rd Qu.:    23124   3rd Qu.:  0.5684      3rd Qu.: 78.00          
##  Max.   :643641279   Max.   :100.0000      Max.   :100.00          
##  NA's   :2330        NA's   :18971         NA's   :18971           
##  pct_despesas_moderno pct_despesas_organizacao
##  Min.   :  0.00       Min.   :  0.0000        
##  1st Qu.:  0.00       1st Qu.:  0.0000        
##  Median :  0.00       Median :  0.0000        
##  Mean   : 15.28       Mean   :  1.7186        
##  3rd Qu.: 21.15       3rd Qu.:  0.0778        
##  Max.   :100.00       Max.   :100.0000        
##  NA's   :18971        NA's   :18971
```






## Exportação



``` r
dir.create(pasta$prod_candidatos, showWarnings = FALSE, recursive = TRUE)



write.xlsx(
  financiamento_gerais,
  file      = file.path(pasta$prod_candidatos, "financiamento_gerais.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)
```










