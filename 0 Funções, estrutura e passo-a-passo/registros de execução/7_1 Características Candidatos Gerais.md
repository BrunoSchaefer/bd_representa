---
title: "7_1 Características Candidatos Gerais"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-11-04"
output: html_document
---




Dados das características dos candidatos das eleições gerais (2010, 2014, 2018, 2022 e 2026)

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



## Dados Candidatos

### Carregar dados


``` r
bd_rec <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.candidatos") %>%
  dplyr::filter(ano %in% c(2010, 2014, 2018, 2022, 2026), tipo_eleicao %in% tipos_ordinarios) %>%
  dplyr::select(
    ano, cargo, sigla_uf, tipo_eleicao,
    sequencial,   # identificador da candidatura no TSE (chave para bens e resultados)
    numero_partido, sigla_partido, numero,
    nome, nome_urna, titulo_eleitoral, cpf,
    situacao, data_nascimento, idade, genero, instrucao, ocupacao,
    estado_civil, nacionalidade, sigla_uf_nascimento, 
    municipio_nascimento, email, raca
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
## Successfully connected to table `basedosdados.br_tse_eleicoes.candidatos`.
```

``` r
candidatos <- basedosdados::bd_collect(bd_rec)
rm(bd_rec); gc()
```

```
##           used  (Mb) gc trigger  (Mb) max used  (Mb)
## Ncells 2334696 124.7    3810950 203.6  3810950 203.6
## Vcells 7951840  60.7   12377050  94.5 10224883  78.1
```

``` r
names(candidatos)
```

```
##  [1] "ano"                  "cargo"                "sigla_uf"            
##  [4] "tipo_eleicao"         "sequencial"           "numero_partido"      
##  [7] "sigla_partido"        "numero"               "nome"                
## [10] "nome_urna"            "titulo_eleitoral"     "cpf"                 
## [13] "situacao"             "data_nascimento"      "idade"               
## [16] "genero"               "instrucao"            "ocupacao"            
## [19] "estado_civil"         "nacionalidade"        "sigla_uf_nascimento" 
## [22] "municipio_nascimento" "email"                "raca"
```

``` r
summary(candidatos)
```

```
##       ano          cargo             sigla_uf         tipo_eleicao      
##  Min.   :2010   Length:127382      Length:127382      Length:127382     
##  1st Qu.:2014   Class :character   Class :character   Class :character  
##  Median :2018   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :2018                                                           
##  3rd Qu.:2022                                                           
##  Max.   :2026                                                           
##                                                                         
##   sequencial        numero_partido     sigla_partido         numero         
##  Length:127382      Length:127382      Length:127382      Length:127382     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##      nome            nome_urna         titulo_eleitoral       cpf           
##  Length:127382      Length:127382      Length:127382      Length:127382     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##    situacao         data_nascimento          idade          genero         
##  Length:127382      Min.   :1918-07-02   Min.   :18.00   Length:127382     
##  Class :character   1st Qu.:1962-01-30   1st Qu.:40.00   Class :character  
##  Mode  :character   Median :1970-07-11   Median :48.00   Mode  :character  
##                     Mean   :1970-07-15   Mean   :48.23                     
##                     3rd Qu.:1979-04-21   3rd Qu.:56.00                     
##                     Max.   :2007-09-12   Max.   :95.00                     
##                     NA's   :123          NA's   :124                       
##   instrucao           ocupacao         estado_civil       nacionalidade     
##  Length:127382      Length:127382      Length:127382      Length:127382     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  sigla_uf_nascimento municipio_nascimento    email               raca          
##  Length:127382       Length:127382        Length:127382      Length:127382     
##  Class :character    Class :character     Class :character   Class :character  
##  Mode  :character    Mode  :character     Mode  :character   Mode  :character  
##                                                                                
##                                                                                
##                                                                                
## 
```


## Filtrar bases de dados





``` r
table(candidatos$situacao, useNA = "always")
```

```
## 
##                                     cancelado 
##                                            76 
##                                       cassado 
##                                            92 
##                           cassado com recurso 
##                                             3 
##                                      deferido 
##                                         92437 
##                          deferido com recurso 
##                                           182 
##                                      falecido 
##                                            17 
##                                   falecimento 
##                                             5 
##                                    indeferido 
##                                          9310 
##                        indeferido com recurso 
##                                           213 
##                    nao conhecimento do pedido 
##                                            78 
##                          pedido nao conhecido 
##                                            15 
##                        pendente de julgamento 
##                                             4 
##                                      renuncia 
##                                          3931 
## substituto majoritario pendente de julgamento 
##                                             1 
##             substituto pendente de julgamento 
##                                             1 
##                                          <NA> 
##                                         21017
```

``` r
# 2026: a basedosdados (consulta de 04/11/2026) ainda não traz a situação da
# candidatura (NA em todas as linhas). Aproximação: é considerada apta a
# candidatura que recebeu votos no 1º turno. Em 2022 esse critério recupera
# 99,9% das candidaturas deferidas a deputado federal (9.453 de 9.455), com
# 0,6% de excesso (indeferidas com recurso que chegaram à urna).
votados_2026 <- basedosdados::bdplyr("basedosdados.br_tse_eleicoes.resultados_candidato") %>%
  dplyr::filter(ano == 2026, turno == 1) %>%
  dplyr::select(cargo, sigla_uf, numero_candidato, votos) %>%
  basedosdados::bd_collect() %>%
  dplyr::group_by(cargo, sigla_uf, numero_candidato) %>%
  dplyr::summarise(votos_t1 = sum(as.numeric(votos), na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(votos_t1 > 0) %>%
  dplyr::transmute(cargo,
                   # presidente: a UF difere entre as fontes; liga só por cargo e número
                   uf_chave  = if_else(cargo == "presidente", "BR", sigla_uf),
                   num_chave = as.character(numero_candidato), votado_2026 = TRUE) %>%
  distinct()
```

```
## Successfully connected to table
## `basedosdados.br_tse_eleicoes.resultados_candidato`.
```

``` r
candidatos <- candidatos %>%
  mutate(uf_chave  = if_else(cargo == "presidente", "BR", sigla_uf),
         num_chave = as.character(numero),
         eh_2026   = as.integer(ano) == 2026L) %>%
  left_join(votados_2026, by = c("cargo", "uf_chave", "num_chave")) %>%
  mutate(votado_2026 = eh_2026 & coalesce(votado_2026, FALSE),
         situacao = if_else(eh_2026 & is.na(situacao) & votado_2026, "deferido", situacao)) %>%
  select(-uf_chave, -num_chave, -eh_2026, -votado_2026)

# Situação em 2026 após a aproximação
candidatos %>% filter(ano == 2026) %>% count(cargo, situacao)
```

```
## # A tibble: 16 × 3
##    cargo              situacao     n
##    <chr>              <chr>    <int>
##  1 1 suplente         <NA>       349
##  2 2 suplente         <NA>       350
##  3 deputado distrital deferido   413
##  4 deputado distrital <NA>        20
##  5 deputado estadual  deferido 10474
##  6 deputado estadual  <NA>       821
##  7 deputado federal   deferido  7097
##  8 deputado federal   <NA>       706
##  9 governador         deferido   186
## 10 governador         <NA>        27
## 11 presidente         deferido    14
## 12 presidente         <NA>         2
## 13 senador            deferido   281
## 14 senador            <NA>        38
## 15 vice-governador    <NA>       223
## 16 vice-presidente    <NA>        16
```

``` r
candidaturas_gerais <- candidatos %>%
  filter(situacao %in% c("deferido com recurso", "deferido"))
```




# Exportar 



``` r
dir.create(pasta$prod_candidatos, showWarnings = FALSE, recursive = TRUE)



write.xlsx(
  candidaturas_gerais,
  file      = file.path(pasta$prod_candidatos, "candidaturas_gerais.xlsx"),
  rowNames  = FALSE,
  overwrite = TRUE
)
```








