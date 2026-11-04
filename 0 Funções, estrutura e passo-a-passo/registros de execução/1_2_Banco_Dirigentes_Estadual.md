---
title: "1.2 Banco Dirigentes Estadual"
author: "Bruno Schaefer (IESP-UERJ)"
date: "2026-06-10"
output: html_document
---



## Introdução

### Pacotes


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
library(data.table)
```

```
## Warning: pacote 'data.table' foi compilado no R versão 4.5.3
```

``` r
library(stringr)
```

```
## Warning: pacote 'stringr' foi compilado no R versão 4.5.2
```

``` r
library(lubridate)
```

```
## Warning: pacote 'lubridate' foi compilado no R versão 4.5.2
```

```
## 
## Anexando pacote: 'lubridate'
```

```
## Os seguintes objetos são mascarados por 'package:data.table':
## 
##     hour, isoweek, isoyear, mday, minute, month, quarter, second, wday,
##     week, yday, year
```

```
## Os seguintes objetos são mascarados por 'package:base':
## 
##     date, intersect, setdiff, union
```

``` r
library(stringi)
library(openxlsx)
```

```
## Warning: pacote 'openxlsx' foi compilado no R versão 4.5.2
```

## Carregar dados

Dados baixados de: https://dadosabertos.tse.jus.br/dataset/delegados-partidarios/resource/771736c9-f28a-4463-9b69-d5803c07dec3

Data 29/11/2025


``` r
# Importação ------------------------------------------------------------------
pasta_dirigentes <- pasta$brutos_dirigentes

arquivos <- list.files(
  path       = pasta_dirigentes,
  pattern    = "(?i)\\.csv$",
  full.names = TRUE
)

dados_dirigentes <- rbindlist(
  lapply(arquivos, function(f) {
    dt <- fread(f, sep = ";", encoding = "Latin-1", colClasses = "character")
    dt[, arquivo_origem := basename(f)]
    dt[, partido := str_remove(basename(f), "^orgao_partidario_") |>
         str_remove("(?i)\\.csv$")]
    dt
  }),
  fill = TRUE
)

# Remover linhas duplicadas ---------------------------------------------------
# Os arquivos do TSE trazem cerca de 208 mil linhas exatamente repetidas
# (≈ 4% do total), que inflariam as contagens de membros.
n_antes <- nrow(dados_dirigentes)
dados_dirigentes <- unique(dados_dirigentes)
cat("Linhas duplicadas removidas:", n_antes - nrow(dados_dirigentes), "\n")
```

```
## Linhas duplicadas removidas: 208344
```

``` r
summary(dados_dirigentes)
```

```
##   DT_GERACAO         HH_GERACAO        SQ_ORGAO_PARTIDARIO  SG_PARTIDO       
##  Length:4811573     Length:4811573     Length:4811573      Length:4811573    
##  Class :character   Class :character   Class :character    Class :character  
##  Mode  :character   Mode  :character   Mode  :character    Mode  :character  
##   NR_PARTIDO         NM_PARTIDO        CD_TIPO_ORGAO_PARTIDARIO
##  Length:4811573     Length:4811573     Length:4811573          
##  Class :character   Class :character   Class :character        
##  Mode  :character   Mode  :character   Mode  :character        
##  NM_TIPO_ORGAO_PARTIDARIO CD_TIPO_ABRANGENCIA DS_TIPO_ABRANGENCIA
##  Length:4811573           Length:4811573      Length:4811573     
##  Class :character         Class :character    Class :character   
##  Mode  :character         Mode  :character    Mode  :character   
##  DT_INICIO_VIGENCIA_ORGAO DT_FIM_VIGENCIA_ORGAO    SG_UF          
##  Length:4811573           Length:4811573        Length:4811573    
##  Class :character         Class :character      Class :character  
##  Mode  :character         Mode  :character      Mode  :character  
##     SG_UE              NM_UE           CD_MUNICIPIO       NM_MUNICIPIO      
##  Length:4811573     Length:4811573     Length:4811573     Length:4811573    
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  DS_ENDERECO_ORGAO  NR_CEP_ORGAO       NR_FONE_FIXO_ORGAO NR_FONE_CEL_ORGAO 
##  Length:4811573     Length:4811573     Length:4811573     Length:4811573    
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  NR_FONE_COMERCIAL_ORGAO NR_FAX_ORGAO       TX_EMAIL_ORGAO    
##  Length:4811573          Length:4811573     Length:4811573    
##  Class :character        Class :character   Class :character  
##  Mode  :character        Mode  :character   Mode  :character  
##  SQ_CARGO_MEMBRO    DS_CARGO_MEMBRO     SQ_MEMBRO          NM_MEMBRO        
##  Length:4811573     Length:4811573     Length:4811573     Length:4811573    
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  NR_TITULO_ELEITORAL_MEMBRO DT_INICIO_EXERCICIO_MEMBRO DT_FIM_EXERCICIO_MEMBRO
##  Length:4811573             Length:4811573             Length:4811573         
##  Class :character           Class :character           Class :character       
##  Mode  :character           Mode  :character           Mode  :character       
##   CD_GENERO          DS_GENERO         DS_SITU_EXERC_MEMBRO
##  Length:4811573     Length:4811573     Length:4811573      
##  Class :character   Class :character   Class :character    
##  Mode  :character   Mode  :character   Mode  :character    
##  DS_SITU_EXERC_ORGAO_PARTIDARIO arquivo_origem       partido         
##  Length:4811573                 Length:4811573     Length:4811573    
##  Class :character               Class :character   Class :character  
##  Mode  :character               Mode  :character   Mode  :character
```

## Tratamento de dados

### Abrangência e filtragem

Esta etapa filtra o banco para manter apenas os registros de abrangência estadual e regional (DF), que correspondem às direções partidárias nos estados e no Distrito Federal. Em seguida, remove acentos de todas as colunas de texto, padronizando a codificação para Latin-ASCII a fim de evitar problemas de comparação de strings nas etapas seguintes.


``` r
table(dados_dirigentes$DS_TIPO_ABRANGENCIA)
```

```
## 
##      ESTADUAL     MUNICIPAL      NACIONAL REGIONAL (DF)    ZONAL (DF) 
##        185440       4590722         24716          6373          4322
```

``` r
# Filtrar abrangência
dados_dirigentes_est <- dados_dirigentes[
  DS_TIPO_ABRANGENCIA %in% c("ESTADUAL", "REGIONAL (DF)")
]

# Remover acentos nas colunas texto
cols_chr <- names(dados_dirigentes_est)[sapply(dados_dirigentes_est, is.character)]

dados_dirigentes_est[,
  (cols_chr) := lapply(.SD, stri_trans_general, id = "Latin-ASCII"),
  .SDcols = cols_chr
]

cat("Observações após filtragem:", nrow(dados_dirigentes_est), "\n")
```

```
## Observações após filtragem: 191813
```

``` r
table(dados_dirigentes_est$DS_TIPO_ABRANGENCIA)
```

```
## 
##      ESTADUAL REGIONAL (DF) 
##        185440          6373
```

### Datas e expansão por ano

#### Critério mais restritivo

O banco `dirigente_ano_est` tem como unidade de análise o **ano-dirigente-partido-UF**. Para ser incluído em determinado ano, o dirigente precisa ter exercido o cargo por pelo menos **183 dias** dentro daquele ano calendário. O período ativo é calculado como a interseção entre o intervalo do mandato e o intervalo do ano (1º de janeiro a 31 de dezembro). Dirigentes sem data de fim registrada são tratados como em exercício até 31/12/2025. Como o join não-equi do `data.table` sobrescreve as colunas utilizadas na condição de cruzamento, as datas originais são preservadas em colunas auxiliares (`dt_inicio`, `dt_fim2`) antes do join, sendo essas usadas no cálculo dos dias ativos. Este banco é indicado para analisar **quem comandava o partido** em cada estado a cada ano.

Registros com data de início implausível (anterior a 1980, como 30/12/1899 ou 16/03/0208) são descartados, pois não é possível saber em que anos estiveram ativos.

Cada pessoa recebe um identificador (`id_membro`): o título eleitoral, ou o sequencial do membro no TSE quando o título está ausente. O TSE mantém várias versões do mesmo vínculo (mesmo órgão, pessoa e cargo) que diferem apenas na data de fim; por isso, após o cruzamento com os anos, mantém-se uma única linha por **ano-órgão-pessoa-cargo**, a de maior permanência no ano.


``` r
# Identificador da pessoa -----------------------------------------------------
dados_dirigentes_est[, id_membro := fifelse(
  grepl("^[0-9]+$", NR_TITULO_ELEITORAL_MEMBRO) &
    !grepl("^0+$", NR_TITULO_ELEITORAL_MEMBRO),
  NR_TITULO_ELEITORAL_MEMBRO,
  paste0("SQ", SQ_MEMBRO)
)]

# Datas -----------------------------------------------------------------------
dados_dirigentes_est[, dt_inicio := dmy(DT_INICIO_EXERCICIO_MEMBRO)]
dados_dirigentes_est[
  DT_FIM_EXERCICIO_MEMBRO == "#NULO",
  DT_FIM_EXERCICIO_MEMBRO := NA_character_
]
dados_dirigentes_est[, dt_fim := dmy(DT_FIM_EXERCICIO_MEMBRO)]

# Datas de início ausentes ou implausíveis
cat("Registros sem data de início válida:",
    dados_dirigentes_est[is.na(dt_inicio) | year(dt_inicio) < 1980, .N], "\n")
```

```
## Registros sem data de início válida: 0
```

``` r
dados_dirigentes_est <- dados_dirigentes_est[!is.na(dt_inicio) & year(dt_inicio) >= 1980]

ultimo_dia <- as.Date("2025-12-31")
dados_dirigentes_est[, dt_fim2 := fifelse(is.na(dt_fim), ultimo_dia, dt_fim)]

# Cópias usadas no join (serão sobrescritas pelo data.table) ------------------
dados_dirigentes_est[, join_inicio := dt_inicio]
dados_dirigentes_est[, join_fim    := dt_fim2]

# Expansão por ano ------------------------------------------------------------
years   <- 2000:2025
anos_dt <- data.table(
  ANO        = years,
  start_date = as.Date(paste0(years, "-01-01")),
  end_date   = as.Date(paste0(years, "-12-31"))
)

# Join não-equi ---------------------------------------------------------------
base_join_est <- dados_dirigentes_est[
  anos_dt,
  on = .(join_inicio <= end_date, join_fim >= start_date),
  nomatch = 0,
  allow.cartesian = TRUE
]

# Dias ativos usando colunas preservadas --------------------------------------
base_join_est[, inicio_no_ano := pmax(dt_inicio, as.Date(paste0(ANO, "-01-01")))]
base_join_est[, fim_no_ano    := pmin(dt_fim2,   as.Date(paste0(ANO, "-12-31")))]
base_join_est[, dias_ativos   := as.integer(fim_no_ano - inicio_no_ano) + 1L]

# Versões repetidas do mesmo vínculo: uma linha por ano-órgão-pessoa-cargo -----
n_antes <- nrow(base_join_est)
setorder(base_join_est, -dias_ativos)
base_join_est <- unique(
  base_join_est,
  by = c("ANO", "SQ_ORGAO_PARTIDARIO", "id_membro", "DS_CARGO_MEMBRO")
)
cat("Versões repetidas de vínculos removidas:", n_antes - nrow(base_join_est), "\n")
```

```
## Versões repetidas de vínculos removidas: 92607
```

``` r
dirigente_ano_est <- base_join_est[dias_ativos >= 183]

cat("dirigente_ano_est (>= 183 dias):", nrow(dirigente_ano_est), "\n")
```

```
## dirigente_ano_est (>= 183 dias): 241600
```

``` r
summary(dirigente_ano_est$dias_ativos)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   183.0   293.0   365.0   324.9   365.0   366.0
```

#### Critério mais amplo

O banco `dirigente_ano_est2` tem a mesma unidade de análise **ano-dirigente-partido-UF**, mas com critério mais inclusivo: o dirigente é registrado em determinado ano se exerceu o cargo em **qualquer momento** daquele ano, mesmo que por apenas um dia. A coluna `dias_ativos` é mantida para fins diagnósticos e para futuras análises de duração. Este banco é indicado para analisar **rotatividade**, entrada e saída de dirigentes, e sobreposição de mandatos nas direções estaduais.


``` r
dirigente_ano_est2 <- copy(base_join_est)

cat("dirigente_ano_est2 (qualquer dia):", nrow(dirigente_ano_est2), "\n")
```

```
## dirigente_ano_est2 (qualquer dia): 392092
```

``` r
summary(dirigente_ano_est2$dias_ativos)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  -123.0   111.0   258.0   233.6   365.0   366.0
```

### Tipos de cargos

#### Banco restritivo

Esta etapa cria indicadores binários de cargo para o banco restritivo estadual. A variável `setorial_mulher` identifica cargos ligados à secretaria ou coordenação de mulheres. A variável `presidente` identifica presidentes titulares das direções estaduais, excluindo vice-presidentes, presidentes honorários, de conselhos, setoriais e outros cargos que contenham "president" mas não correspondam à presidência executiva. A variável `tesoureiro` identifica tesoureiros titulares, excluindo adjuntos, suplentes, vogais e segundos e terceiros tesoureiros. A variável `cargo_decisao` agrega presidente e tesoureiro como cargos de decisão.


``` r
## Setorial mulher ------------------------------------------------------------
dirigente_ano_est[, setorial_mulher :=
  fifelse(grepl("MULH", DS_CARGO_MEMBRO, ignore.case = TRUE), 1L, 0L)
]

## Presidente -----------------------------------------------------------------
dirigente_ano_est[, presidente :=
  fifelse(
    grepl("PRESIDENT|PRESIDENC", DS_CARGO_MEMBRO, ignore.case = TRUE) &
      !grepl(
        paste(c(
          "VICE", "HONRA", "CONSELHO", "ETICA", "FISCAL",
          "JOVEM", "JUVENTUDE", "MULHER", "MOVIMENTO",
          "FUNDAC", "INTERIN", "AFASTAD", "EXERCICIO",
          "LICENCIAD", "SUBSTITUT", "PORTA VOZ",
          "COMUNITARIO", "SETORIAL", "SECRETARIA",
          "COORDENADOR", "MEMBRO", "CONSULTIVO",
          "EMPRESARIAL", "PARLAMENTAR", "FRENTE",
          "INSTITUTO", "AFRO", "DIVERSIDADE", "ESPORTE",
          "MAPI", "JS/PDT", "EX-PRESIDENTE",
          "AMT/PDT", "PRESIDENTE DE FORMACAO POLITICA"
        ), collapse = "|"),
        DS_CARGO_MEMBRO, ignore.case = TRUE
      ),
    1L, 0L
  )
]

## Tesoureiro -----------------------------------------------------------------
dirigente_ano_est[, tesoureiro :=
  fifelse(
    grepl("TESOUR", DS_CARGO_MEMBRO, ignore.case = TRUE) &
      !grepl(
        paste(c(
          "ADJUNT", "SUBSTITUT", "SUPLENTE", "VICE",
          "COORDENACAO", "MEMBRO", "VOGAL",
          "LICENCIAD", "EXERCICIO",
          # segundos e terceiros tesoureiros (equivalentes a adjuntos)
          "SEGUNDO", "TERCEIRO", "2. TESOUR", "3. TESOUR", "2O TESOUR", "3O TESOUR"
        ), collapse = "|"),
        DS_CARGO_MEMBRO, ignore.case = TRUE
      ),
    1L, 0L
  )
]

## Cargo de decisão -----------------------------------------------------------
dirigente_ano_est[, cargo_decisao := fifelse(presidente == 1L | tesoureiro == 1L, 1L, 0L)]

## Verificação ----------------------------------------------------------------
cat("Setorial mulher:\n"); print(table(dirigente_ano_est$setorial_mulher))
```

```
## Setorial mulher:
```

```
## 
##      0      1 
## 239849   1751
```

``` r
cat("Presidente:\n");      print(table(dirigente_ano_est$presidente))
```

```
## Presidente:
```

```
## 
##      0      1 
## 233414   8186
```

``` r
cat("Tesoureiro:\n");      print(table(dirigente_ano_est$tesoureiro))
```

```
## Tesoureiro:
```

```
## 
##      0      1 
## 233344   8256
```

``` r
cat("Cargo decisão:\n");   print(table(dirigente_ano_est$cargo_decisao))
```

```
## Cargo decisão:
```

```
## 
##      0      1 
## 225158  16442
```

#### Banco amplo

Esta etapa replica os mesmos indicadores de cargo para o banco amplo estadual (`dirigente_ano_est2`), utilizando os mesmos critérios de inclusão e exclusão definidos acima para garantir comparabilidade entre os dois bancos.


``` r
## Setorial mulher ------------------------------------------------------------
dirigente_ano_est2[, setorial_mulher :=
  fifelse(grepl("MULH", DS_CARGO_MEMBRO, ignore.case = TRUE), 1L, 0L)
]

## Presidente -----------------------------------------------------------------
dirigente_ano_est2[, presidente :=
  fifelse(
    grepl("PRESIDENT|PRESIDENC", DS_CARGO_MEMBRO, ignore.case = TRUE) &
      !grepl(
        paste(c(
          "VICE", "HONRA", "CONSELHO", "ETICA", "FISCAL",
          "JOVEM", "JUVENTUDE", "MULHER", "MOVIMENTO",
          "FUNDAC", "INTERIN", "AFASTAD", "EXERCICIO",
          "LICENCIAD", "SUBSTITUT", "PORTA VOZ",
          "COMUNITARIO", "SETORIAL", "SECRETARIA",
          "COORDENADOR", "MEMBRO", "CONSULTIVO",
          "EMPRESARIAL", "PARLAMENTAR", "FRENTE",
          "INSTITUTO", "AFRO", "DIVERSIDADE", "ESPORTE",
          "MAPI", "JS/PDT", "EX-PRESIDENTE",
          "AMT/PDT", "PRESIDENTE DE FORMACAO POLITICA"
        ), collapse = "|"),
        DS_CARGO_MEMBRO, ignore.case = TRUE
      ),
    1L, 0L
  )
]

## Tesoureiro -----------------------------------------------------------------
dirigente_ano_est2[, tesoureiro :=
  fifelse(
    grepl("TESOUR", DS_CARGO_MEMBRO, ignore.case = TRUE) &
      !grepl(
        paste(c(
          "ADJUNT", "SUBSTITUT", "SUPLENTE", "VICE",
          "COORDENACAO", "MEMBRO", "VOGAL",
          "LICENCIAD", "EXERCICIO",
          # segundos e terceiros tesoureiros (equivalentes a adjuntos)
          "SEGUNDO", "TERCEIRO", "2. TESOUR", "3. TESOUR", "2O TESOUR", "3O TESOUR"
        ), collapse = "|"),
        DS_CARGO_MEMBRO, ignore.case = TRUE
      ),
    1L, 0L
  )
]

## Cargo de decisão -----------------------------------------------------------
dirigente_ano_est2[, cargo_decisao := fifelse(presidente == 1L | tesoureiro == 1L, 1L, 0L)]
```

### Banco de dirigentes estaduais

Esta etapa filtra os dois bancos para o período a partir de 2015, ano a partir do qual a cobertura dos dados é considerada suficientemente consistente para análise. O banco restritivo e o amplo mantêm a abrangência estadual e regional (DF) já filtrada na etapa de limpeza.


``` r
## Restritivo -----------------------------------------------------------------
banco_dirigentes_estadual <- dirigente_ano_est[ANO >= 2015]

cat("Observações (restritivo):", nrow(banco_dirigentes_estadual), "\n")
```

```
## Observações (restritivo): 220230
```

``` r
cat("Anos cobertos:", min(banco_dirigentes_estadual$ANO),
    "a", max(banco_dirigentes_estadual$ANO), "\n")
```

```
## Anos cobertos: 2015 a 2025
```

``` r
print(table(banco_dirigentes_estadual$ANO))
```

```
## 
##  2015  2016  2017  2018  2019  2020  2021  2022  2023  2024  2025 
## 11708 21060 22865 24946 22637 23718 21842 19129 17739 17898 16688
```

``` r
## Amplo ----------------------------------------------------------------------
banco_dirigentes_estadual2 <- dirigente_ano_est2[ANO >= 2015]

cat("Observações (amplo):", nrow(banco_dirigentes_estadual2), "\n")
```

```
## Observações (amplo): 363171
```

``` r
cat("Anos cobertos:", min(banco_dirigentes_estadual2$ANO),
    "a", max(banco_dirigentes_estadual2$ANO), "\n")
```

```
## Anos cobertos: 2015 a 2025
```

``` r
print(table(banco_dirigentes_estadual2$ANO))
```

```
## 
##  2015  2016  2017  2018  2019  2020  2021  2022  2023  2024  2025 
## 19714 24063 40843 37270 45563 32762 37638 36822 34474 24374 29648
```

## Turnover

### Funções auxiliares

Esta etapa define duas funções reutilizáveis de cálculo de turnover, aplicadas a seguir tanto ao banco restritivo quanto ao amplo. A função `calcular_turnover_nomes()` compara, para cada par de anos consecutivos dentro de um grupo (partido × UF × tipo de órgão), os conjuntos de pessoas (identificadas por `id_membro`, o título eleitoral) presentes em `t` e em `t-1`, calculando entradas, saídas, permanências e as proporções correspondentes. A função `calcular_turnover_genero_decisao()` opera apenas sobre os ocupantes de cargos de decisão e produz indicadores de transição de gênero: se havia mulher em `t-1` e deixou de haver em `t`, se havia homem e deixou de haver, e se o gênero se manteve entre os dois anos. As funções são vetorizadas: o conjunto de pessoas de `t-1` é deslocado para `t` e cruzado pessoa a pessoa com o de `t`, o que dá o mesmo resultado de comparar os anos um a um, em uma fração do tempo (no nível municipal, de horas para minutos). O turnover também é calculado separadamente para mulheres e para homens (`turnover_prop_mulheres`, `turnover_prop_homens` e, nos cargos de decisão, `turnover_prop_mulheres_decisao` e `turnover_prop_homens_decisao`): entradas mais saídas do grupo sobre o total de pessoas do grupo presentes em algum dos dois anos.


``` r
# Função 1: turnover geral de nomes -------------------------------------------
# Compara o conjunto de pessoas de cada grupo em t com o de t-1 (somente anos
# consecutivos presentes no grupo). Implementação vetorizada: o conjunto de
# t-1 é deslocado para t e cruzado por pessoa. Inclui o turnover separado de
# mulheres e de homens.
calcular_turnover_nomes <- function(dt, by_vars, filtro = NULL) {

  if (!is.null(filtro)) dt <- dt[eval(filtro)]

  k   <- c(by_vars, "ANO")
  atu <- unique(dt[, c(k, "id_membro", "DS_GENERO"), with = FALSE], by = c(k, "id_membro"))
  atu[, `:=`(fem = DS_GENERO %in% "FEMININO", masc = DS_GENERO %in% "MASCULINO")]
  ant <- copy(atu)[, ANO := ANO + 1L]          # pessoas de t-1, rotuladas como t

  atu[, esta_t1 := FALSE][ant, on = c(k, "id_membro"), esta_t1 := TRUE]
  ant[, esta_t  := FALSE][atu, on = c(k, "id_membro"), esta_t  := TRUE]

  st <- atu[, .(
    n_membros_t       = .N,
    n_permanencias    = sum(esta_t1),
    mulheres_t        = sum(fem),
    homens_t          = sum(masc),
    perm_mulheres     = sum(fem  & esta_t1),
    perm_homens       = sum(masc & esta_t1),
    entradas_mulheres = sum(fem  & !esta_t1),
    entradas_homens   = sum(masc & !esta_t1)
  ), by = k]

  st1 <- ant[, .(
    n_membros_t1    = .N,
    mulheres_t1     = sum(fem),
    homens_t1       = sum(masc),
    saidas_mulheres = sum(fem  & !esta_t),
    saidas_homens   = sum(masc & !esta_t)
  ), by = k]

  res <- merge(st, st1, by = k)                # só anos com t e t-1 presentes
  res[, `:=`(
    delta_membros = n_membros_t - n_membros_t1,
    n_entradas    = n_membros_t  - n_permanencias,
    n_saidas      = n_membros_t1 - n_permanencias
  )]
  res[, `:=`(
    turnover_abs           = n_entradas + n_saidas,
    turnover_prop          = (n_entradas + n_saidas) /
      pmax(n_membros_t + n_membros_t1 - n_permanencias, 1L),
    turnover_prop_mulheres = (entradas_mulheres + saidas_mulheres) /
      pmax(mulheres_t + mulheres_t1 - perm_mulheres, 1L),
    turnover_prop_homens   = (entradas_homens + saidas_homens) /
      pmax(homens_t + homens_t1 - perm_homens, 1L)
  )]
  res[, c("mulheres_t", "homens_t", "mulheres_t1", "homens_t1",
          "perm_mulheres", "perm_homens") := NULL]
  setcolorder(res, c(k, "n_membros_t", "n_membros_t1", "delta_membros",
                     "n_entradas", "n_saidas", "n_permanencias",
                     "turnover_abs", "turnover_prop",
                     "entradas_mulheres", "saidas_mulheres", "turnover_prop_mulheres",
                     "entradas_homens", "saidas_homens", "turnover_prop_homens"))
  res[]
}

# Função 2: turnover de gênero nos cargos de decisão --------------------------
calcular_turnover_genero_decisao <- function(dt, by_vars) {

  k   <- c(by_vars, "ANO")
  atu <- unique(
    dt[cargo_decisao == 1L, c(k, "id_membro", "DS_GENERO"), with = FALSE],
    by = c(k, "id_membro")
  )
  atu[, `:=`(fem = DS_GENERO %in% "FEMININO", masc = DS_GENERO %in% "MASCULINO")]
  ant <- copy(atu)[, ANO := ANO + 1L]          # ocupantes de t-1, rotulados como t

  atu[, esta_t1 := FALSE][ant, on = c(k, "id_membro"), esta_t1 := TRUE]
  ant[, esta_t  := FALSE][atu, on = c(k, "id_membro"), esta_t  := TRUE]

  st <- atu[, .(
    n_decisao_t             = .N,
    perm                    = sum(esta_t1),
    mulheres_t              = sum(fem),
    homens_t                = sum(masc),
    perm_mulheres           = sum(fem  & esta_t1),
    perm_homens             = sum(masc & esta_t1),
    entradas_mulher_decisao = sum(fem  & !esta_t1),
    entradas_homem_decisao  = sum(masc & !esta_t1),
    tem_mulher_t            = any(fem),
    tem_homem_t             = any(masc)
  ), by = k]

  st1 <- ant[, .(
    n_decisao_t1          = .N,
    mulheres_t1           = sum(fem),
    homens_t1             = sum(masc),
    saidas_mulher_decisao = sum(fem  & !esta_t),
    saidas_homem_decisao  = sum(masc & !esta_t),
    tinha_mulher_t1       = any(fem),
    tinha_homem_t1        = any(masc)
  ), by = k]

  res <- merge(st, st1, by = k)                # só anos com t e t-1 presentes
  res[, `:=`(
    delta_decisao          = n_decisao_t - n_decisao_t1,
    n_entradas_decisao     = n_decisao_t  - perm,
    n_saidas_decisao       = n_decisao_t1 - perm,
    perdeu_mulher_decisao  = fifelse(tinha_mulher_t1 & !tem_mulher_t, 1L, 0L),
    perdeu_homem_decisao   = fifelse(tinha_homem_t1  & !tem_homem_t,  1L, 0L),
    manteve_mulher_decisao = fifelse(tinha_mulher_t1 & tem_mulher_t,  1L, 0L),
    manteve_homem_decisao  = fifelse(tinha_homem_t1  & tem_homem_t,   1L, 0L)
  )]
  res[, `:=`(
    turnover_abs_decisao           = n_entradas_decisao + n_saidas_decisao,
    turnover_prop_decisao          = (n_entradas_decisao + n_saidas_decisao) /
      pmax(n_decisao_t + n_decisao_t1 - perm, 1L),
    turnover_prop_mulheres_decisao = (entradas_mulher_decisao + saidas_mulher_decisao) /
      pmax(mulheres_t + mulheres_t1 - perm_mulheres, 1L),
    turnover_prop_homens_decisao   = (entradas_homem_decisao + saidas_homem_decisao) /
      pmax(homens_t + homens_t1 - perm_homens, 1L)
  )]
  res[, c("perm", "tem_mulher_t", "tem_homem_t", "tinha_mulher_t1", "tinha_homem_t1",
          "mulheres_t", "homens_t", "mulheres_t1", "homens_t1",
          "perm_mulheres", "perm_homens") := NULL]
  setcolorder(res, c(k, "n_decisao_t", "n_decisao_t1", "delta_decisao",
                     "n_entradas_decisao", "n_saidas_decisao",
                     "turnover_abs_decisao", "turnover_prop_decisao",
                     "entradas_mulher_decisao", "entradas_homem_decisao",
                     "saidas_mulher_decisao", "saidas_homem_decisao",
                     "turnover_prop_mulheres_decisao", "turnover_prop_homens_decisao",
                     "perdeu_mulher_decisao", "perdeu_homem_decisao",
                     "manteve_mulher_decisao", "manteve_homem_decisao"))
  res[]
}
```

### Turnover — banco restritivo

Esta etapa aplica as duas funções ao banco restritivo estadual (`banco_dirigentes_estadual`), calculando o turnover de nomes e o turnover de gênero nos cargos de decisão, agrupados por partido, UF e tipo de órgão partidário. Os dois resultados são combinados em um único objeto `turnover_est_restritivo`, que será incorporado ao banco de diretórios na etapa seguinte.


``` r
by_vars_est <- c("SG_PARTIDO", "NR_PARTIDO", "SG_UF", "NM_TIPO_ORGAO_PARTIDARIO")

tv_nomes_r   <- calcular_turnover_nomes(banco_dirigentes_estadual, by_vars_est)
tv_decisao_r <- calcular_turnover_genero_decisao(banco_dirigentes_estadual, by_vars_est)

turnover_est_restritivo <- merge(
  tv_nomes_r,
  tv_decisao_r,
  by = c(by_vars_est, "ANO"),
  all = TRUE
)

cat("Turnover estadual restritivo — linhas:", nrow(turnover_est_restritivo), "\n")
```

```
## Turnover estadual restritivo — linhas: 5410
```

### Turnover — banco amplo

Esta etapa replica o mesmo procedimento para o banco amplo estadual (`banco_dirigentes_estadual2`). Como o critério de inclusão é mais permissivo, espera-se um turnover maior, já que dirigentes com mandatos muito curtos — que não atingiriam os 183 dias do critério restritivo — são incluídos, aumentando a rotatividade observada entre anos consecutivos.


``` r
tv_nomes_a   <- calcular_turnover_nomes(banco_dirigentes_estadual2, by_vars_est)
tv_decisao_a <- calcular_turnover_genero_decisao(banco_dirigentes_estadual2, by_vars_est)

turnover_est_amplo <- merge(
  tv_nomes_a,
  tv_decisao_a,
  by = c(by_vars_est, "ANO"),
  all = TRUE
)

cat("Turnover estadual amplo — linhas:", nrow(turnover_est_amplo), "\n")
```

```
## Turnover estadual amplo — linhas: 7834
```

## Diretórios

### Banco de diretórios estadual — restritivo

O banco `banco_diretorios_estadual` tem como unidade de análise o **ano-partido-UF-tipo de órgão**. Cada linha agrega os dirigentes vigentes (≥ 183 dias) de um partido em determinado estado, tipo de órgão e ano, com indicadores de composição por gênero, presença de setorial de mulheres e cargos de decisão. As variáveis de turnover calculadas anteriormente são incorporadas via `merge`, de modo que cada diretório-ano contenha tanto os indicadores de composição quanto os de renovação em relação ao ano anterior.

As contagens (membros, mulheres, presidentes, tesoureiros, cargos de decisão) referem-se a **pessoas distintas** (`id_membro`), e não a linhas: quem ocupa dois cargos no mesmo órgão, ou aparece em dois órgãos do mesmo tipo no ano, é contado uma vez. `total_genero_ni` conta membros sem gênero informado (#NULO ou NAO INFORMADO), que entram no denominador de `proporcao_mulheres`. `n_orgaos` indica quantos órgãos distintos (`SQ_ORGAO_PARTIDARIO`) do mesmo tipo estiveram vigentes no ano (normalmente 1; 2 em anos de renovação da direção). `dias_atividade_ano` é o maior número de dias ativos de um membro do diretório no ano; mede por quanto tempo aquele tipo de órgão esteve em funcionamento no ano e é usado no código 4 para escolher um único diretório por unidade-ano.


``` r
banco_diretorios_estadual <- banco_dirigentes_estadual[,
  .(
    total_membros         = uniqueN(id_membro),
    total_mulheres        = uniqueN(id_membro[DS_GENERO == "FEMININO"]),
    total_genero_ni       = uniqueN(id_membro[!DS_GENERO %in% c("FEMININO", "MASCULINO")]),
    total_setorial_mulher = uniqueN(id_membro[setorial_mulher == 1L]),
    total_presidente      = uniqueN(id_membro[presidente == 1L]),
    mulher_presidente     = uniqueN(id_membro[presidente == 1L & DS_GENERO == "FEMININO"]),
    total_tesoureiro      = uniqueN(id_membro[tesoureiro == 1L]),
    mulher_tesoureiro     = uniqueN(id_membro[tesoureiro == 1L & DS_GENERO == "FEMININO"]),
    total_decisao         = uniqueN(id_membro[cargo_decisao == 1L]),
    mulher_decisao        = uniqueN(id_membro[cargo_decisao == 1L & DS_GENERO == "FEMININO"]),
    n_orgaos              = uniqueN(SQ_ORGAO_PARTIDARIO),
    dias_atividade_ano    = max(dias_ativos)
  ),
  by = .(ANO, SG_PARTIDO, NR_PARTIDO, SG_UF, NM_TIPO_ORGAO_PARTIDARIO)
][,
  `:=`(
    setorial                   = fifelse(total_setorial_mulher > 0, 1L, 0L),
    proporcao_mulheres         = total_mulheres / total_membros,
    proporcao_mulheres_decisao = fifelse(
      total_decisao > 0, mulher_decisao / total_decisao, NA_real_
    ),
    tem_mulher_presidente      = fifelse(mulher_presidente > 0, 1L, 0L),
    tem_mulher_tesoureiro      = fifelse(mulher_tesoureiro > 0, 1L, 0L),
    tem_mulher_decisao         = fifelse(mulher_decisao    > 0, 1L, 0L)
  )
]

# Incorporar turnover ---------------------------------------------------------
banco_diretorios_estadual <- merge(
  banco_diretorios_estadual,
  turnover_est_restritivo,
  by = c(by_vars_est, "ANO"),
  all.x = TRUE
)

cat("Observações:", nrow(banco_diretorios_estadual), "\n")
```

```
## Observações: 7332
```

### Banco de diretórios estadual — amplo

O banco `banco_diretorios_estadual2` replica a mesma estrutura do banco restritivo, mas agrega todos os dirigentes que exerceram o cargo em qualquer momento do ano nos estados. As variáveis de turnover incorporadas aqui refletem o critério amplo e são mais sensíveis a movimentações curtas de pessoal, sendo mais adequadas para análises de rotatividade e instabilidade da composição das direções estaduais.


``` r
banco_diretorios_estadual2 <- banco_dirigentes_estadual2[,
  .(
    total_membros         = uniqueN(id_membro),
    total_mulheres        = uniqueN(id_membro[DS_GENERO == "FEMININO"]),
    total_genero_ni       = uniqueN(id_membro[!DS_GENERO %in% c("FEMININO", "MASCULINO")]),
    total_setorial_mulher = uniqueN(id_membro[setorial_mulher == 1L]),
    total_presidente      = uniqueN(id_membro[presidente == 1L]),
    mulher_presidente     = uniqueN(id_membro[presidente == 1L & DS_GENERO == "FEMININO"]),
    total_tesoureiro      = uniqueN(id_membro[tesoureiro == 1L]),
    mulher_tesoureiro     = uniqueN(id_membro[tesoureiro == 1L & DS_GENERO == "FEMININO"]),
    total_decisao         = uniqueN(id_membro[cargo_decisao == 1L]),
    mulher_decisao        = uniqueN(id_membro[cargo_decisao == 1L & DS_GENERO == "FEMININO"]),
    n_orgaos              = uniqueN(SQ_ORGAO_PARTIDARIO),
    dias_atividade_ano    = max(dias_ativos)
  ),
  by = .(ANO, SG_PARTIDO, NR_PARTIDO, SG_UF, NM_TIPO_ORGAO_PARTIDARIO)
][,
  `:=`(
    setorial                   = fifelse(total_setorial_mulher > 0, 1L, 0L),
    proporcao_mulheres         = total_mulheres / total_membros,
    proporcao_mulheres_decisao = fifelse(
      total_decisao > 0, mulher_decisao / total_decisao, NA_real_
    ),
    tem_mulher_presidente      = fifelse(mulher_presidente > 0, 1L, 0L),
    tem_mulher_tesoureiro      = fifelse(mulher_tesoureiro > 0, 1L, 0L),
    tem_mulher_decisao         = fifelse(mulher_decisao    > 0, 1L, 0L)
  )
]

# Incorporar turnover ---------------------------------------------------------
banco_diretorios_estadual2 <- merge(
  banco_diretorios_estadual2,
  turnover_est_amplo,
  by = c(by_vars_est, "ANO"),
  all.x = TRUE
)

cat("Observações:", nrow(banco_diretorios_estadual2), "\n")
```

```
## Observações: 9641
```

## Exportar bancos

Esta etapa salva os quatro bancos finais em formato `.xlsx` na pasta de saída do projeto. Os bancos de dirigentes contêm uma linha por **ano-dirigente-partido-UF-tipo de órgão**, enquanto os bancos de diretórios contêm uma linha por **ano-partido-UF-tipo de órgão**, com os indicadores de composição e turnover já incorporados.


``` r
pasta_saida <- pasta$prod_dirigentes

library(openxlsx)

# Dirigentes
write.xlsx(
  banco_dirigentes_estadual,
  file.path(pasta_saida, "banco_dirigentes_estadual.xlsx"),
  overwrite = TRUE
)

write.xlsx(
  banco_dirigentes_estadual2,
  file.path(pasta_saida, "banco_dirigentes_estadual2.xlsx"),
  overwrite = TRUE
)

# Diretórios
write.xlsx(
  banco_diretorios_estadual,
  file.path(pasta_saida, "banco_diretorios_estadual.xlsx"),
  overwrite = TRUE
)

write.xlsx(
  banco_diretorios_estadual2,
  file.path(pasta_saida, "banco_diretorios_estadual2.xlsx"),
  overwrite = TRUE
)

cat("Bancos salvos em:", pasta_saida, "\n")
```

```
## Bancos salvos em: <raiz do projeto>/1 Pesquisas individuais/4a Produtos/Dirigentes e diretórios
```
