### Executor do passo-a-passo (bd_representa) ------------------------------------
# Bruno Schaefer (IESP-UERJ)
#
# Roda os códigos do projeto na ordem, cada um em um processo R separado (a
# memória é liberada entre um código e outro). O relatório de cada código
# (.md, com saídas e diagnósticos) e um registro geral (execucao.log) ficam em
# "0 Funções, estrutura e passo-a-passo/registros de execução".
#
# Uso: abrir o projeto (Dirigentes Partidários.Rproj), escolher as etapas em
# `rodar` abaixo e executar este arquivo (source).
#
# Atenção:
#   - Os códigos 6 a 10 baixam dados da basedosdados (BigQuery) e exigem um
#     projeto de cobrança no Google Cloud (set_billing_id nos códigos).
#   - O código 1_3 (dirigentes municipais) leva cerca de 30 minutos e usa muita
#     memória: rode-o sem outros programas pesados abertos.
#   - O código 11 (socioeconômico, etapa "3a") também usa a basedosdados.

library(here)

raiz <- here()
R    <- file.path(R.home("bin"), "Rscript")
reg  <- file.path(raiz, "0 Funções, estrutura e passo-a-passo", "registros de execução")
dir.create(reg, showWarnings = FALSE)

p1 <- "1 Pesquisas individuais"
p2 <- "2 Agregação dos dados"
p3 <- "3 Produtos finais"

passos <- list(
  # Etapa 1 — pesquisas individuais --------------------------------------------
  "1a"  = file.path(p1, "1a Partidos", "Dirigentes e diretórios",
                    c("1_1_Banco_Dirigentes_Nacional.Rmd", "1_2_Banco_Dirigentes_Estadual.Rmd",
                      "1_3_Banco_Dirigentes_Municipal.Rmd")),
  "1a_financ" = file.path(p1, "1a Partidos", "Financiamento partidário",
                    c("2_1 Financiamento Partidário Nacional - Despesas.Rmd",
                      "2_2 Financiamento Partidário Estadual - Despesas.Rmd",
                      "2_3 Financiamento Partidário Municipal - Despesas.Rmd",
                      "3_1 Financiamento Partidário Nacional - Receitas.Rmd",
                      "3_2 Financiamento Partidário Estadual - Receitas.Rmd",
                      "3_3 Financiamento Partidário Municipal - Receitas.Rmd")),
  "1a_partidos" = file.path(p1, "1a Partidos", "Ideologia, democracia interna e institucionalização",
                    "5 Partidos - Ideologia e Características Estatutárias.Rmd"),
  "2a"  = file.path(p1, "2a Eleições",
                    c("Financiamento eleitoral/6_1 Financiamento Eleitoral Gerais.Rmd",
                      "Financiamento eleitoral/6_2 Financiamento Eleitoral Municipais.Rmd",
                      "Características dos candidatos/7_1 Características Candidatos Gerais.Rmd",
                      "Características dos candidatos/7_2 Características Candidatos Municipais.Rmd",
                      "Bens/8_1 Bens Gerais.Rmd",
                      "Bens/8_2 Bens Municipais.Rmd",
                      "Resultados eleitorais/9_1 Votos e Resultados Gerais.Rmd",
                      "Resultados eleitorais/9_2 Votos e Resultados Municipais.Rmd",
                      "Vagas, comparecimento e eleitorado/10_1 Comparecimento, Vagas e Eleitorado Gerais.Rmd",
                      "Vagas, comparecimento e eleitorado/10_2 Comparecimento, Vagas e Eleitorado Municipais.Rmd")),
  "5a"  = file.path(p1, "5a Validação",
                    c("6_1 Financiamento Eleitoral Gerais validação.Rmd",
                      "6_2 Financiamento Eleitoral Municipais validação.Rmd",
                      "7_1 Características Candidatos Gerais validação.Rmd",
                      "7_2 Características Candidatos Municipais validação.Rmd",
                      "8_1 Bens Gerais validação.Rmd",
                      "8_2 Bens Municipais validação.Rmd",
                      "9_1 Votos e Resultados Gerais validação.Rmd",
                      "9_2 Votos e Resultados Municipais validação.Rmd",
                      "10_1 Comparecimento, Vagas e Eleitorado Gerais validação.Rmd",
                      "10_2 Comparecimento, Vagas e Eleitorado Municipais validação.Rmd")),
  "3a"  = file.path(p1, "3a Socioeconômico", "11 Informações Socioeconômicas.Rmd"),
  # Etapa 2 — agregação ----------------------------------------------------------
  "1b"  = file.path(p2, "1b Partidos",
                    c("4 Junção Financiamento partidário e Direções.R",
                      "5_1 Junção Partidos.R",
                      "13 Junção Partidos e Distritos.R")),
  "2b"  = file.path(p2, "2b Eleições", "12 Junção Candidaturas.R"),
  # Etapa 3 — produtos finais ----------------------------------------------------
  "3"   = file.path(p3, c("14 Produtos finais - Partidos.R",
                          "15 Produtos finais - Candidatos.R",
                          "16 Produtos finais - Distritos.R",
                          "17 Produtos finais - Listas partidárias.R",
                          "18 Dicionários.R"))
)

# Etapas a rodar (na ordem). Para refazer tudo:
# rodar <- c("1a", "1a_financ", "1a_partidos", "2a", "5a", "3a", "1b", "2b", "3")
# (se `rodar` já tiver sido definido antes do source, vale o valor definido)
if (!exists("rodar")) rodar <- c("1b", "2b", "3")


### Execução ---------------------------------------------------------------------

log_geral <- file.path(reg, "execucao.log")

registrar <- function(...) {
  linha <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ...)
  message(linha)
  write(linha, file = log_geral, append = TRUE)
}

rodar_arquivo <- function(arquivo) {
  caminho <- file.path(raiz, arquivo)
  nome    <- tools::file_path_sans_ext(basename(arquivo))
  log     <- file.path(reg, paste0(nome, ".log"))
  inicio  <- Sys.time()
  registrar("INÍCIO ", arquivo)

  # O comando vai para um arquivo temporário (evita problemas de aspas e
  # acentos na linha de comando do Windows)
  expr <- if (grepl("\\.Rmd$", arquivo)) {
    c(sprintf("setwd(%s)", deparse(raiz)),
      "knitr::opts_chunk$set(error = FALSE)",
      sprintf("knitr::knit(%s, output = %s, envir = new.env(), quiet = TRUE)",
              deparse(caminho), deparse(file.path(reg, paste0(nome, ".md")))))
  } else {
    c(sprintf("setwd(%s)", deparse(raiz)),
      sprintf("source(%s, encoding = 'UTF-8', echo = FALSE)", deparse(caminho)))
  }
  script <- tempfile(fileext = ".R")
  writeLines(enc2utf8(expr), script, useBytes = TRUE)
  status <- system2(R, c("--encoding=UTF-8", shQuote(script)), stdout = log, stderr = log)
  unlink(script)

  tempo <- round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1)
  registrar(if (status == 0) "OK     " else "ERRO   ", arquivo, " (", tempo, " min)")
  status == 0
}

for (etapa in rodar) {
  for (arquivo in passos[[etapa]]) {
    ok <- rodar_arquivo(arquivo)
    if (!ok) stop("Erro em ", arquivo, ". Veja o relatório em ", reg)
  }
}
registrar("CONCLUÍDO: ", paste(rodar, collapse = ", "))
