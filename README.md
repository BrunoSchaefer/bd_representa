# bd_representa

**Conjunto de bancos de dados sobre partidos políticos e eleições no Brasil**, com foco em desigualdade de gênero e raça na representação política.

Pesquisa: *Desigualdade de Gênero e Partidos* — Bruno Schaefer (IESP-UERJ, [MAPE — Laboratório de Monitoramento e Avaliação de Políticas e Eleições](https://mape.org.br/)).

🔎 **Visualização pública:** <https://brunoschaefer.github.io/bd_representa/>
📦 **Download dos bancos:** [Releases](https://github.com/BrunoSchaefer/bd_representa/releases/latest)

---

## O que há aqui

| Dimensão | Unidade de análise | Bancos (produtos finais) |
|---|---|---|
| Partidos | Dirigente | `bd_dirigentes_nacional`, `bd_dirigentes_estadual`, `bd_dirigentes_municipal_<ano>` (2015–2025) |
| Partidos | Diretório / órgão | `bd_diretorios_nacional`, `bd_diretorios_estadual`, `bd_diretorios_municipal` |
| Eleições | Candidatura | `bd_candidatos_governadoria`, `_senado`, `_camara`, `_assembleia` (2010–2026), `_prefeitura`, `_vereanca` (2012–2024) |
| Eleições | Lista partidária | `bd_partidos_camara`, `bd_partidos_assembleias` (2010–2026), `bd_partidos_vereanca` (2012–2024) |
| Distritos | UF / município | `bd_distritos_estadual` (2010–2026), `bd_distritos_municipal` (2012–2024) |

**Eleições de 2026:** incluídas a partir da Base dos Dados (TSE), 1º turno completo. Ainda não há dados de direções partidárias de 2026, então as variáveis de diretório e dirigentes ficam vazias nas candidaturas e listas de 2026. Os valores monetários de 2026 foram deflacionados a partir do IPCA de 09/2026.

### Visualização

A [página pública](https://brunoschaefer.github.io/bd_representa/) traz:

- **Direções partidárias:** mulheres nas direções nacional, estadual e municipal (2015–2025).
- **Diretórios partidários:** número de diretórios por ano e esfera (nacional, estadual, municipal) e por tipo de órgão (definitivo, provisório, comissão interventora); tipo de órgão por partido; e mapa do percentual de mulheres entre todos os membros, na presidência ou na presidência e tesouraria dos diretórios, por UF ou município, partido e ano.
- **Candidaturas e eleição:** mulheres e pessoas negras entre candidaturas e eleitos, por cargo.
- **Mapa da equidade nas listas partidárias:** o índice DRI por município (vereança) ou UF (deputado estadual/distrital e federal), por ano, grupo (mulheres ou pessoas negras) e medida (candidaturas, eleitos, votos, receitas, recursos públicos), para todas as listas ou para um partido.
- **Comparação entre partidos** e **downloads**.

O DRI vale `(b − p) / √((b − p)² + b(1 − b))`, em que *b* é a participação do grupo e *p* o seu peso no eleitorado (gênero) ou na população (raça). Vai de −1 (grupo ausente) a 1, e 0 indica paridade.

Cada banco tem um dicionário de variáveis em [`3 Produtos finais/dicionarios`](3%20Produtos%20finais/dicionarios) (`.xlsx` e `.csv`).

Os **bancos intermediários** (etapas 1 e 2 do pipeline: `4a Produtos` e `2 Agregação dos dados/*/bancos`) também são publicados na release, para quem quiser auditar ou reaproveitar etapas do processo.

## Como baixar

Os arquivos de dados são grandes demais para o Git (vários têm mais de 1 GB) e por isso ficam anexados às **Releases** do repositório, comprimidos em `.csv.gz`.

```r
# R
url <- "https://github.com/BrunoSchaefer/bd_representa/releases/latest/download/bd_candidatos_camara.csv.gz"
cand <- data.table::fread(url, sep = ";", encoding = "UTF-8")
```

```python
# Python
import pandas as pd
url = "https://github.com/BrunoSchaefer/bd_representa/releases/latest/download/bd_candidatos_camara.csv.gz"
cand = pd.read_csv(url, sep=";")
```

Ou pela linha de comando: `gh release download -R BrunoSchaefer/bd_representa -p "bd_candidatos_*"`.

O arquivo `MANIFESTO.csv` da release lista todos os arquivos, a pasta de origem, o número de linhas e o hash SHA-256.

## Dados pessoais (LGPD)

Os dados vêm de fontes públicas (Portal de Dados Abertos do TSE, Base dos Dados), mas a versão publicada aqui foi **pseudonimizada**:

| Coluna original | Tratamento na versão pública |
|---|---|
| CPF (`cpf`, `NR_CPF_*`) | substituído por um identificador pseudônimo (`*_hash`, HMAC-SHA256 truncado) |
| Título eleitoral (`titulo_eleitoral`, `NR_TITULO_ELEITORAL_*`) | substituído por um identificador pseudônimo (`*_hash`) |
| E-mail | removido |
| Data de nascimento | reduzida ao ano (`ano_nascimento`); `idade` é mantida |

O mesmo CPF ou título gera sempre o mesmo pseudônimo em **todos** os bancos. Por isso ainda dá para cruzar dirigentes e candidaturas e acompanhar uma pessoa ao longo dos anos, mas não dá para recuperar o documento original. A chave secreta usada no hash não é publicada.

Os **dados brutos** do TSE (~40 GB) não são redistribuídos. Os códigos da etapa 1 indicam de onde cada arquivo é baixado ([dadosabertos.tse.jus.br](https://dadosabertos.tse.jus.br/) e [basedosdados.org](https://basedosdados.org/)).

## Estrutura do pipeline

```
0 Funções, estrutura e passo-a-passo/   funções auxiliares, pastas, ordem de execução, logs
1 Pesquisas individuais/                Etapa 1 – coleta, filtragem e categorização (por tema)
   1a Partidos  2a Eleições  3a Socioeconômico  5a Validação
2 Agregação dos dados/                  Etapa 2 – junção dos bancos
3 Produtos finais/                      Etapa 3 – bancos finais e dicionários
4 Análises exploratórias/               análises com os bancos finais (bd_representa_analise.R) e seus resultados (Figuras e tabelas/)
docs/                                   página pública de visualização (GitHub Pages)
ferramentas/                            scripts de publicação (pseudonimização, agregados do site)
```

Para reproduzir: abra `Dirigentes Partidários.Rproj` no RStudio e rode `0 Funções, estrutura e passo-a-passo/0_rodar_etapas.R`. Requisitos e ordem das etapas estão em [`0 Estrutura de Montagem do Banco de Dados.Rmd`](0%20Fun%C3%A7%C3%B5es%2C%20estrutura%20e%20passo-a-passo/0%20Estrutura%20de%20Montagem%20do%20Banco%20de%20Dados.Rmd).

## Como citar

> Schaefer, Bruno. *bd_representa: conjunto de bancos de dados sobre partidos e eleições no Brasil*. IESP-UERJ, MAPE (Laboratório de Monitoramento e Avaliação de Políticas e Eleições), 2026. Disponível em: <https://github.com/BrunoSchaefer/bd_representa>.

No GitHub, o botão **"Cite this repository"** (barra lateral) também traz a referência, gerada a partir do arquivo `CITATION.cff`.

## Licença

Código: MIT. Dados: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.pt_BR). Basta citar a fonte.
