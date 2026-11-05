"""
Dados da seção "Relações" da página (docs/dados/relacoes/), a partir dos bancos
públicos (.csv.gz). Reproduz as bases montadas em
"4 Análises exploratórias/bd_representa_analise.R" (análises 3.1, 3.2, 3.3, F/G,
L, N/O, Q e S), uma linha por unidade de análise, com as variáveis de cada relação.

Uso: python gerar_relacoes_site.py <pasta_com_csv.gz> <pasta_site/dados>

Formato: relacoes.json (manifesto) e um arquivo por relação-recorte (ou
relação-recorte-ano, nos recortes grandes), em colunas:
  {"n": N, "cols": {"ano": [...], "p": [...], "uf": [...], "<var>": [...]}}
"p" e "uf" são índices nas listas "partidos" e "ufs" do manifesto.
"""
import json, math, sys
from pathlib import Path
import numpy as np
import pandas as pd

src, out = Path(sys.argv[1]), Path(sys.argv[2]) / "relacoes"
out.mkdir(parents=True, exist_ok=True)

UF_IBGE = {"11": "RO", "12": "AC", "13": "AM", "14": "RR", "15": "PA", "16": "AP", "17": "TO", "21": "MA",
           "22": "PI", "23": "CE", "24": "RN", "25": "PB", "26": "PE", "27": "AL", "28": "SE", "29": "BA",
           "31": "MG", "32": "ES", "33": "RJ", "35": "SP", "41": "PR", "42": "SC", "43": "RS", "50": "MS",
           "51": "MT", "52": "GO", "53": "DF"}
UFS = sorted(UF_IBGE.values()) + ["BR"]


def ler(nome, cols):
    return pd.read_csv(src / f"{nome}.csv.gz", sep=";", usecols=lambda c: c in cols, dtype=str, low_memory=False)


def num(s):
    return pd.to_numeric(s, errors="coerce")


def inteiro(s):
    return num(s.str.replace(r"\.0$", "", regex=True)).astype("Int64")


PARTIDOS = set()
MANIFESTO = []


def gravar(df, nome, variaveis):
    """df com colunas ano, partido, uf e as variáveis; grava em colunas."""
    df = df.reset_index(drop=True)
    PARTIDOS.update(df["partido"].dropna().unique())
    cols = {"ano": df["ano"].astype(int).tolist(), "p": df["partido"].fillna("?").tolist(),
            "uf": df["uf"].fillna("BR").tolist()}
    for v in variaveis:
        x = num(df[v]).astype(float)
        cols[v] = [None if (math.isnan(a) or math.isinf(a)) else round(a, 4) for a in x]
    json.dump({"n": len(df), "cols": cols}, open(out / f"{nome}.json", "w", encoding="utf-8"),
              ensure_ascii=False, separators=(",", ":"), allow_nan=False)
    return len(df)


def registrar(rel, recortes, dados):
    """dados: {recorte: df}; grava (por ano, se o recorte tiver por_ano) e registra no manifesto."""
    vs = list(rel["variaveis"])
    for r in recortes:
        df = dados[r["id"]]
        anos = sorted(df["ano"].dropna().astype(int).unique().tolist())
        r["anos"] = anos
        if r.get("por_ano"):
            r["n"] = {a: gravar(df[df["ano"] == a], f"{rel['id']}_{r['id']}_{a}", vs) for a in anos}
        else:
            r["n"] = gravar(df, f"{rel['id']}_{r['id']}", vs)
        print(f"  {rel['id']}/{r['id']}: {len(df)} linhas, anos {anos}")
    rel["recortes"] = recortes
    MANIFESTO.append(rel)


# ------------------------------------------------------------------ bancos comuns
COMP = ["total_membros", "proporcao_mulheres", "proporcao_mulheres_decisao", "tem_mulher_presidente",
        "tem_mulher_tesoureiro", "tem_mulher_decisao"]
TURN = ["n_membros_t1", "turnover_prop", "turnover_prop_decisao"]
DESP = ["desp_valor_total_bruto", "desp_valor_aparato_pessoal", "desp_valor_aparato_sedes",
        "desp_valor_aparato_politica", "desp_valor_eleicoes_comunicacao", "desp_valor_eleicoes_publicidade",
        "desp_valor_transferencias"]

dir_nac = ler("bd_diretorios_nacional", ["ANO", "SG_PARTIDO_PAD", "bls_ideologia", "bolognesi_ideologia",
                                         "salgado_democracia_interna", "ribeiro_ipd"] + DESP + TURN)
dir_est = ler("bd_diretorios_estadual", ["ANO", "SG_PARTIDO_PAD", "SG_UF"] + COMP + DESP + TURN)
dir_mun = ler("bd_diretorios_municipal", ["ANO", "SG_PARTIDO_PAD", "SG_UF", "CD_MUNICIPIO"] + COMP + DESP + TURN)
for d in (dir_nac, dir_est, dir_mun):
    d["ANO"] = d["ANO"].astype(int)
dir_mun["CD_MUNICIPIO"] = inteiro(dir_mun["CD_MUNICIPIO"])
dir_nac = dir_nac.drop_duplicates(["ANO", "SG_PARTIDO_PAD"])
dir_est = dir_est.drop_duplicates(["ANO", "SG_PARTIDO_PAD", "SG_UF"])
dir_mun = dir_mun.drop_duplicates(["ANO", "SG_PARTIDO_PAD", "CD_MUNICIPIO"])

LISTA = ["ano", "SG_PARTIDO_PAD", "pct_votos_nominais_distrito", "pct_cadeiras", "n_eleitos",
         "prop_candidatos_mulheres", "prop_receitas_mulheres", "prop_receitas_partidarias_mulheres",
         "prop_receitas_direcao_mulheres", "diretorio_rec_valor_total_bruto"]
part_ver = ler("bd_partidos_vereanca", LISTA + ["sigla_uf", "id_municipio_tse"])
part_ass = ler("bd_partidos_assembleias", LISTA + ["sigla_uf"])
part_cam = ler("bd_partidos_camara", LISTA + ["sigla_uf"])
for d in (part_ver, part_ass, part_cam):
    d["ano"] = d["ano"].astype(int)
part_ver["CD_MUNICIPIO"] = inteiro(part_ver["id_municipio_tse"])

# ------------------------------------------------------------------ 3.1
print("3.1")
m = dir_mun.copy()
m["ano_eleicao"] = 2012 + 4 * ((m["ANO"] - 2013) // 4)
m = m.merge(part_ver[["ano", "SG_PARTIDO_PAD", "CD_MUNICIPIO", "pct_votos_nominais_distrito"]]
            .rename(columns={"ano": "ano_eleicao"}), on=["ano_eleicao", "SG_PARTIDO_PAD", "CD_MUNICIPIO"], how="left")
e = dir_est.copy()
e["ano_eleicao"] = 2010 + 4 * ((e["ANO"] - 2011) // 4)
e = e.merge(part_ass[["ano", "SG_PARTIDO_PAD", "sigla_uf", "pct_votos_nominais_distrito"]]
            .rename(columns={"ano": "ano_eleicao", "sigla_uf": "SG_UF"}), on=["ano_eleicao", "SG_PARTIDO_PAD", "SG_UF"], how="left")


def base31(d):
    return pd.DataFrame({"ano": d["ANO"], "partido": d["SG_PARTIDO_PAD"], "uf": d["SG_UF"],
                         "votos_ant": num(d["pct_votos_nominais_distrito"]).fillna(0),  # sem lista = 0%
                         "membros": num(d["total_membros"]),
                         "prop_mulheres": 100 * num(d["proporcao_mulheres"]),
                         "prop_decisao": 100 * num(d["proporcao_mulheres_decisao"]),
                         "mulher_pres": num(d["tem_mulher_presidente"]),
                         "mulher_tes": num(d["tem_mulher_tesoureiro"])})


registrar({
    "id": "r31", "analise": "3.1", "titulo": "Votação do partido e mulheres na direção",
    "pergunta": "Partidos com mais votos no distrito têm mais mulheres na direção?",
    "unidade": "diretório (partido no distrito, no ano)",
    "variaveis": {"votos_ant": {"rot": "Votação nominal da lista na eleição anterior (% do distrito)", "tipo": "pct"},
                  "membros": {"rot": "Membros da direção", "tipo": "n"},
                  "prop_mulheres": {"rot": "Mulheres na direção (%)", "tipo": "pct"},
                  "prop_decisao": {"rot": "Mulheres em cargos de decisão (%)", "tipo": "pct"},
                  "mulher_pres": {"rot": "Mulher na presidência (0/1)", "tipo": "bin"},
                  "mulher_tes": {"rot": "Mulher na tesouraria (0/1)", "tipo": "bin"}},
    "x": "votos_ant", "y": "prop_mulheres",
    "nota": "A votação é a da eleição anterior ao ano da direção (vereança para os diretórios municipais; Assembleia para os estaduais). Partido sem lista na eleição anterior = 0%.",
}, [{"id": "municipal", "rot": "Diretórios municipais × vereança", "por_ano": True},
    {"id": "estadual", "rot": "Diretórios estaduais × Assembleia"}],
   {"municipal": base31(m), "estadual": base31(e)})

# ------------------------------------------------------------------ 3.2
print("3.2")
V32 = ["prop_candidatos_mulheres", "prop_receitas_mulheres", "prop_receitas_partidarias_mulheres", "prop_receitas_direcao_mulheres"]
lv = part_ver[part_ver["ano"].isin([2016, 2020, 2024])].merge(
    dir_mun[["ANO", "SG_PARTIDO_PAD", "CD_MUNICIPIO", "tem_mulher_presidente", "tem_mulher_decisao"]]
    .assign(ano=lambda x: x["ANO"] + 1).drop(columns="ANO"), on=["ano", "SG_PARTIDO_PAD", "CD_MUNICIPIO"])
dir_est_ant = dir_est[["ANO", "SG_PARTIDO_PAD", "SG_UF", "tem_mulher_presidente", "tem_mulher_decisao"]] \
    .assign(ano=lambda x: x["ANO"] + 1).drop(columns="ANO").rename(columns={"SG_UF": "sigla_uf"})
la = part_ass[part_ass["ano"].isin([2018, 2022])].merge(dir_est_ant, on=["ano", "SG_PARTIDO_PAD", "sigla_uf"])
lc = part_cam[part_cam["ano"].isin([2018, 2022])].merge(dir_est_ant, on=["ano", "SG_PARTIDO_PAD", "sigla_uf"])


def base32(d):
    return pd.DataFrame({"ano": d["ano"], "partido": d["SG_PARTIDO_PAD"], "uf": d["sigla_uf"],
                         "mulher_pres_ant": num(d["tem_mulher_presidente"]),
                         "mulher_dec_ant": num(d["tem_mulher_decisao"]),
                         "cand_m": 100 * num(d["prop_candidatos_mulheres"]),
                         "rec_m": 100 * num(d["prop_receitas_mulheres"]),
                         "rec_part_m": 100 * num(d["prop_receitas_partidarias_mulheres"]),
                         "rec_dir_m": 100 * num(d["prop_receitas_direcao_mulheres"])})


registrar({
    "id": "r32", "analise": "3.2", "titulo": "Mulher na direção e recursos para candidatas",
    "pergunta": "Listas de partidos com mulher na direção destinam mais recursos às candidatas?",
    "unidade": "lista partidária (partido no distrito, na eleição)",
    "variaveis": {"mulher_pres_ant": {"rot": "Mulher na presidência do diretório no ano anterior (0/1)", "tipo": "bin"},
                  "mulher_dec_ant": {"rot": "Mulher em cargo de decisão do diretório no ano anterior (0/1)", "tipo": "bin"},
                  "cand_m": {"rot": "Mulheres entre as candidaturas da lista (%)", "tipo": "pct"},
                  "rec_m": {"rot": "Receitas da lista destinadas a mulheres (%)", "tipo": "pct"},
                  "rec_part_m": {"rot": "Recursos de partidos destinados a mulheres (%)", "tipo": "pct"},
                  "rec_dir_m": {"rot": "Recursos de direções destinados a mulheres (%)", "tipo": "pct"}},
    "x": "mulher_pres_ant", "y": "rec_m", "controle": "cand_m",
    "nota": "Direção medida no ano anterior à eleição (2015→2016 … 2023→2024). Os diretórios começam em 2015, então as eleições são 2016–2024 (municipais) e 2018–2022 (gerais).",
}, [{"id": "vereanca", "rot": "Vereança (diretórios municipais)", "por_ano": True},
    {"id": "assembleia", "rot": "Dep. estadual/distrital (diretórios estaduais)"},
    {"id": "camara", "rot": "Dep. federal (diretórios estaduais)"}],
   {"vereanca": base32(lv), "assembleia": base32(la), "camara": base32(lc)})

# ------------------------------------------------------------------ 3.3
print("3.3")
MED = ["candidatos", "receitas", "receitas_partidarias", "eleitos"]
MED_ROT = {"candidatos": "candidaturas", "receitas": "receitas", "receitas_partidarias": "receitas partidárias", "eleitos": "eleitos"}


def base33(d, cargo, uf):
    out_ = pd.DataFrame({"ano": num(d["ano"]).astype(int), "partido": "?", "uf": uf})
    pib = num(d["pib_per_capita"])
    out_["log_pib"] = np.log(pib.where(pib > 0))
    out_["pib_mil"] = pib / 1000
    for g, gk in (("genero", "g"), ("racial", "r")):
        for med in MED:
            out_[f"{gk}_{med}"] = num(d.get(f"indice_{g}_{med}_{cargo}"))
    return out_


vars33 = {"log_pib": {"rot": "log do PIB per capita", "tipo": "n"},
          "pib_mil": {"rot": "PIB per capita (R$ mil de dez/2025)", "tipo": "n"}}
for g, gk, grot in (("genero", "g", "gênero (mulheres)"), ("racial", "r", "raça (pessoas negras)")):
    for med in MED:
        vars33[f"{gk}_{med}"] = {"rot": f"DRI de {grot}: {MED_ROT[med]}", "tipo": "dri"}
cols_m = ["ano", "id_municipio", "pib_per_capita"] + [f"indice_{g}_{m_}_vereanca" for g in ("genero", "racial") for m_ in MED]
cols_e = ["ano", "sigla_uf", "pib_per_capita"] + [f"indice_{g}_{m_}_{c}" for g in ("genero", "racial") for m_ in MED for c in ("camara", "assembleia")]
dm = ler("bd_distritos_municipal", cols_m)
de = ler("bd_distritos_estadual", cols_e)
registrar({
    "id": "r33", "analise": "3.3", "titulo": "PIB per capita e índices de equidade",
    "pergunta": "Distritos mais ricos têm listas e bancadas mais igualitárias?",
    "unidade": "distrito (município ou UF, na eleição)",
    "variaveis": vars33, "x": "log_pib", "y": "g_eleitos",
    "nota": "DRI: −1 = grupo ausente; 0 = paridade com o eleitorado (gênero) ou a população (raça). O PIB municipal só vai até 2023 (eleições até 2020).",
}, [{"id": "vereanca", "rot": "Municípios (vereança)"},
    {"id": "assembleia", "rot": "UFs (Assembleia)"},
    {"id": "camara", "rot": "UFs (Câmara)"}],
   {"vereanca": base33(dm, "vereanca", dm["id_municipio"].str[:2].map(UF_IBGE)),
    "assembleia": base33(de, "assembleia", de["sigla_uf"]),
    "camara": base33(de, "camara", de["sigla_uf"])})

# ------------------------------------------------------------------ F/G
print("F/G")
fg = pd.DataFrame({"ano": dir_nac["ANO"], "partido": dir_nac["SG_PARTIDO_PAD"], "uf": "BR",
                   "bls": num(dir_nac["bls_ideologia"]), "bolognesi": num(dir_nac["bolognesi_ideologia"]),
                   "salgado": num(dir_nac["salgado_democracia_interna"]), "ribeiro": num(dir_nac["ribeiro_ipd"])})
registrar({
    "id": "rfg", "analise": "F e G", "titulo": "Classificações de ideologia e de democracia interna",
    "pergunta": "As classificações da literatura concordam entre si?",
    "unidade": "partido-ano (direção nacional)",
    "variaveis": {"bls": {"rot": "Ideologia BLS (−1 esquerda a 1 direita)", "tipo": "n"},
                  "bolognesi": {"rot": "Ideologia Bolognesi et al. (0 a 10)", "tipo": "n"},
                  "salgado": {"rot": "Democracia interna (Salgado)", "tipo": "n"},
                  "ribeiro": {"rot": "IPD (Ribeiro, Locatelli e Assis)", "tipo": "n"}},
    "x": "bls", "y": "bolognesi",
    "nota": "As classificações são atribuídas a cada ano pela onda mais recente; partidos repetem-se entre anos.",
}, [{"id": "nacional", "rot": "Partidos (direção nacional)"}], {"nacional": fg})

# ------------------------------------------------------------------ L
print("L")
esf = []
for nome, d, uf in (("nacional", dir_nac, None), ("estadual", dir_est, "SG_UF"), ("municipal", dir_mun, "SG_UF")):
    x = d[d["ANO"] >= 2017][["ANO", "SG_PARTIDO_PAD"] + DESP].copy()
    for c in DESP:
        x[c] = num(x[c])
    x = x.groupby(["ANO", "SG_PARTIDO_PAD"], as_index=False)[DESP].sum(min_count=0)
    x["esfera"] = nome
    esf.append(x)
dl = pd.concat(esf)
dl = dl[dl["desp_valor_total_bruto"] > 0]
t = dl["desp_valor_total_bruto"]
dl = dl.assign(
    pct_aparato=100 * (dl["desp_valor_aparato_pessoal"] + dl["desp_valor_aparato_sedes"] + dl["desp_valor_aparato_politica"]) / t,
    pct_pessoal=100 * dl["desp_valor_aparato_pessoal"] / t,
    pct_eleicoes=100 * (dl["desp_valor_eleicoes_comunicacao"] + dl["desp_valor_eleicoes_publicidade"]) / t,
    pct_transferencias=100 * dl["desp_valor_transferencias"] / t,
    desp_mi=t / 1e6)
dl = dl.merge(dir_nac[["ANO", "SG_PARTIDO_PAD", "bls_ideologia"]], on=["ANO", "SG_PARTIDO_PAD"])
dl = dl.rename(columns={"ANO": "ano", "SG_PARTIDO_PAD": "partido"}).assign(uf="BR", bls=lambda x: num(x["bls_ideologia"]))
VL = ["bls", "pct_aparato", "pct_pessoal", "pct_eleicoes", "pct_transferencias", "desp_mi"]
registrar({
    "id": "rl", "analise": "L", "titulo": "Ideologia e composição das despesas partidárias",
    "pergunta": "Partidos à direita ou à esquerda gastam de forma diferente?",
    "unidade": "partido-ano (soma dos órgãos da esfera)",
    "variaveis": {"bls": {"rot": "Ideologia BLS (−1 esquerda a 1 direita)", "tipo": "n"},
                  "pct_aparato": {"rot": "Despesas com aparato (%)", "tipo": "pct"},
                  "pct_pessoal": {"rot": "Despesas com pessoal (%)", "tipo": "pct"},
                  "pct_eleicoes": {"rot": "Despesas com eleições: comunicação e publicidade (%)", "tipo": "pct"},
                  "pct_transferencias": {"rot": "Transferências (%)", "tipo": "pct"},
                  "desp_mi": {"rot": "Despesas totais (R$ milhões de dez/2025)", "tipo": "n"}},
    "x": "bls", "y": "pct_aparato",
    "nota": "Despesas de 2017 a 2025, somadas por partido em cada esfera; ideologia nacional do partido no ano.",
}, [{"id": e_, "rot": f"Esfera {e_}"} for e_ in ("nacional", "estadual", "municipal")],
   {e_: dl[dl["esfera"] == e_] for e_ in ("nacional", "estadual", "municipal")})

# ------------------------------------------------------------------ N/O
print("N/O")


def base_no(d, chave):
    x = d[d["diretorio_rec_valor_total_bruto"].notna()].copy()
    x["rec"] = num(x["diretorio_rec_valor_total_bruto"])
    soma = x.groupby(["ano", chave], dropna=False)["rec"].transform("sum")
    x["pct_recursos"] = (100 * x["rec"] / soma).where(soma > 0)
    x = x[x["pct_recursos"].notna()]
    return pd.DataFrame({"ano": x["ano"], "partido": x["SG_PARTIDO_PAD"], "uf": x["sigla_uf"],
                         "pct_cadeiras": num(x["pct_cadeiras"]), "pct_votos": num(x["pct_votos_nominais_distrito"]),
                         "pct_recursos": x["pct_recursos"]})


registrar({
    "id": "rno", "analise": "N e O", "titulo": "Bancada eleita e recursos da direção partidária",
    "pergunta": "Partidos que elegem mais têm diretórios com mais recursos no distrito?",
    "unidade": "lista partidária (partido no distrito, na eleição)",
    "variaveis": {"pct_cadeiras": {"rot": "Cadeiras do distrito obtidas pelo partido (%)", "tipo": "pct"},
                  "pct_votos": {"rot": "Votos nominais do partido no distrito (%)", "tipo": "pct"},
                  "pct_recursos": {"rot": "Parcela das receitas dos diretórios do distrito (%)", "tipo": "pct"}},
    "x": "pct_cadeiras", "y": "pct_recursos", "diagonal": True,
    "nota": "Receitas do diretório no ano da eleição (financiamento partidário a partir de 2017). Linha tracejada: proporcionalidade.",
}, [{"id": "vereanca", "rot": "Vereança (municípios)", "por_ano": True},
    {"id": "assembleia", "rot": "Assembleia (UFs)"}],
   {"vereanca": base_no(part_ver, "CD_MUNICIPIO"), "assembleia": base_no(part_ass[part_ass["ano"] < 2026], "sigla_uf")})

# ------------------------------------------------------------------ Q
print("Q")
dq = {}
for k in ("camara", "assembleia", "senado", "governadoria", "prefeitura"):
    cols = ["ano", "sigla_uf", "SG_PARTIDO_PAD", "despesas_internet_can", "pct_votos_distrito"] + (["id_municipio"] if k == "prefeitura" else [])
    c = ler(f"bd_candidatos_{k}", cols)
    c["ano"] = c["ano"].astype(int)
    c["internet"] = num(c["despesas_internet_can"]).fillna(0)
    c["distrito"] = c["id_municipio"] if k == "prefeitura" else c["sigla_uf"]
    c["tot"] = c.groupby(["ano", "distrito"], dropna=False)["internet"].transform("sum")
    c = c[(c["tot"] > 0) & num(c["pct_votos_distrito"]).notna()]
    dq[k] = pd.DataFrame({"ano": c["ano"], "partido": c["SG_PARTIDO_PAD"], "uf": c["sigla_uf"],
                          "pct_internet": 100 * c["internet"] / c["tot"], "pct_votos": num(c["pct_votos_distrito"]),
                          "internet_mil": c["internet"] / 1000})
registrar({
    "id": "rq", "analise": "Q", "titulo": "Gasto com internet e votação",
    "pergunta": "Quem concentra o gasto com internet do distrito tem mais votos?",
    "unidade": "candidatura",
    "variaveis": {"pct_internet": {"rot": "Parcela do gasto com internet do distrito (%)", "tipo": "pct"},
                  "pct_votos": {"rot": "Votos no distrito (%)", "tipo": "pct"},
                  "internet_mil": {"rot": "Gasto com internet (R$ mil de dez/2025)", "tipo": "n"}},
    "x": "pct_internet", "y": "pct_votos",
    "nota": "Só distritos com algum gasto com internet. A vereança (1,75 milhão de candidaturas) fica fora da página pelo volume; o código gerado funciona também para ela.",
}, [{"id": "camara", "rot": "Dep. federal"}, {"id": "assembleia", "rot": "Dep. estadual/distrital"},
    {"id": "senado", "rot": "Senado"}, {"id": "governadoria", "rot": "Governo estadual"},
    {"id": "prefeitura", "rot": "Prefeitura", "por_ano": True}], dq)

# ------------------------------------------------------------------ S
print("S")


def base_s(d, anos, lista, chave_dir, chave_lista):
    x = d[d["ANO"].isin(anos)].copy()
    x = x[num(x["turnover_prop"]).notna() & (num(x["n_membros_t1"]) > 0)]
    l_ = lista[["ano", "SG_PARTIDO_PAD", chave_lista, "n_eleitos"]].assign(ANO=lambda z: z["ano"] + 1).drop(columns="ano")
    if chave_dir != chave_lista:
        l_ = l_.rename(columns={chave_lista: chave_dir})
    x = x.merge(l_, on=["ANO", "SG_PARTIDO_PAD", chave_dir], how="left")
    ne = num(x["n_eleitos"])
    return pd.DataFrame({"ano": x["ANO"], "partido": x["SG_PARTIDO_PAD"], "uf": x["SG_UF"],
                         "sem_eleitos": (ne.isna() | (ne == 0)).astype(int),  # sem lista ou lista sem eleitos
                         "n_eleitos": ne.fillna(0),
                         "turnover": 100 * num(x["turnover_prop"]), "turnover_dec": 100 * num(x["turnover_prop_decisao"]),
                         "membros_t1": num(x["n_membros_t1"])})


registrar({
    "id": "rs", "analise": "S", "titulo": "Desempenho eleitoral e rotatividade da direção",
    "pergunta": "Direções de partidos que não elegeram ninguém se renovam mais no ano seguinte?",
    "unidade": "diretório (partido no distrito, no ano seguinte à eleição)",
    "variaveis": {"sem_eleitos": {"rot": "Não elegeu ninguém na eleição anterior (0/1)", "tipo": "bin"},
                  "n_eleitos": {"rot": "Eleitos na eleição anterior", "tipo": "n"},
                  "turnover": {"rot": "Rotatividade da direção (%)", "tipo": "pct"},
                  "turnover_dec": {"rot": "Rotatividade dos cargos de decisão (%)", "tipo": "pct"},
                  "membros_t1": {"rot": "Membros da direção no ano anterior", "tipo": "n"}},
    "x": "sem_eleitos", "y": "turnover",
    "nota": "Rotatividade = entradas e saídas sobre as pessoas presentes em algum dos dois anos. Municipal: vereança 2016, 2020, 2024 → direção 2017, 2021, 2025; estadual: Assembleia 2018, 2022 → direção 2019, 2023.",
}, [{"id": "municipal", "rot": "Diretórios municipais (após a vereança)", "por_ano": True},
    {"id": "estadual", "rot": "Diretórios estaduais (após a Assembleia)"}],
   {"municipal": base_s(dir_mun, [2017, 2021, 2025], part_ver, "CD_MUNICIPIO", "CD_MUNICIPIO"),
    "estadual": base_s(dir_est, [2019, 2023], part_ass, "SG_UF", "sigla_uf")})

# ------------------------------------------------------------------ manifesto
partidos = sorted(p for p in PARTIDOS if p != "?") + ["?"]
pidx = {p: i for i, p in enumerate(partidos)}
uidx = {u: i for i, u in enumerate(UFS)}
for f in out.glob("r*.json"):  # troca siglas por índices
    d = json.load(open(f, encoding="utf-8"))
    d["cols"]["p"] = [pidx.get(p, pidx["?"]) for p in d["cols"]["p"]]
    d["cols"]["uf"] = [uidx.get(u, uidx["BR"]) for u in d["cols"]["uf"]]
    json.dump(d, open(f, "w", encoding="utf-8"), ensure_ascii=False, separators=(",", ":"), allow_nan=False)
json.dump({"partidos": partidos, "ufs": UFS, "relacoes": MANIFESTO},
          open(out / "relacoes.json", "w", encoding="utf-8"), ensure_ascii=False, indent=1, default=int)
print("ok:", len(MANIFESTO), "relações")
