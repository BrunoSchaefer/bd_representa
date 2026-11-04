"""
Baixa as malhas do IBGE (API de malhas v3, qualidade mínima) e grava caminhos
SVG já projetados para a página (docs/dados/geo/).

Uso: python gerar_malhas_site.py <pasta_saida>   (ex.: ../docs/dados/geo)

Projeção: equirretangular com correção do cosseno a 15° S; escala 1 unidade =
0,01° (aprox. 1 km), coordenadas inteiras e relativas (comandos 'l') para reduzir o
tamanho dos arquivos.
"""
import gzip, json, math, sys, urllib.request
from pathlib import Path

URL = ("https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR"
       "?formato=application/vnd.geo%2Bjson&qualidade=minima&intrarregiao={}")
LON0, LAT0 = -74.1, 5.4          # canto noroeste do Brasil
K = 100.0                        # unidades por grau (aprox. 1 km)
COS = math.cos(math.radians(15))

UF_SIGLA = {"11": "RO", "12": "AC", "13": "AM", "14": "RR", "15": "PA", "16": "AP", "17": "TO",
            "21": "MA", "22": "PI", "23": "CE", "24": "RN", "25": "PB", "26": "PE", "27": "AL",
            "28": "SE", "29": "BA", "31": "MG", "32": "ES", "33": "RJ", "35": "SP", "41": "PR",
            "42": "SC", "43": "RS", "50": "MS", "51": "MT", "52": "GO", "53": "DF"}


def proj(lon, lat):
    return round((lon - LON0) * COS * K), round((LAT0 - lat) * K)


def caminho(geom, bbox):
    polys = geom["coordinates"] if geom["type"] == "MultiPolygon" else [geom["coordinates"]]
    partes = []
    for poly in polys:
        for anel in poly:
            pts = [proj(*p) for p in anel]
            # remove pontos repetidos após o arredondamento
            limpos = [pts[0]] + [p for a, p in zip(pts, pts[1:]) if p != a]
            if len(limpos) < 3:
                continue
            x0, y0 = limpos[0]
            s = [f"M{x0} {y0}l"]
            px, py = x0, y0
            for x, y in limpos[1:]:
                s.append(f"{x - px} {y - py} ")
                px, py = x, y
                bbox[0] = min(bbox[0], x); bbox[1] = min(bbox[1], y)
                bbox[2] = max(bbox[2], x); bbox[3] = max(bbox[3], y)
            bbox[0] = min(bbox[0], x0); bbox[1] = min(bbox[1], y0)
            bbox[2] = max(bbox[2], x0); bbox[3] = max(bbox[3], y0)
            partes.append("".join(s).rstrip() + "z")
    return "".join(partes)


def baixar(nivel):
    with urllib.request.urlopen(URL.format(nivel), timeout=120) as r:
        dados = r.read()
    if dados[:2] == b"\x1f\x8b":  # a API responde comprimido
        dados = gzip.decompress(dados)
    return json.loads(dados)


out = Path(sys.argv[1]); out.mkdir(parents=True, exist_ok=True)
total = [1e9, 1e9, -1e9, -1e9]

ufs, caixas = {}, {}
for f in baixar("UF")["features"]:
    cod = f["properties"]["codarea"]; b = [1e9, 1e9, -1e9, -1e9]
    ufs[UF_SIGLA[cod]] = caminho(f["geometry"], b)
    caixas[UF_SIGLA[cod]] = b
    total = [min(total[0], b[0]), min(total[1], b[1]), max(total[2], b[2]), max(total[3], b[3])]

muns = {}
for f in baixar("municipio")["features"]:
    cod = f["properties"]["codarea"]
    muns[cod] = caminho(f["geometry"], [1e9, 1e9, -1e9, -1e9])

json.dump({"caixa": total, "caixas_uf": caixas, "ufs": ufs}, open(out / "ufs.json", "w"), separators=(",", ":"))
json.dump(muns, open(out / "municipios.json", "w"), separators=(",", ":"))
print(f"{len(ufs)} UFs, {len(muns)} municípios; caixa {total}")
for n in ("ufs.json", "municipios.json"):
    print(n, round((out / n).stat().st_size / 1e6, 2), "MB")

# nomes dos municípios (API de localidades do IBGE)
with urllib.request.urlopen("https://servicodados.ibge.gov.br/api/v1/localidades/municipios?view=nivelado", timeout=120) as r:
    dados = r.read()
if dados[:2] == b"\x1f\x8b":
    dados = gzip.decompress(dados)
nomes = {str(m["municipio-id"]): f'{m["municipio-nome"]} ({m["UF-sigla"]})' for m in json.loads(dados)}
json.dump(nomes, open(out / "nomes.json", "w", encoding="utf-8"), ensure_ascii=False, separators=(",", ":"))
print("nomes:", len(nomes), "| sem nome:", len(set(muns) - set(nomes)))
