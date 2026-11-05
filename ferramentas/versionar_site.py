"""
Carimba uma nova versão na página (docs/index.html) para o navegador buscar os
arquivos novos em vez de usar cópias guardadas (cache).

Troca a versão em estilo.css?v=..., app.js?v=... e data-versao="..." (que o
app.js acrescenta às buscas dos dados). Rode antes de cada commit que altere a página
ou os dados:  python ferramentas/versionar_site.py
"""
import re, time
from pathlib import Path

idx = Path(__file__).resolve().parent.parent / "docs" / "index.html"
v = time.strftime("%Y%m%d%H%M")
s = idx.read_text(encoding="utf-8")
s2 = re.sub(r'href="estilo\.css(\?v=[^"]*)?"', f'href="estilo.css?v={v}"', s)
s2 = re.sub(r'<script src="app\.js(\?v=[^"]*)?"( data-versao="[^"]*")?>', f'<script src="app.js?v={v}" data-versao="{v}">', s2)
if s2.count(v) != 3:
    raise SystemExit("não encontrei as três marcações de versão em index.html")
idx.write_text(s2, encoding="utf-8", newline="")
print("versão", v)
