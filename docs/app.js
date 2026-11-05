/* bd_representa — página de visualização (sem dependências) */
(() => {
  const REPO = "https://github.com/BrunoSchaefer/bd_representa";
  const NIVEIS = [
    { k: "nacional", rotulo: "Nacional", cor: "--s1" },
    { k: "estadual", rotulo: "Estadual", cor: "--s2" },
    { k: "municipal", rotulo: "Municipal", cor: "--s3" },
  ];
  const METRICAS = {
    membros: ["mulheres", "membros", "dos membros"],
    decisao: ["mulheres_decisao", "decisao", "dos cargos de presidência e tesouraria"],
    presidentes: ["mulheres_presidentes", "presidentes", "das presidências"],
  };
  const UFS_NOMES = {AC:"Acre",AL:"Alagoas",AM:"Amazonas",AP:"Amapá",BA:"Bahia",CE:"Ceará",DF:"Distrito Federal",ES:"Espírito Santo",GO:"Goiás",MA:"Maranhão",MG:"Minas Gerais",MS:"Mato Grosso do Sul",MT:"Mato Grosso",PA:"Pará",PB:"Paraíba",PE:"Pernambuco",PI:"Piauí",PR:"Paraná",RJ:"Rio de Janeiro",RN:"Rio Grande do Norte",RO:"Rondônia",RR:"Roraima",RS:"Rio Grande do Sul",SC:"Santa Catarina",SE:"Sergipe",SP:"São Paulo",TO:"Tocantins"};

  const $ = (s) => document.querySelector(s);
  const css = (v) => getComputedStyle(document.documentElement).getPropertyValue(v).trim();
  const pct = (x, d = 1) => (x == null || !isFinite(x)) ? "–" : (x * 100).toLocaleString("pt-BR", { minimumFractionDigits: d, maximumFractionDigits: d }) + "%";
  const int = (x) => Math.round(x).toLocaleString("pt-BR");
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));

  const estado = { partido: "", uf: "", dMetrica: "membros", cCargo: "Vereança", cGrupo: "mulheres", pNivel: "municipal", pAno: null, aGrupo: "finais",
                   mCargo: "vereanca", mAno: null, mMedida: "candidaturas", mGrupo: "mulheres",
                   pDim: "direcoes", pCargo: "vereanca", pMedida: "receitas", pGrupo: "mulheres",
                   dtEscala: "abs", tpNivel: "municipal", tpAno: null, mdNivel: "municipal", mdAno: null, mdCargo: "presidencia",
                   rRel: "r31", rRecorte: null, rAno: null, rX: null, rY: null };
  let DIR = [], CAND = [], ARQ = [];

  const objetos = (j) => j.linhas.map((l) => Object.fromEntries(j.colunas.map((c, i) => [c, l[i]])));

  Promise.all(["dados/direcoes.json", "dados/candidaturas.json", "dados/arquivos.json"].map((u) => fetch(u).then((r) => r.json())))
    .then(([d, c, a]) => { DIR = objetos(d); CAND = objetos(c); ARQ = a; iniciar(); })
    .catch((e) => { document.querySelector("main").insertAdjacentHTML("afterbegin", `<p class="vazio">Não foi possível carregar os dados (${esc(e.message)}).</p>`); });

  /* ------------------------------------------------------------ controles */
  function opcoes(sel, valores, todos) {
    sel.innerHTML = (todos ? `<option value="">${todos}</option>` : "") + valores.map(([v, t]) => `<option value="${esc(v)}">${esc(t)}</option>`).join("");
  }
  function segmentado(id, chave) {
    const g = $(id);
    g.addEventListener("click", (e) => {
      const b = e.target.closest("button"); if (!b) return;
      g.querySelectorAll("button").forEach((x) => x.setAttribute("aria-pressed", x === b));
      estado[chave] = b.dataset.v; desenhar();
    });
  }

  function iniciar() {
    const partidos = [...new Set([...DIR, ...CAND].map((r) => r.partido))].filter((p) => p && p !== "?").sort((a, b) => a.localeCompare(b, "pt-BR"));
    opcoes($("#f-partido"), partidos.map((p) => [p, p]), "Todos os partidos");
    opcoes($("#f-uf"), Object.keys(UFS_NOMES).map((u) => [u, `${u} · ${UFS_NOMES[u]}`]), "Brasil");
    const cargos = ["Vereança", "Prefeitura", "Deputado(a) estadual/distrital", "Deputado(a) federal", "Senado", "Governo estadual"].filter((c) => CAND.some((r) => r.cargo === c));
    opcoes($("#c-cargo"), cargos.map((c) => [c, c]));
    if (!cargos.includes(estado.cCargo)) estado.cCargo = cargos[0];
    const anos = [...new Set(DIR.map((r) => r.ano))].sort();
    estado.pAno = anos[anos.length - 1];
    opcoes($("#p-ano"), anos.slice().reverse().map((a) => [a, a]));

    $("#f-partido").onchange = (e) => { estado.partido = e.target.value; desenhar(); };
    $("#f-uf").onchange = (e) => { estado.uf = e.target.value; desenhar(); };
    $("#c-cargo").onchange = (e) => { estado.cCargo = e.target.value; desenhar(); };
    $("#p-nivel").onchange = (e) => { estado.pNivel = e.target.value; desenhar(); };
    $("#p-ano").onchange = (e) => { estado.pAno = +e.target.value; desenhar(); };
    segmentado("#d-metrica", "dMetrica");
    segmentado("#c-grupo", "cGrupo");
    segmentado("#a-grupo", "aGrupo");
    segmentado("#m-grupo", "mGrupo");
    segmentado("#p-dim", "pDim");
    segmentado("#dt-escala", "dtEscala");
    $("#r-rel").onchange = (e) => { estado.rRel = e.target.value; estado.rRecorte = estado.rX = estado.rY = estado.rAno = null; relacoes(); };
    $("#r-recorte").onchange = (e) => { estado.rRecorte = e.target.value; estado.rAno = null; relacoes(); };
    $("#r-ano").onchange = (e) => { estado.rAno = e.target.value === "todos" ? "todos" : +e.target.value; relacoes(); };
    $("#r-x").onchange = (e) => { estado.rX = e.target.value; if (estado.rY === estado.rX) estado.rY = null; relacoes(); };
    $("#r-y").onchange = (e) => { estado.rY = e.target.value; relacoes(); };
    segmentado("#md-cargo", "mdCargo");
    $("#tp-nivel").onchange = (e) => { estado.tpNivel = e.target.value; desenhar(); };
    $("#tp-ano").onchange = (e) => { estado.tpAno = +e.target.value; desenhar(); };
    $("#md-nivel").onchange = (e) => { estado.mdNivel = e.target.value; desenhar(); };
    $("#md-ano").onchange = (e) => { estado.mdAno = +e.target.value; desenhar(); };
    segmentado("#p-grupo", "pGrupo");
    $("#p-cargo").onchange = (e) => { estado.pCargo = e.target.value; desenhar(); };
    $("#p-medida").onchange = (e) => { estado.pMedida = e.target.value; desenhar(); };
    $("#m-cargo").onchange = (e) => { estado.mCargo = e.target.value; desenhar(); };
    $("#m-ano").onchange = (e) => { estado.mAno = +e.target.value; desenhar(); };
    $("#m-medida").onchange = (e) => { estado.mMedida = e.target.value; desenhar(); };
    matchMedia("(prefers-color-scheme: dark)").addEventListener("change", desenhar);
    let t; addEventListener("resize", () => { clearTimeout(t); t = setTimeout(desenhar, 150); });
    desenhar();
  }

  const filtra = (rows, { partido = true, uf = true } = {}) => rows.filter((r) =>
    (!partido || !estado.partido || r.partido === estado.partido) && (!uf || !estado.uf || r.uf === estado.uf));

  function soma(rows, chaveGrupo, campos) {
    const m = new Map();
    for (const r of rows) {
      const k = chaveGrupo(r);
      let o = m.get(k); if (!o) { o = {}; campos.forEach((c) => (o[c] = 0)); m.set(k, o); }
      campos.forEach((c) => (o[c] += r[c] || 0));
    }
    return m;
  }

  function desenhar() {
    const rec = [estado.partido || "todos os partidos", estado.uf ? UFS_NOMES[estado.uf] : "Brasil"].join(" · ");
    $("#f-resumo").textContent = `Mostrando: ${rec}`;
    destaques(); direcoes(); diretorios(); candidaturas(); mapa(); partidos(); relacoes(); arquivos();
  }

  /* ------------------------------------------------------------ destaques */
  function destaques() {
    const d = filtra(DIR).filter((r) => r.nivel === "municipal");
    const anosD = DIR.filter((r) => r.nivel === "municipal").map((r) => r.ano);
    const ultimo = anosD.length ? Math.max(...anosD) : null;
    const dm = soma(d.filter((r) => r.ano === ultimo), () => 1, ["mulheres", "membros", "mulheres_presidentes", "presidentes"]).get(1);
    const c = filtra(CAND).filter((r) => r.cargo === "Vereança");
    const anosC = CAND.filter((r) => r.cargo === "Vereança").map((r) => r.ano);
    const ultC = anosC.length ? Math.max(...anosC) : null;
    const cm = soma(c.filter((r) => r.ano === ultC), () => 1, ["mulheres", "candidaturas", "eleitas", "eleitos"]).get(1);
    const q = (a, b) => (b > 0 ? pct(a / b) : null);
    const tiles = [
      ["Mulheres nas direções municipais", dm && q(dm.mulheres, dm.membros), dm ? `${ultimo} · ${int(dm.membros)} membros` : "sem dados"],
      ["Presidências municipais ocupadas por mulheres", dm && q(dm.mulheres_presidentes, dm.presidentes), dm ? `${ultimo} · ${int(dm.presidentes)} presidências` : "sem dados"],
      ["Mulheres entre candidaturas à vereança", cm && q(cm.mulheres, cm.candidaturas), cm ? `${ultC} · ${int(cm.candidaturas)} candidaturas` : "sem dados"],
      ["Mulheres entre eleitos à vereança", cm && q(cm.eleitas, cm.eleitos), cm ? `${ultC} · ${int(cm.eleitos)} eleitos(as)` : "sem dados"],
    ];
    $("#destaques").innerHTML = tiles.map(([r, v, c]) =>
      `<div class="tile"><div class="rotulo">${r}</div><div class="valor">${v || "–"}</div><div class="contexto">${esc(c)}</div></div>`).join("");
  }

  /* ------------------------------------------------------------ gráfico de linhas */
  function linhas(el, { series, xs, tabela, rotuloY = "%" }) {
    const ativos = series.filter((s) => s.pontos.some((p) => p.y != null));
    if (!ativos.length) { el.innerHTML = `<p class="vazio">Sem dados para este recorte.</p>`; tabela.innerHTML = ""; return; }
    const W = Math.max(320, el.clientWidth), H = W < 560 ? 260 : 320;
    const m = { t: 12, r: W < 560 ? 12 : 128, b: 28, l: 44 };
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const maxY = Math.min(1, Math.max(0.1, ...ativos.flatMap((s) => s.pontos.map((p) => p.y || 0))) * 1.12);
    const passo = maxY > 0.5 ? 0.2 : maxY > 0.25 ? 0.1 : 0.05;
    const topo = Math.ceil(maxY / passo) * passo;
    const x = (v) => m.l + (xs.length === 1 ? iw / 2 : (xs.indexOf(v) / (xs.length - 1)) * iw);
    const y = (v) => m.t + ih - (v / topo) * ih;
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Gráfico de linhas">`;
    s += `<g class="grade">`;
    for (let v = 0; v <= topo + 1e-9; v += passo) s += `<line x1="${m.l}" x2="${m.l + iw}" y1="${y(v)}" y2="${y(v)}"/><text x="${m.l - 8}" y="${y(v) + 4}" text-anchor="end">${Math.round(v * 100)}${rotuloY}</text>`;
    s += `</g><line class="base" x1="${m.l}" x2="${m.l + iw}" y1="${y(0)}" y2="${y(0)}"/>`;
    const cada = W < 560 && xs.length > 6 ? 2 : 1;
    xs.forEach((v, i) => { if (i % cada === 0 || i === xs.length - 1) s += `<text x="${x(v)}" y="${H - 8}" text-anchor="middle">${v}</text>`; });
    for (const se of ativos) {
      const cor = css(se.cor);
      const segs = []; let atual = [];
      se.pontos.forEach((p) => { if (p.y == null) { if (atual.length) segs.push(atual); atual = []; } else atual.push(p); });
      if (atual.length) segs.push(atual);
      segs.forEach((sg) => { s += `<path d="${sg.map((p, i) => (i ? "L" : "M") + x(p.x) + " " + y(p.y)).join("")}" fill="none" stroke="${cor}" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"/>`; });
      se.pontos.filter((p) => p.y != null).forEach((p) => { s += `<circle cx="${x(p.x)}" cy="${y(p.y)}" r="4" fill="${cor}" stroke="${css("--surface")}" stroke-width="2"/>`; });
    }
    // rótulos diretos no fim das linhas, afastados para não colidir
    if (W >= 560) {
      const rot = ativos.map((se) => { const u = [...se.pontos].reverse().find((p) => p.y != null); return { se, u, yy: y(u.y) }; }).sort((a, b) => a.yy - b.yy);
      for (let i = 1; i < rot.length; i++) if (rot[i].yy - rot[i - 1].yy < 15) rot[i].yy = rot[i - 1].yy + 15;
      rot.forEach(({ se, u, yy }) => { s += `<text x="${x(u.x) + 10}" y="${yy + 4}" style="fill:var(--text)">${esc(se.rotulo)} ${pct(u.y, 0)}</text>`; });
    }
    s += `<line id="mira" x1="0" x2="0" y1="${m.t}" y2="${m.t + ih}" stroke="${css("--muted")}" stroke-dasharray="3 3" visibility="hidden"/>`;
    s += `<rect x="${m.l - 10}" y="0" width="${iw + 20}" height="${H}" fill="transparent" class="alvo"/></svg>`;
    s += `<div class="legenda">${ativos.map((se) => `<span><i style="background:${css(se.cor)}"></i>${esc(se.rotulo)}</span>`).join("")}</div>`;
    el.innerHTML = s;

    const svg = el.querySelector("svg"), mira = svg.querySelector("#mira"), tip = $("#tooltip");
    const mover = (ev) => {
      const r = svg.getBoundingClientRect(), px = ((ev.clientX - r.left) / r.width) * W;
      let melhor = xs[0]; xs.forEach((v) => { if (Math.abs(x(v) - px) < Math.abs(x(melhor) - px)) melhor = v; });
      mira.setAttribute("x1", x(melhor)); mira.setAttribute("x2", x(melhor)); mira.setAttribute("visibility", "visible");
      tip.innerHTML = `<b>${melhor}</b>` + ativos.map((se) => {
        const p = se.pontos.find((q) => q.x === melhor);
        return `<div class="linha"><span><i style="background:${css(se.cor)}"></i>${esc(se.rotulo)}</span><span>${p && p.y != null ? pct(p.y) : "–"}</span></div>` +
          (p && p.det ? `<div class="linha"><span class="dica">${esc(p.det)}</span><span></span></div>` : "");
      }).join("");
      tip.hidden = false;
      const tx = Math.min(ev.clientX + 14, innerWidth - tip.offsetWidth - 8);
      tip.style.left = tx + "px"; tip.style.top = Math.max(8, ev.clientY - tip.offsetHeight - 12) + "px";
    };
    svg.querySelector(".alvo").addEventListener("pointermove", mover);
    svg.querySelector(".alvo").addEventListener("pointerleave", () => { tip.hidden = true; mira.setAttribute("visibility", "hidden"); });

    tabela.innerHTML = `<table><thead><tr><th>Ano</th>${ativos.map((se) => `<th class="n">${esc(se.rotulo)}</th>`).join("")}</tr></thead><tbody>` +
      xs.map((v) => `<tr><td>${v}</td>${ativos.map((se) => { const p = se.pontos.find((q) => q.x === v); return `<td class="n">${p && p.y != null ? pct(p.y) : "–"}${p && p.det ? ` <span class="dica">(${esc(p.det)})</span>` : ""}</td>`; }).join("")}</tr>`).join("") + `</tbody></table>`;
  }

  /* ------------------------------------------------------------ 1. direções */
  function direcoes() {
    const [num, den, txt] = METRICAS[estado.dMetrica];
    const rows = filtra(DIR);
    const xs = [...new Set(DIR.map((r) => r.ano))].sort();
    const series = NIVEIS.filter((n) => !(estado.uf && n.k === "nacional")).map((n) => {
      const m = soma(rows.filter((r) => r.nivel === n.k), (r) => r.ano, [num, den]);
      return { rotulo: n.rotulo, cor: n.cor, pontos: xs.map((a) => { const o = m.get(a); return { x: a, y: o && o[den] >= 5 ? o[num] / o[den] : null, det: o && o[den] ? `${int(o[num])} de ${int(o[den])}` : "" }; }) };
    });
    $("#direcoes .sub").textContent = `Percentual de mulheres ${txt}, por nível da direção e ano${estado.uf ? " (o nível nacional não tem recorte por UF)" : ""}.`;
    linhas($("#g-direcoes"), { series, xs, tabela: $("#t-direcoes") });
  }

  /* ------------------------------------------------------------ 2. candidaturas */
  function candidaturas() {
    const g = estado.cGrupo;
    const rows = filtra(CAND).filter((r) => r.cargo === estado.cCargo);
    const xs = [...new Set(CAND.filter((r) => r.cargo === estado.cCargo).map((r) => r.ano))].sort();
    const campos = ["candidaturas", "mulheres", "raca_informada", "negros", "eleitos", "eleitas", "eleitos_raca_informada", "eleitos_negros"];
    const m = soma(rows, (r) => r.ano, campos);
    const def = g === "mulheres"
      ? [["Candidaturas", "mulheres", "candidaturas", "--s1"], ["Eleitos(as)", "eleitas", "eleitos", "--s2"]]
      : [["Candidaturas", "negros", "raca_informada", "--s1"], ["Eleitos(as)", "eleitos_negros", "eleitos_raca_informada", "--s2"]];
    const series = def.map(([rot, n, d, cor]) => ({ rotulo: rot, cor, pontos: xs.map((a) => { const o = m.get(a); return { x: a, y: o && o[d] >= 1 ? o[n] / o[d] : null, det: o && o[d] ? `${int(o[n])} de ${int(o[d])}` : "" }; }) }));
    $("#c-nota").textContent = g === "negros"
      ? "Pessoas negras = cor/raça preta ou parda. O TSE só informa cor/raça a partir de 2014, e o denominador inclui apenas candidaturas com cor/raça informada."
      : "Candidaturas deferidas em eleições ordinárias, no 1º turno. Eleitos inclui eleitos por quociente partidário e por média.";
    linhas($("#g-candidaturas"), { series, xs, tabela: $("#t-candidaturas") });
  }


  /* ------------------------------------------------------------ zoom dos mapas */
  // Botões +/−/⟲, arrastar (quando ampliado), Ctrl+roda e duplo clique.
  // O zoom é mantido entre redesenhos enquanto a área base (viewBox) não muda.
  function ativarZoom(el) {
    const svg = el.querySelector("svg"); if (!svg) return;
    const base = svg.getAttribute("viewBox").split(" ").map(Number);
    const chave = base.map((v) => Math.round(v)).join(",");
    if (!el._zoom || el._zoom.chave !== chave) el._zoom = { chave, vb: base.slice() };
    const z = el._zoom, MAX = 40;
    const aplicar = () => {
      svg.setAttribute("viewBox", z.vb.join(" "));
      const ampliado = z.vb[2] < base[2] - 1e-6;
      svg.style.cursor = ampliado ? "grab" : "";
      svg.style.touchAction = ampliado ? "none" : "";
      ctl.querySelector("[data-z=menos]").disabled = !ampliado;
      ctl.querySelector("[data-z=reset]").disabled = !ampliado;
    };
    const limitar = () => {
      z.vb[2] = Math.min(base[2], Math.max(base[2] / MAX, z.vb[2]));
      z.vb[3] = z.vb[2] * base[3] / base[2];
      z.vb[0] = Math.min(base[0] + base[2] - z.vb[2], Math.max(base[0], z.vb[0]));
      z.vb[1] = Math.min(base[1] + base[3] - z.vb[3], Math.max(base[1], z.vb[1]));
    };
    const zoomEm = (f, cx, cy) => { // f > 1 amplia; (cx, cy) em coordenadas do mapa
      if (cx == null) { cx = z.vb[0] + z.vb[2] / 2; cy = z.vb[1] + z.vb[3] / 2; }
      const w = z.vb[2] / f, h = z.vb[3] / f;
      z.vb = [cx - (cx - z.vb[0]) / f, cy - (cy - z.vb[1]) / f, w, h]; limitar(); aplicar();
    };
    const ponto = (ev) => { const r = svg.getBoundingClientRect();
      return [z.vb[0] + (ev.clientX - r.left) / r.width * z.vb[2], z.vb[1] + (ev.clientY - r.top) / r.height * z.vb[3]]; };
    let ctl = el.querySelector(".zoom-ctl");
    if (!ctl) {
      ctl = document.createElement("div"); ctl.className = "zoom-ctl";
      ctl.innerHTML = `<button type="button" data-z="mais" aria-label="Ampliar" title="Ampliar">+</button>` +
        `<button type="button" data-z="menos" aria-label="Reduzir" title="Reduzir">−</button>` +
        `<button type="button" data-z="reset" aria-label="Voltar ao mapa inteiro" title="Voltar ao mapa inteiro">⟲</button>`;
      el.appendChild(ctl);
    }
    ctl.onclick = (ev) => { const b = ev.target.closest("button"); if (!b) return;
      if (b.dataset.z === "mais") zoomEm(2); else if (b.dataset.z === "menos") zoomEm(0.5); else { z.vb = base.slice(); aplicar(); } };
    svg.addEventListener("wheel", (ev) => { if (!ev.ctrlKey && !ev.metaKey) return; ev.preventDefault();
      const [cx, cy] = ponto(ev); zoomEm(ev.deltaY < 0 ? 1.4 : 1 / 1.4, cx, cy); }, { passive: false });
    svg.addEventListener("dblclick", (ev) => { ev.preventDefault(); const [cx, cy] = ponto(ev); zoomEm(2, cx, cy); });
    let arr = null;
    svg.addEventListener("pointerdown", (ev) => { if (z.vb[2] >= base[2] - 1e-6) return;
      arr = { x: ev.clientX, y: ev.clientY, vb: z.vb.slice(), mov: false }; svg.setPointerCapture(ev.pointerId); });
    svg.addEventListener("pointermove", (ev) => { if (!arr) return; const r = svg.getBoundingClientRect();
      const dx = (ev.clientX - arr.x) / r.width * z.vb[2], dy = (ev.clientY - arr.y) / r.height * z.vb[3];
      if (Math.abs(ev.clientX - arr.x) + Math.abs(ev.clientY - arr.y) > 3) arr.mov = true;
      z.vb[0] = arr.vb[0] - dx; z.vb[1] = arr.vb[1] - dy; limitar(); aplicar(); svg.style.cursor = "grabbing"; $("#tooltip").hidden = true; });
    const soltar = () => { arr = null; aplicar(); };
    svg.addEventListener("pointerup", soltar); svg.addEventListener("pointercancel", soltar);
    aplicar();
  }

  /* ------------------------------------------------------------ mapa da equidade */
  const FAIXAS = [ // [limite superior, token de cor, rótulo]
    [-0.6, "--div-n4", "−1 a −0,6"], [-0.35, "--div-n3", "−0,6 a −0,35"], [-0.15, "--div-n2", "−0,35 a −0,15"],
    [-0.05, "--div-n1", "−0,15 a −0,05"], [0.05, "--div-0", "−0,05 a 0,05 (paridade)"], [0.15, "--div-p1", "0,05 a 0,15"],
    [0.35, "--div-p2", "0,15 a 0,35"], [0.6, "--div-p3", "0,35 a 0,6"], [1.0001, "--div-p4", "0,6 a 1"],
  ];
  const corDri = (v) => v == null ? css("--sem-dado") : css(FAIXAS.find((f) => v < f[0])[1]);
  const dri = (b, p) => (b == null || p == null) ? null : b === p ? 0 : (b - p) / Math.sqrt((b - p) ** 2 + b * (1 - b));
  const fmtDri = (v) => v == null ? "–" : v.toLocaleString("pt-BR", { minimumFractionDigits: 2, maximumFractionDigits: 2, signDisplay: "exceptZero" });
  const MED_ROT = { candidaturas: "candidaturas", eleitos: "eleitos(as)", votos: "votos nominais", receitas: "receitas", receitas_publicas: "recursos públicos" };
  const cache = {};
  const json = (u) => (cache[u] ||= fetch(u).then((r) => { if (!r.ok) throw new Error(r.status + " " + u); return r.json(); }));
  let IND_MAPA = null, tokenMapa = 0;

  async function mapa() {
    const meu = ++tokenMapa, el = $("#g-mapa");
    try {
      IND_MAPA ||= await json("dados/mapa/indice.json");
      const anos = IND_MAPA[estado.mCargo] || [];
      if (!anos.includes(estado.mAno)) estado.mAno = anos[anos.length - 1];
      opcoes($("#m-ano"), anos.slice().reverse().map((a) => [a, a])); $("#m-ano").value = estado.mAno;
      const municipal = estado.mCargo === "vereanca";
      const [geoUf, geoMun, nomes, doc] = await Promise.all([
        json("dados/geo/ufs.json"), municipal ? json("dados/geo/municipios.json") : null,
        municipal ? json("dados/geo/nomes.json") : null, json(`dados/mapa/${estado.mCargo}_${estado.mAno}.json`)]);
      if (meu !== tokenMapa) return;

      // b ponderado por T sobre as listas (todas ou do partido escolhido)
      const k = doc.medidas.indexOf(estado.mMedida), g = estado.mGrupo === "mulheres" ? 0 : 1;
      const pi = estado.partido ? doc.partidos.indexOf(estado.partido) : -1;
      const acc = new Map();
      for (const l of doc.linhas) {
        if (estado.partido && l[1] !== pi) continue;
        const T = l[2 + 3 * k], b = l[3 + 3 * k + g];
        let o = acc.get(l[0]); if (!o) acc.set(l[0], (o = { num: 0, den: 0, listas: 0, total: 0 }));
        o.listas++; o.total += T;
        if (b != null && T > 0) { o.num += b * T; o.den += T; }
      }
      const val = new Map();
      for (const [d, o] of acc) {
        const p = doc.distritos[d] ? doc.distritos[d][g] : null, b = o.den > 0 ? o.num / o.den : null;
        val.set(d, { ...o, b, p, v: dri(b, p) });
      }
      const ufDe = (d) => municipal ? (nomes[d] || "").slice(-3, -1) : d;
      const visiveis = [...val.entries()].filter(([d]) => !estado.uf || ufDe(d) === estado.uf);

      // geometria
      const [x0, y0, x1, y1] = estado.uf ? geoUf.caixas_uf[estado.uf] : geoUf.caixa, pad = (x1 - x0) * 0.03;
      const vb = `${x0 - pad} ${y0 - pad} ${x1 - x0 + 2 * pad} ${y1 - y0 + 2 * pad}`;
      const nomeDe = (d) => municipal ? (nomes[d] || d) : `${UFS_NOMES[d]} (${d})`;
      let s = `<svg viewBox="${vb}" role="img" aria-label="Mapa do índice de equidade por ${municipal ? "município" : "UF"}">`;
      if (municipal) {
        for (const [cod, path] of Object.entries(geoMun)) {
          if (estado.uf && ufDe(cod) !== estado.uf) continue;
          const o = val.get(cod);
          s += `<path class="d" data-d="${cod}" d="${path}" fill="${corDri(o && o.v)}"/>`;
        }
        for (const [uf, path] of Object.entries(geoUf.ufs)) if (!estado.uf || uf === estado.uf) s += `<path class="uf" d="${path}"/>`;
      } else {
        for (const [uf, path] of Object.entries(geoUf.ufs)) {
          if (estado.uf && uf !== estado.uf) continue;
          const o = val.get(uf);
          s += `<path class="d" data-d="${uf}" d="${path}" fill="${corDri(o && o.v)}"/>`;
        }
      }
      el.innerHTML = s + `</svg>`;
      ativarZoom(el);

      // tooltip
      const tip = $("#tooltip"), svg = el.querySelector("svg");
      svg.addEventListener("pointermove", (ev) => {
        const t = ev.target.closest("path.d"); if (!t) { tip.hidden = true; return; }
        const d = t.dataset.d, o = val.get(d);
        tip.innerHTML = `<b>${esc(nomeDe(d))}</b>` + (o ? `
          <div class="linha"><span>DRI</span><span><strong>${fmtDri(o.v)}</strong></span></div>
          <div class="linha"><span>${estado.mGrupo === "mulheres" ? "Mulheres" : "Pessoas negras"} em ${MED_ROT[estado.mMedida]}</span><span>${pct(o.b)}</span></div>
          <div class="linha"><span>No ${estado.mGrupo === "mulheres" ? "eleitorado" : "população"} (p)</span><span>${pct(o.p)}</span></div>
          <div class="linha"><span>Listas</span><span>${int(o.listas)}</span></div>` : `<div class="linha"><span>Sem lista${estado.partido ? " do " + esc(estado.partido) : ""}</span><span></span></div>`);
        tip.hidden = false;
        tip.style.left = Math.min(ev.clientX + 14, innerWidth - tip.offsetWidth - 8) + "px";
        tip.style.top = Math.max(8, ev.clientY - tip.offsetHeight - 12) + "px";
      });
      svg.addEventListener("pointerleave", () => (tip.hidden = true));

      // resumo, legenda e tabela
      const vs = visiveis.map(([, o]) => o.v).filter((v) => v != null).sort((a, b) => a - b);
      const med = vs.length ? (vs.length % 2 ? vs[(vs.length - 1) / 2] : (vs[vs.length / 2 - 1] + vs[vs.length / 2]) / 2) : null;
      const abaixo = vs.length ? vs.filter((v) => v < -0.05).length / vs.length : null;
      const unid = municipal ? "municípios" : "UFs";
      $("#m-resumo").innerHTML = `<div class="rotulo">DRI mediano (${unid})</div><div class="valor">${fmtDri(med)}</div>
        <div class="contexto">${vs.length ? `${pct(abaixo, 0)} ${municipal ? "dos" : "das"} ${int(vs.length)} ${unid} com dados abaixo da paridade` : "sem dados para este recorte"}</div>`;
      $("#m-legenda").innerHTML = `<div class="titulo">Índice de equidade (DRI)</div>` +
        FAIXAS.slice().reverse().map(([, c, r]) => `<div class="faixa"><i style="background:${css(c)}"></i>${r}</div>`).join("") +
        `<div class="faixa"><i style="background:${css("--sem-dado")};box-shadow:inset 0 0 0 1px ${css("--line")}"></i>sem dados</div>
         <p class="dica">Vermelho: sub-representação. Azul: sobre-representação.</p>`;
      $("#m-sub").textContent = `${estado.mGrupo === "mulheres" ? "Mulheres" : "Pessoas negras"} em ${MED_ROT[estado.mMedida]}, ${estado.mAno}${estado.partido ? `, lista do ${estado.partido}` : ", todas as listas"}. DRI: −1 = grupo ausente; 0 = paridade com ${estado.mGrupo === "mulheres" ? "o eleitorado" : "a população"} do distrito; 1 = grupo detém tudo.`;
      $("#m-nota").textContent = (estado.mGrupo === "negros" && estado.mAno < 2014 ? "O TSE só informa cor/raça a partir de 2014. " : "") +
        "Sem filtro de partido, o índice usa o conjunto das listas do distrito (participação ponderada pelo total da medida). Denominador p: mulheres no eleitorado (TSE) e pessoas pretas e pardas na população (Censo).";
      let linhasTab;
      if (municipal) {
        const porUf = new Map();
        for (const [d, o] of visiveis) { if (o.v == null) continue; const u = ufDe(d); (porUf.get(u) || porUf.set(u, []).get(u)).push(o.v); }
        linhasTab = [...porUf].sort().map(([u, a]) => { a.sort((x, y) => x - y); const m = a.length % 2 ? a[(a.length - 1) / 2] : (a[a.length / 2 - 1] + a[a.length / 2]) / 2;
          return `<tr><td>${u}</td><td class="n">${int(a.length)}</td><td class="n">${fmtDri(m)}</td><td class="n">${pct(a.filter((v) => v < -0.05).length / a.length, 0)}</td></tr>`; });
        $("#t-mapa").innerHTML = `<table><thead><tr><th>UF</th><th class="n">Municípios com dados</th><th class="n">DRI mediano</th><th class="n">Abaixo da paridade</th></tr></thead><tbody>${linhasTab.join("")}</tbody></table>`;
      } else {
        linhasTab = visiveis.sort((a, b) => (a[1].v ?? 9) - (b[1].v ?? 9)).map(([d, o]) =>
          `<tr><td>${esc(nomeDe(d))}</td><td class="n">${fmtDri(o.v)}</td><td class="n">${pct(o.b)}</td><td class="n">${pct(o.p)}</td><td class="n">${int(o.listas)}</td></tr>`);
        $("#t-mapa").innerHTML = `<table><thead><tr><th>UF</th><th class="n">DRI</th><th class="n">Participação (b)</th><th class="n">Referência (p)</th><th class="n">Listas</th></tr></thead><tbody>${linhasTab.join("")}</tbody></table>`;
      }
    } catch (e) {
      if (meu === tokenMapa) el.innerHTML = `<p class="vazio">Não foi possível carregar o mapa (${esc(e.message)}).</p>`;
    }
  }

  /* ------------------------------------------------------------ diretórios */
  const TIPOS_DIR = [ // ordem fixa das cores categóricas
    { k: "definitivo", rot: "Órgão definitivo", cor: "--s1" },
    { k: "provisorio", rot: "Órgão provisório", cor: "--s2" },
    { k: "interventora", rot: "Comissão interventora", cor: "--s3" },
    { k: "executiva", rot: "Só comissão executiva (nacional)", cor: "--s4" },
  ];
  const NIVEL_ROT = { nacional: "Nacional", estadual: "Estadual", municipal: "Municipal" };
  let DT = null, tokenDir = 0;
  const legendaTipos = (el, ks) => { el.innerHTML = TIPOS_DIR.filter((t) => ks.includes(t.k)).map((t) => `<span><i style="background:${css(t.cor)};height:10px;width:10px"></i>${t.rot}</span>`).join(""); };
  const mostraTip = (ev, html) => { const tip = $("#tooltip"); tip.innerHTML = html; tip.hidden = false;
    tip.style.left = Math.min(ev.clientX + 14, innerWidth - tip.offsetWidth - 8) + "px";
    tip.style.top = Math.max(8, ev.clientY - tip.offsetHeight - 12) + "px"; };

  async function diretorios() {
    const meu = ++tokenDir;
    try {
      DT ||= objetos(await json("dados/diretorios.json"));
      if (meu !== tokenDir) return;
      const rows = filtra(DT);
      const anos = [...new Set(DT.map((r) => r.ano))].sort();
      const pct = estado.dtEscala === "pct";
      legendaTipos($("#dt-legenda"), TIPOS_DIR.map((t) => t.k));
      $("#dt-sub").textContent = `${pct ? "Composição dos diretórios por tipo de órgão" : "Número de diretórios em funcionamento por tipo de órgão"}, por ano e esfera${estado.partido ? " — " + estado.partido : ""}${estado.uf ? " — " + UFS_NOMES[estado.uf] : ""}.`;
      const el = $("#g-diretorios"); el.innerHTML = "";
      const tabela = [];
      for (const nivel of ["nacional", "estadual", "municipal"]) {
        const div = document.createElement("div"); div.className = "painel"; el.appendChild(div);
        if (nivel === "nacional" && estado.uf) { div.innerHTML = `<h3>${NIVEL_ROT[nivel]}</h3><p class="vazio">Sem recorte por UF.</p>`; continue; }
        const m = new Map(); // ano -> {tipo: n}
        for (const r of rows) if (r.nivel === nivel) { const o = m.get(r.ano) || m.set(r.ano, {}).get(r.ano); o[r.tipo] = (o[r.tipo] || 0) + r.orgaos; }
        const tot = (a) => Object.values(m.get(a) || {}).reduce((s, v) => s + v, 0);
        const ult = anos[anos.length - 1];
        div.innerHTML = `<h3>${NIVEL_ROT[nivel]}</h3><div class="total">${int(tot(ult))} diretórios em ${ult}</div>`;
        const g = document.createElement("div"); g.className = "grafico"; div.appendChild(g);
        const W = Math.max(220, g.clientWidth), H = 200, mg = { t: 6, r: 4, b: 22, l: pct ? 34 : 44 };
        const iw = W - mg.l - mg.r, ih = H - mg.t - mg.b, bw = iw / anos.length;
        const maxV = pct ? 1 : Math.max(1, ...anos.map(tot));
        const passo = pct ? 0.25 : (() => { const e = Math.pow(10, Math.floor(Math.log10(maxV))); return [1, 2, 2.5, 5, 10].map((k) => k * e).find((s) => maxV / s <= 4); })();
        const topo = pct ? 1 : Math.ceil(maxV / passo) * passo;
        const y = (v) => mg.t + ih - (v / topo) * ih;
        let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Diretórios ${nivel} por ano e tipo"><g class="grade">`;
        for (let v = 0; v <= topo + 1e-9; v += passo) s += `<line x1="${mg.l}" x2="${W - mg.r}" y1="${y(v)}" y2="${y(v)}"/><text x="${mg.l - 6}" y="${y(v) + 4}" text-anchor="end">${pct ? Math.round(v * 100) + "%" : v >= 1000 ? (v / 1000).toLocaleString("pt-BR") + " mil" : v}</text>`;
        s += `</g>`;
        anos.forEach((a, i) => {
          const o = m.get(a) || {}, T = tot(a); let acc = 0;
          const x0 = mg.l + i * bw + bw * 0.15, w = bw * 0.7;
          TIPOS_DIR.forEach((t) => {
            const v = o[t.k] || 0; if (!v) return;
            const vv = pct ? v / T : v, y1 = y(acc + vv), y0 = y(acc);
            s += `<rect class="seg" data-a="${a}" x="${x0}" y="${y1}" width="${w}" height="${Math.max(0, y0 - y1 - 1)}" fill="${css(t.cor)}"/>`;
            acc += vv;
          });
          s += `<rect class="alvo-ano" data-a="${a}" x="${mg.l + i * bw}" y="${mg.t}" width="${bw}" height="${ih}" fill="transparent"/>`;
          if (i % (W < 300 ? 2 : 1) === 0 || i === anos.length - 1) s += `<text x="${x0 + w / 2}" y="${H - 6}" text-anchor="middle">${String(a).slice(2)}</text>`;
          tabela.push([NIVEL_ROT[nivel], a, T, ...TIPOS_DIR.map((t) => o[t.k] || 0)]);
        });
        g.innerHTML = s + `</svg>`;
        g.querySelectorAll(".alvo-ano").forEach((r) => {
          r.addEventListener("pointermove", (ev) => {
            const a = +r.dataset.a, o = m.get(a) || {}, T = tot(a);
            mostraTip(ev, `<b>${NIVEL_ROT[nivel]} · ${a}</b>` + linhaTip("Total", int(T)) +
              TIPOS_DIR.filter((t) => o[t.k]).map((t) => `<div class="linha"><span><i style="background:${css(t.cor)}"></i>${t.rot}</span><span>${int(o[t.k])} (${pct_(o[t.k] / T)})</span></div>`).join(""));
          });
          r.addEventListener("pointerleave", () => ($("#tooltip").hidden = true));
        });
      }
      $("#t-diretorios").innerHTML = `<table><thead><tr><th>Esfera</th><th>Ano</th><th class="n">Total</th>${TIPOS_DIR.map((t) => `<th class="n">${t.rot}</th>`).join("")}</tr></thead><tbody>` +
        tabela.map((l) => `<tr><td>${l[0]}</td><td>${l[1]}</td>${l.slice(2).map((v) => `<td class="n">${int(v)}</td>`).join("")}</tr>`).join("") + `</tbody></table>`;
      tipos(); mapaDir();
    } catch (e) {
      if (meu === tokenDir) $("#g-diretorios").innerHTML = `<p class="vazio">Não foi possível carregar os diretórios (${esc(e.message)}).</p>`;
    }
  }
  const pct_ = (x) => pct(x, 0);

  function tipos() {
    const el = $("#g-tipos"), nivel = estado.tpNivel;
    const anos = [...new Set(DT.map((r) => r.ano))].sort();
    if (!anos.includes(estado.tpAno)) estado.tpAno = anos[anos.length - 1];
    opcoes($("#tp-ano"), anos.slice().reverse().map((a) => [a, a])); $("#tp-ano").value = estado.tpAno;
    const ks = ["definitivo", "provisorio", "interventora"];
    legendaTipos($("#tp-legenda"), ks);
    $("#tp-sub").textContent = `Distribuição dos diretórios ${nivel === "municipal" ? "municipais" : "estaduais"} de cada partido por tipo de órgão, ${estado.tpAno}${estado.uf ? " — " + UFS_NOMES[estado.uf] : ""}.`;
    const m = new Map();
    for (const r of filtra(DT, { partido: false })) {
      if (r.nivel !== nivel || r.ano !== estado.tpAno) continue;
      const o = m.get(r.partido) || m.set(r.partido, { total: 0 }).get(r.partido);
      o[r.tipo] = (o[r.tipo] || 0) + r.orgaos; o.total += r.orgaos;
    }
    const dados = [...m].filter(([p, o]) => p !== "?" && o.total >= 10).map(([p, o]) => ({ p, ...o, def: (o.definitivo || 0) / o.total })).sort((a, b) => b.def - a.def);
    if (!dados.length) { el.innerHTML = `<p class="vazio">Sem dados para este recorte.</p>`; return; }
    const W = Math.max(320, el.clientWidth), bh = 18, gap = 6, mg = { t: 4, r: 56, b: 22, l: 108 };
    const H = mg.t + mg.b + dados.length * (bh + gap), iw = W - mg.l - mg.r, x = (v) => mg.l + v * iw;
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Tipo de órgão por partido"><g class="grade">`;
    for (let v = 0; v <= 1.0001; v += 0.25) s += `<line x1="${x(v)}" x2="${x(v)}" y1="${mg.t}" y2="${H - mg.b}"/><text x="${x(v)}" y="${H - 6}" text-anchor="middle">${v * 100}%</text>`;
    s += `</g>`;
    dados.forEach((d, i) => {
      const yy = mg.t + i * (bh + gap), sel = estado.partido === d.p; let acc = 0;
      s += `<g class="barra" data-i="${i}" opacity="${estado.partido && !sel ? 0.45 : 1}"><rect x="0" y="${yy - gap / 2}" width="${W}" height="${bh + gap}" fill="transparent"/>` +
        `<text x="${mg.l - 8}" y="${yy + bh / 2 + 4}" text-anchor="end" style="fill:var(${sel ? "--text" : "--text-2"});font-weight:${sel ? 700 : 400}">${esc(d.p)}</text>`;
      TIPOS_DIR.filter((t) => ks.includes(t.k)).forEach((t) => {
        const v = (d[t.k] || 0) / d.total; if (!v) return;
        s += `<rect x="${x(acc)}" y="${yy}" width="${Math.max(0, x(acc + v) - x(acc) - 1)}" height="${bh}" fill="${css(t.cor)}"/>`; acc += v;
      });
      s += `<text x="${x(1) + 6}" y="${yy + bh / 2 + 4}">${int(d.total)}</text></g>`;
    });
    el.innerHTML = s + `<text></text></svg>`;
    el.querySelectorAll(".barra").forEach((g) => {
      g.addEventListener("pointermove", (ev) => {
        const d = dados[+g.dataset.i];
        mostraTip(ev, `<b>${esc(d.p)} · ${estado.tpAno}</b>` + linhaTip("Diretórios", int(d.total)) +
          TIPOS_DIR.filter((t) => ks.includes(t.k) && d[t.k]).map((t) => `<div class="linha"><span><i style="background:${css(t.cor)}"></i>${t.rot}</span><span>${int(d[t.k])} (${pct_(d[t.k] / d.total)})</span></div>`).join(""));
      });
      g.addEventListener("pointerleave", () => ($("#tooltip").hidden = true));
    });
  }

  // mapa: % de mulheres na presidência (ou presidência e tesouraria)
  const FAIXAS_SEQ = [[0.1, "--seq-1", "0 a 10%"], [0.2, "--seq-2", "10 a 20%"], [0.3, "--seq-3", "20 a 30%"],
    [0.4, "--seq-4", "30 a 40%"], [0.5, "--seq-5", "40 a 50%"], [1.0001, "--seq-6", "50% ou mais"]];
  const corSeq = (v) => v == null ? css("--sem-dado") : css(FAIXAS_SEQ.find((f) => v < f[0])[1]);
  let tokenMD = 0;
  async function mapaDir() {
    const meu = ++tokenMD, el = $("#g-mapa-dir"), municipal = estado.mdNivel === "municipal";
    try {
      const anos = [...new Set(DT.map((r) => r.ano))].sort();
      if (!anos.includes(estado.mdAno)) estado.mdAno = anos[anos.length - 1];
      opcoes($("#md-ano"), anos.slice().reverse().map((a) => [a, a])); $("#md-ano").value = estado.mdAno;
      const [geoUf, geoMun, nomes, doc] = await Promise.all([json("dados/geo/ufs.json"),
        municipal ? json("dados/geo/municipios.json") : null, municipal ? json("dados/geo/nomes.json") : null,
        municipal ? json(`dados/diretorios/municipal_${estado.mdAno}.json`) : null]);
      if (meu !== tokenMD) return;
      const cg = estado.mdCargo; // membros | presidencia | decisao
      // posições nas linhas municipais: [mun, partido, tipo, decisao, mulheres_decisao, presidentes, mulheres_presidentes, membros, mulheres]
      const IDX = { membros: [8, 7], presidencia: [6, 5], decisao: [4, 3] }[cg];
      const CAMPOS = { membros: ["mulheres", "membros"], presidencia: ["mulheres_presidentes", "presidentes"], decisao: ["mulheres_decisao", "decisao"] }[cg];
      const val = new Map(); // distrito -> {num, den, orgaos}
      const somar = (d, num, den) => { const o = val.get(d) || val.set(d, { num: 0, den: 0, orgaos: 0 }).get(d); o.num += num; o.den += den; o.orgaos++; };
      if (municipal) {
        const pi = estado.partido ? doc.partidos.indexOf(estado.partido) : -1;
        for (const l of doc.linhas) if (!estado.partido || l[1] === pi) somar(l[0], l[IDX[0]], l[IDX[1]]);
      } else {
        for (const r of filtra(DT, { uf: false })) if (r.nivel === "estadual" && r.ano === estado.mdAno) somar(r.uf, r[CAMPOS[0]], r[CAMPOS[1]]);
      }
      for (const o of val.values()) o.v = o.den > 0 ? o.num / o.den : null;
      const ufDe = (d) => municipal ? (nomes[d] || "").slice(-3, -1) : d;
      const nomeDe = (d) => municipal ? (nomes[d] || d) : `${UFS_NOMES[d]} (${d})`;
      const [x0, y0, x1, y1] = estado.uf ? geoUf.caixas_uf[estado.uf] : geoUf.caixa, pad = (x1 - x0) * 0.03;
      let s = `<svg viewBox="${x0 - pad} ${y0 - pad} ${x1 - x0 + 2 * pad} ${y1 - y0 + 2 * pad}" role="img" aria-label="Mapa: mulheres na direção dos diretórios">`;
      const fonte = municipal ? geoMun : geoUf.ufs;
      for (const [cod, path] of Object.entries(fonte)) {
        if (estado.uf && ufDe(cod) !== estado.uf) continue;
        const o = val.get(cod);
        s += `<path class="d" data-d="${cod}" d="${path}" fill="${corSeq(o && o.v)}"/>`;
      }
      if (municipal) for (const [uf, path] of Object.entries(geoUf.ufs)) if (!estado.uf || uf === estado.uf) s += `<path class="uf" d="${path}"/>`;
      el.innerHTML = s + `</svg>`;
      ativarZoom(el);
      const svg = el.querySelector("svg"), cargoTxt = { membros: "entre os membros", presidencia: "na presidência", decisao: "na presidência e tesouraria" }[cg],
            unidCargo = cg === "membros" ? "membros" : "cargos";
      svg.addEventListener("pointermove", (ev) => {
        const t = ev.target.closest("path.d"); if (!t) { $("#tooltip").hidden = true; return; }
        const o = val.get(t.dataset.d);
        mostraTip(ev, `<b>${esc(nomeDe(t.dataset.d))}</b>` + (o ? linhaTip(`Mulheres ${cargoTxt}`, `<strong>${pct(o.v)}</strong>`) +
          linhaTip(`Mulheres / ${unidCargo}`, `${int(o.num)} de ${int(o.den)}`) + linhaTip("Diretórios", int(o.orgaos))
          : `<div class="linha"><span>Sem diretório${estado.partido ? " do " + esc(estado.partido) : ""}</span><span></span></div>`));
      });
      svg.addEventListener("pointerleave", () => ($("#tooltip").hidden = true));
      // resumo, legenda, tabela
      const vis = [...val].filter(([d]) => !estado.uf || ufDe(d) === estado.uf);
      const tot = vis.reduce((a, [, o]) => ((a.n += o.num), (a.d += o.den), (a.o += o.orgaos), a), { n: 0, d: 0, o: 0 });
      const comAlguma = vis.filter(([, o]) => o.num > 0).length;
      const unid = municipal ? "municípios" : "UFs";
      $("#md-resumo").innerHTML = `<div class="rotulo">Mulheres ${cargoTxt}</div><div class="valor">${tot.d ? pct(tot.n / tot.d) : "–"}</div>
        <div class="contexto">${int(tot.n)} de ${int(tot.d)} ${unidCargo} · ${int(tot.o)} diretórios · ${pct_(vis.length ? comAlguma / vis.length : null)} ${municipal ? "dos" : "das"} ${unid} com ao menos uma mulher</div>`;
      $("#md-legenda").innerHTML = `<div class="titulo">Mulheres ${cargoTxt}</div>` +
        FAIXAS_SEQ.slice().reverse().map(([, c, r]) => `<div class="faixa"><i style="background:${css(c)}"></i>${r}</div>`).join("") +
        `<div class="faixa"><i style="background:${css("--sem-dado")};box-shadow:inset 0 0 0 1px ${css("--line")}"></i>sem diretório</div>`;
      $("#md-sub").textContent = `Percentual de mulheres ${cargoTxt} dos diretórios ${municipal ? "municipais" : "estaduais"}, ${estado.mdAno}${estado.partido ? ", " + estado.partido : ", todos os partidos"}.`;
      $("#md-nota").textContent = (municipal ? "As zonas do DF aparecem somadas em Brasília. " : "") +
        (cg === "membros"
          ? (estado.partido ? "Membros distintos da direção do partido no distrito no ano." : "Sem filtro de partido, soma os membros de todos os diretórios do distrito.")
          : (estado.partido ? "Com um partido escolhido, cada distrito tem em geral um só diretório: o mapa mostra onde a presidência é de uma mulher." : "Sem filtro de partido, soma os cargos de todos os diretórios do distrito."));
      const linhasT = municipal
        ? (() => { const u = new Map(); for (const [d, o] of vis) { const k = ufDe(d); const a = u.get(k) || u.set(k, { n: 0, d: 0, o: 0 }).get(k); a.n += o.num; a.d += o.den; a.o += o.orgaos; } return [...u].sort(); })()
        : vis.sort();
      $("#t-mapa-dir").innerHTML = `<table><thead><tr><th>UF</th><th class="n">Diretórios</th><th class="n">${unidCargo === "membros" ? "Membros" : "Cargos"}</th><th class="n">Mulheres</th><th class="n">%</th></tr></thead><tbody>` +
        linhasT.map(([k, o]) => { const n = o.num ?? o.n, d = o.den ?? o.d; return `<tr><td>${k}</td><td class="n">${int(o.orgaos ?? o.o)}</td><td class="n">${int(d)}</td><td class="n">${int(n)}</td><td class="n">${d ? pct(n / d) : "–"}</td></tr>`; }).join("") + `</tbody></table>`;
    } catch (e) {
      if (meu === tokenMD) el.innerHTML = `<p class="vazio">Não foi possível carregar o mapa (${esc(e.message)}).</p>`;
    }
  }

  /* ------------------------------------------------------------ relações */
  // Estatística (sem dependências): Pearson, Spearman, MQO e teste t de Welch.
  function betacf(a, b, x) {
    let qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < 1e-30) d = 1e-30; d = 1 / d; let h = d;
    for (let m = 1; m <= 300; m++) {
      const m2 = 2 * m; let aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < 1e-30) d = 1e-30; c = 1 + aa / c; if (Math.abs(c) < 1e-30) c = 1e-30;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < 1e-30) d = 1e-30; c = 1 + aa / c; if (Math.abs(c) < 1e-30) c = 1e-30;
      d = 1 / d; const del = d * c; h *= del; if (Math.abs(del - 1) < 3e-12) break;
    }
    return h;
  }
  function lgamma(z) { // Lanczos
    const g = [676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];
    if (z < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * z)) - lgamma(1 - z);
    z -= 1; let x = 0.99999999999980993; for (let i = 0; i < 8; i++) x += g[i] / (z + i + 1);
    const t = z + 7.5; return 0.5 * Math.log(2 * Math.PI) + (z + 0.5) * Math.log(t) - t + Math.log(x);
  }
  function ibeta(x, a, b) {
    if (x <= 0) return 0; if (x >= 1) return 1;
    const bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
    return x < (a + 1) / (a + b + 2) ? bt * betacf(a, b, x) / a : 1 - bt * betacf(b, a, 1 - x) / b;
  }
  const pT = (t, df) => (!isFinite(t) || df <= 0) ? NaN : ibeta(df / (df + t * t), df / 2, 0.5); // bicaudal
  function ranks(v) {
    const idx = v.map((x, i) => i).sort((a, b) => v[a] - v[b]), r = new Float64Array(v.length);
    for (let i = 0; i < idx.length;) { let j = i; while (j + 1 < idx.length && v[idx[j + 1]] === v[idx[i]]) j++;
      const m = (i + j) / 2 + 1; for (let k = i; k <= j; k++) r[idx[k]] = m; i = j + 1; }
    return r;
  }
  function pearson(x, y) {
    const n = x.length; let mx = 0, my = 0; for (let i = 0; i < n; i++) { mx += x[i]; my += y[i]; } mx /= n; my /= n;
    let sxy = 0, sxx = 0, syy = 0; for (let i = 0; i < n; i++) { const a = x[i] - mx, b = y[i] - my; sxy += a * b; sxx += a * a; syy += b * b; }
    return { r: sxy / Math.sqrt(sxx * syy), b: sxy / sxx, a: my - (sxy / sxx) * mx, sxx, syy, sxy, mx, my };
  }
  function estatisticas(x, y) {
    const n = x.length; if (n < 3) return { n };
    const P = pearson(x, y), r = P.r, df = n - 2;
    const t = r * Math.sqrt(df / Math.max(1e-300, 1 - r * r));
    const sse = P.syy - P.b * P.sxy, se = Math.sqrt(Math.max(0, sse) / df / P.sxx);
    const rs = n <= 200000 ? pearson(Array.from(ranks(x)), Array.from(ranks(y))).r : NaN;
    const ts = rs * Math.sqrt(df / Math.max(1e-300, 1 - rs * rs));
    return { n, r, p: pT(t, df), rs, ps: pT(ts, df), b: P.b, a: P.a, se, r2: r * r };
  }
  function welch(y0, y1) {
    const m = (v) => v.reduce((s, x) => s + x, 0) / v.length, va = (v, mu) => v.reduce((s, x) => s + (x - mu) ** 2, 0) / (v.length - 1);
    const m0 = m(y0), m1 = m(y1), v0 = va(y0, m0), v1 = va(y1, m1), e0 = v0 / y0.length, e1 = v1 / y1.length;
    const t = (m1 - m0) / Math.sqrt(e0 + e1), df = (e0 + e1) ** 2 / (e0 ** 2 / (y0.length - 1) + e1 ** 2 / (y1.length - 1));
    return { m0, m1, n0: y0.length, n1: y1.length, dif: m1 - m0, t, df, p: pT(t, df), ic0: 1.96 * Math.sqrt(e0), ic1: 1.96 * Math.sqrt(e1), se: Math.sqrt(e0 + e1) };
  }
  const fmtN = (v, d = 2) => v == null || !isFinite(v) ? "–" : v.toLocaleString("pt-BR", { minimumFractionDigits: d, maximumFractionDigits: d });
  const fmtP = (p) => !isFinite(p) ? "–" : p < 0.001 ? "< 0,001" : fmtN(p, 3);
  const estrelas = (p) => p < 0.001 ? "***" : p < 0.01 ? "**" : p < 0.05 ? "*" : "";

  let REL = null, tokenRel = 0, ultimoCodigo = "";
  const SEM_PARTIDO = new Set(["r33"]), SEM_UF = new Set(["rfg", "rl"]);
  const relAtual = () => REL.relacoes.find((r) => r.id === estado.rRel) || REL.relacoes[0];

  async function relacoes() {
    const meu = ++tokenRel, el = $("#g-relacao");
    try {
      REL ||= await json("dados/relacoes/relacoes.json");
      if (!$("#r-rel").options.length) opcoes($("#r-rel"), REL.relacoes.map((r) => [r.id, `${r.analise} · ${r.titulo}`]));
      const rel = relAtual(); estado.rRel = rel.id; $("#r-rel").value = rel.id;
      const rec = rel.recortes.find((r) => r.id === estado.rRecorte) || rel.recortes[0]; estado.rRecorte = rec.id;
      opcoes($("#r-recorte"), rel.recortes.map((r) => [r.id, r.rot])); $("#r-recorte").value = rec.id;
      const anos = rec.anos.slice().reverse();
      const anoOpts = rec.por_ano ? anos.map((a) => [a, a]) : [["todos", "Todos os anos"], ...anos.map((a) => [a, a])];
      if (!anoOpts.some(([v]) => String(v) === String(estado.rAno))) estado.rAno = rec.por_ano ? anos[0] : "todos";
      opcoes($("#r-ano"), anoOpts); $("#r-ano").value = estado.rAno;
      const vs = Object.entries(rel.variaveis);
      if (!rel.variaveis[estado.rX]) estado.rX = rel.x;
      if (!rel.variaveis[estado.rY] || estado.rY === estado.rX) estado.rY = rel.y === estado.rX ? vs.find(([k]) => k !== estado.rX)[0] : rel.y;
      opcoes($("#r-x"), vs.map(([k, v]) => [k, v.rot])); $("#r-x").value = estado.rX;
      opcoes($("#r-y"), vs.filter(([k]) => k !== estado.rX).map(([k, v]) => [k, v.rot])); $("#r-y").value = estado.rY;
      $("#r-pergunta").innerHTML = `${esc(rel.pergunta)}<small>Análise ${esc(rel.analise)} · unidade: ${esc(rel.unidade)}</small>`;
      const ign = [!SEM_PARTIDO.has(rel.id) || !estado.partido ? "" : "o filtro de partido não se aplica (a unidade é o distrito)",
                   !SEM_UF.has(rel.id) || !estado.uf ? "" : "o filtro de UF não se aplica (a unidade é o partido nacional)"].filter(Boolean);
      $("#r-nota").textContent = rel.nota + (ign.length ? " Atenção: " + ign.join("; ") + "." : "");

      const arq = rec.por_ano ? `${rel.id}_${rec.id}_${estado.rAno}` : `${rel.id}_${rec.id}`;
      el.querySelector(".vazio") || (el.style.opacity = 0.5);
      const D = await json(`dados/relacoes/${arq}.json`);
      if (meu !== tokenRel) return; el.style.opacity = 1;
      const C = D.cols, X = C[estado.rX], Y = C[estado.rY];
      const usaP = !SEM_PARTIDO.has(rel.id), usaU = !SEM_UF.has(rel.id);
      const pi = estado.partido && usaP ? REL.partidos.indexOf(estado.partido) : -1, ui = estado.uf && usaU ? REL.ufs.indexOf(estado.uf) : -1;
      const anoF = estado.rAno === "todos" ? null : +estado.rAno;
      const xs = [], ys = [];
      for (let i = 0; i < D.n; i++) {
        if (anoF != null && C.ano[i] !== anoF) continue;
        if (pi >= 0 && C.p[i] !== pi) continue;
        if (ui >= 0 && C.uf[i] !== ui) continue;
        const a = X[i], b = Y[i]; if (a == null || b == null) continue;
        xs.push(a); ys.push(b);
      }
      const vx = rel.variaveis[estado.rX], vy = rel.variaveis[estado.rY];
      const binario = vx.tipo === "bin";
      const filtros = [estado.rAno === "todos" ? "todos os anos" : estado.rAno, pi >= 0 ? estado.partido : "todos os partidos", ui >= 0 ? UFS_NOMES[estado.uf] : "Brasil"].join(" · ");
      if (xs.length < 3) { el.innerHTML = `<p class="vazio">Poucos casos neste recorte (${xs.length}). Tente outro ano, partido ou UF.</p>`; $("#r-stats").innerHTML = ""; }
      else if (binario) desenharGrupos(el, xs, ys, vx, vy, filtros);
      else desenharDispersao(el, xs, ys, vx, vy, rel, filtros);
      ultimoCodigo = codigoR(rel, rec, vx, vy);
      $("#r-codigo-txt").textContent = ultimoCodigo;
    } catch (e) {
      if (meu === tokenRel) el.innerHTML = `<p class="vazio">Não foi possível carregar a relação (${esc(e.message)}).</p>`;
    }
  }

  function statBox(rot, valor, ctx) { return `<div class="stat"><div class="rotulo">${rot}</div><div class="valor">${valor}</div>${ctx ? `<div class="contexto">${ctx}</div>` : ""}</div>`; }

  function desenharDispersao(el, xs, ys, vx, vy, rel, filtros) {
    const S = estatisticas(xs, ys), n = xs.length;
    const W = Math.max(300, el.clientWidth), H = W < 560 ? 300 : 380, m = { t: 10, r: 14, b: 44, l: 52 };
    const iw = W - m.l - m.r, ih = H - m.t - m.b;
    const ext = (v) => { let lo = Infinity, hi = -Infinity; for (const a of v) { if (a < lo) lo = a; if (a > hi) hi = a; } if (lo === hi) { lo -= 1; hi += 1; } const p = (hi - lo) * 0.03; return [lo - p, hi + p]; };
    const [x0, x1] = ext(xs), [y0, y1] = ext(ys);
    const X = (v) => m.l + (v - x0) / (x1 - x0) * iw, Y = (v) => m.t + ih - (v - y0) / (y1 - y0) * ih;
    const ticks = (lo, hi) => { const st = Math.pow(10, Math.floor(Math.log10((hi - lo) / 4))); const k = [1, 2, 2.5, 5, 10].map((q) => q * st).find((s) => (hi - lo) / s <= 6); const o = []; for (let v = Math.ceil(lo / k) * k; v <= hi; v += k) o.push(+v.toFixed(10)); return o; };
    const fmtTick = (v) => Math.abs(v) >= 1000 ? (v / 1000).toLocaleString("pt-BR") + " mil" : v.toLocaleString("pt-BR", { maximumFractionDigits: 2 });
    // médias por décimo de X
    const ord = xs.map((_, i) => i).sort((a, b) => xs[a] - xs[b]), dec = [];
    for (let k = 0; k < 10; k++) { const ini = Math.floor(k * n / 10), fim = Math.floor((k + 1) * n / 10); if (fim <= ini) continue;
      let sx = 0, sy = 0; for (let j = ini; j < fim; j++) { sx += xs[ord[j]]; sy += ys[ord[j]]; } dec.push({ k: k + 1, x: sx / (fim - ini), y: sy / (fim - ini), n: fim - ini }); }
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Dispersão de ${esc(vy.rot)} por ${esc(vx.rot)}"><g class="grade">`;
    for (const v of ticks(y0, y1)) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}"/><text x="${m.l - 6}" y="${Y(v) + 4}" text-anchor="end">${fmtTick(v)}</text>`;
    for (const v of ticks(x0, x1)) s += `<line x1="${X(v)}" x2="${X(v)}" y1="${m.t}" y2="${m.t + ih}"/><text x="${X(v)}" y="${H - 26}" text-anchor="middle">${fmtTick(v)}</text>`;
    s += `</g><text x="${m.l + iw / 2}" y="${H - 6}" text-anchor="middle" style="fill:var(--text)">${esc(vx.rot)}</text>`;
    s += `<text transform="translate(12 ${m.t + ih / 2}) rotate(-90)" text-anchor="middle" style="fill:var(--text)">${esc(vy.rot)}</text>`;
    if (rel.diagonal) { const lo = Math.max(x0, y0), hi = Math.min(x1, y1); if (hi > lo) s += `<line x1="${X(lo)}" y1="${Y(lo)}" x2="${X(hi)}" y2="${Y(hi)}" stroke="${css("--muted")}" stroke-dasharray="4 3"/>`; }
    if (vy.tipo === "dri" && y0 < 0 && y1 > 0) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(0)}" y2="${Y(0)}" stroke="${css("--muted")}"/>`;
    if (isFinite(S.b)) s += `<line x1="${X(x0)}" y1="${Y(S.a + S.b * x0)}" x2="${X(x1)}" y2="${Y(S.a + S.b * x1)}" stroke="${css("--s2")}" stroke-width="2"/>`;
    s += `<path d="${dec.map((d, i) => (i ? "L" : "M") + X(d.x) + " " + Y(d.y)).join("")}" fill="none" stroke="${css("--text")}" stroke-width="1.5"/>`;
    dec.forEach((d, i) => { s += `<circle class="dec" data-i="${i}" cx="${X(d.x)}" cy="${Y(d.y)}" r="5" fill="${css("--text")}" stroke="${css("--surface")}" stroke-width="2"/><circle class="dec" data-i="${i}" cx="${X(d.x)}" cy="${Y(d.y)}" r="12" fill="transparent"/>`; });
    s += `</svg><div class="legenda"><span><i style="background:${css("--s1")};height:8px;width:8px;border-radius:50%"></i>Casos (${int(n)})</span><span><i style="background:${css("--s2")}"></i>Reta de regressão (MQO)</span><span><i style="background:${css("--text")}"></i>Média por décimo de X</span>${rel.diagonal ? `<span><i style="background:${css("--muted")}"></i>Proporcionalidade (y = x)</span>` : ""}</div>`;
    el.innerHTML = s;
    // pontos em canvas (rápido para dezenas de milhares)
    const svg = el.querySelector("svg"), cv = document.createElement("canvas"), dpr = devicePixelRatio || 1, rect = svg.getBoundingClientRect(), esc_ = rect.width / W;
    cv.width = Math.round(rect.width * dpr); cv.height = Math.round(rect.height * dpr); cv.style.width = rect.width + "px"; cv.style.height = rect.height + "px";
    el.insertBefore(cv, svg.nextSibling); svg.style.position = "relative"; svg.style.zIndex = 1;
    const ctx = cv.getContext("2d"); ctx.scale(dpr * esc_, dpr * esc_);
    ctx.fillStyle = css("--s1"); ctx.globalAlpha = n > 20000 ? 0.06 : n > 5000 ? 0.12 : n > 1000 ? 0.25 : n > 200 ? 0.45 : 0.7;
    const rr = n > 5000 ? 1.6 : 2.6;
    for (let i = 0; i < n; i++) { ctx.beginPath(); ctx.arc(X(xs[i]), Y(ys[i]), rr, 0, 6.2832); ctx.fill(); }
    el.querySelectorAll("circle.dec").forEach((c) => {
      c.addEventListener("pointermove", (ev) => { const d = dec[+c.dataset.i];
        mostraTip(ev, `<b>${d.k}º décimo de X</b>` + linhaTip("Média de X", fmtN(d.x)) + linhaTip("Média de Y", fmtN(d.y)) + linhaTip("Casos", int(d.n))); });
      c.addEventListener("pointerleave", () => ($("#tooltip").hidden = true));
    });
    if (!isFinite(S.r)) {
      $("#r-stats").innerHTML = statBox("Sem variação", "–", `${esc(isFinite(S.b) ? vy.rot : vx.rot)} tem o mesmo valor em todos os casos deste recorte, então não há correlação a calcular.`) + statBox("Casos", int(n), esc(filtros));
      return;
    }
    const forca = Math.abs(S.r) < 0.1 ? "muito fraca" : Math.abs(S.r) < 0.3 ? "fraca" : Math.abs(S.r) < 0.5 ? "moderada" : "forte";
    $("#r-stats").innerHTML =
      statBox("Correlação de Pearson (r)", `${fmtN(S.r, 3)}<small> ${estrelas(S.p)}</small>`, `p ${S.p < 0.001 ? "" : "= "}${fmtP(S.p)} · associação ${forca}${S.r < 0 ? " e negativa" : S.r > 0 ? " e positiva" : ""}`) +
      statBox("Correlação de Spearman (ρ)", fmtN(S.rs, 3), `p ${S.ps < 0.001 ? "" : "= "}${fmtP(S.ps)} · por postos, robusta a extremos`) +
      statBox("Inclinação (MQO)", fmtN(S.b, 3), `erro-padrão ${fmtN(S.se, 3)} · R² = ${fmtN(S.r2, 3)}<br>Y muda ${fmtN(S.b, 2)} para cada 1 de X`) +
      statBox("Casos", int(n), esc(filtros));
  }

  function desenharGrupos(el, xs, ys, vx, vy, filtros) {
    const y0 = [], y1 = []; xs.forEach((x, i) => (x >= 0.5 ? y1 : y0).push(ys[i]));
    if (!y0.length || !y1.length) { el.innerHTML = `<p class="vazio">Só há casos em um dos grupos neste recorte.</p>`; $("#r-stats").innerHTML = ""; return; }
    const T = welch(y0, y1), rot0 = "Não (0)", rot1 = "Sim (1)";
    const W = Math.max(300, el.clientWidth), H = 300, m = { t: 16, r: 20, b: 44, l: 56 }, iw = W - m.l - m.r, ih = H - m.t - m.b;
    const hi = Math.max(T.m0 + T.ic0, T.m1 + T.ic1) * 1.15, lo = Math.min(0, T.m0 - T.ic0, T.m1 - T.ic1) * 1.15;
    const Y = (v) => m.t + ih - (v - lo) / (hi - lo) * ih, cx = [m.l + iw * 0.3, m.l + iw * 0.7];
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Médias de ${esc(vy.rot)} por grupo"><g class="grade">`;
    const st = Math.pow(10, Math.floor(Math.log10((hi - lo) / 4))), k = [1, 2, 2.5, 5, 10].map((q) => q * st).find((s_) => (hi - lo) / s_ <= 6);
    for (let v = Math.ceil(lo / k) * k; v <= hi; v += k) s += `<line x1="${m.l}" x2="${W - m.r}" y1="${Y(v)}" y2="${Y(v)}"/><text x="${m.l - 6}" y="${Y(v) + 4}" text-anchor="end">${fmtN(v, k < 1 ? 2 : 0)}</text>`;
    s += `</g><text transform="translate(12 ${m.t + ih / 2}) rotate(-90)" text-anchor="middle" style="fill:var(--text)">${esc(vy.rot)} (média)</text>`;
    [[T.m0, T.ic0, T.n0, rot0, "--s1"], [T.m1, T.ic1, T.n1, rot1, "--s2"]].forEach(([mu, ic, nn, rot, cor], i) => {
      const w = Math.min(90, iw * 0.22);
      s += `<g class="grupo" data-i="${i}"><rect x="${cx[i] - w / 2}" y="${Y(Math.max(mu, 0))}" width="${w}" height="${Math.abs(Y(0) - Y(mu))}" rx="4" fill="${css(cor)}" opacity=".85"/>` +
        `<line x1="${cx[i]}" x2="${cx[i]}" y1="${Y(mu - ic)}" y2="${Y(mu + ic)}" stroke="${css("--text")}" stroke-width="1.5"/>` +
        `<line x1="${cx[i] - 8}" x2="${cx[i] + 8}" y1="${Y(mu + ic)}" y2="${Y(mu + ic)}" stroke="${css("--text")}" stroke-width="1.5"/>` +
        `<line x1="${cx[i] - 8}" x2="${cx[i] + 8}" y1="${Y(mu - ic)}" y2="${Y(mu - ic)}" stroke="${css("--text")}" stroke-width="1.5"/>` +
        `<text x="${cx[i] + w / 2 + 6}" y="${Y(mu) + 4}" style="fill:var(--text);font-weight:600">${fmtN(mu)}</text>` +
        `<text x="${cx[i]}" y="${H - 26}" text-anchor="middle" style="fill:var(--text)">${rot}</text><text x="${cx[i]}" y="${H - 10}" text-anchor="middle">${int(nn)} casos</text></g>`;
    });
    el.innerHTML = s + `</svg><p class="nota">${esc(vx.rot)}. Barras: média; traços: intervalo de 95%.</p>`;
    $("#r-stats").innerHTML =
      statBox("Diferença de médias (1 − 0)", `${fmtN(T.dif)}<small> ${estrelas(T.p)}</small>`, `teste t de Welch: t = ${fmtN(T.t)}, p ${T.p < 0.001 ? "" : "= "}${fmtP(T.p)}`) +
      statBox(`Média com ${rot1.toLowerCase()}`, fmtN(T.m1), `${int(T.n1)} casos`) +
      statBox(`Média com ${rot0.toLowerCase()}`, fmtN(T.m0), `${int(T.n0)} casos`) +
      statBox("Casos", int(T.n0 + T.n1), esc(filtros));
  }

  // ---------------------------------------------------------------- código R
  const R_CAB = (rel, rec, vx, vy) => `# bd_representa — Relações: ${rel.titulo} (Análise Exploratória ${rel.analise})
# Gerado em https://brunoschaefer.github.io/bd_representa/#relacoes
# Recorte: ${rec.rot} | Ano: ${estado.rAno === "todos" ? "todos" : estado.rAno} | Partido: ${estado.partido && !SEM_PARTIDO.has(rel.id) ? estado.partido : "todos"} | UF: ${estado.uf && !SEM_UF.has(rel.id) ? estado.uf : "todas"}
# X: ${vx.rot}
# Y: ${vy.rot}
# Unidade de análise: ${rel.unidade}
#
# Pacotes: install.packages(c("data.table", "R.utils", "ggplot2"))
# Citação: Schaefer, Bruno. bd_representa. IESP-UERJ, MAPE, 2026.
#          https://github.com/BrunoSchaefer/bd_representa

library(data.table)
library(ggplot2)

# Lê um banco da release (baixa uma vez para a pasta temporária)
url_release <- "https://github.com/BrunoSchaefer/bd_representa/releases/latest/download/"
ler <- function(banco, cols) {
  arq <- file.path(tempdir(), paste0(banco, ".csv.gz"))
  if (!file.exists(arq)) download.file(paste0(url_release, banco, ".csv.gz"), arq, mode = "wb")
  fread(arq, select = cols, encoding = "UTF-8")
}
`;
  const vetor = (v) => `c(${v.map((x) => `"${x}"`).join(", ")})`;
  const UF_IBGE_R = `uf_ibge <- c("11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP", "17" = "TO",
             "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL",
             "28" = "SE", "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP", "41" = "PR",
             "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT", "52" = "GO", "53" = "DF")`;
  const DESP_R = ["desp_valor_total_bruto", "desp_valor_aparato_pessoal", "desp_valor_aparato_sedes", "desp_valor_aparato_politica",
    "desp_valor_eleicoes_comunicacao", "desp_valor_eleicoes_publicidade", "desp_valor_transferencias"];

  function montarR(rel, rec) {
    const id = rel.id, r = rec.id;
    if (id === "r31") {
      const mun = r === "municipal";
      const dir = mun ? "bd_diretorios_municipal" : "bd_diretorios_estadual", lis = mun ? "bd_partidos_vereanca" : "bd_partidos_assembleias";
      const chD = mun ? "CD_MUNICIPIO" : "SG_UF", chL = mun ? "id_municipio_tse" : "sigla_uf";
      return `# Diretórios e votação da lista do partido na eleição ANTERIOR ao ano da direção
dir <- ler("${dir}", ${vetor(["ANO", "SG_PARTIDO_PAD", "SG_UF", ...(mun ? ["CD_MUNICIPIO"] : []), "total_membros", "proporcao_mulheres", "proporcao_mulheres_decisao", "tem_mulher_presidente", "tem_mulher_tesoureiro"])})
dir <- unique(dir, by = c("ANO", "SG_PARTIDO_PAD", "${chD}"))
dir[, ano_eleicao := ${mun ? "2012L + 4L * ((ANO - 2013L) %/% 4L)" : "2010L + 4L * ((ANO - 2011L) %/% 4L)"}]   # ${mun ? "2015-16 -> 2012; 2017-20 -> 2016; ..." : "2015-18 -> 2014; 2019-22 -> 2018; ..."}
lis <- ler("${lis}", c("ano", "SG_PARTIDO_PAD", "${chL}", "pct_votos_nominais_distrito"))
d <- merge(dir, lis[, .(ano_eleicao = ano, SG_PARTIDO_PAD, ${chD} = ${chL}, pct_votos_nominais_distrito)],
           by = c("ano_eleicao", "SG_PARTIDO_PAD", "${chD}"), all.x = TRUE)
d <- d[, .(ano = ANO, partido = SG_PARTIDO_PAD, uf = SG_UF,
           votos_ant = fcoalesce(as.numeric(pct_votos_nominais_distrito), 0),   # sem lista = 0%
           membros = total_membros,
           prop_mulheres = 100 * proporcao_mulheres, prop_decisao = 100 * proporcao_mulheres_decisao,
           mulher_pres = tem_mulher_presidente, mulher_tes = tem_mulher_tesoureiro)]`;
    }
    if (id === "r32") {
      const ver = r === "vereanca", lis = { vereanca: "bd_partidos_vereanca", assembleia: "bd_partidos_assembleias", camara: "bd_partidos_camara" }[r];
      const props = ["prop_candidatos_mulheres", "prop_receitas_mulheres", "prop_receitas_partidarias_mulheres", "prop_receitas_direcao_mulheres"];
      return `# Listas partidárias e direção do partido no distrito no ano ANTERIOR à eleição
lis <- ler("${lis}", ${vetor(["ano", "SG_PARTIDO_PAD", "sigla_uf", ...(ver ? ["id_municipio_tse"] : []), ...props])})
lis <- lis[ano %in% ${ver ? "c(2016, 2020, 2024)" : "c(2018, 2022)"}]
dir <- ler("${ver ? "bd_diretorios_municipal" : "bd_diretorios_estadual"}", ${vetor(["ANO", "SG_PARTIDO_PAD", ver ? "CD_MUNICIPIO" : "SG_UF", "tem_mulher_presidente", "tem_mulher_decisao"])})
dir <- unique(dir, by = c("ANO", "SG_PARTIDO_PAD", "${ver ? "CD_MUNICIPIO" : "SG_UF"}"))
d <- merge(lis, dir[, .(ano = ANO + 1L, SG_PARTIDO_PAD, ${ver ? "id_municipio_tse = CD_MUNICIPIO" : "sigla_uf = SG_UF"}, tem_mulher_presidente, tem_mulher_decisao)],
           by = c("ano", "SG_PARTIDO_PAD", "${ver ? "id_municipio_tse" : "sigla_uf"}"))
d <- d[, .(ano, partido = SG_PARTIDO_PAD, uf = sigla_uf,
           mulher_pres_ant = tem_mulher_presidente, mulher_dec_ant = tem_mulher_decisao,
           cand_m = 100 * prop_candidatos_mulheres, rec_m = 100 * prop_receitas_mulheres,
           rec_part_m = 100 * prop_receitas_partidarias_mulheres, rec_dir_m = 100 * prop_receitas_direcao_mulheres)]`;
    }
    if (id === "r33") {
      const med = ["candidatos", "receitas", "receitas_partidarias", "eleitos"], ver = r === "vereanca";
      const ind = ["genero", "racial"].flatMap((g) => med.map((m_) => `indice_${g}_${m_}_${r}`));
      const ren = ["genero", "racial"].flatMap((g) => med.map((m_) => `${g === "genero" ? "g" : "r"}_${m_} = indice_${g}_${m_}_${r}`));
      return `# Distritos: PIB per capita e índices de equidade (DRI)
${ver ? UF_IBGE_R + "\n" : ""}d <- ler("${ver ? "bd_distritos_municipal" : "bd_distritos_estadual"}", ${vetor(["ano", ver ? "id_municipio" : "sigla_uf", "pib_per_capita", ...ind])})
d <- d[, .(ano, partido = NA_character_, uf = ${ver ? "uf_ibge[substr(as.character(id_municipio), 1, 2)]" : "sigla_uf"},
           log_pib = log(fifelse(pib_per_capita > 0, pib_per_capita, NA_real_)), pib_mil = pib_per_capita / 1000,
           ${ren.join(",\n           ")})]`;
    }
    if (id === "rfg") {
      return `# Características nacionais dos partidos (partido-ano)
d <- ler("bd_diretorios_nacional", c("ANO", "SG_PARTIDO_PAD", "bls_ideologia", "bolognesi_ideologia", "salgado_democracia_interna", "ribeiro_ipd"))
d <- unique(d, by = c("ANO", "SG_PARTIDO_PAD"))
d <- d[, .(ano = ANO, partido = SG_PARTIDO_PAD, uf = "BR", bls = bls_ideologia, bolognesi = bolognesi_ideologia,
           salgado = salgado_democracia_interna, ribeiro = ribeiro_ipd)]`;
    }
    if (id === "rl") {
      const banco = `bd_diretorios_${r}`, ch = { nacional: [], estadual: ["SG_UF"], municipal: ["CD_MUNICIPIO"] }[r];
      return `# Despesas partidárias da esfera ${r}, somadas por partido-ano (2017-2025), e ideologia (BLS)
desp <- ler("${banco}", ${vetor(["ANO", "SG_PARTIDO_PAD", ...ch, ...DESP_R])})
desp <- unique(desp, by = ${vetor(["ANO", "SG_PARTIDO_PAD", ...ch])})
v <- ${vetor(DESP_R)}
desp <- desp[ANO >= 2017, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), by = .(ANO, SG_PARTIDO_PAD), .SDcols = v]
desp <- desp[desp_valor_total_bruto > 0]
ideo <- unique(ler("bd_diretorios_nacional", c("ANO", "SG_PARTIDO_PAD", "bls_ideologia")), by = c("ANO", "SG_PARTIDO_PAD"))
d <- merge(desp, ideo, by = c("ANO", "SG_PARTIDO_PAD"))
d <- d[, .(ano = ANO, partido = SG_PARTIDO_PAD, uf = "BR", bls = bls_ideologia,
           pct_aparato = 100 * (desp_valor_aparato_pessoal + desp_valor_aparato_sedes + desp_valor_aparato_politica) / desp_valor_total_bruto,
           pct_pessoal = 100 * desp_valor_aparato_pessoal / desp_valor_total_bruto,
           pct_eleicoes = 100 * (desp_valor_eleicoes_comunicacao + desp_valor_eleicoes_publicidade) / desp_valor_total_bruto,
           pct_transferencias = 100 * desp_valor_transferencias / desp_valor_total_bruto,
           desp_mi = desp_valor_total_bruto / 1e6)]`;
    }
    if (id === "rno") {
      const ver = r === "vereanca", ch = ver ? "id_municipio_tse" : "sigla_uf";
      return `# Listas partidárias: cadeiras obtidas e parcela das receitas dos diretórios do distrito
d <- ler("${ver ? "bd_partidos_vereanca" : "bd_partidos_assembleias"}", ${vetor(["ano", "SG_PARTIDO_PAD", "sigla_uf", ...(ver ? [ch] : []), "pct_cadeiras", "pct_votos_nominais_distrito", "diretorio_rec_valor_total_bruto"])})
d <- d[!is.na(diretorio_rec_valor_total_bruto)${ver ? "" : " & ano < 2026"}]
d[, rec := as.numeric(diretorio_rec_valor_total_bruto)]
d[, pct_recursos := if (sum(rec) > 0) 100 * rec / sum(rec) else NA_real_, by = .(ano, ${ch})]
d <- d[!is.na(pct_recursos), .(ano, partido = SG_PARTIDO_PAD, uf = sigla_uf, pct_cadeiras,
                              pct_votos = pct_votos_nominais_distrito, pct_recursos)]`;
    }
    if (id === "rq") {
      const pref = r === "prefeitura";
      return `# Candidaturas: parcela do gasto com internet do distrito e votação
d <- ler("bd_candidatos_${r}", ${vetor(["ano", "sigla_uf", ...(pref ? ["id_municipio"] : []), "SG_PARTIDO_PAD", "despesas_internet_can", "pct_votos_distrito"])})
d[, internet := fcoalesce(as.numeric(despesas_internet_can), 0)]
d[, tot := sum(internet), by = .(ano, ${pref ? "id_municipio" : "sigla_uf"})]
d <- d[tot > 0 & !is.na(pct_votos_distrito)]   # só distritos com algum gasto com internet
d <- d[, .(ano, partido = SG_PARTIDO_PAD, uf = sigla_uf, pct_internet = 100 * internet / tot,
           pct_votos = pct_votos_distrito, internet_mil = internet / 1000)]`;
    }
    if (id === "rs") {
      const mun = r === "municipal", chD = mun ? "CD_MUNICIPIO" : "SG_UF", chL = mun ? "id_municipio_tse" : "sigla_uf";
      return `# Rotatividade da direção no ano seguinte à eleição e desempenho da lista do partido
dir <- ler("${mun ? "bd_diretorios_municipal" : "bd_diretorios_estadual"}", ${vetor(["ANO", "SG_PARTIDO_PAD", "SG_UF", ...(mun ? ["CD_MUNICIPIO"] : []), "n_membros_t1", "turnover_prop", "turnover_prop_decisao"])})
dir <- unique(dir, by = c("ANO", "SG_PARTIDO_PAD", "${chD}"))
dir <- dir[ANO %in% ${mun ? "c(2017, 2021, 2025)" : "c(2019, 2023)"} & !is.na(turnover_prop) & n_membros_t1 > 0]
lis <- ler("${mun ? "bd_partidos_vereanca" : "bd_partidos_assembleias"}", c("ano", "SG_PARTIDO_PAD", "${chL}", "n_eleitos"))
d <- merge(dir, lis[, .(ANO = ano + 1L, SG_PARTIDO_PAD, ${chD} = ${chL}, n_eleitos)],
           by = c("ANO", "SG_PARTIDO_PAD", "${chD}"), all.x = TRUE)
d <- d[, .(ano = ANO, partido = SG_PARTIDO_PAD, uf = SG_UF,
           sem_eleitos = as.integer(is.na(n_eleitos) | n_eleitos == 0),   # sem lista ou lista sem eleitos
           n_eleitos = fcoalesce(as.numeric(n_eleitos), 0),
           turnover = 100 * turnover_prop, turnover_dec = 100 * turnover_prop_decisao, membros_t1 = n_membros_t1)]`;
    }
    return "";
  }

  function codigoR(rel, rec, vx, vy) {
    const x = estado.rX, y = estado.rY, filt = [];
    if (estado.rAno !== "todos") filt.push(`ano == ${estado.rAno}`);
    if (estado.partido && !SEM_PARTIDO.has(rel.id)) filt.push(`partido == "${estado.partido}"`);
    if (estado.uf && !SEM_UF.has(rel.id)) filt.push(`uf == "${estado.uf}"`);
    const base = `${R_CAB(rel, rec, vx, vy)}
# Base da relação ------------------------------------------------------------------
${montarR(rel, rec)}

# Filtros escolhidos na página ------------------------------------------------------
${filt.length ? `d <- d[${filt.join(" & ")}]` : "# (nenhum filtro)"}
z <- d[!is.na(${x}) & !is.na(${y})]
nrow(z)
`;
    const tituloG = vy.rot.replace(/"/g, "'"), xg = vx.rot.replace(/"/g, "'");
    if (vx.tipo === "bin") return base + `
# Comparação de médias (Y por grupo de X) -------------------------------------------
z[, .(casos = .N, media = mean(${y}), dp = sd(${y})), by = ${x}][order(${x})]
t.test(${y} ~ ${x}, data = z)   # teste t de Welch (variâncias diferentes)
summary(lm(${y} ~ ${x}, data = z))   # diferença de médias como regressão
${rel.controle ? `summary(lm(${y} ~ ${x} + ${rel.controle}, data = z))   # controlando por ${rel.variaveis[rel.controle].rot.toLowerCase()}, como na Análise Exploratória\n` : ""}
ggplot(z[, .(media = mean(${y}), ic = 1.96 * sd(${y}) / sqrt(.N)), by = .(grupo = factor(${x}, 0:1, c("Não (0)", "Sim (1)")))],
       aes(grupo, media)) +
  geom_col(width = 0.5, fill = "#2a78d6") +
  geom_errorbar(aes(ymin = media - ic, ymax = media + ic), width = 0.15) +
  labs(x = "${xg}", y = "${tituloG} (média)",
       caption = "Fonte: bd_representa (Schaefer, 2026). Barras: média; traços: IC de 95%.") +
  theme_minimal()

# Modelos com efeitos fixos e erros agrupados: ver "4 Análises exploratórias/bd_representa_analise.R"
# (análise ${rel.analise}) em https://github.com/BrunoSchaefer/bd_representa
`;
    return base + `
# Correlações e regressão -----------------------------------------------------------
cor.test(z$${x}, z$${y})                                        # Pearson
cor.test(z$${x}, z$${y}, method = "spearman", exact = FALSE)    # Spearman
summary(lm(${y} ~ ${x}, data = z))                               # MQO

# Média de Y por décimo de X
z[, decimo := pmin(10L, as.integer(ceiling(10 * frank(${x}, ties.method = "first") / .N)))]
z[, .(casos = .N, media_x = mean(${x}), media_y = mean(${y})), by = decimo][order(decimo)]

ggplot(z, aes(${x}, ${y})) +
  geom_point(alpha = ${xsN(rel)}, size = 1, colour = "#2a78d6") +${rel.diagonal ? `
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +` : ""}
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#eb6834") +
  labs(x = "${xg}", y = "${tituloG}",
       caption = "Fonte: bd_representa (Schaefer, 2026).") +
  theme_minimal()

# Modelos com efeitos fixos e erros agrupados: ver "4 Análises exploratórias/bd_representa_analise.R"
# (análise ${rel.analise}) em https://github.com/BrunoSchaefer/bd_representa
`;
  }
  const xsN = (rel) => ({ r31: 0.05, r32: 0.1, rno: 0.1, rq: 0.15, rs: 0.05 }[rel.id] || 0.5);

  $("#r-codigo-btn").addEventListener("click", () => {
    const box = $("#r-codigo"), ab = box.hidden; box.hidden = !ab; $("#r-codigo-btn").setAttribute("aria-expanded", ab);
  });
  $("#r-copiar").addEventListener("click", async () => {
    try { await navigator.clipboard.writeText(ultimoCodigo); $("#r-copiado").textContent = "Código copiado."; }
    catch { const t = document.createElement("textarea"); t.value = ultimoCodigo; document.body.appendChild(t); t.select(); document.execCommand("copy"); t.remove(); $("#r-copiado").textContent = "Código copiado."; }
    setTimeout(() => ($("#r-copiado").textContent = ""), 2500);
  });
  $("#r-baixar").addEventListener("click", () => {
    const rel = relAtual(), nome = `bd_representa_relacao_${rel.analise.replace(/\W+/g, "")}_${estado.rRecorte}_${estado.rX}_${estado.rY}.R`;
    const a = document.createElement("a"); a.href = URL.createObjectURL(new Blob([ultimoCodigo], { type: "text/plain;charset=utf-8" }));
    a.download = nome; a.click(); setTimeout(() => URL.revokeObjectURL(a.href), 1000);
  });

  /* ------------------------------------------------------------ 3. partidos (barras) */
  // Barras horizontais por partido. modo "pct": 0 a máx, com linha de referência opcional;
  // modo "dri": divergente a partir de 0 (−1 a 1), cores da escala do mapa.
  function barras(el, dados, { modo = "pct", ref = null, refRot = "", detalhe }) {
    if (!dados.length) { el.innerHTML = `<p class="vazio">Sem dados para este recorte.</p>`; return; }
    const W = Math.max(320, el.clientWidth), bh = 18, gap = 6, m0 = { t: ref != null ? 22 : 8, r: 56, b: 24, l: 108 };
    const H = m0.t + m0.b + dados.length * (bh + gap), iw = W - m0.l - m0.r;
    let x, ticks;
    if (modo === "dri") {
      const lim = Math.min(1, Math.max(0.2, Math.ceil(Math.max(...dados.map((d) => Math.abs(d.v))) * 5) / 5));
      x = (v) => m0.l + ((v + lim) / (2 * lim)) * iw;
      ticks = []; for (let v = -lim; v <= lim + 1e-9; v += lim / 2) ticks.push([v, fmtDri(Math.round(v * 100) / 100)]);
    } else {
      const topo = Math.min(1, Math.ceil(Math.max(ref || 0, ...dados.map((d) => d.v)) * 10) / 10 || 0.1);
      x = (v) => m0.l + (v / topo) * iw;
      ticks = []; for (let v = 0; v <= topo + 1e-9; v += topo > 0.5 ? 0.2 : 0.1) ticks.push([v, Math.round(v * 100) + "%"]);
    }
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Barras por partido"><g class="grade">`;
    for (const [v, t] of ticks) s += `<line x1="${x(v)}" x2="${x(v)}" y1="${m0.t}" y2="${H - m0.b}"/><text x="${x(v)}" y="${H - 6}" text-anchor="middle">${t}</text>`;
    s += `</g>`;
    const x0 = modo === "dri" ? x(0) : m0.l;
    dados.forEach((d, i) => {
      const yy = m0.t + i * (bh + gap), sel = estado.partido === d.p, apagado = estado.partido && !sel;
      const cor = modo === "dri" ? corDri(d.v) : css(apagado ? "--bar-dim" : "--bar");
      const xa = x(d.v), w = Math.max(2, Math.abs(xa - x0)), esq = xa < x0, r = Math.min(4, w / 2);
      const forma = esq
        ? `M${x0} ${yy}h${-(w - r)}a${r} ${r} 0 0 0 ${-r} ${r}v${bh - 2 * r}a${r} ${r} 0 0 0 ${r} ${r}h${w - r}z`
        : `M${x0} ${yy}h${w - r}a${r} ${r} 0 0 1 ${r} ${r}v${bh - 2 * r}a${r} ${r} 0 0 1 ${-r} ${r}h${-(w - r)}z`;
      const rotulo = modo === "dri" ? fmtDri(d.v) : pct(d.v, 0);
      s += `<g class="barra" data-i="${i}" opacity="${apagado && modo === "dri" ? 0.45 : 1}"><rect x="0" y="${yy - gap / 2}" width="${W}" height="${bh + gap}" fill="transparent"/>` +
        `<text x="${m0.l - 8}" y="${yy + bh / 2 + 4}" text-anchor="end" style="fill:var(${sel ? "--text" : "--text-2"});font-weight:${sel ? 700 : 400}">${esc(d.p)}</text>` +
        `<path d="${forma}" fill="${cor}"/>` +
        `<text x="${esq ? xa - 6 : xa + 6}" y="${yy + bh / 2 + 4}" text-anchor="${esq ? "end" : "start"}">${rotulo}</text></g>`;
    });
    if (modo === "dri") s += `<line x1="${x0}" x2="${x0}" y1="${m0.t - 4}" y2="${H - m0.b}" stroke="${css("--muted")}"/>`;
    if (ref != null) s += `<line x1="${x(ref)}" x2="${x(ref)}" y1="${m0.t - 6}" y2="${H - m0.b}" stroke="${css("--text")}" stroke-dasharray="4 3"/>` +
      `<text x="${x(ref)}" y="${m0.t - 9}" text-anchor="middle" style="fill:var(--text)">${esc(refRot)} ${pct(ref, 0)}</text>`;
    el.innerHTML = s + `</svg>`;
    const tip = $("#tooltip");
    el.querySelectorAll(".barra").forEach((g) => {
      g.addEventListener("pointermove", (ev) => {
        tip.innerHTML = detalhe(dados[+g.dataset.i]); tip.hidden = false;
        tip.style.left = Math.min(ev.clientX + 14, innerWidth - tip.offsetWidth - 8) + "px";
        tip.style.top = Math.max(8, ev.clientY - tip.offsetHeight - 12) + "px";
      });
      g.addEventListener("pointerleave", () => (tip.hidden = true));
    });
  }
  const linhaTip = (a, b) => `<div class="linha"><span>${a}</span><span>${b}</span></div>`;

  const DIMS = {
    direcoes: { sub: "Percentual de mulheres nas direções partidárias." },
    candidaturas: { medida: "candidaturas", sub: "Participação do grupo nas candidaturas da lista do partido." },
    eleitos: { medida: "eleitos", sub: "Participação do grupo entre os eleitos da lista do partido." },
    financiamento: { sub: "Participação do grupo nas receitas de campanha dos candidatos do partido." },
    equidade: { sub: "Índice de equidade (DRI) do partido: participação do grupo comparada ao seu peso no eleitorado (mulheres) ou na população (pessoas negras)." },
  };
  let tokenPart = 0;

  async function partidos() {
    const el = $("#g-partidos"), dim = estado.pDim, eleitoral = dim !== "direcoes";
    // mostra só os controles da dimensão
    $("#p-nivel-l").hidden = eleitoral; $("#p-cargo-l").hidden = !eleitoral; $("#p-grupo").hidden = !eleitoral;
    $("#p-medida-l").hidden = !(dim === "financiamento" || dim === "equidade");
    const medOpts = dim === "financiamento" ? ["receitas", "receitas_publicas"] : ["candidaturas", "eleitos", "votos", "receitas", "receitas_publicas"];
    if (!medOpts.includes(estado.pMedida)) estado.pMedida = medOpts[0];
    opcoes($("#p-medida"), medOpts.map((m) => [m, MED_ROT[m][0].toUpperCase() + MED_ROT[m].slice(1)])); $("#p-medida").value = estado.pMedida;
    const grupoTxt = estado.pGrupo === "mulheres" ? "Mulheres" : "Pessoas negras";

    if (!eleitoral) {
      const anos = [...new Set(DIR.map((r) => r.ano))].sort();
      if (!anos.includes(estado.pAno)) estado.pAno = anos[anos.length - 1];
      opcoes($("#p-ano"), anos.slice().reverse().map((a) => [a, a])); $("#p-ano").value = estado.pAno;
      $("#p-sub").textContent = DIMS.direcoes.sub;
      $("#p-nota").textContent = "Só aparecem partidos com pelo menos 20 membros no recorte. O partido escolhido no filtro fica destacado.";
      const rows = filtra(DIR, { partido: false }).filter((r) => r.nivel === estado.pNivel && r.ano === estado.pAno);
      const m = soma(rows, (r) => r.partido, ["mulheres", "membros"]);
      const dados = [...m].filter(([, o]) => o.membros >= 20).map(([p, o]) => ({ p, v: o.mulheres / o.membros, n: o.membros, mu: o.mulheres })).sort((a, b) => b.v - a.v);
      barras(el, dados, { detalhe: (d) => `<b>${esc(d.p)}</b>` + linhaTip("Mulheres", pct(d.v)) + linhaTip("Membros", `${int(d.mu)} de ${int(d.n)}`) });
      return;
    }

    const meu = ++tokenPart;
    try {
      IND_MAPA ||= await json("dados/mapa/indice.json");
      const anos = IND_MAPA[estado.pCargo] || [];
      if (!anos.includes(estado.pAno)) estado.pAno = anos[anos.length - 1];
      opcoes($("#p-ano"), anos.slice().reverse().map((a) => [a, a])); $("#p-ano").value = estado.pAno;
      const municipal = estado.pCargo === "vereanca";
      const [doc, nomes] = await Promise.all([json(`dados/mapa/${estado.pCargo}_${estado.pAno}.json`), municipal ? json("dados/geo/nomes.json") : null]);
      if (meu !== tokenPart) return;
      const ufDe = (d) => municipal ? (nomes[d] || "").slice(-3, -1) : d;
      const med = dim === "candidaturas" || dim === "eleitos" ? DIMS[dim].medida : estado.pMedida;
      const k = doc.medidas.indexOf(med), g = estado.pGrupo === "mulheres" ? 0 : 1;
      // por partido: b = Σ b·T / Σ T e p = Σ p·T / Σ T (p do distrito ponderado pelo peso da lista)
      const acc = new Map();
      for (const l of doc.linhas) {
        if (estado.uf && ufDe(l[0]) !== estado.uf) continue;
        const p = doc.partidos[l[1]], T = l[2 + 3 * k], b = l[3 + 3 * k + g], ref = doc.distritos[l[0]]?.[g];
        let o = acc.get(p); if (!o) acc.set(p, (o = { num: 0, den: 0, pnum: 0, cand: 0, listas: 0 }));
        o.cand += l[2]; o.listas++;
        if (b != null && ref != null && T > 0) { o.num += b * T; o.den += T; o.pnum += ref * T; }
      }
      const dados = [...acc].filter(([p, o]) => p !== "?" && o.cand >= 20 && o.den > 0).map(([p, o]) => {
        const b = o.num / o.den, ref = o.pnum / o.den;
        return { p, b, ref, v: dim === "equidade" ? dri(b, ref) : b, T: o.den, cand: o.cand, listas: o.listas };
      }).sort((a, b) => b.v - a.v);
      // referência geral (todas as listas): peso do grupo ponderado
      const tot = dados.reduce((s, d) => ((s.r += d.ref * d.T), (s.t += d.T), s), { r: 0, t: 0 });
      const refGeral = tot.t ? tot.r / tot.t : null;
      const quem = estado.pGrupo === "mulheres" ? "eleitorado" : "população";
      const fmtT = (v) => med === "receitas" || med === "receitas_publicas" ? "R$ " + int(v) : int(v);
      $("#p-sub").textContent = `${DIMS[dim].sub} ${grupoTxt}, ${$("#p-cargo").selectedOptions[0].text.replace(/ \(.*\)/, "")}, ${estado.pAno}${estado.uf ? ", " + UFS_NOMES[estado.uf] : ""}${dim === "equidade" || dim === "financiamento" ? " (" + MED_ROT[med] + ")" : ""}.`;
      $("#p-nota").textContent = (dim === "equidade"
        ? "DRI: −1 = grupo ausente; 0 = paridade; 1 = grupo detém tudo. O peso de referência de cada partido é o do " + quem + " dos distritos onde ele lançou lista, ponderado pelo tamanho da lista. "
        : `Linha tracejada: peso do grupo no ${quem} dos distritos (ponderado). `) +
        "Só aparecem partidos com pelo menos 20 candidaturas no recorte." + (estado.pGrupo === "negros" && estado.pAno < 2014 ? " O TSE só informa cor/raça a partir de 2014." : "");
      barras(el, dados, {
        modo: dim === "equidade" ? "dri" : "pct", ref: dim === "equidade" ? null : refGeral, refRot: quem === "eleitorado" ? "Eleitorado" : "População",
        detalhe: (d) => `<b>${esc(d.p)}</b>` + (dim === "equidade" ? linhaTip("DRI", `<strong>${fmtDri(d.v)}</strong>`) : "") +
          linhaTip(`${grupoTxt} em ${MED_ROT[med]}`, pct(d.b)) + linhaTip(`No ${quem} (p)`, pct(d.ref)) +
          linhaTip(MED_ROT[med][0].toUpperCase() + MED_ROT[med].slice(1), fmtT(d.T)) + linhaTip("Listas", int(d.listas)),
      });
    } catch (e) {
      if (meu === tokenPart) el.innerHTML = `<p class="vazio">Não foi possível carregar os dados (${esc(e.message)}).</p>`;
    }
  }

  /* ------------------------------------------------------------ 4. arquivos */
  function arquivos() {
    const grupo = estado.aGrupo;
    if (!ARQ.length) { $("#t-arquivos").innerHTML = `<p class="vazio">Os arquivos estão sendo enviados para a <a href="${REPO}/releases">release</a> e aparecem aqui em breve.</p>`; return; }
    const lista = ARQ.filter((a) => grupo === "finais" ? !/^etapa[12]_/.test(a.arquivo) : a.arquivo.startsWith(grupo + "_"));
    const mb = (b) => b >= 1e9 ? (b / 1e9).toLocaleString("pt-BR", { maximumFractionDigits: 2 }) + " GB" : (b / 1e6).toLocaleString("pt-BR", { maximumFractionDigits: 1 }) + " MB";
    const dic = (n) => { const base = n.replace(/\.csv\.gz$/, "").replace(/_\d{4}$/, ""); return grupo === "finais" ? `${REPO}/blob/main/3%20Produtos%20finais/dicionarios/dicionario_${base}_variaveis.csv` : null; };
    $("#t-arquivos").innerHTML = `<table><thead><tr><th>Arquivo</th><th class="n">Tamanho (.gz)</th>${grupo === "finais" ? "<th>Dicionário</th>" : "<th>Origem</th>"}</tr></thead><tbody>` +
      lista.map((a) => `<tr><td><a href="${REPO}/releases/latest/download/${encodeURIComponent(a.arquivo)}">${esc(a.arquivo)}</a></td><td class="n">${mb(a.bytes)}</td>` +
        (grupo === "finais" ? `<td><a href="${dic(a.arquivo)}">variáveis</a></td>` : `<td class="dica">${esc(a.origem)}</td>`) + `</tr>`).join("") + `</tbody></table>`;
  }
})();
