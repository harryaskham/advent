// The solution viewer: years, a 25-day calendar, and one day's critique and code.
// Routes are #/YEAR/DAY. Year files (data/YEAR.json) load on demand and are cached.
(() => {
  const $ = (id) => document.getElementById(id);
  const yearsEl = $("years"), daysEl = $("days"), dayEl = $("day");
  const cache = new Map();
  let index = [], current = null;

  const esc = (s) => s.replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
  const prose = (s) => esc(s).replace(/`([^`]+)`/g, "<code>$1</code>"); // critiques mark code with backticks
  const getYear = (y) => {
    if (!cache.has(y)) cache.set(y, fetch(`data/${y}.json`).then((r) => r.json()));
    return cache.get(y);
  };
  const parse = () => {
    const m = location.hash.match(/^#\/(\d{4})(?:\/(\d{1,2}))?$/);
    const last = index[index.length - 1];
    if (!m) return null;
    const y = index.find((e) => e.year === +m[1]) || last;
    const d = m[2] ? Math.min(Math.max(+m[2], 1), y.days.length) : 1;
    return { year: y.year, day: d };
  };
  const all = () => index.flatMap((y) => y.days.map(([d]) => ({ year: y.year, day: d })));

  function renderNav(route) {
    yearsEl.innerHTML = index.map((y) =>
      `<a href="#/${y.year}/1" class="era-${y.era}"${y.year === route.year ? ' aria-current="true"' : ""}>${y.year}</a>`).join("");
    const y = index.find((e) => e.year === route.year);
    daysEl.innerHTML = y.days.map(([d, title, lines]) =>
      `<a href="#/${y.year}/${d}" title="Day ${d}: ${esc(title)}"${d === route.day ? ' aria-current="page"' : ""}>` +
      `<b>${d}</b><span>${esc(title)}</span><i>${lines}</i></a>`).join("");
  }

  async function renderDay(route, focus) {
    const data = await getYear(route.year);
    const s = data.days.find((d) => d.day === route.day);
    const seq = all(), i = seq.findIndex((e) => e.year === route.year && e.day === route.day);
    const prev = seq[i - 1], next = seq[i + 1];
    const link = (e, label, rel) => e ? `<a href="#/${e.year}/${e.day}" rel="${rel}">${label}</a>` : `<span></span>`;
    document.title = `${route.year} day ${route.day}: ${s.title} · advent`;
    dayEl.innerHTML = `
      <header class="dh">
        <p class="when">${route.year} · day ${route.day}</p>
        <h2 id="day-title" tabindex="-1">${esc(s.title)}</h2>
        <p class="links">
          <a href="https://adventofcode.com/${route.year}/day/${route.day}">The puzzle</a>
          <a href="https://github.com/harryaskham/advent/blob/main/${esc(s.path)}">Source</a>
          <span class="meta">${s.code_lines} lines${s.imports ? ` · ${s.imports} imports` : ""}</span>
        </p>
      </header>
      <div class="crit">
        ${s.approach ? `<p class="approach">${prose(s.approach)}</p><p>${prose(s.critique)}</p>` : `<p class="muted">No critique yet.</p>`}
        ${s.tags.length ? `<p class="tags">${s.tags.map((t) => `<span>${esc(t)}</span>`).join("")}</p>` : ""}
      </div>
      <pre class="code" tabindex="0" aria-label="Solution code"><code>${s.html}</code></pre>
      <nav class="step" aria-label="Previous and next day">${link(prev, "← previous", "prev")}${link(next, "next →", "next")}</nav>`;
    dayEl.querySelectorAll("pre abbr").forEach((a) => a.setAttribute("tabindex", "0"));
    window.dispatchEvent(new Event("resize")); // let site.js mark wide code
    if (focus) $("day-title").focus({ preventScroll: false });
  }

  async function go(focus) {
    let route = parse();
    if (!route) {
      const last = index[index.length - 1];
      route = { year: last.year, day: 1 };
      history.replaceState(null, "", `#/${route.year}/${route.day}`);
    }
    current = route;
    renderNav(route);
    await renderDay(route, focus);
  }

  addEventListener("hashchange", () => go(true));
  addEventListener("keydown", (e) => {
    if (e.altKey || e.ctrlKey || e.metaKey || /input|textarea|select/i.test(e.target.tagName)) return;
    if (e.key !== "ArrowLeft" && e.key !== "ArrowRight") return;
    if (e.target.closest && e.target.closest("pre")) return; // arrows scroll code
    const seq = all(), i = seq.findIndex((x) => x.year === current.year && x.day === current.day);
    const to = seq[i + (e.key === "ArrowRight" ? 1 : -1)];
    if (to) { location.hash = `#/${to.year}/${to.day}`; e.preventDefault(); }
  });
  fetch("data/index.json").then((r) => r.json()).then((d) => { index = d; go(false); });
})();
