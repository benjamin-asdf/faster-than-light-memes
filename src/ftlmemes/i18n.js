// Language picker for translated posts.
// A translated post is one page per language, <base>_en.html, <base>_de.html, ...
// The build (ftlm/write-language-pages) adds <link rel="alternate" hreflang> to
// each of them and writes <base>.html, which our own links point to. That page
// picks the language: localStorage, else ?lang=, else English.
// Picking a language here remembers it and goes to that version.

(function () {
  const names = {en: 'English', de: 'Deutsch'};
  const alternates = [...document.querySelectorAll('link[rel="alternate"][hreflang]')];
  if (alternates.length < 2) return;

  const file = location.pathname.split('/').pop();
  const current = alternates.find(a => a.getAttribute('href') === file);

  const select = document.createElement('select');
  select.id = 'lang-picker';
  select.setAttribute('aria-label', 'Language');
  select.style.width = 'auto';
  select.style.margin = '0';
  for (const a of alternates) {
    const lang = a.getAttribute('hreflang');
    const opt = document.createElement('option');
    opt.value = a.getAttribute('href');
    opt.textContent = names[lang] || lang;
    opt.dataset.lang = lang;
    select.appendChild(opt);
  }
  if (current) select.value = current.getAttribute('href');
  select.addEventListener('change', e => {
    const opt = e.target.selectedOptions[0];
    try { localStorage.setItem('ftlm-lang', opt.dataset.lang); } catch (_e) {}
    location.href = opt.value + location.hash;
  });

  const wrap = document.createElement('div');
  wrap.style.display = 'flex';
  wrap.style.justifyContent = 'flex-end';
  wrap.appendChild(select);
  const content = document.querySelector('#content');
  content.insertBefore(wrap, content.firstChild);
})();
