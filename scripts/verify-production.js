const fs = require('fs'), path = require('path'), crypto = require('crypto');
const { chromium } = require(path.join(process.env.CODEX_NODE_MODULES, 'playwright'));
const base = process.argv[2] || 'http://127.0.0.1:1118';
const dir = path.resolve('tmp/publication-qa', base.startsWith('https:') ? 'public' : 'local');
fs.mkdirSync(dir, { recursive: true });
const report = { base, pages: [], downloads: [], errors: [], failedRequests: [], interactions: {} };
const assert = (v, m) => { if (!v) throw Error(m); };
const routes = ['/', '/research/', '/teaching/', '/code-data/', '/insights/', '/beyond-economics/', '/contact/',
  '/research/household-demand-leisure/', '/research/children-costs-one-adult-household/', '/research/children-labor-supply-equations/',
  '/research/household-demand-leisure/sources/', '/teaching/econometrics-l3/', '/teaching/econometrics-m1/', '/teaching/econometrics-m2/',
  '/teaching/data-analysis-l3/', '/teaching/microeconomics-l1/', '/teaching/econometrics-niort/', '/teaching/microeconomics-iii-niort/',
  '/code-data/homogeneity-readme/', '/insights/quest-ce-quun-modele/', '/insights/homogeneity/',
  '/beyond-economics/calligraphie-de-bonheur/', '/beyond-economics/ma-cherie/', '/beyond-economics/mes-nuits/', '/beyond-economics/on-me-l-a-interdit/',
  '/beyond-economics/on-reading-preferences/', '/beyond-economics/if-i-had-24-hours-left/', '/beyond-economics/echoes-of-childhood/', '/404.html'];
(async () => {
  const browser = await chromium.launch({ headless: true, executablePath: 'C:/Program Files/Google/Chrome/Application/chrome.exe' });
  try {
    const context = await browser.newContext({ viewport: { width: 1440, height: 1000 } });
    const page = await context.newPage();
    page.on('pageerror', e => report.errors.push(e.message));
    page.on('requestfailed', req => { if (req.failure()?.errorText !== 'net::ERR_ABORTED') report.failedRequests.push({ url: req.url(), error: req.failure()?.errorText }); });
    for (const theme of ['dark', 'light']) for (const width of [1440, 390]) {
      await page.setViewportSize({ width, height: width < 500 ? 844 : 1000 });
      await page.goto(base + '/');
      await page.evaluate(t => localStorage.setItem('av-theme', t), theme);
      for (const route of routes) {
        const response = await page.goto(base + route, { waitUntil: 'load' });
        assert(response.status() === 200 || route === '/404.html' && [200, 404].includes(response.status()), route + ': ' + response.status());
        await page.evaluate(() => document.fonts.ready);
        const result = await page.evaluate(() => ({ theme: document.documentElement.dataset.theme,
          width: innerWidth, scrollWidth: document.documentElement.scrollWidth, brokenImages: [...document.images].filter(i => i.complete && i.naturalWidth === 0).map(i => i.src),
          mathErrors: document.querySelectorAll('.katex-error').length, ribbon: !!document.querySelector('.preview-ribbon'), robots: document.querySelector('meta[name=robots]')?.content }));
        assert(result.theme === theme && result.scrollWidth <= width + 1 && !result.mathErrors && !result.ribbon && !result.brokenImages.length, JSON.stringify({ route, ...result }));
        assert(result.robots === (route === '/404.html' ? 'noindex, follow' : 'index, follow'), route + ' robots');
        report.pages.push({ route, ...result });
        if (route === '/' || route === '/research/' && width === 1440) await page.screenshot({ path: path.join(dir, `${route === '/' ? 'home' : 'research'}-${width}-${theme}.png`), fullPage: true });
      }
      console.log(`Checked ${theme} at ${width}px (${routes.length} routes)`);
    }
    await page.goto(base + '/');
    await page.locator('.theme-toggle').click();
    const selected = await page.locator('html').getAttribute('data-theme');
    await page.reload(); assert(await page.locator('html').getAttribute('data-theme') === selected, 'Theme persistence');
    await page.locator('.menu-toggle').click(); assert(await page.locator('#mobile-menu').isVisible(), 'Mobile menu');
    await page.locator('#mobile-menu a[href="/research/"]').click(); assert(page.url() === base + '/research/', 'Mobile navigation');
    assert((await page.locator('body').textContent()).includes('Working papers') && (await page.locator('body').textContent()).includes('Work in progress'), 'Research sections');
    await page.goto(base + '/research/household-demand-leisure/');
    await page.locator('.symbol-definitions summary').first().click(); assert(await page.locator('.symbol-definitions').first().evaluate(e => e.open), 'Equation disclosure');
    await page.goto(base + '/beyond-economics/calligraphie-de-bonheur/');
    await page.locator('.immersive-open').click(); assert(await page.locator('.immersive-reader').evaluate(e => e.open), 'Immersive reader');
    await page.keyboard.press('Escape'); assert(!await page.locator('.immersive-reader').evaluate(e => e.open), 'Immersive reader exit');
    await page.locator('.audio-play-toggle').click(); await page.waitForFunction(() => !document.querySelector('audio').paused);
    await page.locator('.audio-play-toggle').click(); assert(await page.locator('audio').evaluate(e => e.paused), 'Audio pause');
    for (const route of routes.filter(r => r.includes('on-reading-preferences') || r.includes('if-i-had') || r.includes('echoes-of-childhood'))) {
      await page.goto(base + route); await page.locator('audio').evaluate(e => e.play()); await page.waitForFunction(() => !document.querySelector('audio').paused);
      assert(await page.locator('audio').evaluate(e => !e.error && e.readyState >= 2), 'Audio decode ' + route);
      await page.locator('audio').evaluate(e => e.pause());
    }
    await page.goto(base + '/insights/reading-model-based-estimates/'); await page.waitForURL('**/insights/quest-ce-quun-modele/');
    const missing = await page.goto(base + '/unknown-route-publication-check/');
    assert(missing.status() === 404 && (await page.locator('h1').textContent()).includes('not here'), 'Custom 404');
    for (const resource of ['/files/cv/anderson-vil-cv-provisional.pdf', '/files/papers/household-demand-leisure.pdf', '/files/papers/children-costs-one-adult-household.pdf', '/files/papers/children-labor-supply-equations.pdf',
      '/files/teaching/cy-cergy/l3-econometrics/exercises-01.pdf', '/files/teaching/cy-cergy/l3-data-analysis/td-01.zip', '/files/code/homogeneity/homogeneity-code.zip']) {
      const local = path.resolve('public-production', resource.slice(1));
      assert(fs.existsSync(local), 'QA resource path ' + resource);
      const r = await context.request.get(base + resource); assert(r.status() === 200, resource);
      const sha = b => crypto.createHash('sha256').update(b).digest('hex');
      assert(sha(await r.body()) === sha(fs.readFileSync(local)), 'Download differs: ' + resource);
      report.downloads.push({ resource, status: r.status(), sha256: sha(await r.body()) });
    }
    if (base.startsWith('https:')) {
      const expected = process.argv[3]; const r = await context.request.get(base + '/deployment-commit.txt');
      report.sourceCommit = (await r.text()).trim(); assert(report.sourceCommit === expected, 'Served source commit mismatch');
      for (const resource of ['/robots.txt', '/sitemap.xml']) { const r = await context.request.get(base + resource); assert(r.ok(), resource); }
      for (const resource of ['/guide-site.md', '/docs/', '/config.toml', '/preview/']) { const r = await context.request.get(base + resource); assert(r.status() === 404, 'Internal file served: ' + resource); }
    }
    report.interactions = { mobileMenu: true, themePersistence: true, equations: true, immersiveReader: true, audioPlayback: 4, legacyRedirect: true, custom404: true };
    assert(!report.errors.length && !report.failedRequests.length, 'Browser errors: ' + JSON.stringify([report.errors, report.failedRequests]));
    report.result = 'PASS';
    console.log(JSON.stringify({ result: report.result, pageChecks: report.pages.length, downloads: report.downloads.length, interactions: report.interactions }));
  } finally { fs.writeFileSync(path.join(dir, 'report.json'), JSON.stringify(report, null, 2)); await browser.close(); }
})().catch(e => { console.error(e); process.exitCode = 1; });
