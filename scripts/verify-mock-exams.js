// Verify the published resource paths, bytes, and complete interactive quizzes.
const fs = require('fs'), path = require('path'), crypto = require('crypto');
const { chromium } = require(path.join(process.env.CODEX_NODE_MODULES, 'playwright'));
const base = process.argv[2] || 'http://127.0.0.1:1125';
const root = path.resolve(__dirname, '..');
const sha = b => crypto.createHash('sha256').update(b).digest('hex');
const manifest = JSON.parse(fs.readFileSync(path.join(root, 'docs/teaching-update-manifest.json')));
const files = manifest.files.filter(f => /Mock-Exams/.test(f.source));
const archives = manifest.archives.filter(a => /qcm\.zip|mock-exams\.zip/.test(a.url));
const routes = ['/teaching/', '/teaching/econometrics-niort/', '/teaching/microeconomics-iii-niort/', '/teaching/econometrics-m1/', '/teaching/econometrics-m2/', '/code-data/'];
const dir = path.join(root, 'tmp/mock-exams-qa', base.startsWith('https:') ? 'public' : 'local');
fs.mkdirSync(dir, { recursive: true });
const report = { base, pages: [], downloads: [], quizzes: [], errors: [] };
const assert = (v, m) => { if (!v) throw Error(m); };
(async () => {
  const browser = await chromium.launch({ headless: true, executablePath: 'C:/Program Files/Google/Chrome/Application/chrome.exe' });
  try {
    const context = await browser.newContext();
    const page = await context.newPage();
    page.on('pageerror', e => report.errors.push(e.message));
    if (process.argv[3]) {
      const r = await context.request.get(base + '/deployment-commit.txt?t=' + Date.now());
      report.sourceCommit = (await r.text()).trim();
      assert(report.sourceCommit === process.argv[3], 'Served source commit differs');
    }
    for (const route of routes) {
      await page.goto(base + route);
      if (route === '/teaching/') {
        assert((await page.locator('.lede').innerText()).includes('Universit\u00e9'), 'Teaching text encoding');
        assert(!(await page.locator('.teaching-role').allTextContents()).some(t => t.includes('\u00c2')), 'Teaching separator encoding');
      }
      const hrefs = await page.locator('main a[href]').evaluateAll(es => es.map(e => e.getAttribute('href')));
      const expected = [...files, ...archives].filter(f => route === '/code-data/' ||
        route === '/teaching/' && !f.url.endsWith('.zip') ||
        route.includes('econometrics-niort') && f.url.includes('/niort/econometrics/') ||
        route.includes('microeconomics-iii-niort') && f.url.includes('/microeconomics-iii/') ||
        route.includes('econometrics-m2') && (f.url.includes('/m2-econometrics/') || f.url.endsWith('/Mock2_solutions_v1.pdf')) ||
        route.includes('econometrics-m1') && f.url.includes('/m1-econometrics/'));
      for (const f of expected) {
        const count = ['/teaching/', '/code-data/'].includes(route) && f.url.endsWith('/Mock2_solutions_v1.pdf') ? 2 : 1;
        assert(hrefs.filter(h => h === f.url).length === count, route + ' missing/duplicate ' + f.url);
      }
      if (/econometrics-m[12]\/$/.test(route)) assert(await page.locator('.learning-card').count() >= 2, 'Missing mock-exam cards');
      report.pages.push({ route, verifiedResourceLinks: expected.length });
    }
    for (const f of [...files, ...archives]) {
      const response = await context.request.get(base + f.url);
      assert(response.ok(), 'Download ' + f.url);
      const body = await response.body();
      assert(sha(body) === sha(fs.readFileSync(path.join(root, 'static', f.url))), 'Download bytes ' + f.url);
      report.downloads.push({ url: f.url, status: response.status(), sha256: sha(body) });
    }
    for (const width of [1440, 390]) for (const f of files.filter(f => f.url.endsWith('.html'))) {
      await page.setViewportSize({ width, height: 900 });
      await page.goto(base + f.url);
      await page.locator('#go').click();
      let questions = 0;
      const overflow = [];
      while (await page.locator('.opt').count()) {
        assert(++questions <= 150, 'Quiz did not finish');
        const measure = async () => page.evaluate(() => ({ width: innerWidth, scroll: document.documentElement.scrollWidth, question: document.querySelector('.qtext')?.textContent?.slice(0, 100) }));
        const before = await measure();
        if (before.scroll > width + 1) overflow.push(before);
        await page.locator('.opt').first().click();
        assert((await page.locator('#fb').innerText()).trim().length > 0, 'Missing answer feedback');
        const after = await measure();
        if (after.scroll > width + 1) overflow.push(after);
        await page.locator('#next').click();
      }
      assert(await page.locator('#again').count(), 'Missing quiz results/restart');
      await page.screenshot({ path: path.join(dir, `${path.basename(f.url)}-${width}.png`), fullPage: true });
      report.quizzes.push({ url: f.url, width, questions, overflow });
      await page.locator('#again').click();
      assert(await page.locator('.opt').count(), 'Quiz restart failed');
    }
    assert(!report.errors.length, 'JavaScript errors: ' + report.errors.join('; '));
    assert(report.quizzes.every(q => !q.overflow.length), 'Quiz mobile overflow; see report');
    report.result = 'PASS';
  } finally {
    fs.writeFileSync(path.join(dir, 'report.json'), JSON.stringify(report, null, 2));
    await browser.close();
  }
  console.log(JSON.stringify({ result: report.result, pages: report.pages.length, downloads: report.downloads.length,
    quizzes: report.quizzes.map(q => ({ url: q.url, width: q.width, questions: q.questions })) }));
})().catch(e => { console.error(e); process.exitCode = 1; });
