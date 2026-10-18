const fs = require('fs'), path = require('path'), crypto = require('crypto');
const { chromium } = require(path.join(process.env.CODEX_NODE_MODULES, 'playwright'));
const base = process.argv[2] || 'http://127.0.0.1:1122';
const capturesOnly = process.argv.includes('--captures-only');
const root = path.resolve(__dirname, '..');
const build = path.join(root, 'public-preview-update');
const screenshots = path.join(root, 'docs/screenshots/teaching-update');
const reports = path.join(root, 'tmp/teaching-update-qa');
fs.mkdirSync(screenshots, { recursive: true }); fs.mkdirSync(reports, { recursive: true });
const report = { pages: [], downloads: [], interactions: [], errors: [] };
const assert = (v, m) => { if (!v) throw Error(m); };
const sha = b => crypto.createHash('sha256').update(b).digest('hex');
const courses = ['econometrics-l3', 'data-analysis-l3', 'microeconomics-l1', 'econometrics-niort', 'microeconomics-iii-niort', 'econometrics-m1', 'econometrics-m2'];
const routes = capturesOnly ? courses.filter(id => !['econometrics-m1', 'econometrics-m2'].includes(id)).map(id => `/teaching/${id}/`) : ['/', '/research/', '/code-data/', ...courses.map(id => `/teaching/${id}/`)];
(async () => {
  const browser = await chromium.launch({ headless: true, executablePath: 'C:/Program Files/Google/Chrome/Application/chrome.exe' });
  try {
    const context = await browser.newContext({ viewport: { width: 1440, height: 1000 }, acceptDownloads: true });
    const page = await context.newPage();
    page.on('pageerror', e => report.errors.push(e.message));
    for (const theme of ['light', 'dark']) for (const width of (capturesOnly ? [1440, 390] : [1440, 768, 390, 360])) {
      await page.setViewportSize({ width, height: width < 500 ? 844 : 1000 });
      await page.goto(base + '/');
      await page.evaluate(t => localStorage.setItem('av-theme', t), theme);
      for (const route of routes) {
        const response = await page.goto(base + route);
        assert(response.status() === 200, route);
        await page.evaluate(() => document.fonts.ready);
        const result = await page.evaluate(() => ({ theme: document.documentElement.dataset.theme, width: innerWidth, scrollWidth: document.documentElement.scrollWidth,
          brokenImages: [...document.images].filter(i => !i.naturalWidth).length, nestedDisclosures: document.querySelectorAll('main details details').length }));
        assert(result.theme === theme && result.scrollWidth <= width + 1 && !result.brokenImages && !result.nestedDisclosures, JSON.stringify({ route, ...result }));
        if (route.startsWith('/teaching/')) {
          for (const summary of await page.locator('.learning-extras > summary').all()) {
            await summary.focus(); await page.keyboard.press('Enter');
            assert(await summary.evaluate(e => e.parentElement.open), 'Keyboard disclosure ' + route);
          }
          assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), 'Expanded mobile overflow ' + route);
          const focus = page.locator('.learning-primary a, .resource-file-list a').first();
          if (await focus.count()) {
            await focus.focus();
            assert(await focus.evaluate(e => getComputedStyle(e).outlineStyle !== 'none'), 'Visible focus ' + route);
            await focus.hover();
          }
          report.interactions.push({ route, theme, width, keyboard: true });
        } else if (route === '/code-data/') {
          await page.locator('.technical-part').evaluateAll(es => es.forEach(e => e.open = true));
          assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), 'Catalogue overflow');
        }
        if (route.startsWith('/teaching/') && !route.includes('econometrics-m') && [1440, 390].includes(width)) {
          // Show a representative expanded lesson, retaining the compact neighbouring cards.
          await page.locator('.learning-extras').evaluateAll(es => es.forEach((e, i) => e.open = i === (es.length > 2 ? 2 : 0)));
          await page.evaluate(() => document.activeElement?.blur());
          await page.mouse.move(0, 0);
          await page.locator('.learning-resources').screenshot({ style: '.site-header, .detail-navigation, .skip-link { visibility:hidden !important; }', path: path.join(screenshots, `${route.split('/')[2]}-${width}-${theme}.png`) });
        }
        if (route === '/research/' && width === 1440) await page.locator('#papers').screenshot({ path: path.join(screenshots, `research-${theme}.png`) });
        report.pages.push({ route, ...result });
      }
      console.log(`PASS ${theme} ${width}px`);
    }
    if (capturesOnly) { report.result = 'PASS'; return; }
    // Fetch the actual targets from both rendered views, including every individual file.
    const urls = new Set();
    await page.goto(base + '/code-data/');
    for (const url of await page.locator('main a[href^="/files/teaching/"]').evaluateAll(es => es.map(e => e.getAttribute('href')))) urls.add(url);
    await page.goto(base + '/teaching/econometrics-l3/');
    for (const url of await page.locator('.dataset-grid a').evaluateAll(es => es.map(e => e.getAttribute('href')))) urls.add(url);
    const cv = JSON.parse(fs.readFileSync(path.join(root, 'docs/teaching-update-manifest.json'))).cv;
    urls.add(cv.url);
    for (const url of urls) {
      const response = await context.request.get(base + url);
      assert(response.ok(), 'Download ' + url);
      const body = await response.body();
      assert(sha(body) === sha(fs.readFileSync(path.join(build, url))), 'Download bytes ' + url);
      report.downloads.push({ url, status: response.status(), sha256: sha(body) });
    }
    // Follow a dataset download through its real user-facing action.
    const event = page.waitForEvent('download');
    await page.locator('.dataset-grid a').first().click();
    const download = await event;
    const downloaded = await download.path();
    const target = await page.locator('.dataset-grid a').first().getAttribute('href');
    assert(sha(fs.readFileSync(downloaded)) === sha(fs.readFileSync(path.join(build, target))), 'Clicked ZIP bytes');
    await page.goto(base + '/');
    const cvHref = await page.locator('.hero-actions a').filter({ hasText: 'CV' }).getAttribute('href');
    const cvResponse = await context.request.get(base + cvHref);
    assert(sha(await cvResponse.body()) === cv.sha256, 'Preview CV from Home');
    const a11yCV = await page.locator('a').filter({ hasText: /^CV(?:\s*PDF)?$/ }).evaluateAll(es => es.map(e => e.getAttribute('href')));
    assert(a11yCV.length >= 4 && a11yCV.every(url => url === cv.url), 'Menu/Home/mobile CV agreement');
    assert(!report.errors.length, JSON.stringify(report.errors));
    report.result = 'PASS';
    console.log(JSON.stringify({ result: report.result, pageChecks: report.pages.length, downloads: report.downloads.length, keyboardChecks: report.interactions.length }));
  } finally {
    fs.writeFileSync(path.join(reports, capturesOnly ? 'capture-report.json' : 'browser-report.json'), JSON.stringify(report, null, 2));
    await browser.close();
  }
})().catch(e => { console.error(e); process.exitCode = 1; });
