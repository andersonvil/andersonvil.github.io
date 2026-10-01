const fs = require('fs');
const path = require('path');
const { chromium } = require(path.join(process.env.CODEX_NODE_MODULES, 'playwright'));

const root = path.resolve(__dirname, '..');
const output = path.join(root, 'docs', 'screenshots', 'home-v4');
const base = process.env.PREVIEW_URL || 'http://127.0.0.1:1112';
const chrome = process.env.CHROME_PATH || 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe';
const widths = [1440, 1280, 768, 390, 360];
const routes = ['/', '/research/', '/teaching/', '/code-data/', '/insights/', '/beyond-economics/', '/contact/', '/talks/', '/research/household-demand-leisure/', '/research/children-costs-one-adult-household/', '/research/children-labor-supply-equations/', '/preview/', '/404.html'];

function check(condition, message) { if (!condition) throw new Error(message); }

(async () => {
  fs.mkdirSync(output, { recursive: true });
  const browser = await chromium.launch({ headless: true, executablePath: chrome });
  const page = await browser.newPage({ viewport: { width: 1440, height: 1000 } });
  const consoleErrors = [];
  page.on('console', msg => { if (msg.type() === 'error') consoleErrors.push(msg.text()); });
  const results = { generatedAt: new Date().toISOString(), base, routes: [], typography: {}, homePathways: {}, interactions: {}, theme: {}, consoleErrors };

  for (const width of widths) {
    await page.setViewportSize({ width, height: width <= 390 ? 844 : 1000 });
    for (const route of routes) {
      const response = await page.goto(`${base}${route}`, { waitUntil: 'domcontentloaded' });
      const metric = await page.evaluate(() => ({
        viewport: innerWidth,
        scrollWidth: document.documentElement.scrollWidth,
        h1: getComputedStyle(document.querySelector('h1')).fontSize,
        body: getComputedStyle(document.body).fontSize,
        title: document.title,
      }));
      check(response && response.status() < 400, `${route} returned ${response && response.status()}`);
      check(metric.scrollWidth <= metric.viewport + 1, `${route} overflows at ${width}px: ${metric.scrollWidth}`);
      check(parseFloat(metric.body) >= 16, `${route} body text below 16px at ${width}px`);
      results.routes.push({ width, route, status: response.status(), ...metric });
    }
  }

  await page.setViewportSize({ width: 1440, height: 1000 });
  await page.goto(`${base}/`, { waitUntil: 'domcontentloaded' });
  await page.evaluate(() => { localStorage.setItem('av-theme', 'light'); document.documentElement.dataset.theme = 'light'; });
  results.typography.desktop = await page.evaluate(() => ({
    homeName: getComputedStyle(document.querySelector('.hero h1')).fontSize,
    pageTitle: null,
    sectionSubtitle: getComputedStyle(document.querySelector('.home-pathways h2')).fontSize,
    eyebrow: getComputedStyle(document.querySelector('.eyebrow')).fontSize,
    body: getComputedStyle(document.body).fontSize,
  }));
  results.homePathways.desktop = await page.evaluate(() => {
    const items = [...document.querySelectorAll('.home-pathways article')];
    return { labels: items.map(x => x.querySelector('.eyebrow').textContent.trim()), columns: items.map(x => ({ left: Math.round(x.getBoundingClientRect().left), width: Math.round(x.getBoundingClientRect().width) })), childElementCounts: items.map(x => x.children.length) };
  });
  await page.screenshot({ path: path.join(output, 'home-desktop-1440-light.png'), fullPage: true });

  await page.goto(`${base}/research/`, { waitUntil: 'domcontentloaded' });
  results.typography.desktop.pageTitle = await page.locator('.page-hero h1').evaluate(el => getComputedStyle(el).fontSize);
  await page.screenshot({ path: path.join(output, 'research-desktop-1440-light.png'), fullPage: true });

  await page.goto(`${base}/research/household-demand-leisure/`, { waitUntil: 'domcontentloaded' });
  results.typography.desktop.paperTitle = await page.locator('.paper-page h1').evaluate(el => getComputedStyle(el).fontSize);
  results.typography.desktop.paperSubtitle = await page.locator('.paper-body h2').first().evaluate(el => getComputedStyle(el).fontSize);
  await page.evaluate(() => { localStorage.setItem('av-theme', 'dark'); document.documentElement.dataset.theme = 'dark'; document.documentElement.style.colorScheme = 'dark'; });
  await page.locator('#identification').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(output, 'jmp-identification-1440-dark.png'), fullPage: false });
  results.interactions.symbolDisclosure = await page.locator('.symbol-definitions').count() === 1;
  await page.locator('.symbol-definitions summary').click();
  results.interactions.symbolDisclosureOpened = await page.locator('.symbol-definitions').evaluate(el => el.open);
  results.interactions.tableLinks = await page.locator('.table-source-link').evaluateAll(els => els.map(el => el.getAttribute('href')));

  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(`${base}/`, { waitUntil: 'domcontentloaded' });
  await page.evaluate(() => { localStorage.setItem('av-theme', 'light'); document.documentElement.dataset.theme = 'light'; });
  results.typography.mobile = await page.evaluate(() => ({
    homeName: getComputedStyle(document.querySelector('.hero h1')).fontSize,
    sectionSubtitle: getComputedStyle(document.querySelector('.home-pathways h2')).fontSize,
    eyebrow: getComputedStyle(document.querySelector('.eyebrow')).fontSize,
    body: getComputedStyle(document.body).fontSize,
  }));
  results.homePathways.mobile = await page.evaluate(() => ({
    labels: [...document.querySelectorAll('.home-pathways article .eyebrow')].map(x => x.textContent.trim()),
    columns: [...document.querySelectorAll('.home-pathways article')].map(x => Math.round(x.getBoundingClientRect().left)),
  }));
  await page.screenshot({ path: path.join(output, 'home-mobile-390-light.png'), fullPage: true });

  await page.goto(`${base}/teaching/`, { waitUntil: 'domcontentloaded' });
  await page.screenshot({ path: path.join(output, 'teaching-mobile-390-light.png'), fullPage: true });
  results.interactions.teachingOrder = await page.locator('.course-order h3').allTextContents();

  await page.goto(`${base}/beyond-economics/`, { waitUntil: 'domcontentloaded' });
  results.interactions.podcastImage = await page.locator('.media-preview img').evaluate(el => ({ natural: `${el.naturalWidth}x${el.naturalHeight}`, rendered: `${Math.round(el.getBoundingClientRect().width)}x${Math.round(el.getBoundingClientRect().height)}`, objectFit: getComputedStyle(el).objectFit }));
  results.interactions.audioAutoplay = await page.locator('audio').evaluateAll(els => els.map(el => ({ autoplay: el.autoplay, paused: el.paused })));

  await page.goto(`${base}/`, { waitUntil: 'domcontentloaded' });
  results.theme.persisted = await page.locator('html').getAttribute('data-theme');
  await page.getByRole('button', { name: /Switch to (dark|light) theme/ }).click();
  results.theme.afterToggle = await page.locator('html').getAttribute('data-theme');
  results.theme.saved = await page.evaluate(() => localStorage.getItem('av-theme'));

  check(results.homePathways.desktop.childElementCounts.every(n => n === 3), 'Home pathway contains more than label, subtitle, and link');
  check(results.homePathways.mobile.labels.join('|') === 'Research|Teaching|Code & Data|Insights|Beyond Economics|Contact', 'Home mobile pathway order is incorrect');
  check(parseFloat(results.typography.desktop.homeName) >= 36 && parseFloat(results.typography.desktop.homeName) <= 40, 'Desktop Home name outside 36–40px');
  check(parseFloat(results.typography.mobile.homeName) >= 28 && parseFloat(results.typography.mobile.homeName) <= 32, 'Mobile Home name outside 28–32px');
  check(parseFloat(results.typography.desktop.pageTitle) >= 30 && parseFloat(results.typography.desktop.pageTitle) <= 36, 'Desktop page title outside 30–36px');
  check(parseFloat(results.typography.desktop.paperTitle) >= 30 && parseFloat(results.typography.desktop.paperTitle) <= 36, 'Desktop paper title outside 30–36px');
  check(results.interactions.symbolDisclosureOpened, 'Symbol disclosure did not open');
  check(results.interactions.tableLinks.every(link => /#page=\d+$/.test(link)), 'PDF table link lacks a physical page target');
  check(results.interactions.audioAutoplay.every(x => !x.autoplay && x.paused), 'Audio autoplay detected');
  check(consoleErrors.length === 0, `Console errors: ${consoleErrors.join(' | ')}`);

  fs.writeFileSync(path.join(output, 'verification.json'), JSON.stringify(results, null, 2));
  await browser.close();
  console.log(`HOME V4 QA: PASS (${routes.length} routes × ${widths.length} widths)`);
})().catch(error => { console.error(error.stack || error); process.exit(1); });
