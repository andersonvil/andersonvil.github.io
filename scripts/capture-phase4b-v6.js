const fs = require('fs');
const path = require('path');
const { chromium } = process.env.PHASE4B_NODE_MODULES
  ? require(path.join(process.env.PHASE4B_NODE_MODULES, 'playwright'))
  : require('playwright');

const root = path.resolve(__dirname, '..');
const output = path.join(root, 'docs', 'screenshots', 'phase4b-v6');
const base = process.env.PHASE4B_PREVIEW_URL || 'http://127.0.0.1:1112';
const results = [];

async function capture(page, name, route, selector, viewport, theme = 'light') {
  await page.setViewportSize(viewport);
  await page.goto(`${base}${route}`, { waitUntil: 'domcontentloaded' });
  await page.evaluate((value) => {
    localStorage.setItem('av-theme', value);
    document.documentElement.dataset.theme = value;
    document.documentElement.style.colorScheme = value;
  }, theme);
  const target = page.locator(selector);
  await target.scrollIntoViewIfNeeded();
  await page.addStyleTag({ content: '.skip-link,.site-header{visibility:hidden!important}' });
  const metrics = await page.evaluate(() => ({
    width: window.innerWidth,
    scrollWidth: document.documentElement.scrollWidth,
    noindex: document.querySelector('meta[name="robots"]')?.content || '',
  }));
  if (metrics.scrollWidth > metrics.width + 1) throw new Error(`${name}: horizontal overflow ${metrics.scrollWidth} > ${metrics.width}`);
  await target.screenshot({ path: path.join(output, name) });
  results.push({ name, route, selector, theme, ...metrics });
}

(async () => {
  fs.mkdirSync(output, { recursive: true });
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PHASE4B_CHROME || 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe',
  });
  const page = await browser.newPage();
  const desktop = { width: 1440, height: 900 };
  const mobile = { width: 375, height: 812 };
  await capture(page, 'teaching-practice-desktop.png', '/teaching/', '.teaching-practice', desktop);
  await capture(page, 'teaching-practice-mobile.png', '/teaching/', '.teaching-practice', mobile);
  await capture(page, 'home-beyond-desktop.png', '/', '.home-beyond', desktop);
  await capture(page, 'home-beyond-mobile.png', '/', '.home-beyond', mobile);
  await capture(page, 'research-question-jmp-desktop.png', '/research/household-demand-leisure/', '#question', desktop);
  await capture(page, 'research-question-one-adult-desktop.png', '/research/children-costs-one-adult-household/', '#question', desktop);
  await capture(page, 'research-question-labor-desktop.png', '/research/children-labor-supply-equations/', '#question', desktop);
  await capture(page, 'research-question-jmp-mobile-dark.png', '/research/household-demand-leisure/', '#question', mobile, 'dark');
  await capture(page, 'context-return-contact-desktop.png', '/research/?from=contact', '[data-contextual-return-host]', desktop);
  await capture(page, 'audio-player-desktop.png', '/beyond-economics/calligraphie-de-bonheur/', '.poem-audio-player', desktop);
  await capture(page, 'audio-player-mobile.png', '/beyond-economics/ma-cherie/', '.poem-audio-player', mobile);
  await capture(page, 'research-visuals-lab-desktop.png', '/preview/design-lab/research-visuals/', '.design-lab', desktop);
  await capture(page, 'poetry-audio-lab-desktop.png', '/preview/design-lab/poetry-audio/', '.audio-lab', desktop);
  fs.writeFileSync(path.join(output, 'capture-metrics.json'), JSON.stringify(results, null, 2));
  await browser.close();
  console.log(`Captured ${results.length} verified screenshots in ${output}`);
})().catch((error) => {
  console.error(error);
  process.exit(1);
});
