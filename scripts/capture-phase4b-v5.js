const fs = require('fs');
const path = require('path');
const { chromium } = process.env.PHASE4B_NODE_MODULES
  ? require(path.join(process.env.PHASE4B_NODE_MODULES, 'playwright'))
  : require('playwright');

const root = path.resolve(__dirname, '..');
const output = path.join(root, 'docs', 'screenshots', 'phase4b-v5');
const base = process.env.PHASE4B_PREVIEW_URL || 'http://127.0.0.1:1112';
const checks = [];

async function capture(page, name, url, options = {}) {
  const viewport = options.viewport || { width: 1440, height: 900 };
  await page.setViewportSize(viewport);
  await page.goto(`${base}${url}`, { waitUntil: 'networkidle' });
  await page.evaluate((theme) => {
    document.documentElement.dataset.theme = theme;
    document.documentElement.style.colorScheme = theme;
    localStorage.setItem('av-theme', theme);
  }, options.theme || 'light');
  if (options.target) await page.locator(options.target).scrollIntoViewIfNeeded();
  const metrics = await page.evaluate(() => ({
    viewport: window.innerWidth,
    scrollWidth: document.documentElement.scrollWidth,
    titleLines: document.querySelector('.hero h1')
      ? Math.round(document.querySelector('.hero h1').getBoundingClientRect().height / parseFloat(getComputedStyle(document.querySelector('.hero h1')).lineHeight))
      : null,
  }));
  checks.push({ name, ...metrics });
  if (metrics.scrollWidth > metrics.viewport + 1) {
    const offenders = await page.evaluate(() => [...document.querySelectorAll('body *')]
      .map((element) => { const box = element.getBoundingClientRect(); return { tag: element.tagName, className: String(element.className), left: box.left, right: box.right, width: box.width }; })
      .filter((item) => item.left < -1 || item.right > window.innerWidth + 1)
      .slice(0, 8));
    throw new Error(`${name}: horizontal overflow ${metrics.scrollWidth} > ${metrics.viewport}; ${JSON.stringify(offenders)}`);
  }
  await page.screenshot({ path: path.join(output, name), fullPage: options.fullPage ?? false });
}

(async () => {
  fs.mkdirSync(output, { recursive: true });
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PHASE4B_CHROME || 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe',
  });
  const page = await browser.newPage();
  await capture(page, 'after-home-1440-light.png', '/', { viewport: { width: 1440, height: 900 } });
  await capture(page, 'after-research-1440-light.png', '/research/', { viewport: { width: 1440, height: 900 } });
  await capture(page, 'after-teaching-1440-light.png', '/teaching/', { viewport: { width: 1440, height: 900 } });
  await capture(page, 'after-beyond-1440-light.png', '/beyond-economics/', { viewport: { width: 1440, height: 900 } });
  await capture(page, 'after-jmp-header-1440-light.png', '/research/household-demand-leisure/', { viewport: { width: 1440, height: 900 } });
  await capture(page, 'after-jmp-identification-1440-light.png', '/research/household-demand-leisure/', { viewport: { width: 1440, height: 900 }, target: '#identification' });
  await capture(page, 'after-poem-1440-light.png', '/beyond-economics/calligraphie-de-bonheur/', { viewport: { width: 1440, height: 900 }, target: '.poem-canvas' });
  await capture(page, 'after-audio-lab-1440-light.png', '/preview/design-lab/poetry-audio/', { viewport: { width: 1440, height: 900 } });
  await capture(page, 'after-research-lab-1440-light.png', '/preview/design-lab/research-visuals/', { viewport: { width: 1440, height: 900 } });
  for (const width of [375, 768, 1024, 1920]) {
    await capture(page, `responsive-home-${width}.png`, '/', { viewport: { width, height: 900 } });
    await capture(page, `responsive-jmp-${width}.png`, '/research/household-demand-leisure/', { viewport: { width, height: 900 } });
    await capture(page, `responsive-poem-${width}.png`, '/beyond-economics/ma-cherie/', { viewport: { width, height: 900 }, target: '.poem-canvas' });
  }
  const homeChecks = checks.filter((item) => item.name.startsWith('after-home') || item.name.startsWith('responsive-home'));
  for (const item of homeChecks) if (item.titleLines !== 1) throw new Error(`${item.name}: home h1 occupies ${item.titleLines} lines`);
  fs.writeFileSync(path.join(output, 'responsive-metrics.json'), JSON.stringify(checks, null, 2));
  await browser.close();
  console.log(`Captured ${checks.length} screenshots in ${output}`);
})().catch((error) => {
  console.error(error);
  process.exit(1);
});
