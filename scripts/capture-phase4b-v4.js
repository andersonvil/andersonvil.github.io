const fs = require('fs');
const path = require('path');
const { chromium } = process.env.PHASE4B_NODE_MODULES
  ? require(path.join(process.env.PHASE4B_NODE_MODULES, 'playwright'))
  : require('playwright');

const root = path.resolve(__dirname, '..');
const output = path.join(root, 'docs', 'screenshots', 'phase4b-v4');
const base = process.env.PHASE4B_PREVIEW_URL || 'http://127.0.0.1:1111';

async function capture(page, name, url, options = {}) {
  await page.setViewportSize(options.viewport || { width: 1440, height: 900 });
  await page.goto(`${base}${url}`, { waitUntil: 'networkidle' });
  await page.evaluate((theme) => {
    document.documentElement.dataset.theme = theme;
    document.documentElement.style.colorScheme = theme;
    localStorage.setItem('av-theme', theme);
  }, options.theme || 'light');
  if (options.target) await page.locator(options.target).scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(output, name), fullPage: options.fullPage ?? false });
}

(async () => {
  fs.mkdirSync(output, { recursive: true });
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PHASE4B_CHROME || 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe',
  });
  const page = await browser.newPage();
  await capture(page, 'jmp-header-desktop-light.png', '/research/household-demand-leisure/');
  await capture(page, 'jmp-identification-desktop-dark.png', '/research/household-demand-leisure/', { theme: 'dark', target: '#identification' });
  await capture(page, 'jmp-mobile-dark.png', '/research/household-demand-leisure/', { theme: 'dark', viewport: { width: 375, height: 812 } });
  await capture(page, 'research-visuals-lab-desktop.png', '/preview/design-lab/research-visuals/', { fullPage: true });
  await capture(page, 'poetry-typography-lab-desktop.png', '/preview/design-lab/poetry-typography/', { fullPage: true });
  await capture(page, 'calligraphie-mobile.png', '/beyond-economics/calligraphie-de-bonheur/', { theme: 'dark', viewport: { width: 375, height: 812 }, target: '.poem-canvas' });
  await capture(page, 'niort-desktop.png', '/teaching/microeconomics-iii-niort/', { theme: 'dark' });
  await browser.close();
  console.log(`Captured 7 screenshots in ${output}`);
})().catch((error) => {
  console.error(error);
  process.exit(1);
});
