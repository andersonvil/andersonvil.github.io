const { chromium } = require('playwright');
const fs = require('fs');
const path = require('path');

const root = path.resolve(__dirname, '..');
const output = path.join(root, 'docs', 'screenshots', 'phase4b-v3');
const base = process.env.PHASE4B_PREVIEW_URL || 'http://127.0.0.1:1111';

async function capture(page, name, url, options = {}) {
  await page.setViewportSize(options.viewport || { width: 1440, height: 900 });
  await page.goto(`${base}${url}`, { waitUntil: 'networkidle' });
  if (options.theme) {
    await page.evaluate((theme) => {
      document.documentElement.dataset.theme = theme;
      document.documentElement.style.colorScheme = theme;
      localStorage.setItem('av-theme', theme);
    }, options.theme);
  }
  if (options.action) await options.action(page);
  await page.screenshot({ path: path.join(output, name), fullPage: options.fullPage ?? false });
}

(async () => {
  fs.mkdirSync(output, { recursive: true });
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PHASE4B_CHROME || 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe',
  });
  const page = await browser.newPage();
  await capture(page, 'home-desktop-light.png', '/', { theme: 'light' });
  await capture(page, 'home-desktop-dark.png', '/', { theme: 'dark' });
  await capture(page, 'home-mobile-375.png', '/', { theme: 'light', viewport: { width: 375, height: 812 }, fullPage: true });
  await capture(page, 'research-desktop.png', '/research/', { theme: 'light' });
  await capture(page, 'teaching-desktop.png', '/teaching/', { theme: 'light' });
  await capture(page, 'beyond-economics-desktop.png', '/beyond-economics/', { theme: 'light' });
  await capture(page, 'calligraphie-de-bonheur-desktop.png', '/beyond-economics/calligraphie-de-bonheur/', { theme: 'light' });
  await capture(page, 'ma-cherie-mobile.png', '/beyond-economics/ma-cherie/', { theme: 'light', viewport: { width: 375, height: 812 }, fullPage: true });
  await capture(page, 'mes-nuits-mobile.png', '/beyond-economics/mes-nuits/', { theme: 'light', viewport: { width: 375, height: 812 }, fullPage: true });
  await capture(page, 'on-me-l-a-interdit-mobile.png', '/beyond-economics/on-me-l-a-interdit/', { theme: 'light', viewport: { width: 375, height: 812 }, fullPage: true });
  await capture(page, 'immersive-reading-desktop.png', '/beyond-economics/calligraphie-de-bonheur/', {
    theme: 'dark',
    action: async (currentPage) => currentPage.getByRole('button', { name: 'Enter immersive reading' }).click(),
  });
  await capture(page, 'podcast-desktop.png', '/beyond-economics/on-reading-preferences/', { theme: 'light' });
  await browser.close();
  console.log(`Captured 12 screenshots in ${output}`);
})().catch((error) => {
  console.error(error);
  process.exit(1);
});
