const fs = require('fs');
const path = require('path');
const { chromium } = require(path.join(process.env.CODEX_NODE_MODULES, 'playwright'));
(async () => {
  const dir = path.resolve('docs/screenshots/final-v5'); fs.mkdirSync(dir, { recursive: true });
  const browser = await chromium.launch({ executablePath: 'C:/Program Files/Google/Chrome/Application/chrome.exe', headless: true });
  const page = await browser.newPage({ viewport: { width: 1908, height: 908 }, deviceScaleFactor: 1 });
  await page.goto('http://127.0.0.1:1114/');
  await page.evaluate(() => { document.documentElement.dataset.theme='light'; });
  await page.screenshot({ path: path.join(dir, 'before-home-1908-light.png') });
  const metrics = await page.evaluate(() => ({ width:innerWidth, height:innerHeight, dpr:devicePixelRatio, scale:visualViewport.scale, hero:document.querySelector('.hero').getBoundingClientRect().height, jmpTop:document.querySelector('.jmp-band').getBoundingClientRect().top, actionsBottom:document.querySelector('.jmp-band .button-row').getBoundingClientRect().bottom }));
  fs.writeFileSync(path.join(dir, 'before-metrics.json'), JSON.stringify(metrics,null,2));
  console.log(metrics); await browser.close();
})();

