const fs = require('fs');
const path = require('path');
const { chromium } = process.env.PHASE4B_NODE_MODULES
  ? require(path.join(process.env.PHASE4B_NODE_MODULES, 'playwright'))
  : require('playwright');

const base = process.env.PHASE4B_PREVIEW_URL || 'http://127.0.0.1:1112';
const output = path.resolve(__dirname, '..', 'docs', 'screenshots', 'phase4b-v6', 'ui-test-results.json');
const poemSlugs = ['calligraphie-de-bonheur', 'ma-cherie', 'mes-nuits', 'on-me-l-a-interdit'];
const paperSlugs = ['household-demand-leisure', 'children-costs-one-adult-household', 'children-labor-supply-equations'];

function assert(condition, message) {
  if (!condition) throw new Error(message);
}

(async () => {
  const browser = await chromium.launch({
    headless: true,
    executablePath: process.env.PHASE4B_CHROME || 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe',
    args: ['--autoplay-policy=no-user-gesture-required'],
  });
  const page = await browser.newPage({ viewport: { width: 1440, height: 900 } });
  const consoleErrors = [];
  page.on('console', (message) => { if (message.type() === 'error') consoleErrors.push(message.text()); });
  const result = { poems: [], scrollspy: [], context: {}, labSingleTrack: {}, keyboard: {}, accessibility: {}, theme: {}, responsive: [] };

  for (const slug of poemSlugs) {
    await page.goto(`${base}/beyond-economics/${slug}/`, { waitUntil: 'domcontentloaded' });
    const button = page.getByRole('button', { name: 'Play music', exact: true });
    await button.click();
    await page.waitForFunction(() => { const audio = document.querySelector('[data-poem-audio] audio'); return audio && !audio.paused && audio.readyState >= 2 && audio.currentTime > 0; });
    const playing = await page.locator('[data-poem-audio]').evaluate((player) => {
      const audio = player.querySelector('audio');
      return { track: player.dataset.trackTitle, currentTime: audio.currentTime, duration: audio.duration, readyState: audio.readyState, paused: audio.paused };
    });
    await page.getByRole('button', { name: 'Pause music', exact: true }).click();
    const pausedAt = await page.locator('[data-poem-audio] audio').evaluate((audio) => audio.currentTime);
    await button.click();
    await page.waitForFunction((time) => document.querySelector('[data-poem-audio] audio').currentTime > time, pausedAt);
    await page.waitForFunction(() => Number(document.querySelector('.audio-progress').value) > 0);
    await page.locator('.audio-volume').fill('0.25');
    const controls = await page.locator('[data-poem-audio]').evaluate((player) => {
      const audio = player.querySelector('audio');
      const progress = player.querySelector('.audio-progress');
      return { currentTime: audio.currentTime, duration: audio.duration, progressValue: progress.value, progressMax: progress.max, volume: audio.volume, paused: audio.paused, label: player.querySelector('.audio-play-toggle').textContent, status: player.querySelector('.audio-status').textContent };
    });
    assert(playing.track && !playing.paused && playing.duration > 0, `${slug}: playback did not start`);
    assert(Math.abs(controls.volume - 0.25) < 0.01, `${slug}: volume control failed`);
    assert(controls.currentTime > pausedAt && Number(controls.progressValue) > 0, `${slug}: elapsed progress failed`);
    await page.getByRole('button', { name: 'Pause music', exact: true }).click();
    result.poems.push({ slug, playing, pausedAt, resumedAndAdjusted: controls });
  }

  await page.goto(`${base}/preview/design-lab/poetry-audio/`, { waitUntil: 'domcontentloaded' });
  result.labSingleTrack = await page.evaluate(async () => {
    const tracks = [...document.querySelectorAll('audio')];
    await tracks[0].play();
    await new Promise((resolve) => setTimeout(resolve, 120));
    await tracks[1].play();
    await new Promise((resolve) => setTimeout(resolve, 120));
    const state = tracks.map((audio) => ({ paused: audio.paused, currentTime: audio.currentTime }));
    tracks.forEach((audio) => audio.pause());
    return { count: tracks.length, state };
  });
  assert(result.labSingleTrack.count === 4, 'Audio lab must expose four tracks');
  assert(result.labSingleTrack.state[0].paused && !result.labSingleTrack.state[1].paused, 'Starting a second track did not pause the first');

  await page.goto(`${base}/contact/`, { waitUntil: 'domcontentloaded' });
  assert(await page.locator('[data-contextual-return-host]:not([hidden])').count() === 0, 'Direct Contact arrival shows contextual return');
  await page.getByRole('link', { name: 'Explore Research', exact: true }).click();
  await page.waitForURL('**/research/?from=contact');
  result.context.researchFromContact = await page.getByRole('link', { name: /Back to Contact/ }).isVisible();
  await page.getByRole('link', { name: 'View paper details', exact: true }).first().click();
  await page.waitForURL('**/research/household-demand-leisure/?from=contact');
  result.context.paperBackResearch = await page.getByRole('link', { name: 'Back to Research', exact: true }).first().isVisible();
  result.context.paperBackContactCount = await page.getByRole('link', { name: /Back to Contact/ }).count();
  await page.getByRole('link', { name: 'Back to Research', exact: true }).first().click();
  await page.waitForURL('**/research/?from=contact');
  result.context.returnedBackContact = await page.getByRole('link', { name: /Back to Contact/ }).isVisible();
  await page.getByRole('link', { name: 'Research', exact: true }).first().click();
  await page.waitForURL(`${base}/research/`);
  result.context.menuClearsOrigin = await page.getByRole('link', { name: /Back to Contact/ }).count() === 0;
  await page.goto(`${base}/research/?from=https%3A%2F%2Fevil.example`, { waitUntil: 'domcontentloaded' });
  result.context.unsafeOriginIgnored = await page.locator('[data-contextual-return-host]:not([hidden])').count() === 0;
  assert(Object.values(result.context).every((value) => value === true || value === 0), 'Contextual return flow failed');

  for (const slug of paperSlugs) {
    await page.goto(`${base}/research/${slug}/`, { waitUntil: 'domcontentloaded' });
    const initial = await page.locator('.paper-toc a[aria-current="location"]').innerText();
    await page.getByRole('link', { name: 'Methods and data', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('.paper-toc a[aria-current="location"]')?.textContent.trim() === 'Methods and data');
    const methods = await page.locator('.paper-toc a[aria-current="location"]').innerText();
    await page.getByRole('link', { name: 'Findings and limits', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('.paper-toc a[aria-current="location"]')?.textContent.trim() === 'Findings and limits');
    result.scrollspy.push({ slug, initial, methods, findings: await page.locator('.paper-toc a[aria-current="location"]').innerText() });
  }

  await page.goto(`${base}/research/household-demand-leisure/`, { waitUntil: 'domcontentloaded' });
  const keyboardTarget = page.getByRole('link', { name: 'Methods and data', exact: true });
  await keyboardTarget.focus();
  result.keyboard.focusedLabel = await page.evaluate(() => document.activeElement?.textContent.trim());
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('.paper-toc a[aria-current="location"]')?.textContent.trim() === 'Methods and data');
  result.keyboard.enterActivated = page.url().endsWith('#methods');
  result.keyboard.currentSection = await page.locator('.paper-toc a[aria-current="location"]').innerText();
  assert(result.keyboard.focusedLabel === 'Methods and data' && result.keyboard.enterActivated, 'Keyboard navigation failed');

  await page.goto(`${base}/beyond-economics/calligraphie-de-bonheur/`, { waitUntil: 'domcontentloaded' });
  const cdp = await page.context().newCDPSession(page);
  const axTree = await cdp.send('Accessibility.getFullAXTree');
  const axNodes = axTree.nodes.map((node) => ({ role: node.role?.value, name: node.name?.value })).filter((node) => node.role && node.name);
  result.accessibility = {
    playButton: axNodes.some((node) => node.role === 'button' && node.name === 'Play music'),
    progressSlider: axNodes.some((node) => node.role === 'slider' && node.name === 'Music playback position'),
    volumeSlider: axNodes.some((node) => node.role === 'slider' && node.name === 'Music volume'),
    playerRegion: axNodes.some((node) => ['complementary', 'region'].includes(node.role) && node.name === 'Music selected for this poem'),
  };
  assert(Object.values(result.accessibility).every(Boolean), 'Screen-reader semantics are incomplete');

  await page.goto(`${base}/`, { waitUntil: 'domcontentloaded' });
  result.theme.before = await page.locator('html').getAttribute('data-theme');
  await page.getByRole('button', { name: /Switch to (dark|light) theme/ }).click();
  result.theme.after = await page.locator('html').getAttribute('data-theme');
  result.theme.nextLabel = await page.locator('.theme-toggle').getAttribute('aria-label');
  assert(result.theme.before !== result.theme.after && result.theme.nextLabel.includes(result.theme.before), 'Theme toggle failed');

  await page.setViewportSize({ width: 375, height: 812 });
  for (const route of ['/teaching/', '/', '/research/household-demand-leisure/', '/beyond-economics/ma-cherie/']) {
    await page.goto(`${base}${route}`, { waitUntil: 'domcontentloaded' });
    const metrics = await page.evaluate(() => ({ width: innerWidth, scrollWidth: document.documentElement.scrollWidth }));
    assert(metrics.scrollWidth <= metrics.width + 1, `${route}: mobile horizontal overflow`);
    result.responsive.push({ route, ...metrics });
  }

  assert(consoleErrors.length === 0, `Console errors: ${consoleErrors.join(' | ')}`);
  result.consoleErrors = consoleErrors;
  fs.writeFileSync(output, JSON.stringify(result, null, 2));
  await browser.close();
  console.log(`PHASE 4B V6 UI TEST: PASS (${poemSlugs.length} audio players, ${paperSlugs.length} scrollspies, contextual return, mobile layouts)`);
})().catch((error) => {
  console.error(error);
  process.exit(1);
});
