const fs=require('fs'), path=require('path');
const {chromium}=require(path.join(process.env.CODEX_NODE_MODULES,'playwright'));
const root=path.resolve(__dirname,'..'), dir=path.join(root,'docs/screenshots/final-v5');
const base='http://127.0.0.1:1114';
const routes=['/','/research/','/teaching/','/code-data/','/insights/','/beyond-economics/','/contact/','/talks/','/research/household-demand-leisure/','/research/children-costs-one-adult-household/','/research/children-labor-supply-equations/','/insights/quest-ce-quun-modele/','/insights/what-household-averages-can-hide/','/teaching/data-analysis-l3/','/teaching/econometrics-niort/','/beyond-economics/calligraphie-de-bonheur/','/404.html'];
const results={responsive:[],home:[],math:[],interactions:{},errors:[],hover:[]};
const assert=(v,m)=>{if(!v)throw Error(m)};
(async()=>{
fs.mkdirSync(dir,{recursive:true});
const browser=await chromium.launch({headless:true,executablePath:'C:/Program Files/Google/Chrome/Application/chrome.exe'});
const context=await browser.newContext({viewport:{width:1908,height:908},deviceScaleFactor:1,permissions:['clipboard-read','clipboard-write']});
const page=await context.newPage();page.on('pageerror',e=>results.errors.push(e.message));
await page.goto(base+'/');
results.interactions.defaultDark=await page.locator('html').getAttribute('data-theme');assert(results.interactions.defaultDark==='dark','Initial theme must be dark');
await page.locator('.theme-toggle').click();await page.reload();assert(await page.locator('html').getAttribute('data-theme')==='light','Theme must persist');results.interactions.savedTheme=true;
for(const theme of ['light','dark']) for(const width of [1908,1440,768,390,360]) {
  await page.setViewportSize({width,height:width>768?908:844});
  await page.evaluate(t=>{localStorage.setItem('av-theme',t)},theme);
  for(const route of routes){
    const response=await page.goto(base+route,{waitUntil:'load'});await page.evaluate(()=>document.fonts.ready);assert(response.status()<400 || (route==='/404.html' && response.status()===404),`${route} unavailable: ${response.status()}`);
    const m=await page.evaluate(()=>({width:innerWidth,scrollWidth:document.documentElement.scrollWidth,bodySize:getComputedStyle(document.body).fontSize,theme:document.documentElement.dataset.theme,rawMath:document.querySelectorAll('.katex-error').length}));
    results.responsive.push({route,width,theme,...m});
    assert(m.scrollWidth<=width+1,`Global overflow ${route} ${width} ${theme}: ${m.scrollWidth}`);assert(m.rawMath===0,'KaTeX error');
    if(route==='/'){
      const metric=await page.evaluate(()=>({scale:visualViewport.scale,dpr:devicePixelRatio,hero:document.querySelector('.hero').getBoundingClientRect().height,jmpTop:document.querySelector('.jmp-band').getBoundingClientRect().top,actionsBottom:document.querySelector('.jmp-band .button-row').getBoundingClientRect().bottom,labels:[...document.querySelectorAll('.home-pathways article')].map(x=>({label:x.querySelector('.eyebrow').textContent.trim(),x:x.getBoundingClientRect().x,y:x.getBoundingClientRect().y,width:x.getBoundingClientRect().width})),bio:[...document.querySelectorAll('.hero-bio p')].map(e=>({text:e.textContent,align:getComputedStyle(e).textAlign,font:getComputedStyle(e).fontSize}))}));
      results.home.push({width,theme,...metric});
      assert(metric.labels.map(x=>x.label).join('|')==='Research|Teaching|Code & Data|Insights|Beyond Economics|Contact','Home order');
      assert(await page.locator('#jmp-title a').getAttribute('href')==='/research/household-demand-leisure/','Entire JMP title link');
      if(width>768)assert(metric.labels[0].x!==metric.labels[1].x && metric.labels[0].y===metric.labels[1].y,'Home needs two columns');
      await page.screenshot({path:path.join(dir,`after-home-${width}-${theme}.png`),fullPage:width<=390});
      if(width===1908){await page.evaluate(()=>document.querySelector('.preview-ribbon').hidden=true);await page.addStyleTag({content:'.preview-ribbon[hidden]{display:none}'});results.home.at(-1).withoutRibbonActionsBottom=await page.locator('.jmp-band .button-row').evaluate(el=>el.getBoundingClientRect().bottom);await page.screenshot({path:path.join(dir,`future-public-layout-${width}-${theme}.png`)});}
    }
    if(width===1440 && ['/research/','/teaching/','/contact/','/insights/'].includes(route))await page.screenshot({path:path.join(dir,`${route.split('/')[1]}-1440-${theme}.png`),fullPage:true});
    if(width===390 && ['/contact/','/teaching/data-analysis-l3/','/insights/quest-ce-quun-modele/','/beyond-economics/calligraphie-de-bonheur/'].includes(route))await page.screenshot({path:path.join(dir,`${route.split('/').filter(Boolean).join('-')}-390-${theme}.png`),fullPage:true});
  }
}
await page.setViewportSize({width:1440,height:1000});
for(const route of routes.filter(r=>r.startsWith('/research/')&&r!='/research/')){
  await page.goto(base+route);await page.evaluate(()=>{document.documentElement.dataset.theme='light'});
  const formulas=page.locator('.equation-card');
  for(let i=0;i<await formulas.count();i++){
    const e=formulas.nth(i); const m=await e.evaluate(el=>({tex:el.querySelector('annotation')?.textContent,width:el.clientWidth,scroll:el.scrollWidth,mathml:!!el.querySelector('math'),nextDisclosure:el.nextElementSibling?.classList.contains('symbol-definitions')}));
    assert(m.mathml,'Missing accessible MathML');assert(m.nextDisclosure,'Symbols must immediately follow display math');
    const file=`math-${route.split('/')[2]}-${String(i+1).padStart(2,'0')}.png`;await e.screenshot({path:path.join(dir,file)});results.math.push({route,file,...m});
  }
  await page.locator('#identification').scrollIntoViewIfNeeded();await page.screenshot({path:path.join(dir,`${route.split('/')[2]}-identification-light.png`)});
  await page.locator('#findings').scrollIntoViewIfNeeded();await page.screenshot({path:path.join(dir,`${route.split('/')[2]}-findings-light.png`)});
  await page.locator('.symbol-definitions summary').first().focus();await page.keyboard.press('Enter');assert(await page.locator('.symbol-definitions').first().evaluate(e=>e.open),'Keyboard disclosure');
  await page.locator('.copy-citation').click();await page.waitForFunction(()=>document.querySelector('.copy-status').textContent.includes('copied'));assert((await page.locator('.copy-status').textContent()).includes('copied'),'Citation copy');
  results.interactions[route]={keyboardSymbols:true,citation:true,limits:await page.locator('.limit-box h3').textContent(),activeToc:await page.locator('.paper-toc [aria-current="location"]').count()};
}
await page.goto(base+'/contact/');assert(await page.locator('[data-meeting-demo]').count()===0,'Simulation still visible');assert((await page.locator('.meeting-request a').getAttribute('href')).startsWith('mailto:'),'Honest meeting request');await page.locator('.copy-email').click();await page.waitForFunction(()=>document.querySelector('.copy-email-status').textContent.includes('copied'));assert((await page.locator('.copy-email-status').textContent()).includes('copied'),'Email copy');results.interactions.emailFont=await page.locator('.email-address').evaluate(e=>getComputedStyle(e).fontSize);
await page.goto(base+'/');await page.locator('.abstract-toggle').first().click();assert(await page.locator('#home-jmp-abstract').isVisible(),'Abstract');
await page.setViewportSize({width:390,height:844});await page.locator('.menu-toggle').click();assert(await page.locator('#mobile-menu').isVisible(),'Mobile menu');results.interactions.mobileMenu=true;
await page.goto(base+'/beyond-economics/calligraphie-de-bonheur/');assert(await page.locator('audio').evaluate(e=>!e.autoplay && e.paused),'Audio must start paused');await page.locator('.audio-play-toggle').click();await page.waitForTimeout(700);assert(await page.locator('audio').evaluate(e=>!e.paused),'Audio play');await page.locator('.audio-play-toggle').click();results.interactions.audio=true;
await page.goto(base+'/insights/reading-model-based-estimates/');await page.waitForURL('**/insights/quest-ce-quun-modele/');results.interactions.methodsRedirect=true;
// Audit computed hover differences on every distinct interactive style in main routes.
await page.setViewportSize({width:1440,height:1000});
for(const theme of ['light','dark'])for(const route of ['/','/research/','/teaching/','/contact/','/research/household-demand-leisure/','/beyond-economics/calligraphie-de-bonheur/']){
 await page.goto(base+route);await page.evaluate(t=>document.documentElement.dataset.theme=t,theme);
 const seen=new Set();const interactive=page.locator('a:not(.skip-link),button,summary,input[type=range]');
 for(let i=0;i<await interactive.count();i++){const el=interactive.nth(i);if(!await el.isVisible())continue;const type=await el.evaluate(e=>e.tagName+'.'+e.className+'.'+(e.closest('.feature-band')?'band':'')+'.'+(e.closest('.site-footer')?'footer':''));if(seen.has(type))continue;seen.add(type);await page.mouse.move(0,0);const style=()=>el.evaluate(e=>{const s=getComputedStyle(e);return[s.color,s.backgroundColor,s.borderColor,s.accentColor].join('|')});const before=await style();await el.hover();await page.waitForTimeout(220);const after=await style();results.hover.push({route,theme,type,changed:before!==after,before,after,text:(await el.textContent())?.trim().slice(0,60)});}
}
assert(results.errors.length===0,'Browser JS errors');
fs.writeFileSync(path.join(dir,'verification.json'),JSON.stringify(results,null,2));
console.log(JSON.stringify({responsive:results.responsive.length,math:results.math.length,hoverFailures:results.hover.filter(x=>!x.changed),home:results.home.filter(x=>x.width===1908),interactions:results.interactions},null,2));await browser.close();
})().catch(e=>{fs.writeFileSync(path.join(dir,'verification-incomplete.json'),JSON.stringify(results,null,2));console.error(e);process.exit(1)});
