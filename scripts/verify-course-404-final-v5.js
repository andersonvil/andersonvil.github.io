const fs=require('fs'),path=require('path'),{chromium}=require(path.join(process.env.CODEX_NODE_MODULES,'playwright'));
(async()=>{
const browser=await chromium.launch({headless:true,executablePath:'C:/Program Files/Google/Chrome/Application/chrome.exe'}),page=await browser.newPage({viewport:{width:1440,height:1000}}),result={courses:[]};
const response=await page.goto('http://127.0.0.1:1114/unknown-route-for-404-check/');
result.notFound={status:response.status(),title:await page.locator('h1').textContent(),robots:response.headers()['x-robots-tag']};
if(result.notFound.status!==404 || !result.notFound.title.includes('not here'))throw Error('Custom 404 failed');
await page.screenshot({path:path.resolve('docs/screenshots/final-v5/custom-404.png')});
for(const theme of ['light','dark']){
 await page.goto('http://127.0.0.1:1114/teaching/');await page.evaluate(t=>document.documentElement.dataset.theme=t,theme);
 const titles=page.locator('.teaching-group h3 a');
 for(let i=0;i<await titles.count();i++){
  const el=titles.nth(i);await el.scrollIntoViewIfNeeded();await page.mouse.move(0,0);const before=await el.evaluate(e=>getComputedStyle(e).color);await el.hover();const after=await el.evaluate(e=>getComputedStyle(e).color);
  if(before===after)throw Error('Course title does not change color');result.courses.push({theme,title:await el.textContent(),before,after});
 }
}
fs.writeFileSync(path.resolve('docs/screenshots/final-v5/course-404-verification.json'),JSON.stringify(result,null,2));console.log(JSON.stringify({notFound:result.notFound,courseHoverChecks:result.courses.length}));await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
