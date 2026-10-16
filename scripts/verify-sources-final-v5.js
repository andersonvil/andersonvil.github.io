const fs=require('fs'),path=require('path'),{chromium}=require(path.join(process.env.CODEX_NODE_MODULES,'playwright'));
const dir=path.resolve('docs/screenshots/final-v5'), data=JSON.parse(fs.readFileSync('data/scientific-pages.json','utf8'));
(async()=>{
let browser=await chromium.launch({headless:true,executablePath:'C:/Program Files/Google/Chrome/Application/chrome.exe'}),page=await browser.newPage({viewport:{width:1440,height:1000}});let report={pages:[],pdf:[],overflow:[],identification:{},immersive:{}};
for(const paper of data.papers){
 let pdf=await page.request.get('http://127.0.0.1:1114'+paper.pdf);if(pdf.status()!==200)throw Error('PDF unavailable');report.pdf.push({id:paper.id,status:pdf.status(),contentType:pdf.headers()['content-type']});
 for(const source of paper.pages){let res=await page.request.get('http://127.0.0.1:1114'+source.image);if(res.status()!==200)throw Error('Source render unavailable');report.pages.push({id:paper.id,page:source.number,status:res.status()});}
 await page.goto('http://127.0.0.1:1114/research/'+paper.id+'/sources/#pdf-page-'+paper.pages[0].number);await page.locator('#pdf-page-'+paper.pages[0].number).scrollIntoViewIfNeeded();await page.screenshot({path:path.join(dir,`sources-${paper.id}.png`)});
 await page.setViewportSize({width:390,height:844});await page.goto('http://127.0.0.1:1114/research/'+paper.id+'/');
 const formulas=page.locator('.equation-card');for(let i=0;i<await formulas.count();i++){
   let el=formulas.nth(i);let needsScroll=await el.evaluate(e=>e.scrollWidth>e.clientWidth+1);if(needsScroll){await el.focus();await page.keyboard.press('End');await el.evaluate(e=>e.scrollLeft=e.scrollWidth);if(!await el.evaluate(e=>e.scrollLeft>0))throw Error('Inaccessible equation overflow');report.overflow.push({id:paper.id,index:i,scrollable:true});}
 }
 await page.setViewportSize({width:1440,height:1000});
}
await page.goto('http://127.0.0.1:1114/research/household-demand-leisure/');report.identification=await page.locator('#identification').evaluate(e=>({text:e.innerText,figureCount:e.querySelectorAll('figure.source-slide').length,figureWidths:[...e.querySelectorAll('figure.source-slide')].map(x=>x.getBoundingClientRect().width)}));
await page.locator('#identification').scrollIntoViewIfNeeded();await page.screenshot({path:path.join(dir,'final-jmp-identification.png')});
await page.goto('http://127.0.0.1:1114/beyond-economics/calligraphie-de-bonheur/');await page.locator('.immersive-open').click();if(!await page.locator('.immersive-reader').evaluate(e=>e.open))throw Error('Immersive reader failed');await page.keyboard.press('ArrowRight');await page.keyboard.press('Escape');if(await page.locator('.immersive-reader').evaluate(e=>e.open))throw Error('Keyboard exit failed');report.immersive={open:true,arrows:true,escape:true};
await page.setViewportSize({width:1908,height:908});await page.goto('http://127.0.0.1:1114/');await page.evaluate(()=>document.documentElement.dataset.theme='light');await page.screenshot({path:path.join(dir,'final-home-1908-light.png')});
await page.goto('http://127.0.0.1:1114/insights/quest-ce-quun-modele/');await page.locator('.equation-card').screenshot({path:path.join(dir,'math-note-modele.png')});
await page.goto('http://127.0.0.1:1114/contact/');report.emailFont=await page.locator('.email-address').evaluate(e=>getComputedStyle(e).fontSize);
fs.writeFileSync(path.join(dir,'source-viewer-verification.json'),JSON.stringify(report,null,2));console.log(JSON.stringify({sourceImages:report.pages.length,pdfs:report.pdf.length,scrollableEquations:report.overflow.length,immersive:report.immersive,email:report.emailFont}));await browser.close();
})().catch(e=>{console.error(e);process.exit(1)});
