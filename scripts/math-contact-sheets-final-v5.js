const fs=require('fs'),path=require('path'),sharp=require(path.join(process.env.CODEX_NODE_MODULES,'sharp'));
const dir=path.resolve('docs/screenshots/final-v5');
(async()=>{
for(const id of ['household-demand-leisure','children-costs-one-adult-household','children-labor-supply-equations']){
const files=fs.readdirSync(dir).filter(n=>n.startsWith(`math-${id}-`)&&n.endsWith('.png')).sort();let height=0,items=[];
for(const file of files){let img=sharp(path.join(dir,file));let meta=await img.metadata();let w=900,h=Math.round(meta.height*w/meta.width);let png=await img.resize(w,h).png().toBuffer();let title=Buffer.from(`<svg width="900" height="32"><rect width="900" height="32" fill="#edf2f1"/><text x="10" y="22" font-family="Arial" font-size="16" fill="#173044">${file.replace('.png','')}</text></svg>`);items.push({input:title,left:0,top:height},{input:png,left:0,top:height+32});height+=h+48;}
await sharp({create:{width:900,height,channels:4,background:'#ffffff'}}).composite(items).png().toFile(path.join(dir,`math-sheet-${id}.png`));
console.log(id,files.length,'display formula blocks');
}
})();
