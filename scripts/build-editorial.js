// Zola Markdown -> build-time KaTeX HTML+MathML. No remote scripts/fonts.
const fs = require('fs');
const path = require('path');
const katex = require('katex');
const root = path.resolve(__dirname, '..');
const source = name => fs.readFileSync(path.join(root, 'docs/editorial-sources', name), 'utf8').replace(/\r\n/g, '\n');
const supplements = JSON.parse(fs.readFileSync(path.join(root, 'data/editorial-supplements.json'), 'utf8'));
const manifest = [];
function math(tex, displayMode, paper) {
  manifest.push({ paper, displayMode, tex });
  const html=katex.renderToString(tex, { displayMode, throwOnError: true, strict: 'error', output: 'htmlAndMathml' });
  // Markdown must not interpret underscores/asterisks in the hidden TeX annotation.
  // Numeric entities preserve the exact accessible annotation after HTML decoding.
  return html.replace(/(<annotation\b[^>]*>)([\s\S]*?)(<\/annotation>)/g,(_,a,s,b)=>a+Array.from(tex).map(c=>`&#${c.codePointAt(0)};`).join('')+b);
}
function render(text, paper) {
  const blocks = [];
  // Currency is not a math delimiter. Inline formulas never span a line.
  text = text.replace(/\$\$([\s\S]*?)\$\$/g, (_, tex) => {
    const i=blocks.length; blocks.push(`<div class="equation-card" tabindex="0" role="region" aria-label="Mathematical equation">${math(tex.trim(),true,paper)}</div>`); return `\n\nMATHBLOCKTOKEN${i}END\n\n`;
  });
  text = text.replace(/(?<!\\)\$((?:[\\A-Za-z({][^\n$]*?|[0-9][0-9 <>=.,-]*[=<>][^\n$]*?))(?<!\\)\$([.,;:!?]?)/g, (_, tex, punctuation) => `${math(tex,false,paper)}${punctuation ? '&#8288;'+punctuation : ''}`);
  text = text.replace(/MATHBLOCKTOKEN(\d+)END/g, (_, i) => blocks[Number(i)]);
  return text;
}
const labels = ['Research question','Research contribution','Non-technical overview','Abstract','Economic model','Identification','Econometric model & methods','Data & sources','Results & limits'];
const ids = ['question','contribution','overview','abstract','economic-model','identification','econometric-model','data-sources','findings'];
const papers=[];
for (const spec of supplements.papers) {
  let text=source(spec.source).replace(/^---\s*$/gm,'').replace(/^\*\*Equation block\*\*\s*$/gm,'');
  for(const replacement of spec.replacements || []) {
    if (!text.includes(replacement.before)) throw new Error(`Missing insertion anchor in ${spec.source}: ${replacement.before}`);
    text=text.replace(replacement.before,()=>replacement.after);
  }
  const chunks=text.split(/^##\s+/m).slice(1);
  const sections=chunks.map((chunk, index) => {
    const [heading,...lines]=chunk.split('\n');
    let body=lines.join('\n').trim();
    if (ids[index]==='abstract') return { id:'abstract',label:'Abstract',title:'',body:'' };
    const titleMatch=body.match(/^### (.+)\r?\n/);
    const title=titleMatch ? titleMatch[1].trim() : '';
    if(titleMatch) body=body.slice(titleMatch[0].length).trim();
    if(ids[index]==='identification' && spec.id==='household-demand-leisure') {
      body=body.replace('Two assumptions make the full-leisure demand', '<div class="measurement-note">\n\nTwo assumptions make the full-leisure demand');
      body=body.replace('- A.3: children affect the sharing rule only through remaining full income $Y_R$.','- A.3: children affect the sharing rule only through remaining full income $Y_R$.\n\n</div>');
    }
    if(ids[index]==='identification' && spec.id==='children-costs-one-adult-household') {
      body=body.replace('Three ingredients recover','<div class="measurement-note">\n\nThree ingredients recover');
      body=body.replace('- Engel curves are nonlinear in expenditure.','- Engel curves are nonlinear in expenditure.\n\n</div>');
    }
    if(ids[index]==='identification' && spec.id==='children-labor-supply-equations') {
      body=body.replace('**Tractable model (box).**','<div class="measurement-note">\n\n**Tractable model.**');
      body+='\n\n</div>';
    }
    body=body.replace(/^\*\[Table 1 and the Provider \/ Access \/ Exact data used blocks as on the current page\.\]\*\s*$/gm,'');
    body=body.replace(/\*\*Limits (?:acknowledged by the authors?\.|documented by the manuscript \(callout\))\*\*/g,'<div class="limit-box">\n<h3>LIMITS</h3>\n');
    if(body.includes('<div class="limit-box">')) body+='\n\n</div>';
    // Source-only editorial directions are omitted, their requested functions implemented.
    body=body.replace(/\*Illustration caption\.\* /g,'').replace(/\*\*Card 1\. Income effect of two fixed costs\.\*\*/g,'<div class="measurement-note"><h3>Income effect of two fixed costs</h3>').replace(/\*\*Card 2\. Time shift\.\*\*/g,'</div>\n\n<div class="measurement-note"><h3>Time shift</h3>');
    if (body.includes('<h3>Time shift</h3>')) body+='\n\n</div>';
    // Every table/figure citation refers to a verified physical page in the retained PDF.
    body=body.replace(/\b(Table\s+(?:A\.)?\d+|Figure\s+\d+)\b/g,(match) => {
      const page=spec.references[match];
      if(!page) return match;
      return `[${match}](${spec.pdf}#page=${page})`;
    });
    body=body.replace(/\bTables ([\dA.]+)([–—-]| and )([\dA.]+)/g,(match,first,sep,last)=>{
      const n=spec.references[`Table ${first}`];
      return n ? `[${match}](/research/${spec.id}/sources/#pdf-page-${n})` : match;
    });
    // Display equations are always followed immediately by a separate disclosure.
    body=body.replace(/\$\$([\s\S]*?)\$\$/g,(all) => `${all}\n\n<details class="symbol-definitions"><summary>Symbols, units, and indices</summary>\n${spec.symbols}\n</details>\n`);
    return { id:ids[index],label:labels[index],title,body:render(body,spec.id) };
  });
  if(sections.length!==9) throw new Error(`${spec.source}: expected nine source sections, got ${sections.length}`);
  papers.push({id:spec.id,sections,references:spec.references});
}
let note=source('note_modele.md').replace(/^# .+\n/,'');
note=note.replace('> **b → β + γ · δ**','NOTEFORMULA');
note=note.replace(/β/g,'$\\beta$').replace(/γ/g,'$\\gamma$').replace(/δ/g,'$\\delta$').replace('NOTEFORMULA',()=> '$$b \\longrightarrow \\beta + \\gamma\\delta$$');
fs.writeFileSync(path.join(root,'data/paper-editorial.json'),JSON.stringify({papers,note:render(note,'note-modele')},null,2));
fs.writeFileSync(path.join(root,'docs/final-v5-math-manifest.json'),JSON.stringify({engine:`KaTeX ${katex.version}`,formulas:manifest},null,2));
const vendor=path.join(root,'static/vendor/katex'); fs.mkdirSync(vendor,{recursive:true});
fs.copyFileSync(require.resolve('katex/dist/katex.min.css'),path.join(vendor,'katex.min.css'));
fs.cpSync(path.join(path.dirname(require.resolve('katex/dist/katex.min.css')),'fonts'),path.join(vendor,'fonts'),{recursive:true});
fs.copyFileSync(path.join(path.dirname(require.resolve('katex/package.json')),'LICENSE'),path.join(vendor,'LICENSE'));
console.log(`Rendered ${manifest.length} formulas in three papers and the French note with local KaTeX.`);
