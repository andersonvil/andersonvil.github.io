// Zola Markdown -> build-time KaTeX HTML+MathML. No remote scripts/fonts.
const fs = require('fs');
const path = require('path');
const katex = require('katex');
const root = path.resolve(__dirname, '..');
const source = name => fs.readFileSync(path.join(root, 'docs/editorial-sources', name), 'utf8').replace(/\r\n/g, '\n');
const supplements = JSON.parse(fs.readFileSync(path.join(root, 'data/editorial-supplements.json'), 'utf8'));
const equationDetails = JSON.parse(fs.readFileSync(path.join(root, 'data/equation-details.json'), 'utf8'));
const manifest = [];
function math(tex, displayMode, paper) {
  tex=tex.replace(/\\hat\s*(\\[a-zA-Z]+|[a-zA-Z])/g, '\\hat{$1}');
  manifest.push({ paper, displayMode, tex });
  const html=katex.renderToString(tex, { displayMode, throwOnError: true, strict: code=>code==='htmlExtension'?'ignore':'error', trust: context=>context.command==='\\htmlId', output: 'htmlAndMathml' });
  // Markdown must not interpret underscores/asterisks in the hidden TeX annotation.
  // Numeric entities preserve the exact accessible annotation after HTML decoding.
  return html.replace(/(<annotation\b[^>]*>)([\s\S]*?)(<\/annotation>)/g,(_,a,s,b)=>a+Array.from(tex).map(c=>`&#${c.codePointAt(0)};`).join('')+b);
}
function render(text, paper, details) {
  const blocks = [];
  // Currency is not a math delimiter. Inline formulas never span a line.
  text = text.replace(/\$\$([\s\S]*?)\$\$/g, (_, tex) => {
    const i=blocks.length;
    const definitions=details ? details.shift() : null;
    if(details && !definitions) throw Error(`Missing local equation definitions: ${paper}`);
    // Equation scrolling is local; the disclosure remains inside its own card.
    blocks.push(`<div class="equation-card" role="region" aria-label="Mathematical equation"><div class="equation-scroll" tabindex="0">${math(tex.trim(),true,paper)}</div>${definitions ? `<details class="symbol-definitions"><summary>View details</summary><div>${inlineMath(definitions,paper)}</div></details>` : ''}</div>`);
    return `\n\nMATHBLOCKTOKEN${i}END\n\n`;
  });
  text = inlineMath(text,paper);
  text = text.replace(/MATHBLOCKTOKEN(\d+)END/g, (_, i) => blocks[Number(i)]);
  return text;
}
function inlineMath(text,paper) {
  // Test each possible opening dollar independently so dollar amounts do not eat
  // a later formula. Numeric formulas such as $1-\\varphi$ remain valid.
  return scanInlineMath(text,tex=>math(tex,false,paper));
}
function scanInlineMath(text,renderFormula) {
  let result='',cursor=0;
  while(cursor<text.length) {
    const start=text.indexOf('$',cursor);
    if(start<0) return result+text.slice(cursor);
    result+=text.slice(cursor,start);
    const end=text.indexOf('$',start+1),newline=text.indexOf('\n',start+1);
    const tex=end>=0?text.slice(start+1,end):'';
    const valid=end>start+1 && (newline<0 || end<newline) && text[start-1]!=='\\' && /^[\\A-Za-z({\[0-9+−-]/.test(tex) && (/^[\\A-Za-z({\[]/.test(tex) || /^[0-9][0-9.,\s]*[+−=<>^_\\-]/.test(tex) || /^[0-9][0-9.,]*[A-Za-z{\\]/.test(tex) || /^[0-9.,+−-]+$/.test(tex));
    if(!valid) {result+='$';cursor=start+1;continue;}
    result+=renderFormula(tex);cursor=end+1;
    if(/[.,;:!?]/.test(text[cursor]||' ') && cursor<text.length)result+='&#8288;'+text[cursor++];
  }
  return result;
}
function manuscriptLinks(body,spec) {
  const held=[];
  const hold=m=>`HOLDTOKEN${held.push(m)-1}END`;
  body=body.replace(/\$\$[\s\S]*?\$\$|\[[^\]]*\]\([^)]*\)|<\/?[a-zA-Z][^>]*>/g,hold);
  body=scanInlineMath(body,tex=>hold('$'+tex+'$'));
  const link=(label,n,accessible=label)=>n ? `<a class="manuscript-reference" href="${spec.pdf}#page=${n}" aria-label="${accessible} — manuscript physical PDF page ${n}">${label}</a>` : label;
  // A range receives a link for each cited target, including when pages differ.
  body=body.replace(/\b(Tables|Propositions|equations)\s+(\([A.\d]+\)|[A.\d]+)(\s*(?:[–—-]| and )\s*)(\([A.\d]+\)|[A.\d]+)/gi,(m,kind,a,sep,b)=>{
    const singular=/table/i.test(kind)?'Table':/proposition/i.test(kind)?'Proposition':'equation';
    const lookup=n=>singular==='equation' ? spec.equation_pages[n.replace(/[()]/g,'')] : spec.references[`${singular} ${n}`];
    const first=lookup(a),last=lookup(b);
    const result=first && first===last ? link(m,first) : `${link(`${kind} ${a}`,first,`${singular} ${a}`)}${sep}${link(b,last,`${singular} ${b}`)}`;
    return `HOLDTOKEN${held.push(result)-1}END`;
  });
  const keys=Object.keys(spec.references).sort((a,b)=>b.length-a.length).map(k=>k.replace(/[.*+?^${}()|[\]\\]/g,'\\$&'));
  body=body.replace(new RegExp(`(?<![\\w.])(${keys.join('|')})(?![\\w.′])`,'g'),m=>link(m,spec.references[m]));
  body=body.replace(/\b(equations?)\s*(?:\(([0-9]+)\)|([0-9]+))/gi,(m,k,a,b)=>hold(link(m,spec.equation_pages[a||b])));
  body=body.replace(/\b(equations\s+[0-9]+–[0-9]+),\s*([0-9]+)–([0-9]+)/gi,(m,label,a,b)=>`${label}, ${hold(link(`equation ${a}`,spec.equation_pages[a]))}–${hold(link(`equation ${b}`,spec.equation_pages[b]))}`);
  body=body.replace(/\b(restriction) \(([0-9]+)\)/g,(m,k,n)=>`${k} ${hold(link(`(${n})`,spec.equation_pages[n]))}`);
  return body.replace(/HOLDTOKEN(\d+)END/g,(_,i)=>held[Number(i)]);
}
const labels = ['Research question','Research contribution','Non-technical overview','Abstract','Economic model','Identification','Econometric model & methods','Data & sources','Results & limits'];
const ids = ['question','contribution','overview','abstract','economic-model','identification','econometric-model','data-sources','findings'];
const papers=[];
for (const spec of supplements.papers) {
  const details=[...equationDetails[spec.id]];
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
    body=manuscriptLinks(body,spec);
    body=body.replace(/\*\*Main findings\.\*\*/g,'<h3 class="main-findings">Main findings</h3>\n\n');
    return { id:ids[index],label:labels[index],title,body:render(body,spec.id,details) };
  });
  if(sections.length!==9) throw new Error(`${spec.source}: expected nine source sections, got ${sections.length}`);
  if(details.length) throw Error(`Unused local equation definitions: ${spec.id}: ${details.length}`);
  papers.push({id:spec.id,sections,references:spec.references});
}
let note=source('note_modele.md').replace(/^# .+\n/,'');
note=note.replace('> **b → β + γ · δ**','NOTEFORMULA');
note=note.replace(/β/g,'$\\beta$').replace(/γ/g,'$\\gamma$').replace(/δ/g,'$\\delta$').replace('NOTEFORMULA',()=> '$$b \\longrightarrow \\beta + \\gamma\\delta$$');
// The source title's footnote call is rendered in the page's actual h1.
note=note.replace(/^\[\^1\]: (.+)$/m,'<section class="footnotes" aria-label="Notes"><ol><li id="model-note-1">$1 <a href="#model-ref-1" aria-label="Retour à l’appel de note">↩</a></li></ol></section>');
const footnote=note.match(/<section class="footnotes"[\s\S]*?<\/section>/)?.[0] || '';
note=note.replace(footnote,'').trim()+'\n\n'+footnote;
fs.writeFileSync(path.join(root,'data/paper-editorial.json'),JSON.stringify({papers,note:render(note,'note-modele')},null,2));
// Complete supplied teaching note: no prose changes, only presentation and links.
let homogeneity=source('homogeneity.md').replace(/^---[\s\S]*?---\s*/, '');
const equations=new Set();
homogeneity=homogeneity.replace(/\$\$([\s\S]*?)\$\$/g,(all,tex)=>{
  const anchors=[...tex.matchAll(/(?:&&|\\qquad)\s*\((\d+)\)/g)].map(m=>Number(m[1]));
  anchors.forEach(n=>equations.add(n));
  // Row anchors are placed on their exact KaTeX row, not at the top of a long derivation.
  tex=tex.replace(/((?:&&|\\qquad)\s*)\((\d+)\)/g,(_,prefix,n)=>`${prefix}\\htmlId{equation-${n}}{(${n})}`);
  return '$$'+tex+'$$';
});
const heldNote=[];
homogeneity=homogeneity.replace(/\$\$[\s\S]*?\$\$/g,m=>`NOTETOKEN${heldNote.push(m)-1}END`);
homogeneity=scanInlineMath(homogeneity,tex=>`NOTETOKEN${heldNote.push('$'+tex+'$')-1}END`);
homogeneity=homogeneity.replace(/^## (\d+)\. (.+)$/gm,(_,n,title)=>`<h2 id="section-${n}">${n}. ${title}</h2>`)
 .replace(/^### References$/m,'<h3 id="references">References</h3>')
 .replace(/^### Appendix: power of the test$/m,'<h3 id="appendix">Appendix: power of the test</h3>')
 .replace(/!\[Figure ([\dA.]+)\]\(figures\/([^)]*)\)\n\n\*Figure \1: ([^\n]+)\*/g,(_,n,file,caption)=>{
   const png=fs.readFileSync(path.join(root,'static/img/insights/homogeneity',file));
   return `<figure id="figure-${n.toLowerCase().replace('.','-')}"><img src="/img/insights/homogeneity/${file}" alt="Figure ${n}" width="${png.readUInt32BE(16)}" height="${png.readUInt32BE(20)}" loading="lazy"><figcaption>Figure ${n}: ${caption}</figcaption></figure>`;
 })
 .replace(/^\*Table (\d+): (.+)\*$/gm,(_,n,title)=>`<p class="table-caption" id="table-${n}"><em>Table ${n}: ${title}</em></p>`);
// Protect tags (including figure captions' IDs) while linking prose references.
homogeneity=homogeneity.split(/(<[^>]+>)/g).map(s=>s.startsWith('<')?s:s
 .replace(/\bSection (\d+)\b/g,'<a href="#section-$1">Section $1</a>')
 .replace(/\bFigure ([\dA.]+)\b/g,(m,n)=>`<a href="#figure-${n.toLowerCase().replace('.','-')}">${m}</a>`)
 .replace(/\bTable (\d+)\b/g,'<a href="#table-$1">Table $1</a>')
 .replace(/\((\d+)\)/g,(m,n)=>equations.has(Number(n))?`<a href="#equation-${n}">${m}</a>`:m)).join('');
homogeneity=homogeneity.replace(/NOTETOKEN(\d+)END/g,(_,i)=>heldNote[Number(i)]);
fs.writeFileSync(path.join(root,'data/homogeneity.json'),JSON.stringify({body:render(homogeneity,'homogeneity')},null,2));
fs.writeFileSync(path.join(root,'docs/final-v7-math-manifest.json'),JSON.stringify({engine:`KaTeX ${katex.version}`,formulas:manifest},null,2));
const vendor=path.join(root,'static/vendor/katex'); fs.mkdirSync(vendor,{recursive:true});
fs.copyFileSync(require.resolve('katex/dist/katex.min.css'),path.join(vendor,'katex.min.css'));
fs.cpSync(path.join(path.dirname(require.resolve('katex/dist/katex.min.css')),'fonts'),path.join(vendor,'fonts'),{recursive:true});
fs.copyFileSync(path.join(path.dirname(require.resolve('katex/package.json')),'LICENSE'),path.join(vendor,'LICENSE'));
console.log(`Rendered ${manifest.length} formulas across three papers and two notes with local KaTeX.`);
