"""Check integration fidelity, generated local links, math and private-preview guards."""
from pathlib import Path
import json,re,hashlib,tomllib,ast,argparse,zipfile
from urllib.parse import urlsplit,unquote
from bs4 import BeautifulSoup
ROOT=Path(__file__).resolve().parents[1]
parser=argparse.ArgumentParser();parser.add_argument('--build-dir',default='public-preview-final');args=parser.parse_args()
BUILD=(ROOT/args.build_dir).resolve();SOURCE=ROOT.parent/'andersonvil-academic-os-sources'
fail=[];docs={};links=0
for p in BUILD.rglob('*.html'):
    soup=BeautifulSoup(p.read_text(encoding='utf-8'),'html.parser');docs[p]=soup
    ids=[e['id'] for e in soup.select('[id]')]
    duplicates={x for x in ids if ids.count(x)>1}
    if duplicates:fail.append(f'Duplicate ids: {p.relative_to(BUILD)} {duplicates}')
for p,soup in docs.items():
    for e in soup.select('a[href],img[src],source[src],script[src],link[href]'):
        href=e.get('href') or e.get('src');url=urlsplit(href)
        if url.scheme or url.netloc:continue
        target=BUILD/unquote(url.path).lstrip('/') if url.path.startswith('/') else (p.parent/unquote(url.path) if url.path else p)
        if target.is_dir():target=target/'index.html'
        if not target.exists():fail.append(f'Missing file: {p.relative_to(BUILD)} → {href}');continue
        links+=1
        if url.fragment and target.suffix=='.html' and target in docs and not docs[target].find(id=unquote(url.fragment)):
            fail.append(f'Missing anchor: {p.relative_to(BUILD)} → {href}')
    if soup.select_one('.site-header'):
        assert soup.select_one('meta[name=robots]')['content'] in ('index, follow','noindex, follow')
        assert not soup.select_one('.preview-ribbon')
        for n in soup.select('annotation,script,style,code,pre,.katex-mathml,.katex-html'):n.decompose()
        text=soup.get_text(' ',strip=True)
        if re.search(r'\$[^$\n]*\\[a-zA-Z]',text):fail.append('Raw LaTeX: '+str(p.relative_to(BUILD)))
        if re.search(r'provisional|synthetic demonstration|not approved|pending verification|Source-title note|publication blocked|Institutional wording to confirm|Historical dates inside',text,re.I):
            fail.append('Obsolete public note: '+str(p.relative_to(BUILD)))
        if re.search(r'\[[^\]]+\]\([^)]+\)',text):fail.append('Raw Markdown link: '+str(p.relative_to(BUILD)))
ad=tomllib.loads((ROOT/'data/academic.toml').read_text(encoding='utf-8-sig'))
scientific=[]
for paper in ad['papers']:
    p=BUILD/f'research/{paper["id"]}/index.html';soup=BeautifulSoup(p.read_text(encoding='utf-8'),'html.parser')
    assert soup.select_one('.abstract-details p').text==paper['abstract']
    cards=soup.select('.equation-card');assert cards and all(c.select_one(':scope > details.symbol-definitions') for c in cards)
    assert not soup.find(string=re.compile('Symbols, units'))
    assert hashlib.sha256((SOURCE/paper['source_file']).read_bytes()).digest()==hashlib.sha256((BUILD/paper['pdf'].lstrip('/')).read_bytes()).digest()
    assert soup.select_one('#documents').get_text().count('Research code will be made available after publication of the paper.')==1
    for c in soup.select('caption'):assert '. ·' not in c.text and c.text.count('Open manuscript')==1
    refs=[{'label':a.get_text(' ',strip=True),'href':a['href']} for a in soup.select('a[href]') if '#page=' in a['href']]
    scientific.append({'paper':paper['id'],'equations':len(cards),'references':refs})
    # Every scientific citation in prose must be in a link (plain ranges handled separately).
    for el in soup.select('.editorial-prose'):
        for a in el.select('a,.katex'):a.decompose()
        remaining=el.get_text(' ',strip=True)
        unlinked=re.findall(r'\b(?:Table\s+(?:A\.)?\d+|Figure\s+\d+|Propositions?\s+\d+|Corollary\s+\d+|Assumption\s+A\.[123]|equations?\s*\(?\d+)',remaining,re.I)
        if unlinked:fail.append(f'Unlinked scientific references {paper["id"]}: {unlinked}')
manifest=json.loads((ROOT/'docs/final-v7-resource-manifest.json').read_text(encoding='utf-8'))
for f in manifest['files']:
    target=BUILD/f['url'].lstrip('/')
    assert hashlib.sha256(target.read_bytes()).hexdigest()==f['sha256'],f['url']
dataset_files=0
with zipfile.ZipFile(SOURCE/'CY-Cergy-Paris-Université/2026-27/data/Dataset.zip') as z:
    for entry in z.infolist():
        if entry.is_dir():continue
        assert (BUILD/'files/teaching/cy-cergy/l3-econometrics/data'/entry.filename).read_bytes()==z.read(entry)
        dataset_files+=1
note=BeautifulSoup((BUILD/'insights/homogeneity/index.html').read_text(encoding='utf-8'),'html.parser')
assert len(note.select('figure'))==4
for n in range(1,28):assert note.find(id=f'equation-{n}'),n
assert len(note.select('.prose table'))==2
assert note.find(id='homogeneity-note-1') and note.find(id='homogeneity-ref-1')
model=BeautifulSoup((BUILD/'insights/quest-ce-quun-modele/index.html').read_text(encoding='utf-8'),'html.parser')
assert model.find(id='model-note-1') and model.find(id='model-ref-1')
assert not (BUILD/'insights/what-household-averages-can-hide/index.html').exists()
assert not (BUILD/'files/demo').exists()
assert not any(BUILD.rglob('homogeneity.pdf')) and not any(BUILD.rglob('homogeneity.tex')) and not any(BUILD.rglob('homogeneity.md'))
for slug,lines in [('calligraphie-de-bonheur',9),('ma-cherie',7),('mes-nuits',6),('on-me-l-a-interdit',5)]:
    poem=BeautifulSoup((BUILD/f'beyond-economics/{slug}/index.html').read_text(encoding='utf-8'),'html.parser')
    assert len(poem.select('.poem-line'))==lines
    if slug!='calligraphie-de-bonheur':assert not poem.select_one('.immersive-reader') and 'Request access to the full poem' in poem.get_text()
for name in ['ma_cherie_revision_3_J2.pdf','mes_nuits_revision_3_B3.pdf','on_me_l_a_interdit_revision_3_E4.pdf']:assert not list(BUILD.rglob(name))
beyond=BeautifulSoup((BUILD/'beyond-economics/index.html').read_text(encoding='utf-8'),'html.parser')
assert [e.get_text(strip=True) for e in beyond.select('.media-section h3')]==['Suggested Books','If I Had 24 Hours Left','Echoes of Childhood']
assert len(beyond.select('audio[controls]'))==3 and not beyond.select('audio[autoplay]')
for route in ['research','teaching','code-data','insights','beyond-economics','contact']:
    soup=BeautifulSoup((BUILD/f'{route}/index.html').read_text(encoding='utf-8'),'html.parser')
    assert any('Back to Home' in a.text and a['href']=='/' for a in soup.select('main a[href]')),route
teaching=tomllib.loads((ROOT/'data/teaching_resources.toml').read_text(encoding='utf-8-sig'))
cc=[r for g in teaching['groups'] if g['id']=='econometrics-l3' for r in g['resources'] if 'previous-cc/' in r['file']]
assert len(cc)==9 and sum('Answer key' in r['label'] for r in cc)==2
scripts=0;notebooks=0
for p in (ROOT/'static/files').rglob('*'):
    if p.suffix=='.py':ast.parse(p.read_text(encoding='utf-8-sig'));scripts+=1
    if p.suffix=='.ipynb':
        nb=json.loads(p.read_text(encoding='utf-8'));assert 'cells' in nb and 'metadata' in nb;notebooks+=1
report={'html_files':len(docs),'local_links_and_assets':links,'verified_copies':len(manifest['files']),'verified_extracted_dataset_files':dataset_files,'historical_cc':len(cc),'answer_keys':2,'python_syntax':scripts,'notebooks_structure':notebooks,'scientific':scientific,'failures':fail}
(ROOT/'docs/production-static-audit.json').write_text(json.dumps(report,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps({k:v for k,v in report.items() if k!='scientific'},ensure_ascii=False))
if fail:raise SystemExit(1)
