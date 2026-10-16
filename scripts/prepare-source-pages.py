from pathlib import Path
import json, subprocess, re, hashlib, tomllib
from pypdf import PdfReader
root=Path(__file__).resolve().parents[1]
specs=json.loads((root/'data/editorial-supplements.json').read_text(encoding='utf-8'))['papers']
extra={'household-demand-leisure':[17,24,29,30,31,32,39], 'children-costs-one-adult-household':[10,11,13], 'children-labor-supply-equations':[15,16,17,18]}
academic=tomllib.loads((root/'data/academic.toml').read_text(encoding='utf-8-sig'))
metadata={p['id']:p for p in academic['papers']}
previous_path=root/'docs/source-page-hashes.json'
previous=json.loads(previous_path.read_text()) if previous_path.exists() else {}
current={}
poppler='C:/Users/vlade/.cache/codex-runtimes/codex-primary-runtime/dependencies/native/poppler/Library/bin/pdftoppm.exe'
papers=[]
for spec in specs:
    target=root/'static/img/research/sources'/spec['id'];target.mkdir(parents=True,exist_ok=True)
    pdf=root/'static'/spec['pdf'].lstrip('/')
    current[spec['id']]=hashlib.sha256(pdf.read_bytes()).hexdigest()
    pages=sorted(set(spec['references'].values()) | set(extra[spec['id']]) | set(spec['equation_pages'].values()))
    records=[]
    for n in pages:
        prefix=target/f'page-{n}'
        if previous.get(spec['id'])!=current[spec['id']] or not prefix.with_suffix('.png').exists():
            subprocess.run([poppler,'-f',str(n),'-l',str(n),'-singlefile','-scale-to','1800','-png',str(root/'static'/spec['pdf'].lstrip('/')),str(prefix)],check=True,capture_output=True)
        records.append(dict(number=n,image=f'/img/research/sources/{spec["id"]}/page-{n}.png',labels=', '.join(k for k,v in spec['references'].items() if v==n) or 'Economic and econometric specification'))
    papers.append(dict(id=spec['id'],title=metadata[spec['id']]['title'],version=metadata[spec['id']]['version'],pdf=spec['pdf'],pages=records))
    (root/f'content/research/{spec["id"]}-sources.md').write_text(f'+++\ntitle = "Manuscript source pages"\npath = "research/{spec["id"]}/sources"\ntemplate = "source-pages.html"\n[extra]\npaper_id = "{spec["id"]}"\n+++\n',encoding='utf-8')
(root/'data/scientific-pages.json').write_text(json.dumps({'papers':papers},indent=2),encoding='utf-8')
previous_path.write_text(json.dumps(current,indent=2),encoding='utf-8')
print('Rendered',sum(len(p['pages']) for p in papers),'verified physical PDF pages.')
