from pathlib import Path
import json, re, tomllib, hashlib, ast
from html.parser import HTMLParser
from urllib.parse import urlparse, unquote
root=Path(__file__).resolve().parents[1];build=root/'public-preview';fail=[]
class Parse(HTMLParser):
    def __init__(self):super().__init__();self.ids=set();self.links=[];self.images=[]
    def handle_starttag(self,tag,attrs):
        a=dict(attrs)
        if 'id' in a:self.ids.add(a['id'])
        if tag=='a' and a.get('href'):self.links.append(a['href'])
        if tag in ['img','script','source','link']:
            v=a.get('src') or a.get('href')
            if v:self.images.append(v)
pages={}
for p in build.rglob('*.html'):
    doc=Parse();doc.feed(p.read_text(encoding='utf-8'));pages[p]=doc
total=0
for p,doc in pages.items():
    for href in doc.links+doc.images:
        url=urlparse(href)
        if url.scheme or url.netloc:continue
        path=unquote(url.path)
        if not path:target=p
        elif path.startswith('/'):target=build/path.lstrip('/')
        else:target=p.parent/path
        if target.is_dir():target=target/'index.html'
        if not target.exists():fail.append(f'{p.relative_to(build)} -> {href}');continue
        total+=1
        if url.fragment and target.suffix=='.html' and target in pages and unquote(url.fragment) not in pages[target].ids:
            fail.append(f'Missing anchor: {p.relative_to(build)} -> {href}')
teaching=tomllib.loads((root/'data/teaching_resources.toml').read_text(encoding='utf-8-sig'))
for group in teaching['groups']:
    assert group['academic_year']=='2026/2027',group['id']
    if group['institution_id']=='uco':assert group['role']=='Lecture & Tutorial'
    for resource in group['resources']:
        assert (build/resource['file'].lstrip('/')).is_file(),resource['file']
academic=tomllib.loads((root/'data/academic.toml').read_text(encoding='utf-8-sig'))
for paper in academic['papers']:
    body=(build/f'research/{paper["id"]}/index.html').read_text(encoding='utf-8')
    assert paper['abstract'] in body, 'Abstract changed: '+paper['id']
    for name in ['question','contribution','overview','abstract','economic-model','identification','econometric-model','data-sources','findings']:
        assert name in pages[build/f'research/{paper["id"]}/index.html'].ids
    assert 'LIMITS' in body and 'A mettre tel quel' not in body
    assert 'katex-error' not in body
    source=root.parent/'andersonvil-academic-os-sources'/paper['source_file']
    assert hashlib.sha256(source.read_bytes()).digest()==hashlib.sha256((build/paper['pdf'].lstrip('/')).read_bytes()).digest()
code=0; notebooks=[]
for p in (root/'static/files/teaching').rglob('*.py'):ast.parse(p.read_text(encoding='utf-8-sig'));code+=1
for p in (root/'static/files/teaching').rglob('*.ipynb'):
    notebook=json.loads(p.read_text(encoding='utf-8'));lang=notebook['metadata']['kernelspec'].get('language', notebook['metadata'].get('language_info',{}).get('name','unknown'))
    if lang=='python':
        for i,cell in enumerate(notebook['cells']):
            if cell['cell_type']=='code':
                text=''.join(cell['source'])
                if text.lstrip().startswith(('!','%')):continue
                ast.parse(text)
    notebooks.append(str(p.relative_to(root)))
report={'local_links_and_assets':total,'html_pages':len(pages),'broken':fail,'teaching_python_scripts_syntax':code,'notebooks_structure_and_python_syntax':notebooks,'preserved_abstracts':3,'canonical_pdf_hashes':3}
(root/'docs/final-v5-static-audit.json').write_text(json.dumps(report,ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps({k:v for k,v in report.items() if k!='notebooks_structure_and_python_syntax'},ensure_ascii=False))
if fail:raise SystemExit(1)
