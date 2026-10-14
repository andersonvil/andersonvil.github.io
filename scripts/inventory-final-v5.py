from pathlib import Path
import hashlib, json, zipfile
from pypdf import PdfReader
import xml.etree.ElementTree as ET

root = Path(__file__).resolve().parents[1]
sources = Path('C:/dev/andersonvil-academic-os-sources')
out = root / 'tmp/final-v5'
out.mkdir(parents=True, exist_ok=True)
inventory = []
for p in sorted(sources.rglob('*')):
    if not p.is_file() or p.name.startswith('~$'): continue
    record = dict(path=str(p), bytes=p.stat().st_size, sha256=hashlib.sha256(p.read_bytes()).hexdigest())
    if p.suffix == '.pdf':
        pdf = PdfReader(p)
        record['pages'] = len(pdf.pages)
        pages = [page.extract_text(extraction_mode='layout') for page in pdf.pages]
        record['opening'] = pages[0][:1600]
        target = out / (p.stem + '.txt')
        target.write_text('\n'.join(f'\n=== PDF PAGE {i+1} ===\n{t}' for i,t in enumerate(pages)), encoding='utf-8')
    elif p.suffix == '.odt':
        with zipfile.ZipFile(p) as z:
            doc = ET.fromstring(z.read('content.xml'))
            paragraphs = [''.join(e.itertext()) for e in doc.iter() if e.tag.endswith('}p') or e.tag.endswith('}h')]
        (out / (p.stem+'.txt')).write_text('\n'.join(paragraphs), encoding='utf-8')
    inventory.append(record)
(root / 'docs/final-v5-source-inventory.json').write_text(json.dumps(inventory, ensure_ascii=False, indent=2), encoding='utf-8')
print('Inventoried',len(inventory),'files; extracted PDF and ODT sources.')
for r in inventory:
    if '03-research' in r['path'] and 'opening' in r: print(r['path'], '\n',r['opening'][:900])
