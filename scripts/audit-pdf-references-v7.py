"""Verify that each referenced label exists on its stated physical PDF page."""
from pathlib import Path
from pypdf import PdfReader
import json,re
root=Path(__file__).resolve().parents[1]
specs=json.loads((root/'data/editorial-supplements.json').read_text(encoding='utf-8'))['papers']
records=[];failed=[]
for spec in specs:
    reader=PdfReader(root/'static'/spec['pdf'].lstrip('/'))
    texts=[re.sub(r'\s+','',p.extract_text()).lower() for p in reader.pages]
    for label,n in spec['references'].items():
        key='Assumption'+label if label in ['A.1','A.2','A.2′','A.3'] else label
        present=re.sub(r'\s+','',key).lower() in texts[n-1]
        records.append(dict(paper=spec['id'],label=label,physical_page=n,found_in_page=present))
        if not present:failed.append((spec['id'],label,n))
    for label,n in spec['equation_pages'].items():
        present='('+label+')' in texts[n-1]
        records.append(dict(paper=spec['id'],equation=label,physical_page=n,found_in_page=present))
        if not present:failed.append((spec['id'],'equation '+label,n))
(root/'docs/final-v7-pdf-reference-audit.json').write_text(json.dumps(dict(targets=records,failures=failed),ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps(dict(checked=len(records),failures=failed),ensure_ascii=False))
if failed:raise SystemExit(1)
