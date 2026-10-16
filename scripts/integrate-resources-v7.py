"""Integrate supplied teaching/media resources; retain a source-to-download manifest."""
from pathlib import Path
import json, shutil, tomllib, re, hashlib, zipfile
ROOT=Path(__file__).resolve().parents[1]
SOURCE=ROOT.parent/'andersonvil-academic-os-sources'
records=[]
def copy(src,dest):
    src=Path(src); dest=ROOT/'static'/str(dest).lstrip('/');dest.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(src,dest)
    records.append({'source':str(src.relative_to(SOURCE)), 'url':'/'+dest.relative_to(ROOT/'static').as_posix(),'sha256':hashlib.sha256(src.read_bytes()).hexdigest(),'bytes':src.stat().st_size})
    return '/'+dest.relative_to(ROOT/'static').as_posix()
def resource(src,dest,label,part):
    url=copy(src,dest)
    fmt={'.pdf':'PDF','.py':'Python','.r':'R','.do':'Stata','.ipynb':'Jupyter notebook','.dta':'Stata data','.csv':'CSV','.html':'Interactive HTML','.zip':'ZIP','.md':'Markdown'}.get(src.suffix.lower(),src.suffix[1:].upper())
    return dict(label=label,file=url,part=part,format=fmt,bytes=src.stat().st_size)
def bundle(src,target):
    target=ROOT/'static'/target;target.parent.mkdir(parents=True,exist_ok=True)
    with zipfile.ZipFile(target,'w',zipfile.ZIP_DEFLATED) as z:
        for p in sorted(src.rglob('*')):
            if p.is_file():z.write(p,p.relative_to(src).as_posix())
    return dict(label='Complete folder — supports, code, data and supporting checks',file='/'+target.relative_to(ROOT/'static').as_posix(),format='ZIP',bytes=target.stat().st_size)

teaching=tomllib.loads((ROOT/'data/teaching_resources.toml').read_text(encoding='utf-8-sig'))
groups={g['id']:g for g in teaching['groups']}
for g in groups.values():
    g.pop('year_note',None);g.pop('institutionalWordingToConfirm',None)
    g['parts']=['Course materials']
    for r in g['resources']:r['part']='Course materials'
groups['microeconomics-l1']['title']='Microeconomics — L1'
groups['microeconomics-iii-niort']['title']='Microeconomics III — L3'
g=groups['econometrics-l3'];g['summary']='Chapter-based exercises, teaching datasets, and previous continuous assessments with supplied answer keys.'
g['parts']=['Chapter exercises','Teaching datasets','Previous continuous assessments (CC)']
for r in g['resources']:r['part']='Chapter exercises'
cy=SOURCE/'CY-Cergy-Paris-Université'
ds=cy/'2026-27/data/Dataset.zip'
g['resources'].append(resource(ds,'files/teaching/cy-cergy/l3-econometrics/data/Dataset.zip','Teaching dataset collection — original formats and dictionaries','Teaching datasets'))
with zipfile.ZipFile(ds) as z:
    for info in z.infolist():
        if info.is_dir():continue
        p=Path(info.filename)
        target=ROOT/'static/files/teaching/cy-cergy/l3-econometrics/data'/p
        target.parent.mkdir(parents=True,exist_ok=True);target.write_bytes(z.read(info))
        dataset=p.parts[1];fmt=p.suffix[1:].upper()
        g['resources'].append(dict(label=f'{dataset} — {p.name}',file='/'+target.relative_to(ROOT/'static').as_posix(),part='Teaching datasets',format=fmt,bytes=info.file_size))
for p in sorted((cy/'Previous-Exams/Previous-CC/L3-Econometrics').glob('*.pdf')):
    year=re.search(r'\d{4}-\d{2}',p.stem)[0]
    kind='Answer key' if p.stem.endswith('Rep') else 'Question paper'
    g['resources'].append(resource(p,f'files/teaching/cy-cergy/l3-econometrics/previous-cc/{p.name}',f'CC {year} — {kind}','Previous continuous assessments (CC)'))

# The new folders are the course/session structure supplied by the author.
g=groups['econometrics-niort'];g['title']='Econometrics — L3'
g['summary']='Session 1 introduces covariance, correlation, descriptive evidence, and simple regression. Session 2 develops sampling, probability, estimator distributions and inference, with course and exercise code in Python, R and Stata, notebooks, data and interactive simulators.'
g['topics']=['Covariance and correlation','Simple regression','Sampling and probability','Estimator distributions and inference','Python, R and Stata','Notebooks and interactive simulation']
g['parts']=['Practical instructions','Session 1 — Covariance, correlation and regression','Session 2 — Sampling, probability and inference']
g['resources']=[dict(label='Practical README',file='/teaching/practical-readme/',format='HTML',part='Practical instructions')]
niort=SOURCE/'Niort/2026-27'
for n,part in zip([1,2],g['parts'][1:]):
    session=niort/f'Econométrie/seance0{n}/seance0{n}'
    for p in sorted(session.rglob('*')):
        if not p.is_file():continue
        rel=p.relative_to(session)
        # All supplied files are copied; test reports remain in the full session archive.
        url=copy(p,f'files/teaching/niort/econometrics/session-0{n}/{rel.as_posix()}')
        if 'tests' in rel.parts:continue
        if p.suffix=='.pdf':label='Course support' if rel.parts[0]=='support' else 'Exercise sheet'
        elif p.suffix=='.dta':label=f'Dataset — {p.stem}'
        elif p.suffix=='.html':label='Interactive covariance simulator' if n==1 else 'Interactive sampling simulator'
        elif p.suffix=='.ipynb':label=('Exercises' if p.stem.startswith('td') else 'Course')+' — '+('R' if '_R' in p.stem else 'Python')+' notebook'
        else:label={'seance': 'Course','td':'Exercises','generer_dta':'Data export utility','generer_annexes':'Annex-generation utility'}.get(next((k for k in ['generer_dta','generer_annexes','seance','td'] if p.stem.startswith(k)),''),p.stem)+' — '+p.suffix[1:]
        g['resources'].append(dict(label=label,file=url,part=part,format={'.pdf':'PDF','.dta':'Stata data','.html':'Interactive HTML','.ipynb':'Jupyter notebook'}.get(p.suffix,p.suffix[1:].upper()),bytes=p.stat().st_size))
    b=bundle(session,f'files/teaching/niort/econometrics/session-0{n}.zip');b['part']=part;g['resources'].append(b)

g=groups['microeconomics-iii-niort'];g['summary']='Microeconomic foundations, strategic decisions and Nash equilibrium, followed by risk, insurance, portfolio choice and private information, with exercises, Python/R programs and interactive simulators.'
g['parts']=['Part I — Foundations','Part II — Strategic decisions and games','Part III — Risk and information']
for r in g['resources']:r['part']=g['parts'][0] if 'session-01' in r['file'] else g['parts'][1]
session=niort/'Microéconomie III/partie3'
for p in sorted(session.rglob('*')):
    if not p.is_file():continue
    rel=p.relative_to(session);url=copy(p,'files/teaching/niort/microeconomics-iii/part-3/'+rel.as_posix())
    if 'tests' in rel.parts:continue
    labels={'partie3':'Part III — Course support (sessions V–VIII)','partie3.py':'Course figures and calculations — Python','partie3.R':'Course figures and calculations — R','generer_annexes':'Annex-generation utility','serie05':'Exercise series 5','serie06':'Exercise series 6','serie07':'Exercise series 7','serie08':'Exercise series 8','supplement_pr':'Supplement — Pindyck & Rubinfeld','supplement_varian':'Supplement — Varian','mutualisation':'Risk pooling simulator','diversification-correlation':'Diversification and correlation simulator','choix-portefeuille':'Portfolio choice simulator','cascade-informationnelle':'Information cascade simulator'}
    g['resources'].append(dict(label=labels.get(p.name,labels.get(p.stem,p.name)),file=url,part=g['parts'][2],format={'.pdf':'PDF','.html':'Interactive HTML'}.get(p.suffix,p.suffix[1:].upper()),bytes=p.stat().st_size))
b=bundle(session,'files/teaching/niort/microeconomics-iii/part-3.zip');b['part']=g['parts'][2];g['resources'].append(b)
g['resources'].append(dict(label='Practical README',file='/teaching/practical-readme/#microeconomics',format='HTML',part=g['parts'][2]))

g=groups['data-analysis-l3'];g['parts']=[f'TD0{n}' for n in range(1,6)]
for r in g['resources']:
    n=re.search(r'td-0(\d)',r['file'])[1];r['part']='TD0'+n
for n in range(1,6):
    session=cy/f'2026-27/L3-Data Analysis/TD #{n}'
    existing={Path(r['file']).name for r in g['resources'] if r['part']==f'TD0{n}'}
    for p in sorted(session.rglob('*')):
        if p.is_file() and p.name not in existing:
            label='Session instructions — HTML' if p.name=='index.html' else ('Additional course notes' if 'notes' in p.name else p.name)
            g['resources'].append(resource(p,f'files/teaching/cy-cergy/l3-data-analysis/td-0{n}/{p.relative_to(session).as_posix()}',label,f'TD0{n}'))
    b=bundle(session,f'files/teaching/cy-cergy/l3-data-analysis/td-0{n}.zip');b['part']=f'TD0{n}';g['resources'].append(b)
g=groups['microeconomics-l1'];p=SOURCE/'Saint-Germain-en-Laye/2026-27/Conférence-de-Méthode-Micro/docs_to_read_micro_St-Germain.zip'
g['resources'].append(resource(p,'files/teaching/sciences-po-saint-germain/microeconomics-l1/readings.zip','Session 1 — Accompanying readings','Course materials'))

def scalar(v):
    if isinstance(v,bool):return str(v).lower()
    if isinstance(v,str):return json.dumps(v,ensure_ascii=False)
    if isinstance(v,list):return '['+', '.join(scalar(x) for x in v)+']'
    return str(v)
lines=[]
for k,v in teaching.items():
    if k!='groups':lines.append(f'{k} = {scalar(v)}')
for g in teaching['groups']:
    lines+=['','[[groups]]']
    lines += [f'{k} = {scalar(v)}' for k,v in g.items() if k!='resources']
    for r in g['resources']:
        lines+=['','[[groups.resources]]']+[f'{k} = {scalar(v)}' for k,v in r.items()]
(ROOT/'data/teaching_resources.toml').write_text('\n'.join(lines)+'\n',encoding='utf-8')
# Keep the author's note and every runnable input; the public package excludes note/.
hom=SOURCE/'03-research/homogeneity'
for folder in ['python','R','data','output']:
    for p in sorted((hom/folder).rglob('*')):
        if p.is_file():copy(p,'files/code/homogeneity/'+p.relative_to(hom).as_posix())
copy(hom/'README.md','files/code/homogeneity/README.md')
for p in (hom/'note/figures').glob('*.png'):copy(p,'img/insights/homogeneity/'+p.name)
# Runnable ZIP includes only the computational workflow and supplied outputs.
target=ROOT/'static/files/code/homogeneity/homogeneity-code.zip'
with zipfile.ZipFile(target,'w',zipfile.ZIP_DEFLATED) as z:
    for folder in ['python','R','data','output']:
        for p in sorted((hom/folder).rglob('*')):
            if p.is_file():z.write(p,p.relative_to(hom).as_posix())
    z.write(hom/'README.md','README.md')
shutil.copy2(hom/'README.md',ROOT/'docs/editorial-sources/homogeneity-readme.md')
(ROOT/'docs/final-v7-resource-manifest.json').write_text(json.dumps({'files':records,'courses':{g['id']:len(g['resources']) for g in teaching['groups']}},ensure_ascii=False,indent=2),encoding='utf-8')
print(json.dumps({'copied_files':len(records),'course_resources':{g['id']:len(g['resources']) for g in teaching['groups']}}))
