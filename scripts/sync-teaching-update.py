"""Synchronize approved sources and derive the two resource views (no deployment)."""
from pathlib import Path
from collections import OrderedDict
import hashlib, json, re, shutil, tomllib, zipfile

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT.parent / 'andersonvil-academic-os-sources'
STATIC = ROOT / 'static'
catalogue = ROOT / 'data/teaching_resources.toml'
teaching = tomllib.loads(catalogue.read_text(encoding='utf-8-sig'))
groups = {g['id']: g for g in teaching['groups']}
copies, archives = [], []

def digest(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()

def copy(src, target):
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, target)
    copies.append(dict(source=src.relative_to(SOURCE).as_posix(), url='/' + target.relative_to(STATIC).as_posix(), sha256=digest(target)))

def bundle(files, target):
    target.parent.mkdir(parents=True, exist_ok=True)
    members = {name: digest(p) for name, p in files}
    if target.exists():
        with zipfile.ZipFile(target) as z:
            if set(z.namelist()) == set(members) and all(hashlib.sha256(z.read(name)).hexdigest() == value for name, value in members.items()):
                archives.append(dict(url='/' + target.relative_to(STATIC).as_posix(), members=members))
                return
    with zipfile.ZipFile(target, 'w', zipfile.ZIP_DEFLATED) as z:
        for name, p in sorted(files):
            z.write(p, name)
            members[name] = digest(p)
    archives.append(dict(url='/' + target.relative_to(STATIC).as_posix(), members=members))

# Microeconomics III: the exercise sheet belongs to Part II; the mixed quiz
# spans game theory and risk and therefore has its own self-assessment section.
micro = groups['microeconomics-iii-niort']
practice_part = 'Self-assessment — Game theory and risk'
if practice_part not in micro['parts']:
    micro['parts'].append(practice_part)
micro_source = SOURCE / 'Niort/2026-27/Microéconomie III'
for relative, url, label, part, fmt, category in [
    ('partie2/td/serie01.pdf', '/files/teaching/niort/microeconomics-iii/part-2/td/serie01.pdf', 'Exercise series 1 — Game theory and competitive strategies', micro['parts'][1], 'PDF', 'exercises'),
    ('QCM/lab_microeconomie3.html', '/files/teaching/niort/microeconomics-iii/qcm/lab_microeconomie3.html', 'Online practice quiz (QCM) — Game theory and risk', practice_part, 'Interactive HTML', 'self_assessment'),
]:
    source = (SOURCE / 'Niort/Mock-Exams/Microéconomie L3' / relative
              if category == 'self_assessment' else micro_source / relative)
    copy(source, STATIC / url.lstrip('/'))
    if category == 'self_assessment':
        # Question la10 includes a long sentence in a no-wrap math span.
        target = STATIC / url.lstrip('/')
        mobile_style = '<style id="quiz-mobile-layout">@media(max-width:520px){.qtext .m{white-space:normal}}</style>'
        target.write_text(source.read_text(encoding='utf-8').replace('</head>', mobile_style + '</head>', 1), encoding='utf-8')
        copies[-1].update(source_sha256=copies[-1]['sha256'], sha256=digest(target), adaptation='Mobile wrapping in question text only; source document unchanged')
    resource = next((r for r in micro['resources'] if r['file'] == url), None)
    if resource is None:
        resource = dict(file=url)
        micro['resources'].append(resource)
    resource.update(label=label, part=part, format=fmt, kind=category, bytes=(STATIC / url.lstrip('/')).stat().st_size)
    if fmt == 'PDF':
        resource['pages'] = 6

niort = groups['econometrics-niort']
part = 'Self-assessment — Sessions 1 and 2'
if part not in niort['parts']:
    niort['parts'].append(part)
url = '/files/teaching/niort/econometrics/qcm/lab_econometrie_seances1-2.html'
copy(SOURCE / 'Niort/Mock-Exams/Econométrie/QCM/lab_econometrie_seances1-2.html', STATIC / url.lstrip('/'))
resource = next((r for r in niort['resources'] if r['file'] == url), None)
if resource is None:
    resource = dict(file=url)
    niort['resources'].append(resource)
resource.update(label='Online practice quiz (QCM) — Simple regression and OLS properties', part=part,
                format='Interactive HTML', kind='self_assessment', bytes=(STATIC / url.lstrip('/')).stat().st_size)

m1 = groups['econometrics-m1']
mock_source = SOURCE / 'CY-Cergy-Paris-Université/Mock-Exams/Econometrics-M1'
mock_files = []
for number in (2, 3):
    part = f'Mock exam {number}'
    if part not in m1['parts']:
        m1['parts'].append(part)
    for suffix, label, pages in [('statement', 'Question paper', 7), ('solutions_v1', 'Solutions', 5)]:
        filename = f'Mock{number}_{suffix}.pdf'
        url = '/files/teaching/cy-cergy/m1-econometrics/mock-exams/' + filename
        target = STATIC / url.lstrip('/')
        copy(mock_source / filename, target)
        mock_files.append((filename, target))
        resource = next((r for r in m1['resources'] if r['file'] == url), None)
        if resource is None:
            resource = dict(file=url)
            m1['resources'].append(resource)
        resource.update(label=f'Mock exam {number} — {label}', part=part, format='PDF', kind='mock_exams', pages=pages, bytes=target.stat().st_size)

m2 = groups['econometrics-m2']
# Refresh this folder as one collection: no stale shared M1 link or obsolete
# version may survive in M2's catalogue or complete download.
from pypdf import PdfReader
m2_source = SOURCE / 'CY-Cergy-Paris-Université/Mock-Exams/Econometrics-M2&PhD'
m2['resources'] = [r for r in m2['resources'] if r.get('kind') != 'mock_exams']
m2['parts'] = [part for part in m2['parts'] if not part.startswith('Mock exam')]
m2_files = []
inputs = []
for source in sorted(p for p in m2_source.rglob('*') if p.is_file()):
    match = re.fullmatch(r'Mock(\d+)_(statement|solutions(?:_v\d+)?)\.pdf', source.name)
    if not match:
        raise ValueError(f'Unclassified M2 mock-exam document: {source}')
    inputs.append((int(match[1]), match[2] != 'statement', source))
for number, solutions, source in sorted(inputs):
    part = f'Mock exam {number}'
    if part not in m2['parts']:
        m2['parts'].append(part)
    url = '/files/teaching/cy-cergy/m2-econometrics/mock-exams/' + source.name
    target = STATIC / url.lstrip('/')
    copy(source, target)
    m2_files.append((source.name, target))
    label = 'Solutions' if solutions else 'Question paper'
    m2['resources'].append(dict(file=url, label=f'Mock exam {number} — {label}', part=part,
                               format='PDF', kind='mock_exams', pages=len(PdfReader(source).pages), bytes=target.stat().st_size))

# Archives use the same public bytes as the individual downloads.
for group, part, url, files, label in [
    (micro, practice_part, '/files/teaching/niort/microeconomics-iii/qcm.zip',
     [('lab_microeconomie3.html', STATIC / 'files/teaching/niort/microeconomics-iii/qcm/lab_microeconomie3.html')], 'Practice quiz — Complete download'),
    (niort, niort['parts'][-1], '/files/teaching/niort/econometrics/qcm.zip',
     [('lab_econometrie_seances1-2.html', STATIC / 'files/teaching/niort/econometrics/qcm/lab_econometrie_seances1-2.html')], 'Practice quiz — Complete download'),
    (m1, 'Mock exams — Complete download', '/files/teaching/cy-cergy/m1-econometrics/mock-exams.zip', mock_files, 'Mock exams 2 and 3 — Question papers and solutions'),
    (m2, 'Mock exams — Complete download', '/files/teaching/cy-cergy/m2-econometrics/mock-exams.zip', m2_files, 'Mock exams — Available question papers and solutions'),
]:
    if part not in group['parts']:
        group['parts'].append(part)
    target = STATIC / url.lstrip('/')
    bundle(files, target)
    resource = next((r for r in group['resources'] if r['file'] == url), None)
    if resource is None:
        resource = dict(file=url)
        group['resources'].append(resource)
    resource.update(label=label, part=part, format='ZIP', kind='bundles', bytes=target.stat().st_size)

g = groups['data-analysis-l3']
sessions = SOURCE / 'CY-Cergy-Paris-Université/2026-27/L3-Data Analysis'
for n, folder in enumerate(sorted(sessions.glob('TD #*')), 1):
    if not folder.is_dir():
        continue
    n = int(folder.name.split('#')[1])
    part = f'TD{n:02}'
    prefix = f'/files/teaching/cy-cergy/l3-data-analysis/td-{n:02}/'
    public_files = []
    for p in sorted(folder.rglob('*')):
        if not p.is_file() or p.suffix.lower() not in {'.pdf', '.ipynb', '.csv', '.html', '.py', '.r', '.dta', '.md', '.txt', '.xlsx', '.xls'}:
            continue
        # Keep the established student-only notebooks; instructor archives are not public inputs.
        if re.search(r'corrig|solution|answer', p.name, re.I):
            continue
        rel = p.relative_to(folder).as_posix()
        url = prefix + rel
        target = STATIC / url.lstrip('/')
        copy(p, target)
        public_files.append((rel, target))
        r = next((r for r in g['resources'] if r['file'] == url), None)
        if r is None:
            label = f'{part} — Presentation slides' if 'presentation' in p.stem else p.name
            r = dict(label=label, file=url, part=part)
            g['resources'].append(r)
        r.update(format={'.pdf': 'PDF', '.ipynb': 'Jupyter notebook', '.csv': 'CSV', '.html': 'Interactive HTML'}.get(p.suffix.lower(), p.suffix[1:].upper()), bytes=p.stat().st_size)
    archive = STATIC / f'files/teaching/cy-cergy/l3-data-analysis/td-{n:02}.zip'
    bundle(public_files, archive)
    next(r for r in g['resources'] if r['file'] == '/' + archive.relative_to(STATIC).as_posix())['bytes'] = archive.stat().st_size

g = groups['econometrics-l3']
g['parts'] = ['Chapter exercises', 'Previous continuous assessments (CC)', 'Teaching datasets']
dataset_root = STATIC / 'files/teaching/cy-cergy/l3-econometrics/data/Dataset'
datasets = []
for folder in sorted(dataset_root.iterdir()):
    if not folder.is_dir():
        continue
    target = dataset_root.parent / 'bundles' / (folder.name + '.zip')
    files = [(p.relative_to(folder).as_posix(), p) for p in folder.rglob('*') if p.is_file()]
    bundle(files, target)
    datasets.append(dict(title=folder.name, formats=' · '.join(sorted({p.suffix[1:].upper() for _, p in files})), file='/' + target.relative_to(STATIC).as_posix(), bytes=target.stat().st_size))

def kind(r):
    if r.get('kind'):
        return r['kind']
    p = r['file'].lower()
    label = r['label'].lower()
    if 'readme' in label:
        return 'instructions'
    if 'supplement' in label or 'reading' in label or 'dictionary' in label or 'notes' in label:
        return 'readings'
    if p.endswith('.zip'):
        return 'bundles'
    if p.endswith('.pdf'):
        if 'enonce' in p or 'exercise' in label or '/td/serie' in p or '/exercises-' in p:
            return 'exercises'
        return 'support'
    if p.endswith('.ipynb'):
        return 'notebooks'
    if p.endswith(('.py', '.r', '.do')):
        return 'code'
    if p.endswith('.html'):
        return 'simulators' if 'simulat' in label or 'simulat' in p else 'instructions'
    return 'data'

def cluster(r):
    p = Path(r['file'])
    k = kind(r)
    if '/Dataset/' in r['file']:
        return p.parent.name
    if k in ('code', 'notebooks'):
        name = re.sub(r'_(python|R)$', '', p.stem)
        return {'seance01':'Course', 'seance02':'Course', 'td01':'Exercises', 'td02':'Exercises', 'partie3':'Course figures and calculations', 'generer_annexes':'Annex-generation utility', 'generer_dta':'Data export utility'}.get(name, r['label'])
    return r['label']

labels = dict(support='Course supports', exercises='Exercise sheets', self_assessment='Self-assessment', mock_exams='Mock exams', readings='Supplements & readings', notebooks='Notebooks', code='Code', data='Data', simulators='Simulators', instructions='Practical instructions', bundles='Complete downloads')
order = list(labels)

def technical_sections(resources):
    sections = []
    for k in order:
        rows = OrderedDict()
        for r in resources:
            if kind(r) == k:
                rows.setdefault(cluster(r), []).append(r)
        if rows:
            sections.append(dict(title=labels[k], kind=k, rows=[dict(title=title, resources=rs) for title, rs in rows.items()]))
    return sections

views = []
for g in teaching['groups']:
    cards, technical = [], []
    for r in g['resources']:
        suffix = Path(r['file']).suffix.lower()
        if suffix in ('.py', '.r', '.do'):
            r['format'] = {'.py': 'Python', '.r': 'R', '.do': 'Stata'}[suffix]
    for part in g['parts']:
        rs = [r for r in g['resources'] if r['part'] == part]
        technical.append(dict(title=part, sections=technical_sections(rs)))
        if g['id'] == 'econometrics-l3':
            if part == 'Chapter exercises':
                cards.extend(dict(title=r['label'], primary=[r], sections=[], heading=part if i == 0 else '') for i, r in enumerate(rs))
            elif part.startswith('Previous'):
                years = OrderedDict()
                for r in rs:
                    years.setdefault(re.search(r'\d{4}-\d{2}', r['label'])[0], []).append(r)
                cards.extend(dict(title='CC ' + year, primary=items, sections=[], heading=part if i == 0 else '') for i, (year, items) in enumerate(sorted(years.items(), reverse=True)))
            continue
        primary = [r for r in rs if kind(r) in ('support', 'exercises', 'self_assessment', 'mock_exams')]
        primary.sort(key=lambda r: (kind(r) != 'support', kind(r) == 'mock_exams' and 'Solutions' in r['label'], r['file']))
        extra = [r for r in rs if kind(r) not in ('support', 'exercises', 'self_assessment', 'mock_exams')]
        title = 'Session 1 — Consumers, producers, and market efficiency' if g['id'] == 'microeconomics-l1' else part
        cards.append(dict(title=title, primary=primary, sections=technical_sections(extra)))
    views.append(dict(id=g['id'], cards=cards, technical=technical, datasets=datasets if g['id'] == 'econometrics-l3' else []))

def scalar(v):
    if isinstance(v, bool): return str(v).lower()
    if isinstance(v, str): return json.dumps(v, ensure_ascii=False)
    if isinstance(v, list): return '[' + ', '.join(scalar(x) for x in v) + ']'
    return str(v)

lines = [f'{k} = {scalar(v)}' for k, v in teaching.items() if k != 'groups']
for g in teaching['groups']:
    lines += ['', '[[groups]]'] + [f'{k} = {scalar(v)}' for k, v in g.items() if k != 'resources']
    for r in g['resources']:
        lines += ['', '[[groups.resources]]'] + [f'{k} = {scalar(v)}' for k, v in r.items()]
catalogue.write_text('\n'.join(lines) + '\n', encoding='utf-8')
(ROOT / 'data/course-resource-views.json').write_text(json.dumps(dict(groups=views), ensure_ascii=False, indent=2) + '\n', encoding='utf-8')

cv_source = SOURCE / '04-cv-teaching/cv-academic.pdf'
cv_target = STATIC / 'files/cv/anderson-vil-cv-provisional.pdf'
copy(cv_source, cv_target)
manifest = dict(files=copies, archives=archives, cv=dict(source=cv_source.relative_to(SOURCE).as_posix(), url='/' + cv_target.relative_to(STATIC).as_posix(), sha256=digest(cv_target)))
(ROOT / 'docs/teaching-update-manifest.json').write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
# Refresh hashes in the original copy ledger for replaced resources, retaining all entries.
ledger_path = ROOT / 'docs/final-v7-resource-manifest.json'
ledger = json.loads(ledger_path.read_text(encoding='utf-8'))
for record in ledger['files']:
    if any(record['url'] == c['url'] for c in copies):
        p = STATIC / record['url'].lstrip('/')
        record.update(sha256=digest(p), bytes=p.stat().st_size)
ledger_path.write_text(json.dumps(ledger, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
print(json.dumps(dict(synchronized=len(copies), verified_archives=len(archives), dataset_bundles=len(datasets), cv_sha256=digest(cv_target))))
