"""Check source fidelity, student packages and preservation of the approved site."""
from pathlib import Path
import argparse, hashlib, json, re, subprocess, tomllib, zipfile
from collections import defaultdict
from bs4 import BeautifulSoup

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT.parent / 'andersonvil-academic-os-sources'
parser = argparse.ArgumentParser()
parser.add_argument('--build-dir', default='public-preview-update')
args = parser.parse_args()
BUILD = ROOT / args.build_dir
manifest = json.loads((ROOT / 'docs/teaching-update-manifest.json').read_text(encoding='utf-8'))
current = tomllib.loads((ROOT / 'data/teaching_resources.toml').read_text(encoding='utf-8-sig'))
original = tomllib.loads(subprocess.check_output(['git', 'show', 'main:data/teaching_resources.toml'], cwd=ROOT).decode('utf-8-sig'))
academic = tomllib.loads((ROOT / 'data/academic.toml').read_text(encoding='utf-8-sig'))
original_academic = tomllib.loads(subprocess.check_output(['git', 'show', 'main:data/academic.toml'], cwd=ROOT).decode('utf-8-sig'))
old_groups = {g['id']: g for g in original['groups']}
groups = {g['id']: g for g in current['groups']}
sha = lambda b: hashlib.sha256(b).hexdigest()
page = lambda url: BeautifulSoup((BUILD / url.strip('/') / 'index.html').read_text(encoding='utf-8'), 'html.parser')

for c in manifest['files']:
    assert sha((SOURCE / c['source']).read_bytes()) == c.get('source_sha256', c['sha256'])
    assert sha((BUILD / c['url'].lstrip('/')).read_bytes()) == c['sha256']

# Full inventory of the three teaching source trees, including supplied test files.
index = defaultdict(list)
for p in (BUILD / 'files/teaching').rglob('*'):
    if p.is_file():
        index[sha(p.read_bytes())].append('/' + p.relative_to(BUILD).as_posix())
inventory = []
for folder in ('CY-Cergy-Paris-Université', 'Saint-Germain-en-Laye', 'Niort'):
    for source in sorted((SOURCE / folder).rglob('*')):
        if not source.is_file():
            continue
        relative = source.relative_to(SOURCE).as_posix()
        matches = index[sha(source.read_bytes())]
        reason = 'Byte-identical public copy'
        if not matches:
            if re.fullmatch(r'TD #[123]\.zip', source.name):
                reason = 'Instructor working archive; student files are synchronized from the approved TD folder, corrected notebooks excluded'
            elif source.name == 'lab_microeconomie3.html':
                target = BUILD / 'files/teaching/niort/microeconomics-iii/qcm/lab_microeconomie3.html'
                published = target.read_text(encoding='utf-8')
                assert re.sub(r'<style id="quiz-mobile-layout">.*?</style>', '', published, flags=re.S) == source.read_text(encoding='utf-8')
                matches = ['/' + target.relative_to(BUILD).as_posix()]
                reason = 'Source preserved, with only a mobile question-text wrapping stylesheet added to the public copy'
            else:
                assert source.name in ('jeux-repetes-simulateur.html', 'pile-ou-face-simulateur.html'), relative
                target = BUILD / 'files/teaching/niort/microeconomics-iii' / source.name
                raw = BeautifulSoup(source.read_text(encoding='utf-8'), 'html.parser')
                wrapped = BeautifulSoup(target.read_text(encoding='utf-8'), 'html.parser')
                assert str(raw.select_one('.wrap')) == str(wrapped.select_one('.wrap'))
                assert [str(x) for x in raw.select('style, script')] == [str(x) for x in wrapped.select('style, script')]
                matches = ['/' + target.relative_to(BUILD).as_posix()]
                reason = 'Approved HTML document wrapper preserved; simulator markup, styles and scripts identical to source'
        inventory.append(dict(source=relative, urls=matches, note=reason))
(ROOT / 'docs/teaching-update-source-inventory.json').write_text(json.dumps(inventory, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
archive_members = 0
for archive in manifest['archives']:
    with zipfile.ZipFile(BUILD / archive['url'].lstrip('/')) as z:
        assert set(z.namelist()) == set(archive['members'])
        assert z.testzip() is None
        for name, digest in archive['members'].items():
            assert sha(z.read(name)) == digest
            archive_members += 1
        if '/l3-data-analysis/' in archive['url']:
            assert not any(re.search(r'corrig|solution|answer', name, re.I) for name in z.namelist())

# Every previously catalogued URL survives; all student-facing originals remain on disk.
preserved = 0
for group_id, old in old_groups.items():
    urls = {r['file'] for r in groups[group_id]['resources']}
    assert {r['file'] for r in old['resources']} <= urls, group_id
    preserved += len(old['resources'])
for name in subprocess.check_output(['git', 'ls-tree', '-r', '--name-only', 'main', 'static/files'], cwd=ROOT).decode().splitlines():
    assert (ROOT / name).is_file(), name

catalogue = page('/code-data/')
for group_id, group in groups.items():
    technical = catalogue.select_one('#' + group_id)
    hrefs = {a['href'] for a in technical.select('a[href]')}
    assert {r['file'] for r in group['resources']} <= hrefs, group_id
    course = page(group['url'])
    assert not course.select('details details'), group_id
    if group_id in ('econometrics-m1', 'econometrics-m2'):
        assert group == old_groups[group_id]
        assert not course.select('.learning-card') and len(course.select('.resource-file-list article')) == 1
        baseline = ROOT / 'public-production' / group['url'].strip('/') / 'index.html'
        if baseline.exists():
            before = BeautifulSoup(baseline.read_text(encoding='utf-8'), 'html.parser')
            assert str(course.select_one('.resource-part')) == str(before.select_one('.resource-part'))
    elif group_id != 'econometrics-l3':
        assert {r['file'] for r in group['resources']} <= {a['href'] for a in course.select('a[href]')}

cy = page('/teaching/econometrics-l3/')
assert [h.get_text() for h in cy.select('.learning-section-title')] == ['Chapter exercises', 'Previous continuous assessments (CC)', 'Teaching datasets']
dataset_root = BUILD / 'files/teaching/cy-cergy/l3-econometrics/data/Dataset'
assert len(cy.select('.dataset-grid article')) == len([p for p in dataset_root.iterdir() if p.is_dir()]) == 15
assert (BUILD / 'files/teaching/cy-cergy/l3-econometrics/data/Dataset.zip').read_bytes() == (ROOT / 'static/files/teaching/cy-cergy/l3-econometrics/data/Dataset.zip').read_bytes()
for a in cy.select('.dataset-grid a'):
    folder = dataset_root / a.parent.h4.get_text()
    with zipfile.ZipFile(BUILD / a['href'].lstrip('/')) as z:
        assert set(z.namelist()) == {p.relative_to(folder).as_posix() for p in folder.rglob('*') if p.is_file()}

for card in page('/teaching/econometrics-niort/').select('.learning-card')[1:]:
    assert [a.span.text for a in card.select('.learning-primary a')] == ['Course support', 'Exercise sheet']
micro_cards = page('/teaching/microeconomics-iii-niort/').select('.learning-card')
part3 = next(card for card in micro_cards if card.h3.text == 'Part III — Risk and information')
assert [Path(a['href']).name for a in part3.select('.learning-primary a')] == ['partie3.pdf', 'serie05.pdf', 'serie06.pdf', 'serie07.pdf', 'serie08.pdf']
part2 = next(card for card in micro_cards if card.h3.text == 'Part II — Strategic decisions and games')
assert [Path(a['href']).name for a in part2.select('.learning-primary a')] == ['sessions-02-04.pdf', 'serie01.pdf']
practice = micro_cards[-1]
assert practice.h3.text == 'Self-assessment — Game theory and risk'
assert Path(practice.select_one('.learning-primary a')['href']).name == 'lab_microeconomie3.html'
tds = page('/teaching/data-analysis-l3/').select('.learning-card')
assert [c.h3.text for c in tds] == [f'TD{n:02}' for n in range(1, 6)]
for n in range(2, 6):
    assert 'presentation.pdf' in tds[n-1].select_one('.learning-primary a')['href']

expected_statuses = ['Revise and resubmit — Journal of the European Economic Association', 'Revise and resubmit — Review of Economics of the Household', 'Submitted — The Economic Journal']
for paper, old, status in zip(academic['papers'], original_academic['papers'], expected_statuses):
    assert paper['status_label'] == status
    assert {k: v for k, v in paper.items() if k != 'status_label'} == {k: v for k, v in old.items() if k != 'status_label'}
    assert status in page(paper['url']).select_one('.editorial-status').text
for route in ('/', '/research/'):
    assert [s.text for s in page(route).select('.editorial-status')] == expected_statuses
assert academic['work_in_progress'] == original_academic['work_in_progress']
cv_links = 0
for p in BUILD.rglob('*.html'):
    doc = BeautifulSoup(p.read_text(encoding='utf-8'), 'html.parser')
    page_cv_links = 0
    for a in doc.select('a[href]'):
        if re.search(r'\bCV\b', a.get_text() + ' ' + a.get('aria-label', '')):
            assert a['href'] == manifest['cv']['url'], (p, a)
            cv_links += 1
            page_cv_links += 1
    if doc.select_one('.site-header'):
        assert page_cv_links >= 2, p
assert cv_links > 0
assert sha((BUILD / manifest['cv']['url'].lstrip('/')).read_bytes()) == manifest['cv']['sha256']
report = dict(result='PASS', inventoried_source_files=len(inventory), synchronized_copies=len(manifest['files']), verified_archives=len(manifest['archives']), archive_members=archive_members, preserved_resource_urls=preserved, cv_access_points=cv_links, unchanged_courses=['econometrics-m1', 'econometrics-m2'], protected_editorial_content=True)
(ROOT / 'tmp/teaching-update-qa').mkdir(parents=True, exist_ok=True)
(ROOT / 'tmp/teaching-update-qa/static-report.json').write_text(json.dumps(report, indent=2), encoding='utf-8')
print(json.dumps(report))
