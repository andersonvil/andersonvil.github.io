"""One-time, reviewable integration of the author's final revision (no publication)."""
from pathlib import Path
import re, json, shutil, tomllib

ROOT = Path(__file__).resolve().parents[1]
SOURCES = ROOT.parent / 'andersonvil-academic-os-sources'

def read(p): return (ROOT/p).read_text(encoding='utf-8-sig').replace('\r\n','\n')
def write(p,t):
    p=ROOT/p; p.parent.mkdir(parents=True,exist_ok=True); p.write_text(t,encoding='utf-8')
def edit(p, pairs):
    t=read(p)
    for a,b in pairs:
        if a not in t: raise ValueError(f'Missing edit anchor in {p}: {a[:100]}')
        t=t.replace(a,b)
    write(p,t)

# Keep the supplied originals alongside the final editable web copy.
for name in ['note_modele.md']:
    shutil.copy2(SOURCES/'03-research'/name,ROOT/'docs/editorial-sources'/name)

edit('docs/editorial-sources/paper_jmp.md',[
 ("Couples' choices reveal what that part is worth: when wages change",'Couples’ choices reveal what that part is worth. When wages change'),
 ("Children's consumption, which is never observed, is then recovered Rothbarth-style. Parents' full leisure is compared with that of childless couples who have the same remaining resources.", 'The paper then infers how much of the family’s spending goes to children by comparing parents’ leisure—including the part of childcare they experience as leisure—with that of comparable childless couples. This comparison assumes that parenthood changes neither adults’ preferences for leisure nor how they share the resources left for themselves. Differences in leisure can then reveal resources devoted to children, rather than changes in tastes or redistribution between spouses. After accounting for the time cost of childcare, the model recovers the monetary cost of children.'),
 ('Decisions are Pareto-efficient (collective model, with the unitary model as a special case).','Decisions are Pareto-efficient.')])
edit('docs/editorial-sources/paper_solo.md',[
 ('Adult clothing is the exception: it is recorded for the person who wears it.','Adult clothing is the exception as it is recorded for the person who wears it.'),
 ('clothing is observed by wearer, and one such good suffices;','adult clothing expenditure is separately recorded, providing the single assignable good needed;'),
 ("One difficulty remains: $\\varphi$ and $\\pi$ enter the same demand equation. Proposition 1 gives sufficient conditions to separate them. The benchmark uses an exclusion restriction: decade indicators shift clothing demand, through fashion, norms and relative prices, without affecting the sharing rule or scale economies. They are relevant shifters in the data (joint tests, $p = 0.034$ for women and $p = 0.011$ for men; Table 1). Independence from expenditure, independence from prices and a multiplicative effect of family size serve as alternative restrictions.", 'One difficulty remains because $\\varphi$ and $\\pi$ enter the same demand equation. Proposition 1 gives sufficient conditions to distinguish them. The benchmark uses decade indicators to shift adult clothing demand through fashion, norms, and relative prices, while excluding these indicators from the sharing rule and the scale-economies equation. These indicators are relevant in the data, with joint tests yielding $p=0.034$ for women and $p=0.011$ for men (Table 1). Alternative specifications impose independence from expenditure, independence from prices, or a multiplicative effect of family size.'),
 ('$$\n\\omega =','**Single adult demand (equation 14).**\n\n$$\n\\omega =')])
edit('docs/editorial-sources/paper_2.md',[
 ('The paper asks what the labor supply counterpart should be.','\n\nThe paper asks what the labor supply counterpart should be.'),
 ('*Illustration caption.* **Two channels into labor supply.** Children cost the mother money and time. Both costs lower the resources she keeps, which raises her hours if leisure is normal; the hours of care displace market work, which lowers them. Her preferences are those of a childless woman. Explanatory diagram, not an empirical result.',''),
 ('In the 2019 PSID, at the median, a $1 raise adds 27 hours of work a year for a childless working woman and 40 hours for a working mother; $1,000 of extra resources removes 28 and 25 hours respectively (Table 4).','**Example.** In the 2019 PSID, at the median, a $1 raise adds 27 hours of work a year for a childless working woman and 40 hours for a working mother; $1,000 of extra resources removes 28 and 25 hours respectively (Table 4).'),
 ('Her preferences are $u_f(h+t,\\,c) + \\lambda(n)\\,u_c(m)$: she is altruistic toward a representative child, and market work and childcare enter her utility as perfect substitutes.','Her preferences are $u_f(h+t,c)+\\lambda(n)u_c(m)$. She is altruistic toward a representative child, and market work and childcare enter her utility as perfect substitutes.'),
 ('She finally supplies labor as a childless woman would, with fewer resources.','She finally supplies labor as a childless woman would with fewer resources.'),
 ('A Brown–Levin–Ulph labor supply with participation, translated by children','A Brown–Levin–Ulph labor supply with participation translated by children'),
 ('The sample has 1,270 women: 987 childless women and 283 mothers.','The sample has 1,270 women, of whom 987 are childless women and 283 are mothers.'),
 ("Net total expenditures are positive for mothers ($9,470) and negative for childless women (−$3,010): mothers' earnings fall short of household spending.", 'Net total expenditures are positive for mothers ($9,470) and negative for childless women (−$3,010), reflecting the fact that mothers’ earnings fall short of household spending.'),
 ('Children steepen the wage response and flatten the income response; one child costs a working mother about $21,000 a year','Children steepen the wage response and flatten the income response. One child costs a working mother about $21,000 a year.'),
 ('The gap comes from the price of time: for non-working mothers','The gap comes from the price of time. For non-working mothers'),
 ('the companion paper relaxes this','[my job market paper relaxes this](/research/household-demand-leisure/)'),
 ('Childcare time is sensitive to the sample: 190 hours in the trimmed sample.','Childcare time is sensitive to the sample, reaching 190 hours in the trimmed sample.')])

for p in (ROOT/'docs/editorial-sources').glob('paper_*.md'):
    t=p.read_text(encoding='utf-8')
    t=re.sub(r'\\hat\s*(\\[a-zA-Z]+|[a-zA-Z])',r'\\hat{\1}',t)
    # Only findings tables use parentheses for standard errors.
    t=t.replace('(s.e. ','(')
    t=re.sub(r'(\|[^\n]+\n\n)(\*\*Main findings)',r'\1Standard errors are reported in parentheses.\n\n\2',t)
    p.write_text(t,encoding='utf-8')

sp=json.loads(read('data/editorial-supplements.json'))
maps=[{'Proposition 1':10,'Proposition 2':13,'Proposition 3':14,'Corollary 4':15,'Proposition 5':19,'Proposition 6':22,'Assumption A.1':14,'A.1':14,'A.2':16,'A.2′':17,'A.3':19}, {'Proposition 1':8}, {'Proposition':8,'Assumption L':8}]
eqmaps=[{**{str(i):29 for i in range(9,14)},'14':30,'15':30,'16':30,'17':31,'18':31}, {**{str(i):11 for i in range(14,19)},'19':13}, {'16':15,'17':15,**{str(i):16 for i in range(18,22)},**{str(i):17 for i in range(22,25)},'25':18}]
for p,refs,eqs in zip(sp['papers'],maps,eqmaps):
    p['references'].update(refs);p['equation_pages']=eqs
    p.pop('symbols',None)
    for r in p['replacements']:
        r['after']=r['after'].replace(' This is A.2′ in the October 4 manuscript, not an acronym inferred from an earlier version.','')
        if p['id']=='children-labor-supply-equations' and r['before']=='**Symbols, units and indices.**':
            r['before']='<!-- QUADRATIC-SPECIFICATION -->';r['after']=r['after'].replace('**Symbols, units and indices.**','')
    if p['id']=='children-labor-supply-equations':
        t=read('docs/editorial-sources/paper_2.md');t=re.sub(r'\*\*Symbols, units and indices\.\*\*[^\n]*','<!-- QUADRATIC-SPECIFICATION -->',t);write('docs/editorial-sources/paper_2.md',t)
write('data/editorial-supplements.json',json.dumps(sp,ensure_ascii=False,indent=2)+'\n')

edit('config.toml',[('Private preview — provisional and synthetic content; not approved for publication','Private preview')])
edit('templates/base.html',[
 ('    <span>No content on this build is approved for publication.</span>',''),
 ('aria-label="CV — provisional PDF, not approved for publication"','aria-label="CV — PDF"'),
 ('Private preview · noindex · provisional and synthetic items are blocked from production.','Private preview')])
edit('templates/macros/contextual-return.html',[
 ('<div class="contextual-return-host shell" data-contextual-return-host hidden></div>','<div class="contextual-return-host shell"><a class="contextual-return-link" href="/">← Back to Home</a><nav class="breadcrumb" aria-label="Breadcrumb"><a href="/">Home</a></nav></div>')])
# Main pages have a persistent Home return. Secondary pages retain a parent return.
for name in ['home.html','research.html','teaching-v7.html','contact-v7.html']:
    t=read('templates/'+name)
    t=re.sub(r'<p class="preview-label[^"\n]*">.*?</p>','',t)
    t=re.sub(r'<aside class="shell compact-notice"[\s\S]*?</aside>','',t)
    t=t.replace('Selected research','Research').replace('Selected Research','Research').replace('provisional PDF','PDF')
    t=t.replace('Courses and provisional teaching resources','Courses and teaching resources')
    t=t.replace('Status pending verification','Working paper')
    write('templates/'+name,t)

# New original manuscripts and the validated CV, with stable public filenames.
academic=read('data/academic.toml')
academic=re.sub(r'^title_note = .*\n','',academic,flags=re.M)
academic=academic.replace('version = "September 4, 2026"','version = "October 16, 2026"')
academic=academic.replace('CV — provisional PDF','CV — PDF')
academic=academic.replace('Benchmark sample — mean (standard deviation), 2019 PSID (manuscript Table 2).','Benchmark sample, mean (standard deviation), PSID 2019 (hours/week; dollars; education and age in years)')
academic=academic.replace('Selected means from the harmonized UK sample (manuscript Table 2; 1987 pounds).','Selected means from the harmonized UK sample (1987 pounds/week)')
academic=academic.replace('Selected means, 2019 PSID (manuscript Table 1).','Selected means, PSID 2019 (annual hours; dollars/hour; net expenditure in $000/year)')
write('data/academic.toml',academic)
ad=tomllib.loads(academic)
for p in ad['papers']:shutil.copy2(SOURCES/p['source_file'],ROOT/'static'/p['pdf'].lstrip('/'))
shutil.copy2(SOURCES/'04-cv-teaching/cv-academic.pdf',ROOT/'static/files/cv/anderson-vil-cv-provisional.pdf')
(ROOT/'static/img/presentation').mkdir(exist_ok=True)
shutil.copy2(SOURCES/'05-media-links/p7.jpg',ROOT/'static/img/presentation/michigan-jmp.jpg')

# Archive the former synthetic teaching note outside Zola's content/static trees.
archive=ROOT/'docs/archive/pre-final-v7';archive.mkdir(parents=True,exist_ok=True)
for p in (ROOT/'content/preview-synthetic').rglob('*.md'):
    dest=archive/p.relative_to(ROOT/'content/preview-synthetic');dest.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(p,dest)
    # Retain a real historical redirect, disable all demonstration pages and labs.
    t=p.read_text(encoding='utf-8')
    if p.name!='reading-model-based-estimates.md' and '\nrender = false' not in t:t=t.replace('+++','+++\nrender = false',1)
    p.write_text(t,encoding='utf-8')
# Remove fixture downloads from the generated site, preserving their complete source.
demo=ROOT/'static/files/demo'
if demo.exists():shutil.move(str(demo),str(archive/'demo-files'))
print('Applied final editorial replacements, retained sources and private-preview protection.')
