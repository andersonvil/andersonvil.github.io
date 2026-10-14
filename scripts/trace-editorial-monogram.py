from pathlib import Path
from PIL import Image
import math
root=Path(__file__).resolve().parents[1]
ref=Image.open('C:/dev/andersonvil-academic-os-sources/ChatGPT Image Sep 30, 2026, 07_24_39 PM.png').convert('RGB')
# Trace the approved fourth variant's actual outlines, without a replacement font.
x0,y0=1180,62
pixels=ref.load(); mask=set()
for y in range(126,268):
    for x in range(1209,1425):
        r,g,b=pixels[x,y]
        if min(r,g,b)>140:mask.add((x-x0,y-y0))
edges={}
def edge(a,b):edges.setdefault(a,[]).append(b)
for x,y in mask:
    if (x,y-1) not in mask:edge((x,y),(x+1,y))
    if (x+1,y) not in mask:edge((x+1,y),(x+1,y+1))
    if (x,y+1) not in mask:edge((x+1,y+1),(x,y+1))
    if (x-1,y) not in mask:edge((x,y+1),(x,y))
def simplify(points,tol=1.15):
    if len(points)<3:return points
    ax,ay=points[0];bx,by=points[-1];den=math.hypot(bx-ax,by-ay)
    distances=[abs((by-ay)*x-(bx-ax)*y+bx*ay-by*ax)/den if den else math.hypot(x-ax,y-ay) for x,y in points]
    i=max(range(len(points)),key=lambda i:distances[i])
    if distances[i]>tol:return simplify(points[:i+1],tol)[:-1]+simplify(points[i:],tol)
    return [points[0],points[-1]]
paths=[]
while edges:
    start=next(iter(edges));p=start;loop=[p]
    while True:
        q=edges[p].pop()
        if not edges[p]:del edges[p]
        p=q;loop.append(p)
        if p==start:break
    if len(loop)<12:continue
    # Simplify pixel stairs, then interpolate gentle contour curves locally.
    # Sharp serif corners and the V tip remain sharp; no replacement font is used.
    mid=len(loop)//2;pts=simplify(loop[:mid+1])[:-1]+simplify(loop[mid:])[:-1]
    def sharp(i):
        a=pts[(i-1)%len(pts)];b=pts[i];c=pts[(i+1)%len(pts)]
        u=(b[0]-a[0],b[1]-a[1]);v=(c[0]-b[0],c[1]-b[1])
        den=math.hypot(*u)*math.hypot(*v)
        return not den or (u[0]*v[0]+u[1]*v[1])/den<math.cos(.9)
    corners=[]
    for i,p1 in enumerate(pts):
        p0=pts[(i-1)%len(pts)];p2=pts[(i+1)%len(pts)]
        before=math.dist(p0,p1);after=math.dist(p1,p2)
        radius=0 if sharp(i) else min(1.4,before/4,after/4)
        entry=[p1[k]+(p0[k]-p1[k])*radius/before for k in [0,1]]
        exitp=[p1[k]+(p2[k]-p1[k])*radius/after for k in [0,1]]
        corners.append((entry,p1,exitp))
    commands=[f'M {corners[0][0][0]:.3f} {corners[0][0][1]:.3f}']
    for entry,vertex,exitp in corners:
        commands.append(f'L {entry[0]:.3f} {entry[1]:.3f} Q {vertex[0]} {vertex[1]} {exitp[0]:.3f} {exitp[1]:.3f}')
    paths.append(' '.join(commands)+' Z')
body='<circle cx="136.5" cy="136.5" r="133.2" fill="none" stroke="currentColor" stroke-width="3.8"/>'
body+='<path fill="currentColor" fill-rule="evenodd" d="'+' '.join(paths)+'"/>'
# The reference has an almost invisible A crossbar, retained at its measured position.
body+='<path d="M68 148.5 L109 148.5" fill="none" stroke="currentColor" stroke-opacity=".13" stroke-width="1"/>'
svg='<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 273 273" focusable="false" aria-hidden="true">'+body+'</svg>'
(root/'templates/macros/monogram.html').write_text(svg,encoding='utf-8')
(root/'static/favicon.svg').write_text('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 273 273" style="color:#f7f3ea"><rect width="273" height="273" rx="136.5" fill="#0b1f2c"/>'+body+'</svg>',encoding='utf-8')
(root/'docs/screenshots/final-v5/monogram-comparison.html').write_text('<!doctype html><html lang="en"><meta charset="utf-8"><title>Editorial monogram trace comparison</title><body style="background:#0b1f2c;color:#f7f3ea"><h1>Variant 4 — source and vector trace</h1><img src="/reference-monogram.png" width="768"><div style="width:274px">'+svg+'</div><div style="width:34px">'+svg+'</div></body></html>',encoding='utf-8')
print('Traced',len(paths),'closed glyph contours from the editorial reference.')
