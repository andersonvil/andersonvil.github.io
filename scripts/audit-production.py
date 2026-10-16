"""Portable production artifact checks, also run before publishing in CI."""
import argparse, json, re
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urlsplit, unquote
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument('--build-dir', default='public-production')
args = parser.parse_args()
build = (ROOT / args.build_dir).resolve()
assert build.is_relative_to(ROOT) and build.is_dir()
base = 'https://andersonvil.github.io'

class Page(HTMLParser):
    def __init__(self, text):
        super().__init__(convert_charrefs=True)
        self.links, self.ids, self.meta, self.canonical = [], set(), {}, None
        self.feed(text)
    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if 'id' in a: self.ids.add(a['id'])
        if tag in ('a', 'link') and a.get('href'): self.links.append(a['href'])
        if tag in ('img', 'script', 'source', 'audio') and a.get('src'): self.links.append(a['src'])
        if tag == 'meta': self.meta[a.get('name', a.get('property'))] = a.get('content', '')
        if tag == 'link' and a.get('rel') == 'canonical': self.canonical = a.get('href')

pages = {p: Page(p.read_text(encoding='utf-8')) for p in build.rglob('*.html')}
links = 0
for p, doc in pages.items():
    text = p.read_text(encoding='utf-8')
    assert not re.search(r'Private preview|publication-status="blocked"|data-synthetic="true"|localhost|127\.0\.0\.1', text, re.I), p
    if 'site-header' in text:
        expected = base + '/' + p.relative_to(build).as_posix().removesuffix('index.html')
        assert doc.canonical == expected, (p, doc.canonical, expected)
        assert doc.meta.get('robots') == ('noindex, follow' if p.name == '404.html' else 'index, follow'), p
        assert doc.meta.get('og:url') == expected, p
        assert doc.meta.get('og:title') and doc.meta.get('og:description'), p
    for href in doc.links:
        url = urlsplit(href)
        if url.scheme or url.netloc:
            assert not (url.scheme == 'http' and url.netloc == 'andersonvil.github.io'), href
            if url.netloc != 'andersonvil.github.io': continue
        target = build / unquote(url.path).lstrip('/') if url.path.startswith('/') else p.parent / unquote(url.path) if url.path else p
        target = target.resolve()
        if target.is_dir(): target /= 'index.html'
        assert target.is_relative_to(build) and target.is_file(), (p, href)
        if url.fragment and target in pages and not url.fragment.startswith('page='):
            assert unquote(url.fragment) in pages[target].ids, (p, href)
        links += 1
robots = (build / 'robots.txt').read_text()
assert 'Disallow: /' not in robots and base + '/sitemap.xml' in robots
locations = [e.text for e in ET.parse(build / 'sitemap.xml').iter() if e.tag.endswith('}loc')]
assert locations and all(x.startswith(base + '/') and '/preview' not in x for x in locations)
for internal in ('guide-site.md', 'docs', 'scripts', 'data', 'config.toml', 'node_modules', '.github', 'preview', 'files/demo'):
    assert not (build / internal).exists(), internal
assert (build / 'research/index.html').is_file()
assert (build / '404.html').is_file()
print(json.dumps({'production_html': len(pages), 'checked_links': links, 'sitemap_urls': len(locations), 'production_checks': 'PASS'}))
