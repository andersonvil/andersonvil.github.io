param(
    [string]$BuildDirectory = 'public-preview'
)

$ErrorActionPreference = 'Stop'
$root = (Resolve-Path -LiteralPath $BuildDirectory).Path
$failures = [System.Collections.Generic.List[string]]::new()

$requiredFiles = @(
    'index.html',
    'research\index.html',
    'research\household-demand-leisure\index.html',
    'research\children-costs-one-adult-household\index.html',
    'research\children-labor-supply-equations\index.html',
    'teaching\index.html',
    'teaching\applied-economics-lab\index.html',
    'code-data\index.html',
    'insights\index.html',
    'insights\reading-model-based-estimates\index.html',
    'insights\what-household-averages-can-hide\index.html',
    'beyond-economics\index.html',
    'beyond-economics\notes-on-attention\index.html',
    'contact\index.html',
    'talks\index.html',
    'preview\index.html',
    '404.html',
    'robots.txt',
    'files\demo\anderson-vil-cv-preview.pdf',
    'files\demo\synthetic-household-project\synthetic_households.json',
    'files\demo\synthetic-household-project\analyze_demo.py',
    'files\demo\synthetic-household-project\demo_notebook.ipynb',
    'img\VIL_Anderson-01-petit.jpg'
)

foreach ($relative in $requiredFiles) {
    if (-not (Test-Path -LiteralPath (Join-Path $root $relative) -PathType Leaf)) {
        $failures.Add("Missing required output: $relative")
    }
}

foreach ($forbidden in @('about\index.html', 'job-market\index.html', 'fr\_index.html')) {
    if (Test-Path -LiteralPath (Join-Path $root $forbidden)) {
        $failures.Add("Forbidden route exists: $forbidden")
    }
}

$htmlFiles = Get-ChildItem -LiteralPath $root -Recurse -Filter *.html -File | Where-Object { $_.Name -notlike 'google*.html' }
foreach ($file in $htmlFiles) {
    $html = Get-Content -LiteralPath $file.FullName -Raw
    if ($html -notmatch 'name=(?:"robots"|robots)' -or $html -notmatch 'noindex,\s*nofollow') {
        $failures.Add("Noindex metadata missing: $($file.FullName.Substring($root.Length + 1))")
    }
    if ($html -notmatch '<html[^>]+lang=(?:"en"|en)') {
        $failures.Add("Non-English page language: $($file.FullName.Substring($root.Length + 1))")
    }
    $h1Count = [regex]::Matches($html, '<h1(?:\s|>)', 'IgnoreCase').Count
    if ($h1Count -ne 1) { $failures.Add("Expected exactly one h1 in $($file.FullName.Substring($root.Length + 1)); found $h1Count") }
    $hrefMatches = [regex]::Matches($html, 'href=(?:"(?<url>/[^"#?]*)"|(?<url>/[^ >#?]*))', 'IgnoreCase')
    foreach ($match in $hrefMatches) {
        $url = $match.Groups['url'].Value
        if ($url -eq '/') { $target = Join-Path $root 'index.html' }
        elseif ([System.IO.Path]::HasExtension($url)) { $target = Join-Path $root $url.TrimStart('/').Replace('/', '\') }
        else { $target = Join-Path $root ($url.Trim('/').Replace('/', '\') + '\index.html') }
        if (-not (Test-Path -LiteralPath $target -PathType Leaf)) {
            $failures.Add("Broken internal link in $($file.FullName.Substring($root.Length + 1)): $url")
        }
    }
}

$homeHtml = Get-Content -LiteralPath (Join-Path $root 'index.html') -Raw
$menuPattern = 'Home.*Research.*CV.*Teaching.*Code (?:&amp;|&) Data.*Insights.*Beyond Economics.*Contact'
if ($homeHtml -notmatch $menuPattern) { $failures.Add('The primary navigation does not contain the exact eight entries in order.') }

$activePages = @{
    'index.html' = '/'
    'research\index.html' = '/research/'
    'research\household-demand-leisure\index.html' = '/research/'
    'teaching\applied-economics-lab\index.html' = '/teaching/'
    'insights\reading-model-based-estimates\index.html' = '/insights/'
    'beyond-economics\notes-on-attention\index.html' = '/beyond-economics/'
    'contact\index.html' = '/contact/'
}
foreach ($entry in $activePages.GetEnumerator()) {
    $html = Get-Content -LiteralPath (Join-Path $root $entry.Key) -Raw
    $escaped = [regex]::Escape($entry.Value)
    if ($html -notmatch "aria-current=(?:`"page`"|page)[^>]+href=(?:`"$escaped`"|$escaped)") {
        $failures.Add("Active navigation state missing in $($entry.Key) for $($entry.Value)")
    }
}

$researchHtml = Get-Content -LiteralPath (Join-Path $root 'research\index.html') -Raw
if ([regex]::Matches($researchHtml, 'class=(?:"abstract-toggle"|abstract-toggle)', 'IgnoreCase').Count -ne 3) { $failures.Add('Research must expose three abstract disclosures.') }
if ($researchHtml -notmatch 'aria-expanded=(?:"false"|false)' -or $researchHtml -notmatch 'aria-controls=') { $failures.Add('Abstract disclosures are missing required ARIA state.') }

$insightsHtml = Get-Content -LiteralPath (Join-Path $root 'insights\index.html') -Raw
if ([regex]::Matches($insightsHtml, '\d+ min read', 'IgnoreCase').Count -lt 2) { $failures.Add('Insight cards are missing calculated reading time.') }

$contactHtml = Get-Content -LiteralPath (Join-Path $root 'contact\index.html') -Raw
if ($contactHtml -notmatch 'class=(?:"[^"]*copy-email[^"]*"|[^ >]*copy-email[^ >]*)' -or $contactHtml -notmatch 'aria-live=(?:"polite"|polite)') { $failures.Add('Contact copy-email interaction or live feedback is missing.') }

$siteJs = Get-Content -LiteralPath (Join-Path $root 'js\site.js') -Raw
$siteCss = Get-Content -LiteralPath (Join-Path $root 'styles\site.css') -Raw
foreach ($token in @('av-theme', 'localStorage', 'abstract-toggle', 'copy-email')) {
    if ($siteJs -notmatch [regex]::Escape($token)) { $failures.Add("Required JavaScript behavior missing: $token") }
}
if ($homeHtml -notmatch 'prefers-color-scheme') { $failures.Add('Operating-system theme preference bootstrap is missing.') }
if ($siteCss -notmatch ':root\[data-theme="dark"\]' -or $siteCss -notmatch 'prefers-reduced-motion') { $failures.Add('Dark-theme tokens or reduced-motion support are missing.') }

$talks = Get-Content -LiteralPath (Join-Path $root 'talks\index.html') -Raw
if ($homeHtml -match 'href=(?:"/talks/"|/talks/)') { $failures.Add('Talks appears in the Home primary navigation or content.') }
if ($talks -notmatch 'Synthetic demonstration content') { $failures.Add('Talks is not visibly labeled synthetic.') }

$robots = Get-Content -LiteralPath (Join-Path $root 'robots.txt') -Raw
if ($robots -notmatch 'Disallow:\s*/') { $failures.Add('robots.txt does not block the entire preview.') }

if ($failures.Count -gt 0) {
    Write-Host 'SITE VALIDATION: FAIL' -ForegroundColor Red
    $failures | Sort-Object -Unique | ForEach-Object { Write-Host " - $_" }
    exit 1
}

Write-Host "SITE VALIDATION: PASS ($($htmlFiles.Count) HTML files checked)" -ForegroundColor Green
exit 0
