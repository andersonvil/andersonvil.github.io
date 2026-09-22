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
    'contact\index.html',
    'talks\index.html',
    'preview\index.html',
    '404.html',
    'robots.txt',
    'files\demo\anderson-vil-cv-preview.pdf',
    'files\demo\synthetic-household-project\synthetic_households.json',
    'files\demo\synthetic-household-project\analyze_demo.py',
    'files\demo\synthetic-household-project\demo_notebook.ipynb'
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

$htmlFiles = Get-ChildItem -LiteralPath $root -Recurse -Filter *.html -File
foreach ($file in $htmlFiles) {
    $html = Get-Content -LiteralPath $file.FullName -Raw
    if ($html -notmatch '<meta\s+name="robots"\s+content="noindex, nofollow') {
        $failures.Add("Noindex metadata missing: $($file.FullName.Substring($root.Length + 1))")
    }
    if ($html -match '<html[^>]+lang="(?!en")') {
        $failures.Add("Non-English page language: $($file.FullName.Substring($root.Length + 1))")
    }
    $hrefMatches = [regex]::Matches($html, 'href="(?<url>/[^"#?]*)"', 'IgnoreCase')
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
$menuPattern = 'Home.*Research.*CV.*Teaching.*Code &amp; Data.*Insights.*Contact'
if ($homeHtml -notmatch $menuPattern) { $failures.Add('The primary navigation does not contain the exact seven entries in order.') }

$talks = Get-Content -LiteralPath (Join-Path $root 'talks\index.html') -Raw
if ($homeHtml -match 'href="/talks/"') { $failures.Add('Talks appears in the Home primary navigation or content.') }
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
