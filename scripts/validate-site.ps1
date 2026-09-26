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
    'teaching\econometrics-l3\index.html',
    'teaching\econometrics-m1\index.html',
    'teaching\econometrics-m2\index.html',
    'teaching\microeconomics-l1\index.html',
    'teaching\microeconomics-iii-niort\index.html',
    'teaching\applied-economics-lab\index.html',
    'code-data\index.html',
    'insights\index.html',
    'insights\reading-model-based-estimates\index.html',
    'insights\what-household-averages-can-hide\index.html',
    'beyond-economics\index.html',
    'beyond-economics\calligraphie-de-bonheur\index.html',
    'beyond-economics\ma-cherie\index.html',
    'beyond-economics\mes-nuits\index.html',
    'beyond-economics\on-me-l-a-interdit\index.html',
    'beyond-economics\on-reading-preferences\index.html',
    'contact\index.html',
    'talks\index.html',
    'preview\index.html',
    'preview\design-lab\research-visuals\index.html',
    'preview\design-lab\poetry-typography\index.html',
    'preview\design-lab\poetry-audio\index.html',
    '404.html',
    'robots.txt',
    'files\cv\anderson-vil-cv-provisional.pdf',
    'files\papers\household-demand-leisure.pdf',
    'files\papers\children-costs-one-adult-household.pdf',
    'files\papers\children-labor-supply-equations.pdf',
    'files\slides\household-demand-leisure-eea-2026.pdf',
    'files\teaching\cy-cergy\l3-econometrics\exercises-01.pdf',
    'files\teaching\cy-cergy\m1-econometrics\econometrics-exercises.pdf',
    'files\teaching\cy-cergy\m2-econometrics\econometrics-exercises.pdf',
    'files\teaching\sciences-po-saint-germain\microeconomics-l1\session-01.pdf',
    'files\teaching\niort\microeconomics-iii\session-01.pdf',
    'files\teaching\niort\microeconomics-iii\sessions-02-04.pdf',
    'files\media\on-reading-preferences.m4a',
    'files\demo\synthetic-household-project\synthetic_households.json',
    'files\demo\synthetic-household-project\analyze_demo.py',
    'files\demo\synthetic-household-project\demo_notebook.ipynb',
    'img\VIL_Anderson-01-petit.jpg',
    'img\podcast-reading-preferences-poster.jpg',
    'img\poetry\calligraphie-de-bonheur.webp',
    'img\poetry\ma-cherie.webp',
    'img\poetry\mes-nuits.webp',
    'img\poetry\on-me-l-a-interdit.webp'
    'img\research\identification\slide-10.svg'
    'img\research\identification\slide-11.svg'
    'files\licenses\poetry-audio-candidates.txt'
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
    $html = Get-Content -Encoding UTF8 -LiteralPath $file.FullName -Raw
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

$homeHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'index.html') -Raw
$menuPattern = 'Home.*Research.*CV.*Teaching.*Code (?:&amp;|&) Data.*Insights.*Beyond Economics.*Contact'
if ($homeHtml -notmatch $menuPattern) { $failures.Add('The primary navigation does not contain the exact eight entries in order.') }

$activePages = @{
    'index.html' = '/'
    'research\index.html' = '/research/'
    'research\household-demand-leisure\index.html' = '/research/'
    'teaching\applied-economics-lab\index.html' = '/teaching/'
    'insights\reading-model-based-estimates\index.html' = '/insights/'
    'beyond-economics\calligraphie-de-bonheur\index.html' = '/beyond-economics/'
    'beyond-economics\ma-cherie\index.html' = '/beyond-economics/'
    'beyond-economics\on-reading-preferences\index.html' = '/beyond-economics/'
    'contact\index.html' = '/contact/'
}
foreach ($entry in $activePages.GetEnumerator()) {
    $html = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root $entry.Key) -Raw
    $escaped = [regex]::Escape($entry.Value)
    if ($html -notmatch "aria-current=(?:`"page`"|page)[^>]+href=(?:`"$escaped`"|$escaped)") {
        $failures.Add("Active navigation state missing in $($entry.Key) for $($entry.Value)")
    }
}

$researchHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'research\index.html') -Raw
if ([regex]::Matches($researchHtml, 'class=(?:"abstract-toggle"|abstract-toggle)', 'IgnoreCase').Count -ne 3) { $failures.Add('Research must expose three abstract disclosures.') }
if ($researchHtml -notmatch 'aria-expanded=(?:"false"|false)' -or $researchHtml -notmatch 'aria-controls=') { $failures.Add('Abstract disclosures are missing required ARIA state.') }

$bioParagraphs = @(
    'I am a household and labor economist studying how families allocate money and time, and how children and caregiving reshape consumption, labor supply, leisure, and the distribution of resources within households.',
    'I develop and estimate structural models using applied microeconometric methods, expenditure data, and time-use data to recover economic quantities that household surveys do not directly reveal.',
    'I hold a Ph.D. in Economics from CY Cergy Paris Université, where I was supervised by Olivier Donni. In 2023, I was a visiting researcher at KU Leuven, hosted by Frederic Vermeulen.'
)
foreach ($paragraph in $bioParagraphs) {
    if (-not $homeHtml.Contains($paragraph)) { $failures.Add("Exact biography paragraph missing: $paragraph") }
}
if ($homeHtml -notmatch 'portrait-identity[^>]*>\s*<strong>Anderson Vil</strong>') { $failures.Add('Portrait caption name is missing.') }

foreach ($paperPath in @('research\household-demand-leisure\index.html', 'research\children-costs-one-adult-household\index.html', 'research\children-labor-supply-equations\index.html')) {
    $paperHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root $paperPath) -Raw
    if ($paperHtml -notmatch 'Real manuscript.*provisional private preview.*publication blocked') { $failures.Add("Real/provisional paper status missing: $paperPath") }
    if ($paperHtml -match 'Download synthetic paper|Synthetic paper preview') { $failures.Add("Synthetic paper label remains: $paperPath") }
    if ($paperHtml -notmatch 'Data sources and access' -or $paperHtml -notmatch 'Official provider site') { $failures.Add("Structured data access section missing: $paperPath") }
    if ($paperHtml -match 'motivation-visual|research-motivation-svg') { $failures.Add("Unapproved motivation visual appears on normal paper page: $paperPath") }
    if ($paperHtml -notmatch 'Back to Research') { $failures.Add("Persistent context link missing: $paperPath") }
}
$jmpHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'research\household-demand-leisure\index.html') -Raw
foreach ($token in @('Download slides', 'How the model is identified', 'slide-10.svg', 'slide-11.svg', 'Accessible text equivalent: time cost', 'Accessible text equivalent: monetary cost', '<math', 'ER66001–ER71570')) {
    if ($jmpHtml -notmatch [regex]::Escape($token)) { $failures.Add("JMP complement content missing: $token") }
}
foreach ($forbiddenLabel in @('Proposition 3', 'Further', 'Engel curve')) {
    if ($jmpHtml -match [regex]::Escape($forbiddenLabel)) { $failures.Add("Removed slide-control label remains visible in JMP HTML: $forbiddenLabel") }
}
foreach ($nonJmp in @('research\children-costs-one-adult-household\index.html', 'research\children-labor-supply-equations\index.html')) {
    $nonJmpHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root $nonJmp) -Raw
    if ($nonJmpHtml -match 'Download slides') { $failures.Add("Slides CTA must not appear on $nonJmp") }
}
$abstractEvidence = @(
    'We find that mothers perceive 44% of their childcare time as labor, compared to 35% for fathers.',
    'the results find no evidence of per-child resource dilution by family size once estimation uncertainty is accounted for',
    'Results suggest labor supply responsiveness to wages is higher for women with children, while the opposite holds for exogenous incomes.'
)
foreach ($evidence in $abstractEvidence) {
    if (-not $homeHtml.Contains($evidence)) { $failures.Add("Exact PDF abstract evidence missing from Home: $evidence") }
    if (-not $researchHtml.Contains($evidence)) { $failures.Add("Exact PDF abstract evidence missing from Research: $evidence") }
}
if (-not $researchHtml.Contains('Olivier Donni and Anderson Vil') -or -not $researchHtml.Contains('class=byline>Anderson Vil')) { $failures.Add('Paper author order or sole-author line is missing from Research.') }

$teachingHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'teaching\index.html') -Raw
foreach ($title in @('Econometrics — L3', 'Econometrics — M1', 'Econometrics — M2 & PhD', 'Microeconomics — Undergraduate \(L1\)', 'Microeconomics III — Undergraduate \(L3\), Niort')) {
    if ($teachingHtml -notmatch $title) { $failures.Add("Teaching group missing: $title") }
}
if ($teachingHtml -notmatch 'Academic year to confirm' -or $teachingHtml -notmatch '2026–2027') { $failures.Add('Teaching year conflict is not represented explicitly.') }
$niortHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'teaching\microeconomics-iii-niort\index.html') -Raw
if ($niortHtml -notmatch 'institutional wording to confirm' -or $niortHtml -notmatch 'French-language material') { $failures.Add('Niort institutional conflict or language label is missing.') }

$fullPoem = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'beyond-economics\calligraphie-de-bonheur\index.html') -Raw
if ([regex]::Matches($fullPoem, 'class=(?:"poem-stanza"|poem-stanza)', 'IgnoreCase').Count -ne 2) { $failures.Add('Calligraphie de bonheur must contain exactly two complete stanzas.') }
if ([regex]::Matches($fullPoem, 'class=(?:"poem-line"|poem-line)', 'IgnoreCase').Count -ne 9) { $failures.Add('Calligraphie de bonheur must contain exactly nine explicit verse lines.') }
if ($fullPoem -notmatch 'Anderson, 19/10/2023') { $failures.Add('Calligraphie de bonheur source signature is missing.') }
if ($fullPoem -notmatch 'Enter immersive reading' -or $fullPoem -notmatch 'immersive-reader') { $failures.Add('Immersive reading is missing for Calligraphie de bonheur.') }

$restrictedPoems = @{
    'ma-cherie' = @{ Lines = 7; Signature = 'Écrit par Anderson le 03 mars 2023\.' }
    'mes-nuits' = @{ Lines = 6; Signature = 'Écrit par Anderson le 04 mai 2024\.' }
    'on-me-l-a-interdit' = @{ Lines = 5; Signature = 'Écrit par Anderson le 22 août 2026 à 4h30 du matin\.' }
}
foreach ($slug in $restrictedPoems.Keys) {
    $poemHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root "beyond-economics\$slug\index.html") -Raw
    if ([regex]::Matches($poemHtml, 'class=(?:"poem-stanza"|poem-stanza)', 'IgnoreCase').Count -ne 1) { $failures.Add("Restricted poem must contain exactly one stanza: $slug") }
    if ([regex]::Matches($poemHtml, 'class=(?:"poem-line"|poem-line)', 'IgnoreCase').Count -ne $restrictedPoems[$slug].Lines) { $failures.Add("Restricted poem has incorrect explicit verse-line count: $slug") }
    if ($poemHtml -notmatch $restrictedPoems[$slug].Signature) { $failures.Add("Source signature missing: $slug") }
    if ($poemHtml -notmatch 'First stanza — full text available on request\.' -or $poemHtml -notmatch 'Request access to the full poem') { $failures.Add("Request-access path missing: $slug") }
    if ($poemHtml -match 'href=(?:"[^\"]*(?:ma_cherie|mes_nuits|on_me_l_a_interdit)[^\"]*\.pdf|[^ >]*(?:ma_cherie|mes_nuits|on_me_l_a_interdit)[^ >]*\.pdf)') { $failures.Add("Restricted poem exposes a complete-poem PDF link: $slug") }
    if ($poemHtml -match 'immersive-reader') { $failures.Add("Restricted poem unexpectedly contains immersive extra content: $slug") }
}

foreach ($slug in @('calligraphie-de-bonheur', 'ma-cherie', 'mes-nuits', 'on-me-l-a-interdit')) {
    $poemHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root "beyond-economics\$slug\index.html") -Raw
    if ($poemHtml -notmatch 'poem-canvas' -or $poemHtml -notmatch "/img/poetry/$slug.webp") { $failures.Add("Source-faithful background missing: $slug") }
    if ($poemHtml -notmatch 'Play music · selection pending' -or $poemHtml -notmatch 'disabled' -or $poemHtml -match '<audio|<source') { $failures.Add("Unselected poem-music control state is invalid: $slug") }
    if ($poemHtml -notmatch 'Back to Beyond Economics') { $failures.Add("Context navigation missing on poem: $slug") }
}

$visualLab = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'preview\design-lab\research-visuals\index.html') -Raw
if ([regex]::Matches($visualLab, 'class=(?:"[^"]*candidate-card[^"]*"|[^ >]*candidate-card[^ >]*)', 'IgnoreCase').Count -ne 9) { $failures.Add('Research visual lab must contain exactly nine candidates.') }
foreach ($direction in @('Evidence-led', 'Human context', 'Editorial concept')) { if ([regex]::Matches($visualLab, "Candidate \d · $([regex]::Escape($direction))").Count -ne 3) { $failures.Add("Research visual direction must appear once per paper: $direction") } }
if ($visualLab -match 'selected provisionally|candidate-card selected') { $failures.Add('Research visual lab must not preselect a candidate.') }
if ([regex]::Matches($visualLab, 'Light · paper-column size').Count -ne 9 -or [regex]::Matches($visualLab, 'Dark · paper-column size').Count -ne 9) { $failures.Add('Research candidates must be shown at paper-column size in both themes.') }
if ([regex]::Matches($visualLab, 'Editorial recommendation:').Count -ne 3) { $failures.Add('Research visual lab must include one non-binding recommendation per paper.') }
$typeLab = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'preview\design-lab\poetry-typography\index.html') -Raw
if ([regex]::Matches($typeLab, 'data-type-sample=', 'IgnoreCase').Count -ne 12 -or [regex]::Matches($typeLab, 'data-typography-select=', 'IgnoreCase').Count -ne 4) { $failures.Add('Poetry lab must contain three variants for each of four poems.') }
if ($typeLab -notmatch 'no font file is bundled' -or $typeLab -notmatch 'Literary serif — recommended') { $failures.Add('Typography licensing or recommendation is missing.') }
$audioLab = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'preview\design-lab\poetry-audio\index.html') -Raw
if ([regex]::Matches($audioLab, 'class=(?:"[^"]*candidate-card[^"]*"|[^ >]*candidate-card[^ >]*)', 'IgnoreCase').Count -ne 3) { $failures.Add('Poetry audio lab must contain exactly three candidates.') }
if ($audioLab -match '<audio|autoplay|<source') { $failures.Add('Poetry audio lab must not load or embed unapproved audio.') }
if ($audioLab -notmatch 'CC0 1\.0 Universal' -or $audioLab -notmatch 'No track is selected') { $failures.Add('Poetry audio licensing or approval gate is missing.') }
if ($audioLab -notmatch 'Composer credit' -or $audioLab -notmatch 'Performer credit' -or $audioLab -notmatch 'Fit by poem' -or $audioLab -notmatch 'Rights status') { $failures.Add('Poetry audio credit, fit, or rights detail is missing.') }

$restrictedPdfNames = @('ma_cherie_revision_3_J2.pdf', 'mes_nuits_revision_3_B3.pdf', 'on_me_l_a_interdit_revision_3_E4.pdf')
foreach ($name in $restrictedPdfNames) {
    if (Get-ChildItem -LiteralPath $root -Recurse -File | Where-Object { $_.Name -eq $name }) { $failures.Add("Restricted complete PDF present in build: $name") }
}

$mediaHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'beyond-economics\on-reading-preferences\index.html') -Raw
if ($mediaHtml -notmatch '<audio\s+controls\s+preload=(?:"metadata"|metadata)' -or $mediaHtml -match '\sautoplay(?:\s|=|>)') { $failures.Add('Podcast player controls/preload/autoplay state is invalid.') }
if ($mediaHtml -notmatch 'Transcript pending review') { $failures.Add('Podcast transcript review status is missing.') }

$insightsHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'insights\index.html') -Raw
if ([regex]::Matches($insightsHtml, '\d+ min read', 'IgnoreCase').Count -lt 2) { $failures.Add('Insight cards are missing calculated reading time.') }

$contactHtml = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'contact\index.html') -Raw
if ($contactHtml -notmatch 'class=(?:"[^"]*copy-email[^"]*"|[^ >]*copy-email[^ >]*)' -or $contactHtml -notmatch 'aria-live=(?:"polite"|polite)') { $failures.Add('Contact copy-email interaction or live feedback is missing.') }

$siteJs = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'js\site.js') -Raw
$siteCss = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'styles\site.css') -Raw
foreach ($token in @('av-theme', 'localStorage', 'abstract-toggle', 'copy-email', 'data-typography-select')) {
    if ($siteJs -notmatch [regex]::Escape($token)) { $failures.Add("Required JavaScript behavior missing: $token") }
}
if ($homeHtml -notmatch 'prefers-color-scheme') { $failures.Add('Operating-system theme preference bootstrap is missing.') }
if ($siteCss -notmatch ':root\[data-theme="dark"\]' -or $siteCss -notmatch 'prefers-reduced-motion') { $failures.Add('Dark-theme tokens or reduced-motion support are missing.') }
if ($siteCss -notmatch '\.poem-line[^\{]*\{[^\}]*white-space:\s*nowrap' -or $siteCss -notmatch 'overflow-x:\s*auto') { $failures.Add('Poetry anti-wrap and local-overflow rules are missing.') }

$talks = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'talks\index.html') -Raw
if ($homeHtml -match 'href=(?:"/talks/"|/talks/)') { $failures.Add('Talks appears in the Home primary navigation or content.') }
if ($talks -notmatch 'Synthetic demonstration content') { $failures.Add('Talks is not visibly labeled synthetic.') }

$robots = Get-Content -Encoding UTF8 -LiteralPath (Join-Path $root 'robots.txt') -Raw
if ($robots -notmatch 'Disallow:\s*/') { $failures.Add('robots.txt does not block the entire preview.') }

if ($failures.Count -gt 0) {
    Write-Host 'SITE VALIDATION: FAIL' -ForegroundColor Red
    $failures | Sort-Object -Unique | ForEach-Object { Write-Host " - $_" }
    exit 1
}

Write-Host "SITE VALIDATION: PASS ($($htmlFiles.Count) HTML files checked)" -ForegroundColor Green
exit 0
