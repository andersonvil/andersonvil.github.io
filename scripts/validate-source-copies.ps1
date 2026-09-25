param(
    [string]$SourceRoot = 'C:\dev\andersonvil-academic-os-sources'
)

$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
if (-not (Test-Path -LiteralPath $SourceRoot -PathType Container)) {
    Write-Host "SOURCE COPY VALIDATION: SKIP (external source folder unavailable: $SourceRoot)" -ForegroundColor Yellow
    exit 0
}

$pairs = @(
    @('03-research\paper-1.pdf', 'static\files\papers\children-costs-one-adult-household.pdf'),
    @('03-research\paper-2.pdf', 'static\files\papers\children-labor-supply-equations.pdf'),
    @('03-research\paper-3.pdf', 'static\files\papers\household-demand-leisure.pdf'),
    @('04-cv-teaching\cv-academic.pdf', 'static\files\cv\anderson-vil-cv-provisional.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_1.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-01.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_2.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-02.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_3.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-03.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_4.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-04.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_5.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-05.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_6.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-06.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_7.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-07.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\L3-Econometrics\Exercices_8.pdf', 'static\files\teaching\cy-cergy\l3-econometrics\exercises-08.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\M1-Econometrics\Exercises_M1_2025.pdf', 'static\files\teaching\cy-cergy\m1-econometrics\econometrics-exercises.pdf'),
    @('CY-Cergy-Paris-Université\2026-27\M2-Econometrics\Exercises_M2_2025-26.pdf', 'static\files\teaching\cy-cergy\m2-econometrics\econometrics-exercises.pdf'),
    @('Saint-Germain-en-Laye\2026-27\Conférence-de-Méthode-Micro\seance1_microeconomie_st-Germain.pdf', 'static\files\teaching\sciences-po-saint-germain\microeconomics-l1\session-01.pdf')
)

$failures = [System.Collections.Generic.List[string]]::new()
foreach ($pair in $pairs) {
    $source = Join-Path $SourceRoot $pair[0]
    $copy = Join-Path $repo $pair[1]
    if (-not (Test-Path -LiteralPath $source -PathType Leaf)) { $failures.Add("Missing external source: $($pair[0])"); continue }
    if (-not (Test-Path -LiteralPath $copy -PathType Leaf)) { $failures.Add("Missing repository copy: $($pair[1])"); continue }
    $sourceHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $source).Hash
    $copyHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $copy).Hash
    if ($sourceHash -ne $copyHash) { $failures.Add("Copy differs from source: $($pair[1])") }
}

$restrictedNames = @('ma_cherie_revision_3_J2.pdf', 'mes_nuits_revision_3_B3.pdf', 'on_me_l_a_interdit_revision_3_E4.pdf')
foreach ($name in $restrictedNames) {
    if (Get-ChildItem -LiteralPath $repo -Recurse -File | Where-Object { $_.Name -eq $name }) {
        $failures.Add("Restricted complete poem PDF exists inside the repository: $name")
    }
}

if ($failures.Count -gt 0) {
    Write-Host 'SOURCE COPY VALIDATION: FAIL' -ForegroundColor Red
    $failures | ForEach-Object { Write-Host " - $_" }
    exit 1
}

Write-Host "SOURCE COPY VALIDATION: PASS ($($pairs.Count) byte-identical provisional copies; restricted PDFs absent)" -ForegroundColor Green
exit 0
