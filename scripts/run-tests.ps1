$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path

& (Join-Path $PSScriptRoot 'build-preview.ps1')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

& (Join-Path $PSScriptRoot 'validate-site.ps1')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

& (Join-Path $PSScriptRoot 'test-reading-times.ps1')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$demo = Join-Path $repo 'static\files\demo\synthetic-household-project'
python (Join-Path $demo 'analyze_demo.py') (Join-Path $demo 'synthetic_households.json')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

python (Join-Path $PSScriptRoot 'test-demo-notebook.py')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

& (Join-Path $PSScriptRoot 'check-production.ps1') -RootPath $repo
if ($LASTEXITCODE -eq 0) {
    Write-Host 'NEGATIVE PRODUCTION TEST: FAIL (preview repository was accepted)' -ForegroundColor Red
    exit 1
}
Write-Host 'NEGATIVE PRODUCTION TEST: PASS (preview repository was blocked)' -ForegroundColor Green

$cleanFixture = Join-Path $repo 'tests\fixtures\production-clean'
& (Join-Path $PSScriptRoot 'check-production.ps1') -RootPath $cleanFixture
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host 'POSITIVE PRODUCTION TEST: PASS (clean fixture was accepted)' -ForegroundColor Green
