$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
& (Join-Path $PSScriptRoot 'build-preview.ps1') -OutputDirectory 'public-preview-final'
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
& (Join-Path $PSScriptRoot 'validate-site.ps1') -BuildDirectory 'public-preview-final'
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
& powershell -NoProfile -ExecutionPolicy Bypass -File (Join-Path $PSScriptRoot 'check-production.ps1') -RootPath $repo
if ($LASTEXITCODE -eq 0) { throw 'Private preview was accepted for production.' }
Write-Host 'NEGATIVE PRODUCTION TEST: PASS (private preview blocked)'
& powershell -NoProfile -ExecutionPolicy Bypass -File (Join-Path $PSScriptRoot 'check-production.ps1') -RootPath (Join-Path $repo 'tests/fixtures/production-clean')
if ($LASTEXITCODE -ne 0) { throw 'Clean production fixture was rejected.' }
Write-Host 'POSITIVE PRODUCTION TEST: PASS (clean fixture accepted)'
exit 0
