param(
    [string]$OutputDirectory = 'public-production'
)

$ErrorActionPreference = 'Stop'
& (Join-Path $PSScriptRoot 'check-production.ps1')
if ($LASTEXITCODE -ne 0) {
    Write-Host 'Production build was not started.' -ForegroundColor Yellow
    exit $LASTEXITCODE
}
& (Join-Path $PSScriptRoot 'build-preview.ps1') -OutputDirectory $OutputDirectory
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
& (Join-Path $PSScriptRoot 'check-production.ps1') -BuildDirectory $OutputDirectory
exit $LASTEXITCODE
