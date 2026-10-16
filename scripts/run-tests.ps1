$ErrorActionPreference = 'Stop'
& (Join-Path $PSScriptRoot 'build-production.ps1')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
& (Join-Path $PSScriptRoot 'validate-site.ps1') -BuildDirectory 'public-production'
exit $LASTEXITCODE
