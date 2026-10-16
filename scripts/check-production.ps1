param(
    [string]$RootPath = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path,
    [string]$BuildDirectory = ''
)
$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path -LiteralPath $RootPath).Path
$config = Get-Content -LiteralPath (Join-Path $repo 'config.toml') -Raw
if ($config -notmatch 'site_mode\s*=\s*"production"' -or $config -notmatch 'generate_sitemap\s*=\s*true') {
    throw 'Production mode and sitemap must be enabled.'
}
if ($BuildDirectory) {
    python (Join-Path $PSScriptRoot 'audit-production.py') --build-dir $BuildDirectory
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
Write-Host 'PRODUCTION CHECK: PASS'
exit 0
