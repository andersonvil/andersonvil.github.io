param(
    [string]$OutputDirectory = 'public-preview'
)

$ErrorActionPreference = 'Stop'
zola build --force --output-dir $OutputDirectory
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host "Preview build completed: $OutputDirectory"
