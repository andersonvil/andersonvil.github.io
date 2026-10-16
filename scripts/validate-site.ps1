param([string]$BuildDirectory = 'public-preview-final')
$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
python (Join-Path $PSScriptRoot 'audit-final-v7.py') --build-dir $BuildDirectory
exit $LASTEXITCODE
