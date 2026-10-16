param(
    [string]$OutputDirectory = 'public-preview'
)

$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
if (-not (Test-Path -LiteralPath (Join-Path $repo 'node_modules\katex'))) {
    throw 'The editorial renderer requires the pinned local dependency. Run npm ci in the repository first.'
}
node (Join-Path $repo 'scripts\build-editorial.js')
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
$target = [System.IO.Path]::GetFullPath((Join-Path $repo $OutputDirectory))
if (-not $target.StartsWith($repo + [System.IO.Path]::DirectorySeparatorChar, [System.StringComparison]::OrdinalIgnoreCase)) {
    throw "Preview output must remain inside the repository: $target"
}
if (Test-Path -LiteralPath $target) {
    Remove-Item -LiteralPath $target -Recurse -Force
}
zola build --force --output-dir $target
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
Write-Host "Preview build completed: $OutputDirectory"
