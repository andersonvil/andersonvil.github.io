param(
    [Parameter(Mandatory = $false)]
    [string]$RootPath = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
)

$ErrorActionPreference = 'Stop'
$resolvedRoot = (Resolve-Path -LiteralPath $RootPath).Path
$configPath = Join-Path $resolvedRoot 'config.toml'
if ((Test-Path -LiteralPath $configPath) -and ((Get-Content -LiteralPath $configPath -Raw) -match 'site_mode\s*=\s*"preview"')) {
    Write-Host 'PRODUCTION CHECK: BLOCKED - Private preview; publication requires explicit authorization.' -ForegroundColor Yellow
    exit 1
}
$violations = [System.Collections.Generic.List[string]]::new()

$blockedDirectories = @(
    'content\preview-synthetic',
    'static\files\demo'
)

foreach ($relative in $blockedDirectories) {
    $candidate = Join-Path $resolvedRoot $relative
    if (Test-Path -LiteralPath $candidate) {
        $violations.Add("Preview-only directory remains: $relative")
    }
}

$textExtensions = @('.html', '.md', '.toml', '.json', '.js', '.ts', '.py', '.yaml', '.yml')
$patterns = @(
    'placeholder\s*[:=]\s*true',
    'synthetic\s*[:=]\s*true',
    'publicationStatus\s*[:=]\s*["'']?blocked',
    'Synthetic demonstration content\s*[—-]\s*not for publication',
    'href\s*=\s*["'']#(?:["''])',
    'example\.invalid'
)

$files = Get-ChildItem -LiteralPath $resolvedRoot -Recurse -File | Where-Object {
    $_.Extension -in $textExtensions -and
    $_.FullName -notmatch '[\\/]\.git[\\/]' -and
    $_.FullName -notmatch '[\\/]themes[\\/]' -and
    $_.FullName -notmatch '[\\/]tests[\\/]' -and
    $_.FullName -notmatch '[\\/]public(?:-preview|-production)?[\\/]' -and
    $_.FullName -notmatch '[\\/]scripts[\\/]check-production\.ps1$'
}

foreach ($file in $files) {
    $content = Get-Content -LiteralPath $file.FullName -Raw
    foreach ($pattern in $patterns) {
        if ($content -match $pattern) {
            $relative = $file.FullName.Substring($resolvedRoot.Length + 1)
            $violations.Add("Blocked marker '$pattern' in $relative")
        }
    }
    if ($file.Extension -eq '.html') {
        $buttons = [regex]::Matches($content, '<button\b[^>]*>', 'IgnoreCase')
        foreach ($button in $buttons) {
            if ($button.Value -notmatch 'data-filter=|copy-citation|file-test|theme-toggle|menu-toggle|abstract-toggle|copy-email|audio-play-toggle|immersive-(?:open|close|prev|next)') {
                $relative = $file.FullName.Substring($resolvedRoot.Length + 1)
                $violations.Add("Button without a recognized functional destination in ${relative}: $($button.Value)")
            }
        }
    }
}

$inventory = Join-Path $resolvedRoot 'data\preview_inventory.toml'
if (Test-Path -LiteralPath $inventory) {
    $inventoryText = Get-Content -LiteralPath $inventory -Raw
    if ($inventoryText -match 'registry_status\s*=\s*"open"' -or $inventoryText -match 'status\s*=\s*"open"') {
        $violations.Add('The preview inventory still contains open entries.')
    }
}

if ($violations.Count -gt 0) {
    Write-Host 'PRODUCTION CHECK: BLOCKED' -ForegroundColor Red
    $violations | Sort-Object -Unique | ForEach-Object { Write-Host " - $_" }
    exit 1
}

Write-Host 'PRODUCTION CHECK: PASS' -ForegroundColor Green
exit 0
