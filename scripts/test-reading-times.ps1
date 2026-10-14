$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$model = Get-Content -LiteralPath (Join-Path $repo 'data\preview_synthetic.toml') -Raw
$fixtures = @(
    @{ Id = 'household-averages'; Path = 'content\preview-synthetic\insights\what-household-averages-can-hide.md' }
)

foreach ($fixture in $fixtures) {
    $raw = Get-Content -LiteralPath (Join-Path $repo $fixture.Path) -Raw
    $parts = $raw -split '\+\+\+', 3
    if ($parts.Count -lt 3) { throw "Front matter could not be separated in $($fixture.Path)" }
    $body = $parts[2] -replace '(?m)^#{1,6}\s+.*$', '' -replace '(?m)^[-*]\s+', '' -replace '(?m)^\d+\.\s+', ''
    $wordCount = [regex]::Matches($body, "\b[\p{L}\p{N}’'-]+\b").Count
    $expected = [math]::Max(1, [math]::Ceiling($wordCount / 220))
    $entryPattern = '(?s)id\s*=\s*"' + [regex]::Escape($fixture.Id) + '".*?reading_minutes\s*=\s*(?<minutes>\d+)'
    $match = [regex]::Match($model, $entryPattern)
    if (-not $match.Success) { throw "Reading time missing from the central model for $($fixture.Id)" }
    $actual = [int]$match.Groups['minutes'].Value
    if ($actual -ne $expected) { throw "Reading time mismatch for $($fixture.Id): model=$actual, expected=$expected from $wordCount words" }
    Write-Host "READING TIME: $($fixture.Id) = $actual min ($wordCount words at 220 wpm)"
}

Write-Host 'READING TIME TEST: PASS' -ForegroundColor Green
