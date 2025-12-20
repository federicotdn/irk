# PowerShell port of benchmark.sh for Windows compatibility

param(
    [Parameter(Mandatory=$true, Position=0)]
    [string]$CaseName,

    [Parameter(ValueFromRemainingArguments=$true)]
    [string[]]$RemainingArgs
)

$ErrorActionPreference = "Stop"

function Print-Separator {
    Write-Host ("-" * 80)
}

# Parse the benchmark.json location
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$jsonFile = Join-Path $scriptDir "benchmark.json"
$reposDir = Join-Path $scriptDir "repos"
$rootDir = Get-Location

# Check if case exists and list available cases if not
$json = Get-Content $jsonFile | ConvertFrom-Json
if (-not $json.findSymbolDefinition.PSObject.Properties[$CaseName]) {
    Write-Error "usage: $($MyInvocation.MyCommand.Name) <case-name> [--flags...]"
    Write-Host "available cases:" -ForegroundColor Yellow
    $json.findSymbolDefinition.PSObject.Properties.Name | ForEach-Object { Write-Host "  $_" }
    exit 1
}

# Parse flags
$cabalExtraFlags = ""
$irkFlags = @()
$rtsFlags = ""
$parseIrkFlags = $false
$useRg = $false
$useProfiling = $false

foreach ($arg in $RemainingArgs) {
    if ($arg -eq "--") {
        $parseIrkFlags = $true
    }
    elseif ($arg -eq "--profile") {
        $cabalExtraFlags = "--enable-profiling --ghc-options=`"-fprof-late`""
        $rtsFlags = "+RTS -pj -RTS"
        $useProfiling = $true
    }
    elseif ($arg -eq "--rg") {
        $useRg = $true
    }
    elseif ($parseIrkFlags) {
        $irkFlags += $arg
    }
}

# Read scenario from JSON
$scenario = $json.findSymbolDefinition.$CaseName
$repoId = $scenario.repo
$language = $scenario.lang
$symbol = $scenario.symbol

# Read repo info
$repoInfo = $json.repos.$repoId
if (-not $repoInfo) {
    Write-Error "error: repo '$repoId' not found in benchmark.json"
    exit 1
}

$gitUrl = $repoInfo.git
$commit = $repoInfo.commit

$repoName = [System.IO.Path]::GetFileNameWithoutExtension($gitUrl)
$repoPath = Join-Path $reposDir $repoName

Write-Host "scenario: $CaseName"
Write-Host "  git: $gitUrl"
Write-Host "  commit: $commit"
Write-Host "  language: $language"
Write-Host "  symbol: $symbol"
Print-Separator

# Clone repo if needed
if (-not (Test-Path $reposDir)) {
    New-Item -ItemType Directory -Path $reposDir | Out-Null
}

if (-not (Test-Path $repoPath)) {
    Write-Host "Cloning repository..."
    git clone --filter=blob:none $gitUrl $repoPath
}

# Checkout specific commit
Push-Location $repoPath
try {
    $currentCommit = git rev-parse HEAD
    if ($currentCommit -ne $commit) {
        Write-Host "Checking out commit $commit..."
        git checkout $commit
    }
}
finally {
    Pop-Location
}

Set-Location $rootDir

# Build command
$irkFlagsStr = $irkFlags -join " "
# Use double quotes for the outer string and escaped double quotes for arguments
$cmd = "irk find -w `"$repoPath`" -l `"$language`" `"$symbol`" $irkFlagsStr $rtsFlags".Trim()

if ($useRg) {
    Write-Host "running rg..."
    $rgCmd = "rg -t `"$language`" --word-regexp `"$symbol`" `"$repoPath`" --column --no-heading"
    try {
        Invoke-Expression $rgCmd
    } catch {
        # Ignore errors (like no matches found)
    }

    Print-Separator
    Write-Host "running rg benchmark..."
    hyperfine -i --warmup 2 $rgCmd
}
elseif ($useProfiling) {
    $installDir = Join-Path $env:USERPROFILE ".local\bin"
    $cabalCmd = "cabal install exe:irk -j --installdir=`"$installDir`" --overwrite-policy=always"
    if ($cabalExtraFlags) {
        $cabalCmd += " $cabalExtraFlags"
    }
    Invoke-Expression $cabalCmd

    if (Test-Path "irk.prof") {
        Remove-Item "irk.prof"
    }

    Print-Separator
    Write-Host "profiling irk..."
    Measure-Command { Invoke-Expression $cmd }
}
else {
    $installDir = Join-Path $env:USERPROFILE ".local\bin"
    Invoke-Expression "cabal install exe:irk -j --installdir=`"$installDir`" --overwrite-policy=always"

    Print-Separator
    Write-Host "running irk..."
    Invoke-Expression $cmd

    Print-Separator
    Write-Host "running irk benchmark..."
    hyperfine --warmup 2 $cmd
}

# Move profile file if profiling was enabled
if ($useProfiling -and (Test-Path "irk.prof")) {
    $timestamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $newname = "irk.$timestamp.prof"
    Move-Item "irk.prof" $newname
    Print-Separator
    Write-Host "profile saved to $newname"
}
