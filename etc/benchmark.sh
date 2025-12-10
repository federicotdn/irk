#!/usr/bin/env bash
set -e

require_cmd() {
    command -v "$1" >/dev/null 2>&1 || { echo "error: '$1' not found" >&2; exit 1; }
}

require_cmd jq
require_cmd git
require_cmd hyperfine

if [ $# -lt 1 ]; then
    echo "Usage: $0 <case-name> [--flags...]" >&2
    echo "Available cases:" >&2
    jq -r '.findSymbolDefinition | keys[]' "$(dirname "$0")/benchmark.json" >&2
    exit 1
fi

case_name="$1"
shift
irk_flags="$*"
root_dir="$(pwd)"
script_dir="$(cd "$(dirname "$0")" && pwd)"
json_file="$script_dir/benchmark.json"
repos_dir="$script_dir/repos"

# Read scenario from JSON
scenario=$(jq -r ".findSymbolDefinition[\"$case_name\"]" "$json_file")
if [ "$scenario" = "null" ]; then
    echo "error: case '$case_name' not found in benchmark.json" >&2
    exit 1
fi

repo_id=$(echo "$scenario" | jq -r '.repo')
current=$(echo "$scenario" | jq -r '.current')
symbol=$(echo "$scenario" | jq -r '.symbol')

# Read repo info
repo_info=$(jq -r ".repos[\"$repo_id\"]" "$json_file")
if [ "$repo_info" = "null" ]; then
    echo "error: repo '$repo_id' not found in benchmark.json" >&2
    exit 1
fi

git_url=$(echo "$repo_info" | jq -r '.git')
commit=$(echo "$repo_info" | jq -r '.commit')

repo_name=$(basename "$git_url" .git)
repo_path="$repos_dir/$repo_name"

echo "Scenario: $case_name"
echo "  git: $git_url"
echo "  commit: $commit"
echo "  current: $current"
echo "  symbol: $symbol"
echo ""

mkdir -p "$repos_dir"
if [ ! -d "$repo_path" ]; then
    echo "Cloning $repo_name..."
    git clone --filter=blob:none "$git_url" "$repo_path"
fi

cd "$repo_path"

current_commit=$(git rev-parse HEAD)
if [ "$current_commit" != "$commit" ]; then
    echo "Checking out commit $commit..."
    git checkout "$commit"
fi

cd "$root_dir"
make install

active_file="$repo_path/$current"

echo ""
echo "Running irk..."
cmd="irk search '$active_file' '$repo_path' '$symbol' $irk_flags"
bash -c "$cmd"

echo ""
echo "Running irk benchmark..."
hyperfine --warmup 2 "$cmd"
