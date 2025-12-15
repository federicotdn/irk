#!/usr/bin/env bash
set -e

print_sep() {
    printf -- "-%.0s" {1..80}; echo
}

if [ $# -lt 1 ]; then
    echo "usage: $0 <case-name> [--flags...]" >&2
    echo "available cases:" >&2
    jq -r '.findSymbolDefinition | keys[]' "$(dirname "$0")/benchmark.json" >&2
    exit 1
fi

case_name="$1"
shift

# Parse flags
cabal_extra_flags=""
irk_flags=""
rts_flags=""
parse_irk_flags=false
use_rg=false
use_profiling=false
for arg in "$@"; do
    if [ "$arg" = "--" ]; then
        parse_irk_flags=true
    elif [ "$arg" = "--profile" ]; then
        cabal_extra_flags="--enable-profiling --ghc-options=\"-fprof-late\""
        rts_flags="+RTS -pj -RTS"
        use_profiling=true
    elif [ "$arg" = "--rg" ]; then
        use_rg=true
    elif [ "$parse_irk_flags" = true ]; then
        irk_flags="$irk_flags $arg"
    fi
done

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
language=$(echo "$scenario" | jq -r '.lang')
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

echo "scenario: $case_name"
echo "  git: $git_url"
echo "  commit: $commit"
echo "  language: $language"
echo "  symbol: $symbol"
print_sep

mkdir -p "$repos_dir"
if [ ! -d "$repo_path" ]; then
    git clone --filter=blob:none "$git_url" "$repo_path"
fi

cd "$repo_path"

current_commit=$(git rev-parse HEAD)
if [ "$current_commit" != "$commit" ]; then
    git checkout "$commit"
fi

cd "$root_dir"

cmd="irk find -w '$repo_path' -l '$language' '$symbol' $irk_flags $rts_flags"

if [ "$use_rg" = true ]; then
    echo "running rg..."
    cmd="rg -t '$language' --word-regexp '$symbol' '$repo_path' --column --no-heading"
    bash -c "$cmd" || true

    print_sep
    echo "running rg benchmark..."
    hyperfine -i --warmup 2 "$cmd"
elif [ "$use_profiling" = true ]; then
    make install CABAL_EXTRA_FLAGS="$cabal_extra_flags"
    rm -f irk.prof

    print_sep
    echo "profiling irk..."
    time bash -c "$cmd"
else
    make install

    print_sep
    echo "running irk..."
    bash -c "$cmd"

    print_sep
    echo "running irk benchmark..."
    hyperfine --warmup 2 "$cmd"
fi

# Move profile file if profiling was enabled
# Can then be visualized with https://www.speedscope.app
if [ "$use_profiling" = true ] && [ -f "irk.prof" ]; then
    timestamp=$(date +"%Y%m%d-%H%M%S")
    newname="irk.$timestamp.prof"
    mv irk.prof "$newname"
    print_sep
    echo "profile saved to $newname"
fi
