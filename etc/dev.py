from typing import Any
import argparse
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path
from subprocess import CalledProcessError

BENCHMARKS = {
    "repos": {
        "k8s": {
            "git": "https://github.com/kubernetes/kubernetes.git",
            "commit": "03e14cc9432975dec161de1e52d7010f9711a913",
        },
        "linux": {
            "git": "https://github.com/torvalds/linux.git",
            "commit": "9d9c1cfec01cdbf24bd9322ed555713a20422115",
        },
        "wikiquote": {
            "git": "https://github.com/federicotdn/wikiquote.git",
            "commit": "2ded998e4f7c5f08e0c66bd34c747a6842a2aedb",
        },
    },
    "findSymbolDefinition": {
        "k8s": {"repo": "k8s", "lang": "go", "symbol": "genMarkdown"},
        "k8s-new": {"repo": "k8s", "lang": "go", "symbol": "New"},
        "k8s-vendor": {"repo": "k8s", "lang": "go", "symbol": "IgnoreTopFunction"},
        "k8s-notfound": {
            "repo": "k8s",
            "lang": "go",
            "symbol": "ThisSymbolDoesNotExist",
        },
        "wq": {"repo": "wikiquote", "lang": "python", "symbol": "quote_of_the_day"},
        "linux": {"repo": "linux", "lang": "c", "symbol": "io_madvise"},
    },
}


def run(*args: Any, verbose: bool = False, **kwargs: Any):
    kwargs.setdefault("check", True)
    if verbose:
        print("dev:", " ".join(args))
    return subprocess.run(args, **kwargs)


def sep() -> None:
    print("-" * 80)


def install(profile: bool = False) -> None:
    install_dir = Path.home() / ".local" / "bin"
    args = [
        "cabal",
        "install",
        "exe:irk",
        "-j",
        f"--installdir={install_dir}",
        "--overwrite-policy=always",
    ]
    if profile:
        args.extend(["--enable-profiling", '--ghc-options="-fprof-late"'])

    try:
        run(*args, verbose=True)
    except CalledProcessError as e:
        if sys.platform == "win32":
            print(
                "hint: the .exe file may currently be running, close it and try again.",
                file=sys.stderr,
            )
        print("error:", e)
        exit(1)


def vendor() -> None:
    run("cabal", "freeze")
    if os.path.exists("vendor"):
        shutil.rmtree("vendor")
    os.makedirs("vendor")

    with open("cabal.project.freeze") as f:
        for line in f:
            if "any." not in line:
                continue

            if match := re.search(r"any\.([^ ]*) ==([^,]*)", line):
                pkg = f"{match.group(1)}-{match.group(2)}"
                run(
                    "cabal",
                    "get",
                    "--destdir=./vendor",
                    pkg,
                    check=False,
                )

    os.remove("cabal.project.freeze")


def benchmark(
    case_name: str,
    profile: bool = False,
    use_rg: bool = False,
) -> None:
    script_dir = Path(__file__).parent
    root_dir = Path.cwd()
    repos_dir = script_dir / "repos"

    if case_name not in BENCHMARKS["findSymbolDefinition"]:
        print("invalid case; available cases:")
        for case in BENCHMARKS["findSymbolDefinition"].keys():
            print(f"  {case}")
        exit(1)

    scenario = BENCHMARKS["findSymbolDefinition"][case_name]
    repo_id = scenario["repo"]
    language = scenario["lang"]
    symbol = scenario["symbol"]

    repo_info = BENCHMARKS["repos"][repo_id]
    git_url = repo_info["git"]
    commit = repo_info["commit"]

    repo_name = Path(git_url).stem
    repo_path = repos_dir / repo_name

    print(f"scenario: {case_name}")
    print(f"  git: {git_url}")
    print(f"  commit: {commit}")
    print(f"  language: {language}")
    print(f"  symbol: {symbol}")
    sep()

    repos_dir.mkdir(exist_ok=True)

    if not repo_path.exists():
        run("git", "clone", "--filter=blob:none", git_url, str(repo_path))

    result = run(
        "git", "rev-parse", "HEAD", cwd=repo_path, capture_output=True, text=True
    )
    current_commit = result.stdout.strip()
    if current_commit != commit:
        run("git", "checkout", commit, cwd=repo_path)

    os.chdir(root_dir)

    if use_rg:
        cmd = [
            "rg",
            "-t",
            language,
            "--word-regexp",
            symbol,
            str(repo_path),
            "--column",
            "--no-heading",
        ]
        print("running rg...")
        run(*cmd)
        sep()
        print("running rg benchmark...")
        cmd_str = " ".join(cmd)
        run("hyperfine", "-i", "--warmup", "2", cmd_str)
    elif profile:
        install(profile=True)

        if os.path.exists("irk.prof"):
            os.remove("irk.prof")

        sep()
        print("profiling irk...")
        cmd = [
            "irk",
            "find",
            "-w",
            str(repo_path),
            "-l",
            language,
            symbol,
            "+RTS",
            "-pj",
            "-RTS",
        ]

        start = time.perf_counter()
        run(*cmd)
        elapsed = time.perf_counter() - start
        print(f"Elapsed time: {elapsed:.3f}s")

        if os.path.exists("irk.prof"):
            timestamp = time.strftime("%Y%m%d-%H%M%S")
            newname = f"irk.{timestamp}.prof"
            os.rename("irk.prof", newname)
            sep()
            print(f"profile saved to {newname}")
    else:
        install()

        sep()
        print("running irk...")
        cmd = ["irk", "find", "-w", str(repo_path), "-l", language, symbol]

        start = time.perf_counter()
        run(*cmd)
        elapsed = time.perf_counter() - start
        print(f"elapsed time: {elapsed:.3f}s")

        sep()
        print("running irk benchmark...")
        cmd_str = " ".join(cmd)
        run("hyperfine", "--warmup", "2", cmd_str)


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)

    subparsers.add_parser("install")
    subparsers.add_parser("vendor")

    bench_parser = subparsers.add_parser("benchmark")
    bench_parser.add_argument("case_name")
    bench_parser.add_argument("--profile", action="store_true")
    bench_parser.add_argument("--rg", action="store_true")

    args = parser.parse_args()

    if args.command == "install":
        install()
    elif args.command == "vendor":
        vendor()
    elif args.command == "benchmark":
        benchmark(args.case_name, args.profile, args.rg)


if __name__ == "__main__":
    main()
