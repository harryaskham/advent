#!/usr/bin/env bash
# Publish site/ to this repository's gh-pages branch and make sure Pages serves it.
#
#   scripts/publish-site.sh            # check, build-check, publish, request a Pages build
#
# Mirrors the shared publisher (harryaskham.github.io/.github/workflows/publish-pages.yml):
# a temporary shallow clone of gh-pages, replaced by one commit naming the source revision,
# pushed with gh's credential helper (no token in URLs, argv or config). It also enables
# Pages (legacy, gh-pages:/) the first time.
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"
python3 scripts/site/check.py
repo=$(gh repo view --json nameWithOwner --jq .nameWithOwner)
[[ "$repo" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || { echo "unexpected repository name: $repo" >&2; exit 1; }
source_rev=$(git rev-parse --short HEAD)
git diff --quiet HEAD -- site || { echo "site/ has uncommitted changes; commit them first" >&2; exit 1; }
export GIT_TERMINAL_PROMPT=0
remote="https://github.com/$repo.git"
rgit() { git -c credential.helper= -c 'credential.helper=!gh auth git-credential' "$@"; }
dir=$(mktemp -d); trap 'rm -rf "$dir"' EXIT INT TERM
if rgit ls-remote --exit-code --heads "$remote" gh-pages >/dev/null 2>&1; then
  rgit clone -q --depth 1 --single-branch --branch gh-pages "$remote" "$dir"
  git -C "$dir" rm -r -q --ignore-unmatch . >/dev/null
else
  git -C "$dir" init -q --initial-branch=gh-pages
fi
tar -C site -chf - --exclude=.git . | tar -C "$dir" -xf -
touch "$dir/.nojekyll"
git -C "$dir" add -A
if git -C "$dir" diff --cached --quiet; then
  echo "gh-pages already matches site/ at $source_rev"
else
  git -C "$dir" -c user.name="$(git config user.name)" -c user.email="$(git config user.email)" \
    commit -q -m "Publish ${repo#*/} site from $source_rev"
  rgit -C "$dir" push -q "$remote" HEAD:gh-pages
  echo "pushed gh-pages from $source_rev"
fi
want='{"build_type":"legacy","source":{"branch":"gh-pages","path":"/"}}'
mode=$(gh api "repos/$repo/pages" --jq '.build_type+":"+(.source.branch//"")+":"+(.source.path//"")' 2>/dev/null || echo disabled)
if [ "$mode" = disabled ]; then
  gh api -X POST "repos/$repo/pages" --input - <<< "$want" >/dev/null 2>&1 || true  # a push may have enabled it (409)
  for _ in $(seq 10); do  # a new site takes a few seconds to appear
    mode=$(gh api "repos/$repo/pages" --jq '.build_type+":"+(.source.branch//"")+":"+(.source.path//"")' 2>/dev/null || echo disabled)
    [ "$mode" != disabled ] && break
    sleep 3
  done
fi
if [ "$mode" != "legacy:gh-pages:/" ]; then
  gh api -X PUT "repos/$repo/pages" --input - <<< "$want" >/dev/null
  mode=$(gh api "repos/$repo/pages" --jq '.build_type+":"+(.source.branch//"")+":"+(.source.path//"")')
fi
[ "$mode" = "legacy:gh-pages:/" ] || { echo "Pages source is $mode, not legacy:gh-pages:/" >&2; exit 1; }
gh api -X POST "repos/$repo/pages/builds" --jq '"Pages build " + .status' || echo "build request failed; the push still triggers one"
gh api "repos/$repo/pages" --jq '"Serving " + .html_url'
