#!/usr/bin/env bash
set -euo pipefail

usage() {
  printf 'Usage: ./publish_release.sh VERSION [OUTPUT_DIRECTORY]\n'
}

if [[ $# == 1 && $1 == --help ]]; then
  usage
  exit 0
fi
if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage >&2
  exit 1
fi

version=$1
if [[ ! $version =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][A-Za-z0-9.-]+)?$ ]]; then
  printf 'Supply a release version without the v prefix, e.g. 3.0.0\n' >&2
  exit 1
fi
directory=${2:-data/v$version}
stem="$directory/hh_acs_measures-v$version"
assets=("$stem.parquet" "$stem.measures.yml" "$stem.CPI.csv")
notes="$stem.notes.md"

for file in "${assets[@]}" "$notes"; do
  if [[ ! -f $file || ! -s $file ]]; then
    printf 'Missing or empty release file: %s\n' "$file" >&2
    exit 1
  fi
done
if ! command -v gh >/dev/null 2>&1; then
  printf 'Install the GitHub CLI (gh) and sign in with gh auth login.\n' >&2
  exit 1
fi

gh release create "v$version" \
  "${assets[@]}" \
  --repo geomarker-io/hh_acs_measures \
  --draft \
  --verify-tag \
  --title "Harmonized Historical ACS Measures $version" \
  --notes-file "$notes"

printf '\nDraft release created. Open the draft URL above on GitHub and review the assets and notes.\n'
printf 'When ready, click the edit (pencil) button, then Publish release.\n'
