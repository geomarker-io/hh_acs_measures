#!/usr/bin/env bash
# Run from the repository root: bash tests/test-publish.sh
# A fake gh records arguments; this test never contacts GitHub.
set -euo pipefail

publisher="$(pwd)/publish_release.sh"
test_dir=$(mktemp -d)
trap 'rm -rf "$test_dir"' EXIT
mkdir -p "$test_dir/bin" "$test_dir/data/v3.0.0"
cat > "$test_dir/bin/gh" <<'SH'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$GH_TEST_ARGUMENTS"
exit "${GH_TEST_EXIT_CODE:-0}"
SH
chmod +x "$test_dir/bin/gh"
export PATH="$test_dir/bin:$PATH"
export GH_TEST_ARGUMENTS="$test_dir/arguments"
cd "$test_dir"

expect_failure() {
  rm -f "$GH_TEST_ARGUMENTS"
  if bash "$publisher" "$@" > "$test_dir/error" 2>&1; then
    printf 'Expected failure: %s\n' "$*" >&2
    exit 1
  fi
  [[ ! -e $GH_TEST_ARGUMENTS ]]
}

bash "$publisher" --help > "$test_dir/help"
[[ ! -e $GH_TEST_ARGUMENTS ]]
expect_failure
expect_failure v3.0.0
expect_failure 3.0
expect_failure 3.0.0 extra extra
expect_failure 3.0.0

stem=data/v3.0.0/hh_acs_measures-v3.0.0
for extension in parquet measures.yml CPI.csv notes.md; do
  printf 'fixture\n' > "$stem.$extension"
done
# Missing/empty assets and notes must fail before invoking gh.
for extension in parquet measures.yml CPI.csv notes.md; do
  mv "$stem.$extension" "$stem.$extension.saved"
  expect_failure 3.0.0
  : > "$stem.$extension"
  expect_failure 3.0.0
  mv "$stem.$extension.saved" "$stem.$extension"
done
bash "$publisher" 3.0.0 > "$test_dir/output"
[[ $(cat "$test_dir/output") == *'Publish release'* ]]
cat > "$test_dir/expected" <<'ARGS'
release
create
v3.0.0
data/v3.0.0/hh_acs_measures-v3.0.0.parquet
data/v3.0.0/hh_acs_measures-v3.0.0.measures.yml
data/v3.0.0/hh_acs_measures-v3.0.0.CPI.csv
--repo
geomarker-io/hh_acs_measures
--draft
--verify-tag
--title
Harmonized Historical ACS Measures 3.0.0
--notes-file
data/v3.0.0/hh_acs_measures-v3.0.0.notes.md
ARGS
cmp "$test_dir/expected" "$GH_TEST_ARGUMENTS"

# A custom output directory with spaces must arrive as intact arguments.
directory='custom output'
mkdir "$directory"
for extension in parquet measures.yml CPI.csv notes.md; do
  printf 'fixture\n' > "$directory/hh_acs_measures-v3.0.1-beta.1.$extension"
done
bash "$publisher" 3.0.1-beta.1 "$directory" > "$test_dir/output"
cat > "$test_dir/expected" <<'ARGS'
release
create
v3.0.1-beta.1
custom output/hh_acs_measures-v3.0.1-beta.1.parquet
custom output/hh_acs_measures-v3.0.1-beta.1.measures.yml
custom output/hh_acs_measures-v3.0.1-beta.1.CPI.csv
--repo
geomarker-io/hh_acs_measures
--draft
--verify-tag
--title
Harmonized Historical ACS Measures 3.0.1-beta.1
--notes-file
custom output/hh_acs_measures-v3.0.1-beta.1.notes.md
ARGS
cmp "$test_dir/expected" "$GH_TEST_ARGUMENTS"

# Propagate publishing errors instead of reporting success.
export GH_TEST_EXIT_CODE=7
status=0
bash "$publisher" 3.0.0 || status=$?
[[ $status == 7 ]]
printf 'Publishing tests passed: file checks, version arguments, quoted paths, draft creation, notes-file option, publication instructions and CLI error propagation; no network calls.\n'
