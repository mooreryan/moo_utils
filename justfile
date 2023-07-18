browser := "firefox"
cov_dir := "/tmp/moo_util"
cov_file := cov_dir / "moo_util"

build:
  dune build

build_w:
  dune build -w

build_r:
  dune build --profile=release

build_rw:
  dune build --profile=release -w

check:
  dune build @check

check_w:
  dune build @check -w

promote:
  dune promote

test:
  dune runtest

test_coverage:
  #!/usr/bin/env bash
  set -euxo pipefail
  if [ -d {{ cov_dir }} ]; then rm -r {{ cov_dir }}; fi
  mkdir -p {{ cov_dir }}
  BISECT_FILE={{ cov_file }} dune runtest --no-print-directory \
  --instrument-with bisect_ppx --force
  bisect-ppx-report html --coverage-path {{ cov_dir }}
  bisect-ppx-report summary --coverage-path {{ cov_dir }}

test_coverage_open: test_coverage
  {{ browser }} _coverage/index.html
