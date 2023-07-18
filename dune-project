(lang dune 3.9)

; (name moo_utils)

(generate_opam_files true)

(source
 (github mooreryan/moo_utils))

(authors "Ryan M. Moore")

(maintainers "Ryan M. Moore")

(license MIT)

(documentation https://github.com/mooreryan/moo_utils)

(package
 (name moo_utils_iter)
 (version 0.1.0)
 (synopsis "Utils for the iter package")
 (description "Utils for the iter package")
 (depends
  base
  dune
  iter
  ocaml
  (ppx_inline_test :with-test))
 (tags
  (moo_utils iter)))
