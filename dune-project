(lang dune 3.6)

(name annabear)

(generate_opam_files true)

(source
 (github muqiuhan/annabear))

(authors "Muqiu Han")

(maintainers "Muqiu Han")

(license MIT)

(documentation https://github.com/muqiuhan/annabear)

(package
 (name annabear)
 (synopsis "parser combinators library")
 (allow_empty)
 (description "parser combinators library")
 (depends ocaml dune core alcotest)
 (tags
  (parser "parser combinator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
