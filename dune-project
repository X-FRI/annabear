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
 (synopsis "OCaml parser combinators library")
 (description "OCaml parser combinators library")
 (depends ocaml dune omtl)
 (tags
  (parser "parser combinator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
