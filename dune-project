(lang dune 3.11)
(generate_opam_files true)
(package
  (name kot)
  (authors
    "François Pottier <francois.pottier@inria.fr>"
    "Juliette Ponsonnet <juliette.ponsonnet@ens-lyon.fr>"
  )
  (maintainers
    "François Pottier <francois.pottier@inria.fr>"
  )
  (source (uri git+https://github.com/fpottier/kot.git))
  (homepage https://github.com/fpottier/kot/)
  (bug_reports https://github.com/fpottier/kot/issues)
  (synopsis "Persistent catenable deques")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.12))
    (monolith :with-test)
    (ocamlfind :with-test)
  )
)
