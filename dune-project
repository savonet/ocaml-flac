(lang dune 2.8)
(version 0.4.0)
(name flac)
(source (github savonet/ocaml-flac))
(license GPL-2.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name flac)
 (synopsis "Bindings to libflac")
 (depends
  conf-libflac
  conf-pkg-config
  (ocaml (>= 4.03.0))
  dune
  dune-configurator)
 (depopts
  (ogg (>= 0.7.0)))
)
