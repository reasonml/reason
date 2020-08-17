OCAML_SECONDARY_COMPILER=$1

cat >ocaml-secondary-compiler.conf <<EOF
path(secondary) = "$OCAML_SECONDARY_COMPILER/lib"
destdir(secondary) = "$OCAML_SECONDARY_COMPILER/lib"
stdlib(secondary) = "$OCAML_SECONDARY_COMPILER/lib"
ocamlc(secondary) = "$OCAML_SECONDARY_COMPILER/bin/ocamlc"
ocamlopt(secondary) = "$OCAML_SECONDARY_COMPILER/bin/ocamlopt"
ocamlcp(secondary) = "$OCAML_SECONDARY_COMPILER/bin/ocamlcp"
ocamlmklib(secondary) = "$OCAML_SECONDARY_COMPILER/bin/ocamlmklib"
ocamlmktop(secondary) = "$OCAML_SECONDARY_COMPILER/bin/ocamlmktop"
ocamldoc(secondary) = "$OCAML_SECONDARY_COMPILER/bin/ocamldoc"
ocamldep(secondary) = "$OCAML_SECONDARY_COMPILER/bin/ocamldep"
EOF
