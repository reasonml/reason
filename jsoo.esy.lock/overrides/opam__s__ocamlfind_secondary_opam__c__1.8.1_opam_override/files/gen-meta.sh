OCAML_SECONDARY_COMPILER=$1

cat >META <<EOF
description = "OCaml Secondary Compiler"
version = "4.08.1"
directory = "$OCAML_SECONDARY_COMPILER/bin"
EOF
