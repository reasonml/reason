testPath="miscTests/export_tests"


ocamlc -dsource -ppx ./ppx_export.native miscTests/export_tests/test.ml -o test.native
