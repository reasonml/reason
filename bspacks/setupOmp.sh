# switch to 4.02.3. Bspacking means we're sending the final bundle to BuckleScript, which is still on 4.02
opam switch 4.02.3
rm -rf ./omp
git clone https://github.com/ocaml-ppx/ocaml-migrate-parsetree.git ./omp
cd omp
# if there's any error, check if you have everything installed. You should already from opam pin-ing the reason repo (which depends on omp)
make

cd ./_build/default/src

rm -rf ./*.cm*
rm -rf ./*.o
rm -rf ./*.a

# bspack needs the fully processed files with ppx already applied to them, and
# jBuilder keeps them around in files like `x.pp.ml`, so rename them to `x.ml`
extensions fixed.
mv ./ast_402.pp.ml ./ast_402.ml
mv ./ast_403.pp.ml ./ast_403.ml
mv ./ast_404.pp.ml ./ast_404.ml
mv ./ast_405.pp.ml ./ast_405.ml
mv ./ast_406.pp.ml ./ast_406.ml
mv ./migrate_parsetree.pp.ml ./migrate_parsetree.ml
mv ./migrate_parsetree_402_403.pp.ml ./migrate_parsetree_402_403.ml
mv ./migrate_parsetree_402_403_migrate.pp.ml ./migrate_parsetree_402_403_migrate.ml
mv ./migrate_parsetree_403_402.pp.ml ./migrate_parsetree_403_402.ml
mv ./migrate_parsetree_403_402_migrate.pp.ml ./migrate_parsetree_403_402_migrate.ml
mv ./migrate_parsetree_403_404.pp.ml ./migrate_parsetree_403_404.ml
mv ./migrate_parsetree_403_404_migrate.pp.ml ./migrate_parsetree_403_404_migrate.ml
mv ./migrate_parsetree_404_403.pp.ml ./migrate_parsetree_404_403.ml
mv ./migrate_parsetree_404_403_migrate.pp.ml ./migrate_parsetree_404_403_migrate.ml
mv ./migrate_parsetree_404_405.pp.ml ./migrate_parsetree_404_405.ml
mv ./migrate_parsetree_404_405_migrate.pp.ml ./migrate_parsetree_404_405_migrate.ml
mv ./migrate_parsetree_405_404.pp.ml ./migrate_parsetree_405_404.ml
mv ./migrate_parsetree_405_404_migrate.pp.ml ./migrate_parsetree_405_404_migrate.ml
mv ./migrate_parsetree_405_406.pp.ml ./migrate_parsetree_405_406.ml
mv ./migrate_parsetree_405_406_migrate.pp.ml ./migrate_parsetree_405_406_migrate.ml
mv ./migrate_parsetree_406_405.pp.ml ./migrate_parsetree_406_405.ml
mv ./migrate_parsetree_406_405_migrate.pp.ml ./migrate_parsetree_406_405_migrate.ml
mv ./migrate_parsetree_ast_io.pp.ml ./migrate_parsetree_ast_io.ml
mv ./migrate_parsetree_compiler_functions.pp.ml ./migrate_parsetree_compiler_functions.ml
mv ./migrate_parsetree_def.pp.ml ./migrate_parsetree_def.ml
mv ./migrate_parsetree_driver.pp.ml ./migrate_parsetree_driver.ml
mv ./migrate_parsetree_versions.pp.ml ./migrate_parsetree_versions.ml
mv ./migrate_parsetree_ast_io.pp.mli ./migrate_parsetree_ast_io.mli
mv ./migrate_parsetree_def.pp.mli ./migrate_parsetree_def.mli
mv ./migrate_parsetree_driver.pp.mli ./migrate_parsetree_driver.mli
mv ./migrate_parsetree_versions.pp.mli ./migrate_parsetree_versions.mli

# switch back to the version you're supposed to be on to when working on Reason
opam switch 4.04.2
