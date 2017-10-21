echo "**This script is switching you to ocaml 4.02.3 for the subsequent bspacking. Please switch back to your own version afterward. Thanks!**\n"
# switch to 4.02.3. Bspacking means we're sending the final bundle to BuckleScript, which is still on 4.02
opam switch 4.02.3
eval $(opam config env)
rm -rf ./omp
git clone https://github.com/ocaml-ppx/ocaml-migrate-parsetree.git ./omp
pushd omp
# if there's any error, check if you have everything installed. You should already from opam pin-ing the reason repo (which depends on omp)
make

pushd ./_build/default/src

rm -rf ./*.cm*
rm -rf ./*.o
rm -rf ./*.a

# # bspack needs the fully processed files with ppx already applied to them, and
# # jBuilder keeps them around in files like `x.pp.ml`, so rename them to `x.ml`
for i in ./*.pp.{ml,mli}; do mv $i ${i/.pp/}; done

popd
popd

# now, download google closure compiler
mkdir -p closure-compiler
pushd closure-compiler
curl -O http://dl.google.com/closure-compiler/compiler-20170910.tar.gz
tar -xzf compiler-20170910.tar.gz

popd
