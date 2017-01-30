Debugging Grammar Conflicts:
-------------------------
Run the main parser through menhir with the `--explain` flag to have it print out
details about the conflict.  `menhir src/reason_parser.mly --explain`. The debug 
information can be found at `src/reason_parser.conflicts`.

Debugging the parser state at runtime:
------------------------
If you set the environment variable as follows, the parser state will be printed out as it parses files.

```sh
export OCAMLRUNPARAM='p'
```

Testing:
------------------
Run the tests in the `./formatTest/` directory and observe differences in
output. The test files contain the most obscure syntax forms intentionally.

Cutting a release:
-------------------
- Make sure local changes are properly committed
- Update remote:
```
git fetch;
git reset --hard origin/master
```
- Prerelease:
```
env version=x.y.z make pre_release
```
- Check everything is ok locally
- Release!:
```
env version=x.y.z make release
```
- Use [opam-publish](https://github.com/ocaml/opam-publish) to publish the latest version to opam. 
