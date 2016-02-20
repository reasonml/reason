Debugging Yacc Grammar Conflicts:
-------------------------
Run the main parser through yacc with the `-v` flag to have it print out
details about the conflict.  `ocamlyacc -v src/reason_parser.mly`. The debug 
information can be found at `src/reason_parser.output`.

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

