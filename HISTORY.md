## 3.0.0

Our biggest release! **Please see our blog post** on https://reasonml.github.io/community/blog/#reason-3.

Summary: this is, practically speaking, a **non-breaking** change. You can mix and match two projects with different syntax versions in BuckleScript 2 (which just got release too! Go check), and they'll Just Work (tm).

To upgrade your own project, we've released a script, https://github.com/reasonml/upgradeSyntaxFrom2To3

Improvements:

- Much better printing for most common idioms.
- Even better infix operators formatting for `==`, `&&`, `>` and the rest (#1380, #1386, etc.).
- More predictable keyword swapping behavior (#1539).
- BuckleScript's `Js.t {. foo: bar}` now formats to `{. "foo": bar}`, just like its value counterpart (`[%bs.obj {foo: 1}]` to `{"foo": bar}`.
- `[@foo]`, `[@@foo]` and `[@@@foo]` are now unified into `[@foo]` and placed in front instead of at the back.
- `!` is now the logical negation. It was `not` previously.
- Dereference was `!`. Now it's a postfix `^`.
- Labeled argument with type now has punning!
- String concat is now `++` instead of the old `^`.
- For native, Reason now works on OCaml 4.05 and the latest topkg (#1438).
- Record field punning for module field prefix now prints well too: `{M.x, y}` is `{M.x: x, y: y}`.
- JSX needs `{}` like in JS.
- Fix reason-specific keywords printing in interface files (e.g. `==`, `match`, `method`).
- Record punning with renaming (#1517).
- The combination of functon label renaming + type annotation + punning is now supported!
- Fix LOTS of bugs regarding parsing & formatting (closing around 100 improvement-related issues!).
- Official `refmt.js`, with public api. See `README.md`.
- Official `refmt` native public API too.
- **New JS application/abstraction syntax**. Yes yes, we know. Despite the 100+ fixes, this one's all you cared about. Modern software engineering ¯\\_(ツ)_/¯. Please do read the blog post though.

Breaking Changes:

- Remove `--use-stdin` and `--is-interface-pp` option from refmt; they've been deprecated for a long time now
- Remove unused binaries: `reup`, etc.
- Remove the old `reactjs_jsx_ppx.ml`. You've all been on `reactjs_jsx_ppx_2.ml` for a long time now.
- Reserved keywords can no longer be used as an `external` declaration's labels.

Deprecated:

- Deprecate `--add-printers` option from refmt; we'll have a better strategy soon.

## 1.13.7

- Much better infix operators (e.g. |>) formatting! (#1259)

## 1.13.6

- Changelog got sent into a black hole
