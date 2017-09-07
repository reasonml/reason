## 1.13.8?

- Even better infix operators formatting for `==`, `&&`, `>` and the rest (#1380, #1386)

## 3.0.0

TODO: re-enable rtop integration tests in makefile (./miscTests/rtopIntegrationTest.sh) before the release

- Remove unused binaries: `reup`, etc.
- Remove the old `reactjs_jsx_ppx.ml`. You've all been on `reactjs_jsx_ppx_2.ml` for a long time now.
- Remove `--add-printers` from `refmt`. The feature wasn't stable enough; we'll find a better way soon. Sorry about that!
- New syntax based on (but not entirely) #1299
