Contributors: Lexing and Parsing Type Parameters:
===================================================

Lexing and Parsing type parameters with angle brackets such as the following:

```reason
type t<'x> = list<'x>;
```

is difficult because type parameter angle brackets can "stack" at the end of a
nested parameterized type, resulting in something that looks a lot like an
infix operator beginning with greater-than symbol `>`.

```reason
type t<'x> = something<list<'x>>;
```

In the original implementation of the Reason lexer
`reason_declarative_lexer.mll`, it would tokenize what appears to be infix
operators as a single token so in that case will tokenize `>>` at the end of
the type definition. But in order to correctly parse parameterized types, we
nee to be able to "balance" the opening and closing angle brackets.

Another potential lexing conflict with infix operators is in the parsing of
default values in named arguments with type annotations.

```reason
let f = (~name: list<thing>=[myThing]) => {..};
```

In that example, the `>=` could be parsed as an infix operator if we are not
careful.

Any workable solution will need to maintain multiple `GREATER` tokens when
lexing `>>>` so they can be used for balancing type parameters. There are a few
options:

**Solution:**

In `Reason_single_parser.ml`, we use the same token splitting technique, where
upon a failure to parse on a token, we examine the token and determine if we
can split it into several tokens. We already did this for when we lexed the
token `=?` and failed to parse with it (we split it into `=`, `?'`).  When we
fail to parse with a lexed token beginning with `>`, we split all of the
leading `>` characters into a stream of `GREATER` tokens, and split the
remaining as best as possible.

