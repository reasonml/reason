# Context: https://github.com/reasonml/reason/pull/674

# We can't directly call `rtop -stdin` because it circumvents what we're trying to
# test. See rtop.sh for the reason. We want to make sure utop's reason
# integration is legit,

# `utop -stdin` wouldn't work because it somehow processes the code before
# invoking the reason plugin, so `echo someReasonCode | utop -stdin` would
# always error.

# Given the above, we're gonna test that utop integration works by piping code
# into it and asserting the existence of some output.
  $ echo "let f = a => a;" | rtop | grep "let f: 'a => 'a = <fun>"
  # let f: 'a => 'a = <fun>;

  $ echo "let f = (a) => 1 + \"hi\";" | rtop | grep -o "This expression has type string but an expression was expected of type"
  This expression has type string but an expression was expected of type
