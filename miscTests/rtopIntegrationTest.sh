# Context: https://github.com/facebook/reason/pull/674

# We can't directly call `rtop -stdin` because it circumvents what we're trying to
# test. See rtop.sh for the reason. We want to make sure utop's reason
# integration is legit,

# `utop -stdin` wouldn't work because it somehow processes the code before
# invoking the reason plugin, so `echo someReasonCode | utop -stdin` would
# always error.

# Given the above, we're gonna test that utop integration works by piping code
# into it and asserting the existence of some output.
echo "Testing rtop..."
echo "let f a => a;" \
| utop -init src/rtop_init.ml -I $HOME \
| grep "let f : 'a => 'a = <fun>" > /dev/null
if [ $? -ne 0 ]; then
  echo "rtop is failing! Failed to evaluate \`let f a => a;\`"
  exit 1
else
  echo "let f a => 1 + \"hi\";" \
  | utop -init src/rtop_init.ml -I $HOME \
  | grep "Error: This expression has type" > /dev/null
  if [ $? -ne 0 ]; then
    echo "rtop is failing! Failed to (correctly) error on \`let f a => 1 + \"hi\";\`"
    exit 1
  fi
fi
