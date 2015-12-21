#!/usr/bin/env bash

# 0. Touches .utoprc (otherwise utop will fail).
# 1. Avoids "#use"ing ~/.ocamlinit before reason is required.
#    Otherwise, the ~/.ocamlinit is written in `.ml` syntax that will
#    choke the reason parser.
# 2. Add the $HOME directory to the include paths so that in step 3
#     we can actually load the ~/.ocamlinit (but before we require
#     reason).
# 3. Run ml rtop_init.ml that "#use"es ".ocamlinit", and then finally
#    require's reason, so that reason is required after .ocamlinit is
#    parsed in standard syntax.

touch $HOME/.utoprc
touch $HOME/.utop-history
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
utop -init $DIR/rtop_init.ml -I $HOME
