(*---------------------------------------------------------------------------
   Copyright (c) 2011 The cmdliner programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Manpage = Cmdliner_manpage
module Term = struct
  include Cmdliner_term
  include Cmdliner_term_deprecated
end
module Cmd = struct
  module Exit = Cmdliner_info.Exit
  module Env = Cmdliner_info.Env
  include Cmdliner_cmd
  include Cmdliner_eval
end
module Arg = Cmdliner_arg
