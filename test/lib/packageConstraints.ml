module Old = Reason_omp.Ast_55.Outcometree
module New = Reason_omp.Ast_56.Outcometree

let old_package =
  Old.Otyp_module
    { opack_path = Oide_ident { printed_name = "S" }
    ; opack_constraints =
        [ "t", Otyp_var (false, "a")
        ; "M.t", Otyp_var (false, "b")
        ; "M.N.u", Otyp_var (false, "c")
        ]
    }

let new_package =
  New.Otyp_module
    { opack_path = Oide_ident { printed_name = "S" }
    ; opack_constraints =
        [ [ "t" ], Otyp_var (false, "a")
        ; [ "M"; "t" ], Otyp_var (false, "b")
        ; [ "M"; "N"; "u" ], Otyp_var (false, "c")
        ]
    }

let () =
  assert (Reason_omp.Migrate_55_56.copy_out_type old_package = new_package);
  assert (Reason_omp.Migrate_56_55.copy_out_type new_package = old_package)
