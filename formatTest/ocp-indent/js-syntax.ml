(* s *)

let _ =
  [%raise_structural_sexp
    "feature's tip is already an ancestor of new base"
    { feature_tip = (old_tip : Rev.t)
    ; new_base    = (new_base : Rev.t)
    }]

let _ =
  [%raise_structural_sexp "feature's tip is already an ancestor of new base"
    { feature_tip = (old_tip : Rev.t)
    ; new_base    = (new_base : Rev.t)
    }
  ]
