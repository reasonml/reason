(** ocaml patterns
    (http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html)
*)

let _ = function

    x -> () |

    _ -> () |

    'a'  -> () |

    x as y -> () |

    (x: 'a -> 'b) -> () |

    x | y  -> () |

    Some x -> () |

    `Var x -> () |

    #ty -> () |

    x, y -> () |

    { f1 = x;
      f2 = y;
      f3 = z;
      _
    } -> () |

    [ x;
      y;
      z;
    ] -> () |

    x::y
    :: z -> () |

    [| x;
       y;
       z;
    |] -> () |

    lazy w -> ()
