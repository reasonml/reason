let read_raw_gen_ic read_pixel ic l c max =
  let img = Index8.create c l in
  let greymap =
    { Color.max = max;
      Color.map =
        let make_grey i = {r = i; g = i; b = i} in
        Array.init (max + 1) make_grey} in
  img.Index8.colormap <- greymap;
  for i = 0 to l - 1 do
    for j = 0 to c - 1 do
      Index8.set img j i (read_pixel ic)
    done
  done;
  img;;

let func_darken_only org level =
  let level = 255 - level in
  { r = if org.r > level then level else org.r;
    g = if org.g > level then level else org.g;
    b = if org.b > level then level else org.b };;

let f = function
  | { f1 = Foo
         | Bar;
      f2 = _; f3 = Foo
                 | Bar }
    -> { f1 = Foo,
              Bar;
         f2 = xxx
              = yyy; f3 = Foo,
                          Bar }

let _ =
  match a with
  | { kind =
        x } -> ()
  | { LibIndex.kind =
        x } -> ()

let x = { kind =
            x },
        { LibIndex.kind =
            x }
