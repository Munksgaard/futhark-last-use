let main (x: i32): i32 =
  let xs = iota x
  let xs[1] = xs[0]

  let ys =
    if xs[0] > 0 then
      xs
    else
      indices xs

  let y = reduce (+) 0 ys

  in y
