let psum = scan (+) 0

let main (xss: [][]i32) =
  #[incremental_flattening(only_intra)]
  map (psum >-> psum >-> psum)
      xss
