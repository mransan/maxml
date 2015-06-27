
let dispatch e ?hash nb =   
  let a = Array.make nb React.E.never in 
  for i=0 to nb-1 do 
    let counter = ref i in 
    let e' = React.E.filter (fun x -> 
      counter := (!counter + 1) mod nb; 
      (*
      Printf.printf "counter %i %b\n" !counter  (!counter = 0);
      *)
      !counter = 0
    ) e in  
    Array.set a i e'
  done;
  a
