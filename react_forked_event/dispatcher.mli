

val dispatch : 
  'a React.event -> 
  ?hash:('a -> int) -> 
  int ->
  'a React.event array 

