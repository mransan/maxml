
(** One dimention world with discrtize location bounded by a min and max value
 *)
module World = struct 
  type t = (int * int) 
  
  let where (min, max)  = function 
    | i when i = min -> `Min
    | i when i = max -> `Max
    | i when (i > min && i < max) -> `Inside 
    | _ -> failwith "Outside of world limit"

  let random_location (min,max) = 
    (Random.int (max-min) + min)

end 

(** A person is modeled with a position in the World as well as a 
    given speed. At each time step the person is moving at its current
    speed and will update its speed with a random behavior. 

    Note that a speed of -1 means it goes from right to left
  *)
module Person = struct 

  type t = {
    id       : char;
    position : int;
    speed    : int; 
  }


  (** [next world person] returns the new person with both 
      speed and position updated
    *)
  let next world ({id=_; speed; position} as p) = 
    let position = position + speed in 
    let speed = match World.where world position with 
      | `Min | `Max -> - speed 
      | `Inside     -> if (Random.int 100) < 75 then speed else - speed in  
    {p with speed;position} 


  (** [create word id] create the given person with random speed and 
      location within the given world.
    *)
  let create world id = 
    let speed = (Random.int 1) + 2 in 
    let speed = if Random.int 1 = 0 then -speed else speed in 
    {id; speed; position=World.random_location world}

end 


(** A dog is a character in the simulation which movement  are driven
    by a Person. The dog will always follow the given person 
    at a constant speed of 1 which is slower than the person. 

    A dog is therefore constantly chasing after a person
 *)
module Dog = struct 
  type t = { 
    id : char;
    position : int;
  }

  (** A dog follow a person at a constant speed of 1
   *)
  let next world {Person.position=p_pos; _ } ({position=pos; _ } as d) = 
    let step = if (p_pos - pos) > 0 then 1 else -1 in 
    {d with position= pos + step } 

  let create world id = 
    {id; position = World.random_location world} 
end 



(** Display utilities to print the world and the character in it
 *)
module Display = struct 
  let burn_cpu () = 
    let rec fib_aux n a b =
      match n with
      | 0 -> a
      | _ -> fib_aux (n-1) b (a+b)
    in
  ignore @@ fib_aux 100_000_000 0 1

  let position_of_character = function 
      |`Person {Person.position; _} -> position
      |`Dog    {Dog.position; _ } -> position 

  let id_of_character = function 
      |`Person {Person.id; _} -> id
      |`Dog    {Dog.id; _ } -> id

  let print (min,max) characters  = 
    let characters = List.sort (fun x y -> 
        compare (position_of_character x)(position_of_character y) 
      ) characters  in 
    print_char '\r'; 
    let last_position = List.fold_left (fun x character  -> 
      let position = position_of_character character in 
      for i=x to position - 1 do 
        print_char  '-' 
      done; 
      print_char @@ id_of_character character ; 
      position+1
    ) min characters in 

    for i=last_position to max do 
        print_char '-'
    done
end  



(** Helper module to create React.event and React signals for a
    simulation
  *)
module Events = struct 


  (** [person_e person world time_e] return the event and signal 
      correspoding to the [person]. This event will ensure that the 
      person event and signal is constantly updated upon [time_e]
      happening
   *)
  let person_e person world time_e = 
     let person = ref person in 
     let person_e = React.E.map (fun () -> 
       print_char 'c';
       person := Person.next world !person;
       `Person !person 
     ) time_e in 
     person_e, React.S.hold (`Person !person) person_e  
     (* React.S.hold !person person_e *) 

  (** [dog_e dog world person_e] returns the event and signal 
      for the given [dog] following the person simulated by [person_e] 
      event. 

      Note that in this setup a dog is only moving if the person he is 
      following is moving too. 
      TODO: modify this behavior so that the dog keeps on chasing the person
      at every clock update
    *)
  let dog_e dog world person_e = 
    let dog = ref dog  in  
    let dog_e = React.E.map (fun c -> 
      let (_:unit) = match c with 
        | `Person person -> dog := Dog.next world person !dog 
        | `Dog    dog -> failwith "Dog cannot follow another dog" in 
      `Dog !dog 
    ) person_e in 
    dog_e, React.S.hold (`Dog !dog) dog_e   


  (** Side effect event to print the world every time either a dog 
      or a person is updated.
   *)
  let print_e world characters_e : unit React.event= 
    let select_e = React.E.select (List.map fst characters_e ) in 
    React.E.map (fun _ -> 
      let characters = List.map (fun (_,s) -> React.S.value s) characters_e in 
      Display.print world characters;
      flush stdout
    ) select_e  
       
end 

let () = 

  Random.self_init (); 

  let world  = (0, 80) in 
  let person = Person.create world '!' in 
  let dog1    = Dog.create world '@' in 
  let dog2    = Dog.create world '@' in 

  let (time_e:unit React.event), send = React.E.create () in  
  let person_e = Events.person_e person world time_e in 
  let dog1_e    = Events.dog_e dog1 world (fst person_e) in 
  let dog2_e    = Events.dog_e dog2 world (fst person_e) in 
  let (_:unit React.event) = Events.print_e world [person_e;dog1_e;dog2_e] in 
  
  for i=0 to 1_000 do 
    Display.burn_cpu ();
    let (_:unit) = send () in 
    () 
  done 




