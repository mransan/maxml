### TODO 

#### Fields
group and map fields
verify all the litterals:
https://developers.google.com/protocol-buffers/docs/reference/proto2-spec#string_literals


#### Mapping 


```Javascript
message A {
  oneof choice1 {
    int64  c1  = 1;
    string c2  = 2;
    double c3  = 3; 
  }
}
```

will map to 

```OCaml

type A_choice1 = 
 | C1 of int64
 | C2 of string 
 | C3 of float 

type A = {
    choice1 : A_choice1; 
}
```

#### Note on scoping 

##### Types 

The compiler is manipulating 2 kinds of scope in order to resolve field type to the appropriate message type. 

First a field can be defined in the following way:

```Javascript
required p1.p2.Msg1.Msg2 x = 1 
```

where n1,n2 are package names while Msg1,Msg2 are message definition. 

The first type of scope we defined is for the field type in file [astc.ml](astc.ml):
```OCaml
Astc.field_scope = string list 
```
It is defined as a string since we cannot extract more semantic information from the type definition. (ie there is nothing preventing a user from defining a package name following the naming convention (not syntax rule) of messages. 


Each message is defined within its own scope which comes from either outer most message definition as well as package declaration. This scoping can clearly be extracted at parsing time. Hence the following definition:

```OCaml
type message_scope_item = 
  | Namespace of string 
  | Message_name of string 

type message_scope = message_scope_item list 
```

Note that this type allows for package definition within a message definition. This is not allowed by the protobuf specification and is therefore enforced at runtime. 


##### Resolving types

In order to resolve type we first transform message scope into field scope and then keep on searching the message in 
all the possible scope according to the protobuf specifications:

> Type name resolution in the protocol buffer language works like C++: 
> first the innermost scope is searched, then the next-innermost, and so on, 
> with each package considered to be "inner" to its parent package. 
> 
> A leading '.' (for example, .foo.bar.Baz) means to start from the outermost scope instead.


