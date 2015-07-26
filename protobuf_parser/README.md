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
