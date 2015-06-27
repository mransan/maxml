
module Sub =
  struct 
      type t = {strike: float ; ticker : string } 
  end;; 

module Test =
  struct 
    type t = 
      {
        strike : float; 
        ticker : string; 
        sub : Sub.t 
      }
      let  create s t = 
      {
        strike = s; 
        ticker = t ; 
        sub = {Sub.ticker = t^"Sub"; Sub.strike = 1.} 
      } 
  end;;


let t = Test.create 1.0 "test" in 

Printf.printf "1 - strike: %f, test: %s \n" 
               t.Test.strike 
               t.Test.sub.Sub.ticker;;

open Test;;

let t = create 1.0 "test" in 

Printf.printf "2 - strike: %f, test: %s \n" 
               t.strike 
               t.sub.Sub.ticker;;

(* In the case above strike, ticker and sub field name have been 
 * binded to the Test module fields. 
 *
 * Any Sub module fields must be explicitely declared
 *)

open Sub;;

let t = create 1.0 "test" in 

Printf.printf "3 - strike: %f, test: %s \n" 
               t.Test.strike 
               t.sub.ticker;;
(* In the case Test.strike had to explicitely be defined 
 * The reason is that Sub was the latest module opened, hence
 * redefining the strike field at global scope (ie bindind 
 * the strike field to Sub.strike)
 *
 * t.sub.ticker does not have to be fully defined as 
 * t.Test.sub.Sub.ticker because 
 *   a) sub field name is unique and binded to Sub.sub
 *   b) ticker field name is not unique but binded to Sub.ticker
 *      which matches in this case
 *)


let t = {ticker = "sub"; strike = 2.0} in 

Printf.printf "4 - test: %s \n" 
               t.ticker ;;
(* Because Sub was the latest module opened, ticker and
 * strike are currenctly binded to the fields in Sub.
 *)

open Test;;

(* Now that test is the latest module open the code 
 * above won't compile anylonger. ticker and strike are now 
 * binded to the fields in Test module and therefore the compiler
 * computes that t is of Test.t which means that the variable
 * must also defined the sub fields:
 *)

let t = 
  {
    ticker = "test"; 
    strike = 1.0; 
    sub = {Sub.ticker = "sub"; Sub.strike = 2.0}
  }


