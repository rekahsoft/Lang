(* (C) Copyright Collin Doering 2013
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.

 * File: rdm.sml
 * Author: Collin J. Doering <rekahsoft@gmail.com>
 * Date: Jan 30, 2013
 *)

(* int -> int option *)
fun factorial (n : int) = 
    let
	(* int * int -> int option *)
	fun fact (n : int, acc : int) =
	    if n > 0 then fact(n - 1, n * acc) else acc
    in
	if n < 0 then NONE else SOME (fact(n,1))
    end

(* int * int -> int option *)
fun expodentiate (base : int, exp : int) = 
    let
	(* int * int -> int *)
	fun expodentiateh (exp : int, acc : int) =
	    if exp = 0 then acc else expodentiateh(exp - 1, base * acc)
    in
	if exp < 0 then NONE
	else if base = 0 then SOME 0
	else SOME (expodentiateh(exp, 1))
    end

(* ('a * 'b -> 'b) * 'b * 'a list -> 'b  *)
fun myfoldr (f, i, xs) =
    if null xs then i else f(hd xs, myfoldr(f, i, tl xs))


fun mymap f [] = []
  | mymap f (x::xs) = f(x)::mymap f xs

(* de-sugared version of the above function mymap *)
fun mymap' f =
    fn xs => case xs of
		 [] => []
	       | x::xs => f(x)::(mymap' f xs)

(* ('a -> 'b) * 'a list -> 'b list *)
fun mymap2 f xs =
    let fun aux [] acc = acc
	  | aux (x::xs) acc = aux xs (f(x)::acc)
    in
	rev (aux xs [])
    end

datatype ThreeNums = ONE | TWO | THREE

(* ThreeNums -> Int *)
fun evalthreenum ONE = 1
  | evalthreenum TWO = 2
  | evalthreenum THREE = 3

(* a few cool functions having to do with currying *)

(* ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
fun curry f a b = f(a,b)
(* de-sugared: fun curry f = fn a => fn b => f(a.b) *)

(* ('a -> 'b -> 'c) -> ('a * 'b -> 'c) *)
fun uncurry f (a,b) = f a b
(* de-sugared: fun uncurry f = fn (a,b) => f a b *)

(* A list datatype *)
datatype 'a List = Nil
		 | Cons of 'a * 'a List

fun list2nativelist Nil = []
  | list2nativelist (Cons(x,xs)) = x::(list2nativelist xs)

datatype 'a BTree = Empty
		  | Node of 'a * 'a BTree * 'a BTree

fun leaf a = Node(a,Empty,Empty)

(* 'a BTree -> 'a -> 'a BTree *)
fun insert a Empty = leaf a
  | insert a (Node(b,Empty,Empty)) = if a = b
				     then leaf b
				     else if a < b
				     then Node(b,leaf a,Empty)
				     else Node(b,Empty,leaf a)
  | insert a (Node(b,rhs,lhs)) = if a = b
				 then Node(b,rhs,lhs)
				 else if a < b
				 then Node(b,insert a rhs,lhs)
				 else Node(b,rhs,insert a lhs)

val tr = Node(2,leaf 1,leaf 3)
