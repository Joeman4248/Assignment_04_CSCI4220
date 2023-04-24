(* Joseph Clarke
 * CSCI4220
 * Assignment 4
 * Due: 04/25/2023 *)


print "\n--------------------  Problem 1  --------------------\n";

fun alternate([], []) = []
  | alternate(x::xs, y::ys) = x::y::alternate(xs, ys)
  | alternate(xs, ys) = raise Fail "Arrays are of unequal length";

alternate([1, 3, 5], [2, 4, 6]);
alternate(["a", "c", "e"], ["b", "d", "f"]);

print "\n--------------------  Problem 2  --------------------\n";

fun minus(xs, []) = xs
  | minus([], ys) = []
  | minus(x::xs, y::ys) = 
		if (x = y) then 
			minus(xs, ys)    (* do not add x to return list *)
  		else if (x > y) then 
			minus(x::xs, ys) (* move onto next element in ys *)
		else
			x::minus(xs, y::ys); (* add x to returned list *)

minus([1, 1, 1, 2, 2], [1, 1, 2, 3]);
minus([1, 1, 2, 3], [1, 1, 1, 2, 2]);

print "\n--------------------  Problem 3  --------------------\n";

fun union([], ys) = ys (* once xs is exhausted, add ys to return list *)
  | union(xs, []) = xs
  | union(x::xs, ys) = 
		(* if x exists in ys *)
		if (List.exists(fn y => (x = y)) ys) then 
			union(xs, ys)     (* do not add x to return list *)
		else 
			x::union(xs, ys); (* add x to return list *)

union(["a", "b", "c", "d", "e"], ["c", "d", "e", "f", "g"]);
union([1, 2, 3, 4, 5], [4, 5, 6, 7, 8]);

print "\n--------------------  Problem 4  --------------------\n";

fun intersection([], _) = []
  | intersection(x::xs, ys) = 
		(* if x exists in ys *)
		if (List.exists(fn y => (x = y)) ys) then
			x::intersection(xs, ys)
		else
			intersection(xs, ys);

fun multiSetIntersection([]) = []
  | multiSetIntersection([xs]) = xs
  | multiSetIntersection(xs::xss) = intersection(xs, multiSetIntersection(xss));

(* intersection test *)
(* intersection([1, 2, 3, 4, 5], [4, 5, 6, 7, 8]); *)

multiSetIntersection([ [1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7] ]);
multiSetIntersection([ [1, 8, 7, 3], [8, 1, 6, 3], [7, 5, 1, 3], [6, 3, 4, 1] ]);

print "\n--------------------  Problem 5  --------------------\n";

fun crossProduct(set1, set2) = 
	List.concat(List.map(fn a => List.map(fn b => (a, b)) set2) set1);

crossProduct([1, 2, 3], [4, 5, 6]);

print "\n--------------------  Problem 6  --------------------\n";

fun powerset([]) = [[]] 
  | powerset(x::xs) = 
		let
			val next_step = powerset(xs)
		in
			(List.map(fn a => x::a) next_step) @ next_step
		end;

powerset([1, 2]);
powerset([1, 2, 3]);

print "\n--------------------  Problem 7  --------------------\n";

fun finiteListRepresentation(f, n) = 
	let 
		val range = List.tabulate(n, fn i => i+1); (* [1, 2, ... n] *)
	in 
		List.map(fn x => (x, f(x))) range
	end;

finiteListRepresentation(fn x => x*x, 5);
finiteListRepresentation(fn x => 2*x, 5);

print "\n--------------------  Problem 8  --------------------\n";

fun update_helper([], new_pair, added) = []
  | update_helper(list, new_pair, true) = list
  | update_helper(old_pair::list, (x, y), false) = 
		val (v, _) = old_pair
		if x > v then      (x, y)::old_pair::update_helper(list, (x, y), true)
		else if x = v then (x, y)::update_helper(list, (x, y), true)
		else               old_pair::update_helper(list, (x, y), false);

fun update(list, new_pair) = List.rev(update_helper(List.rev(list), new_pair, false));

val FLR = finiteListRepresentation(fn x => x*x, 5);

update(FLR, (2,3));
update(FLR, (6,36));
