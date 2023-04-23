print "\n - Course Name: CSCI4220 Principles Of Programming Languages\n - Student Name: Russell Brogan\n - Homework: Assignment #4\n - Due Date: April 25, 2023 - 11:59PM\n\n";

print "\n( ------------- Problem 1 ------------- )\n";

exception List_Length_Exception;
val list1 : int list = [1, 3, 5]
val list2 : int list = [2, 4, 6]
fun alternate([], []) = []
| alternate(x :: list1, y :: list2) = x :: y :: alternate(list1, list2)
| alternate(list1, list2) = raise List_Length_Exception;
alternate(list1, list2);

print "\n\n( ------------- Problem 2 ------------- )\n";
val list1 : int list = [1, 1, 1, 2, 2]
val list2 : int list = [1, 1, 2, 3]
fun minus(list1, []) = list1
| minus([], list2) = []
| minus(x :: list1, y :: list2) = 
if x > y 
then minus(x :: list1, list2)
else if x = y 
then minus(list1, list2)
else x :: minus(list1, y :: list2);

minus(list1, list2);
minus(list2, list1);

print "\n\n( ------------- Problem 3 ------------- )\n";
val s1 : string list = ["a", "b", "c", "d", "e"]
val s2 : string list = ["c", "d", "e", "f", "g"]
fun union_helper x [] = false
| union_helper x (y::ys) = x = y orelse union_helper x ys;

fun union ([],s2) = s2
| union (x::xs,s2) = 
	if union_helper x s2 
	then union(xs, s2) 
	else x::union(xs, s2);
	union (s1, s2);

print "\n\n( ------------- Problem 4 ------------- )\n";
val s1 : int list = [1, 2, 3, 5]
val s2 : int list = [1, 3, 4]
val s3 : int list = [1, 2, 3, 4, 5]
val s4 : int list = [2, 3, 4]

fun intersection([], _) = []
| intersection(x::xs, ys) = 
if List.exists(fn y => x = y) ys
then x :: intersection(xs, ys)
else intersection(xs, ys);

fun multiSetIntersection([]) = []
| multiSetIntersection([xs]) = xs
| multiSetIntersection(xs::xss) = intersection(xs, multiSetIntersection(xss));
multiSetIntersection([s1, s2, s3, s4]);

print "\n\n( ------------- Problem 5 ------------- )\n";
val S1 : int list = [1, 2, 3]
val S2 : int list = [2, 4, 6]
fun CartesianProduct(S1, S2) = 
List.concat(List.map(fn x => List.map(fn y => (x, y)) S2) S1);
CartesianProduct(S1, S2);

print "\n\n( ------------- Problem 6 ------------- )\n";
val list_of_numbers : int list = [1, 2, 3]
fun powerset_helper x y = x::y
fun powerset([]) = [[]]
  | powerset
  | powerset(x::y) = 
		let
			val powerset = powerset(y)
		in

		end;
	
powerset(list_of_numbers);

print "\n\n( ------------- Problem 7 ------------- )\n";
fun posIntegerSquare x = x * x;
fun finiteListRepresentation(x, y) = 
let
    fun finiteListRepresentation_helper(list, 0) = list
    | finiteListRepresentation_helper(list, count) = finiteListRepresentation_helper((count, x count)::list, count - 1)
in
    finiteListRepresentation_helper([], y)
end;
finiteListRepresentation(posIntegerSquare, 5);


print "\n\n( ------------- Problem 8 ------------- )\n";
val FLR : (int * int) list = [(1,1),(2,4),(3,9),(4,16),(5,25)]
fun insert([], list) = list
| insert(a::b, list) = a :: insert(b, list)
fun update_helper FLR(x, y) list =
case FLR of [] => insert(list, [(x, y)])
| (x1, y1)::xs => 
if(x = x1)
then (insert(insert(list, [(x, y)]), xs))
else(update_helper xs (x, y) (insert(list, [(x1, y1)])))
fun update(FLR, (x, y)) = update_helper FLR(x, y) [];
update(FLR, (2, 3));
update(FLR, (6, 36));
