(* Joseph Clarke
 * CSCI4220
 * Assignment 4
 * Due: 04/25/2023 *)


print "\n--------------------  Problem 1  --------------------\n";

exception List_Length_Exception;

fun alternate([], []) = [] 
  | alternate(v1 :: list1, v2 :: list2) = v1 :: v2 :: alternate(list1, list2)
  | alternate(list1, list2) = raise List_Length_Exception; 

alternate([1, 3, 5], [2, 4, 6]);


print "\n--------------------  Problem 2  --------------------\n";

fun minus(list1, []) = list1
  | minus([], list2) = []
  | minus(v1 :: list1, v2 :: list2) = 
		if (v1 = v2)      then minus(list1, list2)			(* don't add element to ret. array *)
  		else if (v1 > v2) then minus(v1 :: list1, list2)	(* move onto next element in list2 *)
		else              v1 :: minus(list1, v2 :: list2);  (* add element to ret. array *)

minus([1, 1, 1, 2, 2], [1, 1, 2, 3]);
minus([1, 1, 2, 3], [1, 1, 1, 2, 2]);

print "\n--------------------  Problem 3  --------------------\n";

fun union([], set2) = set2
  | union(v1 :: set1, set2) = 
		
