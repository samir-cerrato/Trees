
Control.Print.printDepth := 100;
use "hw05-lib.sml";

(* ---------------------------------------------------------------------- *)
(* contains *)

(*Purpose: returns true if i is an element of t and returns false if not
Example:
contains ([1,2,3,4,5,6,8] , 1) = true
contains ([1,2,3,4,5,6,8] , 5) = true 
contains ([1,2,3,4,5,6,8] , 0) = false
contains ([1,2,3,4,5,6,8] , 7) = false*)
fun contains(t : tree, i : int) : bool = 
    case t of
      Empty => false
      | Node(l,x,r) => case (x=i) of
                        true => true
                        | false => case (i<x) of
                                    true => contains(l,i)
                                    | false => contains(r,i)

fun test_contains() = 
    (testb "c1" (contains (fromlist [1,2,3,4,5,6,8], 1)) true;
    testb "c2" (contains (fromlist [1,2,3,4,5,6,8], 5)) true;
    testb "c3" (contains (fromlist [1,2,3,4,5,6,8], 0)) false;
    testb "c4" (contains (fromlist [1,2,3,4,5,6,8], 7)) false
     )

(* ---------------------------------------------------------------------- *)
(* quicksort on trees *)

fun combine (t1 : tree, t2 : tree) : tree = 
    case t1 of
        Empty => t2
      | Node(l1,x1,r1) => Node(combine(l1,r1),x1,t2)

(* Task *)
(*Purpose: filters out the nodes greater than i in t and returns a tree
with the nodes less than i *)
fun filter_less (t : tree, i : int) : tree =
    case t of 
      Empty => Empty
      | Node(l,x,r) => case x < i of 
                        true => Node(filter_less(l,i), x, filter_less(r,i))
                        | false => combine(filter_less(l,i), filter_less(r,i))

(*Purpose: filters out the nodes less than i in t and returns a tree
with the nodes greater than i*)                        
fun filter_greatereq (t : tree, i : int) : tree = 
    case t of
      Empty => Empty
      | Node(l,x,r) => case x >= i of
                        true => Node(filter_greatereq(l,i), x, filter_greatereq(r,i))
                        | false => combine(filter_greatereq(l,i), filter_greatereq(r,i))

(* filter is hard to test on its own because the order of the items in the resulting tree 
   is up to you.  you should try running filter on some trees in smlnj and 
   see if the results look reasonable.*)

val a_tree = Node(Node(leaf 1, 2, leaf 3), 4, Node(leaf 5, 6, leaf 7))
    
(* Task *)
(*Purpose: Sorts a tree from a pivot. The left hand side will contain the nodes less than the pivot.
The right side will have the nodes greater than the pivot. Then both sides are combined into a single,
sorted tree,
Examples:
quicksort_t [8,1,4,5,6,7,3,2] = [1,2,3,4,5,6,7,8] 
quicksort_t [9,1,4,2,3,5] = [1,2,3,4,5,9]
quicksort_t [10,5,19,38,4,6,69] = [4,5,6,10,19,38,69]
quicksort_t [2,1,3,4,8,12,62,50] = [1,2,3,4,8,12,50,62]*)
fun quicksort_t (t : tree) : tree =
    case t of 
      Empty => Empty
      | Node(l,x,r) => Node(quicksort_t(combine(filter_less(l,x), filter_less(r,x))), 
                            x, quicksort_t(combine(filter_greatereq(r,x), filter_greatereq(l,x))))
      

fun test_quicksort()=
    ((* one way to test is to convert to and from lists -- 
        this way different arrangements of the same items in a tree 
        won't affect the results
        *)
     testil "f1" (tolist (quicksort_t (fromlist [8,1,4,5,6,7,3,2]))) [1,2,3,4,5,6,7,8];
     testil "f2" (tolist (quicksort_t (fromlist [9,1,4,2,3,5]))) [1,2,3,4,5,9];
     testil "f3" (tolist (quicksort_t (fromlist [10,5,19,38,4,6,69]))) [4,5,6,10,19,38,69];
     testil "f4" (tolist (quicksort_t (fromlist [2,1,3,4,8,12,62,50]))) [1,2,3,4,8,12,50,62]
     )
    
(* ---------------------------------------------------------------------- *)
(* rebalance *)

(* Task *)
(*Purpose: seperates a tree T into left and right subtrees, T1 and T2 respectively.
T1 contains the leftmost i elements of T, in their orignal order, and T2 the remaining elements,
also in their original order.
Example:
takeanddrop (Node(leaf 1, 2, leaf 3), 0) = (Empty, Node(leaf 1, 2, leaf 3)
takeanddrop (Node(leaf 1, 2, leaf 3), 1) = (leaf 1, Node(Empty, 2, leaf 3)
takeanddrop (Node(leaf 1, 2, leaf 3), 2) = (Node(leaf 1, 2, Empty), leaf 3)
takeanddrop (Node(leaf 1, 2, leaf 3), 3) = (Node(leaf 1, 2, leaf 3), Empty*)
fun takeanddrop (t : tree, i : int) : tree * tree = 
    case t of 
      Empty => (Empty, Empty)
      | Node(l,x,r) => case i > size(l) of 
                          true => let val (sl,sr) = takeanddrop(r,i-1-size(l))
                                  in (Node(l,x,sl),sr) end
                            | false => let val (dl,dr) = takeanddrop(l,i)
                                        in (dl, Node(dr,x,r)) end

fun test_tad()=
    (
     testtt "tad1" (takeanddrop (Node(leaf 1, 2, leaf 3), 0)) (Empty, Node(leaf 1, 2, leaf 3));
     testtt "tad2" (takeanddrop (Node(leaf 1, 2, leaf 3), 1)) (leaf 1, Node(Empty, 2, leaf 3));
     testtt "tad3" (takeanddrop (Node(leaf 1, 2, leaf 3), 2)) (Node(leaf 1, 2, Empty), leaf 3);
     testtt "tad4" (takeanddrop (Node(leaf 1, 2, leaf 3), 3)) (Node(leaf 1, 2, leaf 3), Empty)
     )
    
(* the rest of rebalance interms of your takeanddrop *)
fun halves (t : tree) : tree * int * tree =
    let
      val (l , vr) = takeanddrop (t , (size t) div 2)
      val (Node (Empty, v , Empty) , r) = takeanddrop (vr , 1)
    in
      (l , v , r)
    end

fun rebalance (t : tree) : tree =
    case t
     of Empty => Empty
      | _ =>
        let
          val (l , x , r) = halves t
        in
          Node (rebalance l , x , rebalance r)
        end

(* ---------------------------------------------------------------------- *)

fun run() =
    (test_contains();
     test_quicksort();
     test_tad())