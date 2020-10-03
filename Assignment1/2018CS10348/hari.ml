exception InvalidInput;;
exception UnequalVectorSize;;
exception UnequalMatrixShape;;
exception IncompatibleMatrixShape;;
exception SingularMatrix;;


(*VECTOR IMPLEMENTATIONS*)

(*find dimension of vector*)
(** this is rec function in which i map the [] to 0 and the vector is non-empty then i will take out first 
                element of vector and the add 1 to next rec call on remaining vector.
                nd this is how atlast we will remove every element of vector one by one with adding oe for every element in vector.*)

let rec vdim (v:vector): int= match v with
	[]-> 0					(** Base case*)
	|x::xs-> 1+ vdim(xs);;			(** Induction steps *)
(*make zero vector *)
(**  if int =0 then return [] else call rec function which will add 0 to empty vector with decresing the 
                const int and when int become zero return then vector.*)
let rec makearr1 n list f1= 
	if n=0 then list else makearr1 (n-1) (f1 list) f1;;      (* it is like accumalator, it will insert 0.0 , n number of times..*)   
let f1 acc= 0.0::acc;;			(* inserting the 0.0 in given list*)

let rec mkzerov (n:int) :vector= makearr1 n [] f1;;

(**find zero vetor or not  *)
(* Here i have made a flag that will update for every element of vector if any element is not 0 then 
                it will come out from recurssion and return false;*)
let arrzero1 list = if vdim list =0 		(* base case*)
					then false
			      else 
					clear1 list;;
						
let rec clear1 list = match list with
		[]-> true
		| x::xs-> if (x/.1.0)<>0.0 then  	(* checking if division give 0.0 as quotiont as 0.0 divided by 1.0 will give 0*)
		
			false 
		else 
			clear1 xs;;

let iszerov (v:vector): bool= arrzero1 v;;

(**adding two vector  *)
(* Here we take hd of both vectors and add them nd append to new vector.
                here if any of them is empty then we are returning others vector.*)
let rec addv (v1:vector) (v2:vector): vector= match v1 with 
		[]-> v2			(* base case if one vector is empty then return other *)
		|x::xs-> (match v2 with	(* induction steps for vector v1 *)
			[]->v1		(* base case for v2*)
			|y::ys-> (x+.y)::(addv xs ys) (*induction steps for v2 *)
			);;

(**scalar multiplication of vector *)
(*  here we take hd of vector and multiple it by float and append to a vector. If vector is empty then
                retunr empty vector.*)
let rec scalarmultv (c:float) (v:vector):vector=
	match v with 
	[]->[]			(* base case for v*)
	|x::xs-> (c*.x)::scalarmultv c xs;; 	(*induction steps for v *)

(** dot product of vectors*)
(*   Here we take the hds of both the vectors and multiple them and it to the next call of dotprodv on tls
                of vectors. If any vector is empty then we returning 0.*)
let rec dotprodv (v1:vector) (v2:vector):float=match v1 with	
	[]-> 0.0		(* base case for v1*)
	|x::xs-> (match v2 with 	(* indunction step for v1*)
		[]-> 0.0		(* base case for v2*)
		|y::ys-> (x*.y)+. (dotprodv xs ys) (* indunction step for v2*)
	) ;;
(* cross product of two vector*)
(* As this is vectors of dim 3 that is why I have access element using get_nth function which will return
                the element of any vector given it index.
                So simply i have used formula and return the vector as output.*)
let rec get_nth3 list m l=match list with
	[]-> raise (Failure "Error")		(* base case if the list is [] then raise error*)
	| x::xs-> if l!=m then 
			get_nth3 xs m (l+1)	(* rec function till the counter is not equal to index position*)
		else				(*if counter is equal to index then exit form rec *)
			x;;	
let rec get_nth2 (list:vector) m= get_nth3 list m 0;;
 let rec crossprodv (v1:vector) (v2:vector):vector= 
	if vdim v1=0 || vdim v2=0 then 	(* if any of them is equal to o dimensional matrix then return []*)
	[]
	else				(*acccesing the elemests of vector and applied mathematical formula *)
		[(get_nth2 v1 1)*.(get_nth2 v2 2)-.(get_nth2 v1 2)*.(get_nth2 v2 1);
		-.(get_nth2 v1 0)*.(get_nth2 v2 2)+.(get_nth2 v2 0)*.(get_nth2 v1 2);
		+.(get_nth2 v1 0)*.(get_nth2 v2 1)-.(get_nth2 v1 1)*.(get_nth2 v2 0)];;
(*finsih vector *)



`(*) for finding the length of matrix*)
let rec length arr= match arr with 
  	[]-> 0			(* Base case *)
  	| x::xs-> 1+length xs;;		(*induction steps for xs*)

let rec mdim (m:matrix):int*int= match m with
  	[]->(0,0)	(*base case*)
  	| x::xs -> (length m, length x);; 


(*) for finding the matrix is zero matrix or not *)
(*  Here i have applied the iszerov function for every elements of matrix. As i have already mentioned the correctness of iszerov 
                so it will work correct. Now I have checked the is all outputs of iszeorv are true or not if not then that is not zero matrix else 
                it is zero matrix.
                Base cases if the matrix is empty ( [] ) then return false.*)
let arrzero list = if length list =0 (* base cse *)
					then false 
			      else 
					clear list;;
						
let rec clear list = match list with
		[]-> true	(*base case*)
		| x::xs-> if (x/.1.0)<>0.0 then false else clear xs;;

let iszerom (list:matrix):bool = if length list =0 
					then false
				else	clear1 list;;
						  

let rec clear1 list = match list with 
		[] -> true
		| x::xs -> if arrzero x= false then
					false
				else
					clear1 xs;;

(*) return zero matrix *)
(*  Here first I have calcaluted zero vector of the given dimension. As i have mentioned earlier the correctness of calcaluting the 
                zero vector so it will calculate right. Now what i have done is I have insert that that zeros vector till the dimension for rows
                as we have worked out zero vector where we were inserting 0 and here we are inserting zero vectors.*)
let rec makearr n list f= 
	if n=0 then list else makearr (n-1) (f list) f;;	(* it is like accumalator, it will insert 0.0 , n number of times..*)  
let f acc= 0.0::acc;;

let rec mkz n m list= if  n=0 then list
	else 
		mkz (n-1) m (fu m list);;

let fu m list=    
  let list1= makearr m [] f in 	(* inseritng all zerov in a list*)
  list1:: list;;

let rec mkzerom (m_:int)(n_:int):matrix= 
		if m_<0 || n_<0 then		(* invalid input*)
			raise (Failure "InvalidInput")
		else
		mkz m_ n_ [];;
(*adding two matrix *)
(*  As we have made addv and we have given the correctness of addv. So what i am doing is, I am taking every elements form both matrixex and then 
                adding them like addv.*)
let rec listadd a b = match a with
  []-> b			(* same as addv*)
  | x::xs-> (match b with
  	[]-> a	
   	| y::ys->(x+.y)::(listadd xs ys)
	);;

let rec addm (a:matrix) (b:matrix)=match  a with
	[]-> b			(*base case for a *)
	| x::xs -> (match b with  (* induction step for a*)
	[]-> a				(* base case for b*)
	| y::ys-> (listadd x y)::(addm xs ys)		(*indunction steps for b:adding the two vectors of two matrix and inerting them into one *)
 );;

(*) scalar multiplication*)
let rec mult list a= match list with
	[]-> []				(* base cse*)
	| x::xs-> (a*.x)::mult xs a;;		(* indunctio steps for list mult*)
let rec scalarmultm (m:float) (list:matrix):matrix= match list with
  []-> []			(* base case*)
  | x::xs-> (mult x m)::scalarmultm m xs;;		(*induction step for matrox: applying mult on every element of matrix  *)

(*unit matrix..*)
let rec unit m n l list f1 = if l!=m then 		(*it is like accumalator which will accumalate a list *)
						if l=n then 	(*if the index = counter then insert 1.0 *)
							unit m n (l+1) ( f1 list 1.0) f1 
						else (*if the index != counter then insert 0.0 *)
							unit m n (l+1) (f1 list 0.0) f1 
					else 
						list;;	 


let f1 acc k= k::acc;;				(** inserting a element into list*)


let rec mkunitm1 m n list = if m=0 then list else 		(*after getting a vector we will append it ti new list *)
					mkunitm1 (m-1) n (f2 n (m-1) list);;
let f2 m k list= let list1= unit m k 0 [] f1 in
		list1::list;; 		

let rec rev' list res= match list with
		[]-> res		(* base case*)
	| x::xs-> rev' xs (x::res);;	(* induction step for rev' *)


let mkunitm (m_:int):matrix=  
			if m_< 0 then 	(* base case:if m <0 then raise failure*)
				raise InvalidInput
			else
			let list1=mkunitm1 m_ m_ [] in
			rev' list1 [];;




(*check whether it is unit matrxi or not *)

let rec check3 list m n flag l= if m=0 then flag else
if l!=m then
	match list with
	| x::t-> if flag !=false then
					if l=n then
					
					let a= checkmatch x 1.0  in 
					check3 t m n a (l+1)
					else 
					let a= checkmatch x 0.0   in 
					check3 t m n a (l+1)
	 		else 
			flag

else 
	flag;;	




let checkmatch x y=x=y;;

let rec isunitm1 (list:matrix) m l flag= match list with 
		[]-> flag
	| x::xs-> if flag =false then 
				false
			else
				let a= check3 x m (l) true 0 in
				isunitm1 xs m (l+1) a;;
	
(*let a= [[1.0;0.0;0.0];[0.0;1.0;0.0];[0.0;0.0;1.0]];;*)
let rec isunitm (m:matrix): bool= 
	if m=[] then raise InvalidInput 
	else 
	match m with
	|x::xs-> isunitm1 m (length x) 0 true;;



(*let b= [[1;0;0];[0;1;0];[0;1;1]];;*)

(* finding multiplicaiton of two matrxes*)

let rec mapn f lists =
  assert (lists <> []);
  if List.mem [] lists then
    []
  else
    f (List.map List.hd lists) :: mapn f (List.map List.tl lists);;
 
let multm (m1:matrix) (m2:matrix) :matrix=
let (a,b)= mdim m1 in	
let (c,d)= mdim m2 in
	if (b!=c) then 
	raise InvalidInput else 
  List.map
    (fun row ->
      mapn
       (fun column ->
         List.fold_left (+.) 0.0
          (List.map2 ( *. ) row column))
       m2)
    m1;;

(*mult [0;0;18] [[1;2;3];[1;2;3];[1;2;3]] 0;;
multm [[0.0;0.0;18.0]] [[1.0;2.0;3.0];[1.0;2.0;3.0];[1.0;2.0;3.0]];;*)

(*inding transpose of a matrix*)
let rec transm (list:matrix):matrix= match list with
	[] ->[]
	|[]::xss-> transm xss
	| (x::xs)::xss-> 
			(x::List.map List.hd xss)::transm(xs::List.map List.tl xss);;

(*transm [[1.0;2.0;3.0];[1.0;2.0;3.0];[1.0;2.0;3.0]] ;;*)


(*finding the determinant of matrix*)
let rec get_nth1 list m l=match list with
	[]-> raise (Failure "Error")
	| x::xs-> if l!=m then 
			get_nth1 xs m (l+1)
		else	
			x;;
let rec get_nth list m= get_nth1 list m 0;;



let det_uniq list= 
		 (get_nth( get_nth list 0) 0)*.(get_nth( get_nth list 1) 1)-.(get_nth( get_nth list 0 ) 1)*.(get_nth( get_nth list 1) 0);;

let rec delete list m lst l=match list with	
	[]->List.rev lst
	| x::xs-> if l=m then 
			delete xs m lst (l+1)
		else	
			delete xs m (x::lst) (l+1);;
let rec even a= if a mod 2 =0 then 1 else -1;; 
let rec deletem list m acc i = 

	match list with	
	[]->  List.tl (List.rev acc)
	|x::xs-> 
		let l=	get_nth list i in 
		let cc= (delete l m [] 0)::acc in
		deletem xs m cc i;;
let rec detm_helper (m: matrix) (i:int) = match m with
               []->1.0
          |    x::xs ->
                    (if i > List.length m then
		     0.0
                    else if i mod 2 = 1 then 
			(get_nth (List.hd m) (i-1)) *. (detm_helper (deletem m (i-1) [] 0) 1 ) +. detm_helper m (i+1)
                    else 
		    	((-1.0) *. (get_nth (List.hd m) (i-1))) *. (detm_helper (deletem m (i-1) [] 0) 1) +. detm_helper m (i+1));;

let rec detm (m:matrix): float = detm_helper m 1;;



(*let a= [[2.0;3.0;4.0];[5.0;6.0;4.0];[7.0;8.0;1.0]];;
9
let b=[[4.0;5.0;8.0;4.0;6.0];[1.0;45.0;8.0;1.0;2.0];[7.0;15.0;1.0;54.0;1.0];[4.0;5.0;2.0;7.0;6.0];[9.0;70.0;10.0;11.0;12.0]]
-78240*)


(*finding inverse of matrix*)
let rec remove (v: vector) (i:int) (a: int) = if i=a then List.tl v else List.hd v :: remove (List.tl v) (i+1) a;;

let rec cofactorm (m1: matrix) (a:int) (b:int): matrix =
                         if m1 = [] then []
                         else if a = 1 then cofactorm (List.tl m1) (a-1) b
                         else (remove (List.hd m1) 1 b) :: cofactorm (List.tl m1) (a-1) b;;
let rec minor (m: matrix) (i:int) = if i mod 2 = 0 then detm m else (-1.0) *. detm m;;
let rec cofactorrow (m: matrix) (i:int) (a:int) = if i > List.length m then 
						[]
                                   else 
				   (minor (cofactorm m a i) (i+a)) :: cofactorrow m (i+1) a;;
let rec cofactormatrix (m: matrix) (i:int) = if i > (List.length m) then []
                              else cofactorrow m 1 i :: cofactormatrix m (i+1);;

let rec adjoinm (m: matrix) = transm (cofactormatrix m 1);;

let rec invm (m:matrix): matrix = if m=[] then
		raise InvalidInput 
		else if	(detm m)=0.0 then 
			raise SingularMatrix else
		scalarmultm (1.0 /. detm m) (adjoinm m);;



