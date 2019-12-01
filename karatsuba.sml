(* This function takes a string and returns a list of string each of size 4 except the first element whose size can be anything between 1 and 4. It does it by splitting it.   *)

fun listString(s) = if (size(s) <= 4) then [s]
                    else listString(substring(s,0,size(s)-4)) @ [(substring(s,size(s)-4,4))];

(* This function takes a list of string and converts it into a list of integer.*)
fun getInt([]) = [] 
|   getInt(x::y) = [valOf(Int.fromString(x))] @ getInt(y);


(* This function takes a string and converts it into a list of integers by calling the function (getInt[]) and (listString(s)) *)
fun fromString(s) = getInt(listString(s));


(* This function takes a list of integer and remove the zero element from the starting until it encounters a non zero element.*)
fun trim([]) = []
|   trim([0]) = [0]
|   trim(l1 as x1::y1) = if x1 = 0 then trim(y1)
                         else l1;

(* This function takes a list of integer and convert each element into a string of size of 4 *)
fun toStr([]) = ""
|   toStr( l2 as x2::y2) =  let val a = (Int.toString(x2))
				val b = size(a)                             
			    in if b = 4 then (a ^ toStr(y2))
                               else if b = 3 then ("0" ^ a ^ toStr(y2))
                                    else if b = 2 then ("00" ^ a ^ toStr(y2))
                                         else ("000" ^ a ^ toStr(y2))
                            end;
(* This function takes a list of integer and converts it into a string  with the help of toStr([]) function. *)
fun toString([]) = ""
|   toString(l1 as x1::y1) = let val l2 = trim(l1)
                             in (Int.toString(hd(l2))) ^ (toStr(tl(l2)))
			     end;


(* This function takes a list of integers and size of list we want the final list to be,it adds zeroes to the start for making the list of desired size.*)
fun prefixzero(c::d, 0) = c::d
|   prefixzero(c::d, n) = [0000] @ prefixzero(c::d, n-1);

(*  This function takes  two list of integers and makes the both of identical size with help of prefixzero function *)
fun lengthequal([],[])=([],[]) 
   | lengthequal(l1 as x::y, l2 as z::t) = if length(l1)=length(l2) then (l1,l2)
                                           else if length(l1)>length(l2) then (l1,prefixzero(l2,length(l1)-length(l2)))
                                                else (prefixzero(l1,length(l2)-length(l1)),l2);

(* This function takes two list of integers and a carry and returns the sum output. It does the addition in reverse direction, it has been programmed like this so that we can pass the reverse of the list as argument into this function and its output would be reversed by the function calling it, this way one would get the desired sum. *)
fun sum([],[],0) = []
|   sum([],[],c) = [c]

|   sum([],l1,c) = if ((hd(l1)+c) mod 10000) = 0 then (0::(sum([],tl(l1),c)))
                   else (hd(l1)+c)::tl(l1)

|   sum(l1,[],c) = if ((hd(l1)+c) mod 10000) = 0 then (0::(sum(tl(l1),[],c)))
                   else (hd(l1)+c)::tl(l1)

|   sum(l1 as x1::y1, l2 as x2::y2, c) = let val s1 = x1 + x2 + c
					 in ((s1 mod 10000)) :: (sum(y1, y2, s1 div 10000)) 
					 end;

(* This funciton takes two list of integers, a borrow  and it is doing subtraction in reverse and its output will be reversed by the function calling it. The function calling it passes the reversed list as an argument into it. *) 

fun sub([],[],0) = []
|   sub(l1 as x1::y1,[],b) = if (b=0) then l1
                             else if (x1 = 0) then ([9999]@(sub(y1,[],1)))
                                  else ([x1-1] @ y1)
|   sub(l1 ,l2 ,b) = let val(s,borrow) = if (b <=0) then if( hd(l1) >= hd(l2)) then ((hd(l1)-hd(l2)),0)
                                                                            else ((hd(l1)+10000-hd(l2)),1)
							    else if ((hd(l1)-1) < hd(l2)) then ((hd(l1)+10000-1-hd(l2)),1) 	
								 else ((hd(l1)-1-hd(l2)),0) 
                     in ([s]@(sub(tl(l1),tl(l2),borrow)))
                     end;

(* This function takes two list and compares it, it returns 1 when 1st one is greater, 2 when 2nd one is greater and 0 when both are equal *)

fun comp([],[]) = 0
|   comp(l1,l2) = if (length(l1) > length(l2)) then 1
                  else if (length(l1) < length(l2)) then 2
                       else if(hd(l1) < hd(l2)) then 2
                            else if(hd(l1) > hd(l2)) then 1
                                 else comp(tl(l1), tl(l2));


(* This function takea two list, compares both with the help of comp([],[]) function and and passes the reverse of both lists as an argument to sub function *)

fun subtract(l1,l2) = let val(a,b) = (trim(l1),trim(l2))
                      in  if(comp(a,b) = 1) then sub(rev(a),rev(b),0)
                          else sub(rev(b),rev(a),0)
                      end;

(* This function takes two arguments, a list if integer and an integer. It appends the zero as per the the 2nd argument passed to it. *)
 
fun padding([],0)= []
 |  padding(l1,0)=l1
 |  padding(l1,p) = padding(l1 @ [0], p-1);


(* This is karatsuba function which takes two list of integers and returns the output after multiplying both list and returns the final list. It uses many function like padding, sum, subtract, trim just to complete the wholw multiplication process *)

fun karatsuba [] [] = []
|   karatsuba [] l1 = [0]
|   karatsuba l1 [] = [0]
|   karatsuba [a] [b] = fromString(Int.toString(a*b))
|   karatsuba (l1 as x1::y1) (l2 as x2::y2) = let val(l3,l4) = lengthequal(l1,l2)
					   val n = length(l3)
                                           val m = if((n mod 2) = 0) then n div 2
                                                   else ((n div 2) + 1)
					   val p = n - m
                       			   val x0 = List.drop(l3,p)
                                           val x1 = List.take(l3,p)
                       			   val y0 = List.drop(l4,p)
                                           val y1 = List.take(l4,p)
                       			   val z2 = karatsuba x1 y1
                       			   val z0 = karatsuba x0 y0
                       			   val s1 = rev(subtract (trim(x0),trim(x1)))
                       			   val s2 = rev(subtract(trim(y1),trim(y0)))
                                           val sign1 = comp(trim(x0),trim(x1))
                       			   val sign2 = comp(trim(y1),trim(y0))
                       			   val mul1 = karatsuba s1 s2
                       			   val t1 = rev(sum(rev(trim(z2)),rev(trim(z0)),0))
                       			   val z1 = if ( sign1 <> sign2) then rev(subtract(mul1, t1))
                                		    else rev(sum(rev(mul1), rev(t1),0)) 
                       			   val p1 = padding(z2,2*m)
                       			   val p2 = padding(z1,m)
                       			   val sum1 = rev(sum(rev(p1),rev(p2),0))
                                       in rev(sum(rev(trim(z0)),rev(trim(sum1)),0))
                  		       end;


(* Exception is handled here, when somebody gives an undesired input through stringtester function *)

exception Invalid_Input_exception;

fun stringtester [] = true
|   stringtester (h::t) = if Char.compare(chr(48),h) = EQUAL then stringtester t
                          else if Char.compare(chr(49),h) = EQUAL then stringtester t
			  else if Char.compare(chr(50),h) = EQUAL then stringtester t
                          else if Char.compare(chr(51),h) = EQUAL then stringtester t
                          else if Char.compare(chr(52),h) = EQUAL then stringtester t
                          else if Char.compare(chr(53),h) = EQUAL then stringtester t
                          else if Char.compare(chr(54),h) = EQUAL then stringtester t
                          else if Char.compare(chr(55),h) = EQUAL then stringtester t
                          else if Char.compare(chr(56),h) = EQUAL then stringtester t
			  else if Char.compare(chr(57),h) = EQUAL then stringtester t
                          else raise Invalid_Input_exception ;

fun stringtest ""=false
|stringtest s = stringtester(explode(s))
  handle Invalid_Input => false;


(* This function is called by the factorial function and it returns the factorial of the list which it takes it as an input, it does it by calling the karatsuba function recursively. *)

fun fact([0]) = [1]
|   fact(l2 as x1::y1) = karatsuba l2 (fact(rev(subtract(l2,[1]))));

(* This function is used to calculate the factorial of the number we want to know, it takes string as an input and it returns output of the function in string form only. *)
fun factorial x = if (stringtest x) then 
                  let val a = trim(fromString(x))
	          in toString(trim(fact a))
                  end
                  else "Invalid Input value";




