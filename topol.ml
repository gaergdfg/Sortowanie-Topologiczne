open PMap

(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(** zwraca liste zaleznosci wierzcholka w grafie *)
let znajdz_sasiadow mapa wierzcholek =
	try find wierzcholek mapa
	with _ -> []


(** dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il lub
	podnosi wyjatek [Cykliczne], jesli zaleznosci sa cykliczne *)
let topol graf =
	let odwiedzony = ref empty in
	let kolejnosc = ref empty in
	let sasiedzi = ref empty in
	let n = ref 0 in
	let numer = ref 0 in
	let rec dfs teraz poprzedni czy_pierwszy = begin
		odwiedzony := add teraz 1 !odwiedzony;
		List.iter
			(fun x ->
				if 	not czy_pierwszy &&
					mem x !odwiedzony &&
					find x !odwiedzony = 1 then
					raise Cykliczne
				else if	(not czy_pierwszy && x = poprzedni) || 
						(mem x !odwiedzony && find x !odwiedzony = 2) then ()
				else dfs x teraz false)
			(znajdz_sasiadow !sasiedzi teraz);
		kolejnosc := add !numer teraz !kolejnosc;
		odwiedzony := add teraz 2 !odwiedzony;
		incr numer;
		incr n;
	end in
	let odp = ref [] in begin
		(** tworzy mape z sasiadami na podstawie listy *)
		List.iteri
			(fun i (x, l) ->
				sasiedzi :=
					if mem x !sasiedzi then
						let teraz = find x !sasiedzi in
							add x (l@teraz) !sasiedzi
					else
						add x l !sasiedzi)
			graf;
		(** puszcza dfs'a z kazdego jeszcze nieodwiedzonego wierzcholka *)
		List.iter
			(fun (x, _) ->
				if mem x !odwiedzony then ()
				else dfs x x true)
			graf;
		(** odtwarza liste-odpowiedz na podstawie czasow post-order *)
		for i = 0 to !n - 1 do
			odp := (find i !kolejnosc)::(!odp);
			()
		done;
		!odp
	end


(* Testy z Forum *)
(*
let pos a l = 
	let poz = ref 0 and use = ref false in
	List.iter (fun x -> if (x <> a && (!use) = false) then poz := (!poz) + 1
	           else use := true ) l;
	(!poz);;	
(***FUNKCJA SPRAWDZAJĄCA POPRAWNOŚĆ ROZWIĄZANIA, GDY NIE MA CYKLU***)
let is_valid l ans =
	let rec pom_valid acc li ans =
		match li with 
			|[] -> acc
			|(f,s)::t -> 
				if (List.fold_left (fun a x -> (if (pos f ans) > (pos x ans) then false else a)) 
				(true) s) then (pom_valid acc t ans)
				else (pom_valid false t ans)
	in pom_valid true l ans	
			 
(*******TESTY*******)
(*******CYKLICZNE*******)
let l = (1,[1])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l =(1,[2])::(2,[3])::(3,[2])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l = (1,[2])::(2,[3])::(3,[4;5])::(4,[5])::[];;
assert(is_valid l (topol l));;
let l = (1,[2])::(2,[3])::(3,[4;5])::(4,[2;5])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
(******NIEISTNIEJĄCE_W_LIŚCIE_GŁÓWNEJ_WIERZCHOŁKI******)
let l = [];;
let l = (1,[0])::l;;
assert (is_valid l (topol l));;
let l = (2,[0])::l;;
assert (is_valid l (topol l));;
(*******SPRAWDZENIE_CZY_DZIAŁA_NA_INNYCH_TYPACH_NIŻ_INT******)
let l = [];;
let l = ('a',['b';'d'])::l;;
let l = ('b',['c';'d'])::l;;
let l = ('c',['d'])::l;;
assert (is_valid l (topol l));;
let l = [];;
let l = ("fst",["snd";"thr"])::l;;
let l = ("xyz",["abc";"snd"])::l;;
let l = ("cos",["fst";"xyz"])::l;;
assert (is_valid l (topol l));;
let l = [];;
let l = (true,[false])::l;;
let l = (false,[true])::l;;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
(******TESTY_RÓŻNE******)
let l = [];;
let l = (1,[0])::l;;
assert (is_valid l (topol l));;
let l = (0,[2])::l;;
assert (is_valid l (topol l));;
let l = (2,[3])::l;;
assert (is_valid l (topol l));;
let l = (4,[2;3])::l;;
assert (is_valid l (topol l));;
let l = (6,[2;3])::l;;
assert (is_valid l (topol l));;
let l = (9,[10;11])::l;;
assert (is_valid l (topol l));;
let l = (10,[9])::l;;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
*)