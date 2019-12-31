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
		List.iteri
			(fun i (x, l) ->
				sasiedzi :=
					if mem x !sasiedzi then
						let teraz = find x !sasiedzi in
							add x (l@teraz) !sasiedzi
					else
						add x l !sasiedzi)
			graf;
		List.iter
			(fun (x, _) ->
				if mem x !odwiedzony then ()
				else dfs x x true)
			graf;
		for i = 0 to !n - 1 do
			odp := (find i !kolejnosc)::(!odp);
			()
		done;
		!odp
	end
