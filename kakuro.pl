% Rafael Serra e Oliveira 99311
% Projeto de Logica para Programacao, Y1S2 LEIC-A IST, 2020/21

:- [codigo_comum, puzzles_publicos].

combinacoes_soma(N, Els, Soma, Combs) :-
	setof(
		Comb,
		(
			combinacao(N, Els, Comb),
			sum_list(Comb, Soma)
		),
		Combs
	).

permutacoes_soma(N, Els, Soma, Perms) :-
	combinacoes_soma(N, Els, Soma, Combs),
	setof(P, (maplist(permutation, Combs, P)), Unmerged),
	append(Unmerged, WithDuplicates),
	list_to_set(WithDuplicates, Perms).

sem_listas([]).
sem_listas([E]) :- \+(is_list(E)), !.
sem_listas([P | R]) :- \+(is_list(P)), sem_listas(R).

soma_dir([H, V], Dir, Soma) :- Dir = h -> Soma = H; Soma = V.

separar_ultimo([E], [], E).
separar_ultimo([P, Q], [P], Q).
separar_ultimo([P | R], I, Ultimo) :-
	separar_ultimo(R, IParcial, Ultimo),
	append([P], IParcial, I).

/*espaco_fila([[H, V] | R], Esp, Dir) :-
	sem_listas(R),
	soma_dir([H, V], Dir, Soma),
	Esp = espaco(Soma, R).*/

espaco_fila(Fila, Esp, Dir) :-
	espaco_fila_aux(Fila, Esps, Dir, []),
	member(Esp, Esps).

espaco_fila_aux([], _, _, _). % (*)
espaco_fila_aux(Fila, Esps, Dir, Acc) :-
	separar_ultimo(Fila, R, Ultimo),
	is_list(Ultimo), !,
	soma_dir(Ultimo, Dir, Soma),
	Esp = espaco(Soma, Acc),
	append([Esp], Esps, NEsps),
	espaco_fila_aux(R, NEsps, Dir, []).
espaco_fila_aux(Fila, Esps, Dir, Acc) :-
	separar_ultimo(Fila, R, Ultimo),
	append([Ultimo], Acc, NAcc),
	espaco_fila_aux(R, Esps, Dir, NAcc).

/* falta casas vazias */