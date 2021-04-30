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

obter_soma(H, V, Dir, Soma) :- Dir = h -> Soma = H; Soma = V.

espaco_fila([[H | V] | R], Esp, Dir) :-
	sem_listas(R),
	obter_soma(H, V, Dir, Soma),
	Esp = espaco(Soma, R).
