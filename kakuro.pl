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

soma_dir([V, H], Dir, Soma) :- Dir = h -> Soma = H; Soma = V.

separar_ultimo([E], [], E).
separar_ultimo([P, Q], [P], Q).
separar_ultimo([P | R], I, Ultimo) :-
	separar_ultimo(R, IParcial, Ultimo),
	append([P], IParcial, I).

espaco_nao_vazio(espaco(Soma, _)) :- Soma > 0.

espacos_fila([], Esps, _, [], AccEsps) :-
	!,
	include(espaco_nao_vazio, AccEsps, Esps). % (*)
espacos_fila(Fila, Esps, Dir, AccEls, AccEsps) :-
	separar_ultimo(Fila, R, Ultimo),
	is_list(Ultimo),
	!,
	soma_dir(Ultimo, Dir, Soma),
	Esp = espaco(Soma, AccEls),
	append([Esp], AccEsps, NEsps),
	espacos_fila(R, Esps, Dir, [], NEsps).
espacos_fila(Fila, Esps, Dir, AccEls, AccEsps) :-
	separar_ultimo(Fila, R, Ultimo),
	\+(is_list(Ultimo)),
	!,
	append([Ultimo], AccEls, NEls),
	espacos_fila(R, Esps, Dir, NEls, AccEsps).

espaco_fila(Fila, Esp, Dir) :-
	espacos_fila(Fila, Esps, Dir, [], []),
	member(Esp, Esps).