% Rafael Serra e Oliveira 99311
% Projeto de Logica para Programacao, Y1S2 LEIC-A IST, 2020/21

:- [codigo_comum, puzzles_publicos].

% TODO: APAGAR ISTO
% Puzzle = [[[0, 0], [0, 0], [0, 0], [17, 0], [10, 0]], [[0, 0], [24, 0], [11, 3], P24, P25], [[0, 16], P32, P33, P34, P35], [[0, 26], P42, P43, P44, P45], [[0, 17], P52, P53, [0, 0], [0, 0]]]

combinacoes_soma(N, Els, Soma, Combs) :-
	setof(
		Comb,
		(
			combinacao(N, Els, Comb),
			sum_list(Comb, Soma)
		),
		Combs
	).

permutacoes_soma_aux([], []).
permutacoes_soma_aux([P | R], Perms) :-
	setof(
		Perm,
		(
			permutation(P, Perm),
			P \== Perm
		),
		ToAdd
	),
	permutacoes_soma_aux(R, PR),
	append(PR, ToAdd, Perms).

permutacoes_soma(N, Els, Soma, Perms) :-
	combinacoes_soma(N, Els, Soma, Combs),
	permutacoes_soma_aux(Combs, PermsSemCombs),
	append(Combs, PermsSemCombs, Perms).

soma_dir([V, H], Dir, Soma) :- Dir = h -> Soma = H; Soma = V.

separar_ultimo([E], [], E).
separar_ultimo([P, Q], [P], Q).
separar_ultimo([P | R], I, Ultimo) :-
	separar_ultimo(R, IParcial, Ultimo),
	append([P], IParcial, I).

espaco_vazio(espaco(_, Els)) :- length(Els, 0).

espacos_fila_aux([], Esps, _, [], AccEsps) :-
	!,
	exclude(espaco_vazio, AccEsps, Esps). % (*)
espacos_fila_aux(Fila, Esps, Dir, AccEls, AccEsps) :-
	separar_ultimo(Fila, R, Ultimo),
	is_list(Ultimo),
	!,
	soma_dir(Ultimo, Dir, Soma),
	Esp = espaco(Soma, AccEls),
	append([Esp], AccEsps, NEsps),
	espacos_fila_aux(R, Esps, Dir, [], NEsps).
espacos_fila_aux(Fila, Esps, Dir, AccEls, AccEsps) :-
	separar_ultimo(Fila, R, Ultimo),
	\+(is_list(Ultimo)),
	!,
	append([Ultimo], AccEls, NEls),
	espacos_fila_aux(R, Esps, Dir, NEls, AccEsps).

espacos_fila(Dir, Fila, Esps) :-
	espacos_fila_aux(Fila, Esps, Dir, [], []).

espaco_fila(Fila, Esp, Dir) :-
	espacos_fila(Dir, Fila, Esps),
	member(Esp, Esps).

espacos_puzzle(Puzzle, Espacos) :-
	mat_transposta(Puzzle, PuzzleT),
	maplist(espacos_fila(h), Puzzle, EspsH),
	maplist(espacos_fila(v), PuzzleT, EspsV),
	append(EspsH, EspacosH),
	append(EspsV, EspacosV),
	append(EspacosH, EspacosV, Espacos).

listas_independentes([], _) :- !.
listas_independentes(_, []) :- !.
listas_independentes([P1 | R1], [P2 | R2]) :-
	P1 \== P2,
	listas_independentes([P1], R2),
	listas_independentes([P2], R1),
	listas_independentes(R1, R2).

espacos_com_posicoes_comuns(Esp, Esp) :- !, false.
espacos_com_posicoes_comuns(espaco(_, Els1), espaco(_, Els2)) :-
	\+(listas_independentes(Els1, Els2)).
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
	bagof(
		E,
		(
			member(E, Espacos),
			espacos_com_posicoes_comuns(Esp, E)
		),
		Esps_com
	).

permutacoes_soma_espacos_aux(espaco(Soma, Els), [espaco(Soma, Els), Perms]) :-
	length(Els, Len),
	permutacoes_soma(Len, [1, 2, 3, 4, 5, 6, 7, 8, 9], Soma, Perms).

permutacoes_soma_espacos(Espacos, Perms_soma) :-
	maplist(permutacoes_soma_espacos_aux, Espacos, Perms_soma).

permutacoes_soma_espaco(Esp, [[Esp, Perms] | _], Perms) :- !.
permutacoes_soma_espaco(Esp, [_ | R], Perms) :-
	permutacoes_soma_espaco(Esp, R, Perms).

permutacoes_compativeis(espaco(_, Els1), _, espaco(_, Els2), _) :-
	listas_independentes(Els1, Els2).
permutacoes_compativeis(espaco(_, Els1), P1, espaco(_, Els2), P2) :-
	Els1 = P1,
	Els2 = P2.

permutacao_possivel_espaco_aux(Perms_soma, OrigEsp, Perm, Esp) :-
	permutacoes_soma_espaco(Esp, Perms_soma, Perms),
	findall( % se usasse include ele unificava
		P,
		(
			member(P, Perms),
			permutacoes_compativeis(OrigEsp, Perm, Esp, P)	
		),
		Possiveis
	),
	Possiveis == Perms.

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	permutacoes_soma_espaco(Esp, Perms_soma, Perms),
	member(Perm, Perms),
	espacos_com_posicoes_comuns(Espacos, Esp, Comuns),
	setof(
		P,
		maplist(permutacao_possivel_espaco_aux(Perms_soma, Esp, P), Comuns),
		Possiveis
	),
	member(Perm, Possiveis).