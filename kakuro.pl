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
	exclude(espaco_vazio, AccEsps, Esps).
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
	include(espacos_com_posicoes_comuns(Esp), Espacos, Esps_com).

permutacoes_soma_espacos_aux(espaco(Soma, Els), [espaco(Soma, Els), Perms]) :-
	length(Els, Len),
	permutacoes_soma(Len, [1, 2, 3, 4, 5, 6, 7, 8, 9], Soma, Perms).

permutacoes_soma_espacos(Espacos, Perms_soma) :-
	maplist(permutacoes_soma_espacos_aux, Espacos, Perms_soma).

permutacoes_soma_espaco(Esp, [[Esp, Perms] | _], Perms) :- !.
permutacoes_soma_espaco(Esp, [_ | R], Perms) :-
	permutacoes_soma_espaco(Esp, R, Perms).

permutacao_possivel_espaco_aux(Perms_soma, espaco(_, Els1), Perm, Esp) :-
	permutacoes_soma_espaco(Esp, Perms_soma, Perms),
	Esp = espaco(_, Els2),
	% se usasse include, a unificacao seria preservada
	findall(P, (member(P, Perms), Els1 = Perm, Els2 = P), Possiveis),
	Possiveis \== [].

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	permutacoes_soma_espaco(Esp, Perms_soma, Perms),
	member(Perm, Perms),
	espacos_com_posicoes_comuns(Espacos, Esp, Comuns),
	maplist(permutacao_possivel_espaco_aux(Perms_soma, Esp, Perm), Comuns).

permutacoes_possiveis_espaco(Espacos, P_soma, Esp, [Els, Poss]) :-
	Esp = espaco(_, Els),
	setof(P, P_soma^permutacao_possivel_espaco(P, Esp, Espacos, P_soma), Poss).

permutacoes_possiveis_espacos(Esps, Perms_poss_esps) :-
	permutacoes_soma_espacos(Esps, Ps_soma),
	maplist(permutacoes_possiveis_espaco(Esps, Ps_soma), Esps, Perms_poss_esps).

numeros_comuns([], _) :- !, false.
numeros_comuns(Lst_perms, Nums_comuns) :-
	nth1(1, Lst_perms, First),
	length(First, LenEach),
	findall(
		(Pos, Num),
		(
			between(1, LenEach, Pos),
			nth1(Pos, First, Num),
			forall(member(Y, Lst_perms), nth1(Pos, Y, Num))
		),
		Nums_comuns
	).

aplica_atribuicao(Vars, (Pos, Num)) :-
	nth1(Pos, Vars, Var),
	Var = Num.

substitui_comuns(_, []) :- !.
substitui_comuns(Vars, Nums) :-
	maplist(aplica_atribuicao(Vars), Nums).

atribui_comuns_aux([Vars, Perms]) :-
	numeros_comuns(Perms, Nums),
	substitui_comuns(Vars, Nums).

atribui_comuns(Perms_possiveis) :-
	maplist(atribui_comuns_aux, Perms_possiveis).

retira_impossiveis_aux([Vars, Perms], [Vars, Novas_perms]) :-
	exclude(\=(Vars), Perms, Novas_perms).

retira_impossiveis(Perms_possiveis, Novas_perms_possiveis) :-
	maplist(retira_impossiveis_aux, Perms_possiveis, Novas_perms_possiveis).

simplifica(Perms_poss, Novas_perms_poss) :-
	atribui_comuns(Perms_poss),
	retira_impossiveis(Perms_poss, Novas_perms_poss),
	Perms_poss == Novas_perms_poss.
simplifica(Perms_poss, Novas_perms_poss) :-
	atribui_comuns(Perms_poss),
	retira_impossiveis(Perms_poss, Intermedias),
	Perms_poss \== Intermedias,
	simplifica(Intermedias, Novas_perms_poss).

inicializa(Puzzle, Perms_poss) :-
	espacos_puzzle(Puzzle, Espacos),
	permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
	simplifica(Perms_poss_esps, Perms_poss).

primeiro_com_perms_tamanho([[V, P] | _], Len, [V, P]) :-
	length(P, Len), !.
primeiro_com_perms_tamanho([_ | R], Len, E) :-
	primeiro_com_perms_tamanho(R, Len, E).

escolhe_menos_alternativas(Perms_poss, [VarsEscolha, PermsEscolha]) :-
	maplist(nth1(2), Perms_poss, Perms),
	maplist(length, Perms, Lens),
	exclude(>(2), Lens, ProperLens),
	ProperLens \== [],
	min_list(ProperLens, MinLen),
	primeiro_com_perms_tamanho(Perms_poss, MinLen, [VarsEscolha, PermsEscolha]).

experimenta_perm([Esp, Lst_perms], Perms_poss, Novas_perm_poss) :-
	member(Perm, Lst_perms),
	Esp = Perm,
	append(Antes, [[Esp, Lst_perms] | Depois], Perms_poss),
	append(Antes, [[Esp, [Perm]] | Depois], Novas_perm_poss).

resolve_aux(Perms_poss, Perms_poss) :-
	forall(member([_, Perms], Perms_poss), length(Perms, 1)).
resolve_aux(Perms_poss, Novas_perms_poss) :-
	escolhe_menos_alternativas(Perms_poss, Escolha),
	experimenta_perm(Escolha, Perms_poss, Intermedias),
	simplifica(Intermedias, Simplificadas),
	resolve_aux(Simplificadas, Novas_perms_poss).

resolve(Puz) :-
	inicializa(Puz, Perms_poss),
	resolve_aux(Perms_poss, _).

/* FIXME: delete below

erro gaspar

Var_case_convention
Se tiver aridade diferente, usar o mesmo nome

*/