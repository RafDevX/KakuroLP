% Rafael Serra e Oliveira 99311
% Projeto de Logica para Programacao, Y1S2 LEIC-A IST, 2020/21

:- [codigo_comum].

% % [ 3.1.1 ] % %
%
% combinacoes_soma(N, Els, Soma, Combs)
% Sendo N um inteiro, Els uma lista de inteiros e Soma um inteiro, o predicado
% e verdadeiro se Combs for a lista ordenada cujos elementos sao as combinacoes
% N a N dos elementos de Els cuja soma e Soma.
combinacoes_soma(N, Els, Soma, Combs) :-
	setof(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Soma)), Combs).


% % [ 3.1.2 ] % %
%
% permutacoes_soma_aux(Lst, Perms)
% Sendo Lst uma lista de combinacoes, o predicado e verdadeiro se Perms for
% a lista de permutacoes de Lst excluindo os proprios elementos.
permutacoes_soma_aux([], []).
permutacoes_soma_aux([P | R], Perms) :-
	setof(Perm, (permutation(P, Perm), P \== Perm), ToAdd),
	permutacoes_soma_aux(R, PR),
	append(PR, ToAdd, Perms).

% permutacoes_soma(N, Els, Soma, Perms)
% Sendo N um inteiro, Els uma lista de inteiros e Soma um inteiro, o predicado
% e verdadeiro se Perms for a lista ordenada cujos elementos sao as permutacoes
% das combinacoes N a N dos elementos de Els cuja soma e Soma.
permutacoes_soma(N, Els, Soma, Perms) :-
	combinacoes_soma(N, Els, Soma, Combs),
	permutacoes_soma_aux(Combs, PermsSemCombs),
	append(Combs, PermsSemCombs, Perms).


% % [ 3.1.4 ] % %
%
% soma_dir([H, V], Dir, Soma)
% Sendo H e V inteiros, o predicado e verdadeiro se Soma for unificavel com H,
% caso Dir seja o atomo h, ou se Soma for unificavel com V, caso contrario.
soma_dir([V, H], Dir, Soma) :- Dir = h -> Soma = H; Soma = V.

% separar_ultimo(Lst, R, El)
% Sendo Lst e R listas, o predicado e verdadeiro se El for o ultimo elemento
% de Lst e R for a lista obtida removendo esse ultimo elemento de Lst.
separar_ultimo(Lst, R, El) :-
	last(Lst, El),
	append(R, [El], Lst).

% espaco_vazio(Esp)
% Sendo Esp um espaco, o predicado e verdadeiro se a lista das posicoes de Esp
% for unificavel com a lista vazia.
espaco_vazio(espaco(_, [])).

% espacos_fila_aux(Fila, Esps, Dir, AccEls, AccEsps)
% Sendo Fila uma fila (linha ou coluna) de um puzzle e Dir um dos atomos h ou v,
% o predicado e verdadeiro se Esps for a lista dos espacos de Fila, sendo AccEls
% e AccEsps acumuladores (respetivamente, de elementos e espacos) para facilitar
% o processo recursivo.
espacos_fila_aux([], Esps, _, [], AccEsps) :-
	!, exclude(espaco_vazio, AccEsps, Esps).
espacos_fila_aux(Fila, Esps, Dir, AccEls, AccEsps) :-
	separar_ultimo(Fila, R, Ultimo),
	is_list(Ultimo), !,
	soma_dir(Ultimo, Dir, Soma),
	Esp = espaco(Soma, AccEls),
	append([Esp], AccEsps, NEsps),
	espacos_fila_aux(R, Esps, Dir, [], NEsps).
espacos_fila_aux(Fila, Esps, Dir, AccEls, AccEsps) :-
	separar_ultimo(Fila, R, Ultimo),
	append([Ultimo], AccEls, NEls),
	espacos_fila_aux(R, Esps, Dir, NEls, AccEsps).

% espacos_fila(Dir, Fila, Esps)
% Sendo Dir um dos atomos h ou v e Fila uma fila de um puzzle, o predicado e
% verdadeiro se Esps for a lista dos seus espacos.
espacos_fila(Dir, Fila, Esps) :-
	espacos_fila_aux(Fila, Esps, Dir, [], []).


% % [ 3.1.3 ] % %
%
% espaco_fila(Fila, Esp, Dir)
% Sendo Fila uma fila de um puzzle e Dir um dos atomos h ou v, o predicado e
% verdadeiro se Esp for um espaco de Fila.
espaco_fila(Fila, Esp, Dir) :-
	espacos_fila(Dir, Fila, Esps),
	member(Esp, Esps).


% % [ 3.1.5 ] % %
%
% espacos_puzzle(Puzzle, Espacos)
% Sendo Puzzle um puzzle, o predicado e verdadeiro se Espacos for a lista de
% todos os espacos de Puzzle.
espacos_puzzle(Puzzle, Espacos) :-
	mat_transposta(Puzzle, PuzzleT),
	maplist(espacos_fila(h), Puzzle, EspsH),
	maplist(espacos_fila(v), PuzzleT, EspsV),
	append(EspsH, EspacosH),
	append(EspsV, EspacosV),
	append(EspacosH, EspacosV, Espacos).


% % [ 3.1.6 ] % %
%
% listas_independentes(L1, L2)
% Sendo L1 e L2 duas listas, o predicado e verdadeiro se essas listas nao
% tiverem elementos em comum. TODO: redo with intersection
listas_independentes([], _) :- !.
listas_independentes(_, []) :- !.
listas_independentes([P1 | R1], [P2 | R2]) :-
	P1 \== P2,
	listas_independentes([P1], R2),
	listas_independentes([P2], R1),
	listas_independentes(R1, R2).

% espacos_com_posicoes_comuns_aux(Esp1, Esp2)
% Sendo Esp1 e Esp2 dois espacos, o predicado e verdadeiro se esses dois espacos
% tiverem variaveis em comum e nao forem o mesmo espaco.
espacos_com_posicoes_comuns_aux(Esp1, Esp2) :- Esp1 == Esp2, !, false.
espacos_com_posicoes_comuns_aux(espaco(_, Els1), espaco(_, Els2)) :-
	\+ listas_independentes(Els1, Els2).

% espacos_com_posicoes_comuns(Espacos, Esp, EspsCom)
% Sendo Espacos uma lista de espacos e Esp um espaco, o predicado e verdadeiro
% se EspsCom for a lista de espacos com variaveis em comum com Esp, exceptuando
% o proprio Esp.
espacos_com_posicoes_comuns(Espacos, Esp, EspsCom) :-
	include(espacos_com_posicoes_comuns_aux(Esp), Espacos, EspsCom).


% % [ 3.1.7 ] % %
%
% permutacoes_soma_espacos_aux(Esp, [Esp, Perms])
% Sendo Esp um espaco, o predicado e verdadeiro quando Perms e a lista ordenada
% de permutacoes cuja soma e igual a soma de Esp.
permutacoes_soma_espacos_aux(espaco(Soma, Els), [espaco(Soma, Els), Perms]) :-
	length(Els, Len),
	permutacoes_soma(Len, [1, 2, 3, 4, 5, 6, 7, 8, 9], Soma, Perms).

% permutacoes_soma_espacos(Espacos, Perms_soma)
% Sendo Espacos uma lista de espacos, o predicado e verdadeiro se Perms_soma e
% a lista de listas de 2 elementos, em que o 1o elemento e um espaco de Espacos
% e o 2o e a lista ordenada de permutacoes cuja soma e igual a soma do espaco.
permutacoes_soma_espacos(Espacos, Perms_soma) :-
	maplist(permutacoes_soma_espacos_aux, Espacos, Perms_soma).

% % [ 3.1.8 ] % %
% permutacoes_soma_espaco(Esp, Perms_soma, Perms)
% Sendo Esp um espaco e Perms_soma uma lista de listas tal como obtida pelo
% predicado permutacoes_soma_espacos, o predicado e verdadeiro se Perms for
% a lista ordenada de permutacoes associada por Perms_soma a Esp.
permutacoes_soma_espaco(Esp, [[EspI, Perms] | _], Perms) :- Esp == EspI, !.
permutacoes_soma_espaco(Esp, [_ | R], Perms) :-
	permutacoes_soma_espaco(Esp, R, Perms).

% permutacao_possivel_espaco_aux(Perms_soma, Esp1, Perm, Esp2)
% Sendo Esp1 e Esp2 dois espacos, Perm uma permutacao e ainda Perms_soma uma
% lista de listas como obtida pelo predicado permutacoes_soma_espacos, o
% predicado e verdadeiro se Perm for simultaneamente aplicavel aos elementos de
% Esp1 como aos de Esp2.
permutacao_possivel_espaco_aux(Perms_soma, espaco(_, Els1), Perm, Esp) :-
	permutacoes_soma_espaco(Esp, Perms_soma, Perms),
	Esp = espaco(_, Els2),
	% se usasse include, a unificacao seria preservada
	findall(P, (member(P, Perms), Els1 = Perm, Els2 = P), Possiveis),
	Possiveis \== [].

% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Sendo Perm uma permutacao, Esp um espaco, Espacos uma lista de espacos e ainda
% Perms_soma uma lista tal como obtida pelo predicado permutacoes_soma_espacos,
% o predicado e verdadeiro se Perm for uma permutacao possivel para o espaco
% Esp, nao havendo conflitos com nenhum outro espaco em Espacos.
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	permutacoes_soma_espaco(Esp, Perms_soma, Perms),
	member(Perm, Perms),
	espacos_com_posicoes_comuns(Espacos, Esp, Comuns),
	maplist(permutacao_possivel_espaco_aux(Perms_soma, Esp, Perm), Comuns).


% % [ 3.1.9 ] % %
%
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% Sendo Espacos uma lista de espacos, Perms_soma uma lista de listas como obtida
% pelo predicado permutacoes_soma_espacos e Esp um espaco, o predicado e
% verdadeiro se Perms_poss for uma lista de 2 elementos em que o primeiro e a
% lista de variaveis de Esp e o segundo e a lista ordenada de permutacoes
% possiveis para o espaco Esp.
permutacoes_possiveis_espaco(Espacos, P_soma, Esp, [Els, Poss]) :-
	Esp = espaco(_, Els),
	setof(P, P_soma^permutacao_possivel_espaco(P, Esp, Espacos, P_soma), Poss).


% % [ 3.1.10 ] %
%
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Sendo Espacos uma lista de espacos, o predicado e verdadeiro se
% Perms_poss_esps for a lista de permutacoes possiveis para esses espacos.
permutacoes_possiveis_espacos(Esps, Perms_poss_esps) :-
	permutacoes_soma_espacos(Esps, Ps_soma),
	maplist(permutacoes_possiveis_espaco(Esps, Ps_soma), Esps, Perms_poss_esps).


% % [ 3.1.11 ] % %
%
% numeros_comuns(Lst_perms, Numeros_comuns)
% Sendo Lst_perms uma lista de permutacoes, o predicado e verdadeiro se
% Numeros_comuns for uma lista de pares (pos, num) e todas as listas de
% Lst_perms contiverem o mesmo numero, num, na posicao pos.
numeros_comuns([], _) :- !, false.
numeros_comuns(Lst_perms, Nums_comuns) :-
	nth1(1, Lst_perms, First),
	length(First, LenEach),
	findall((Pos, Num), (
		between(1, LenEach, Pos),
		nth1(Pos, First, Num),
		forall(member(Y, Lst_perms), nth1(Pos, Y, Num))
	), Nums_comuns).


% % [ 3.1.12 ] % %
%
% aplica_atribuicao(Vars, (Pos, Num))
% Sendo Vars uma lista, Pos uma posicao uma variavel dessa lista e Num um
% numero, o predicado e verdadeiro se o elemento de Vars na posicao Pos for
% unificavel com Num.
aplica_atribuicao(Vars, (Pos, Num)) :-
	nth1(Pos, Vars, Var),
	Var = Num.

% substitui_comuns(Vars, Nums)
% Sendo Vars uma lista e Nums uma lista de pares (pos, num), o predicado e
% verdadeiro se para todas as posicoes pos for possivel unificar o elemento de
% Vars nessa posicao com num.
substitui_comuns(_, []) :- !.
substitui_comuns(Vars, Nums) :-
	maplist(aplica_atribuicao(Vars), Nums).

% atribui_comuns_aux([Vars, Perms])
% Sendo Vars uma lista de elementos de um espaco e Perms uma lista de
% permutacoes possiveis para esse espaco, o predicado e verdadeiro se Vars for
% unificavel com os numeros comuns obtidos pelo predicado numeros_comuns.
atribui_comuns_aux([Vars, Perms]) :-
	numeros_comuns(Perms, Nums),
	substitui_comuns(Vars, Nums).

% atribui_comuns(Perms_possiveis)
% Sendo Perms_possiveis uma lista de permutacoes possiveis, o predicado e
% verdadeiro se essa lista for atualizavel para ser atribuido a cada espaco
% numeros comuns a todas as permutacoes possiveis para esse espaco.
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

resolve_aux(Perms_poss, Novas_perms_poss) :-
	escolhe_menos_alternativas(Perms_poss, Escolha), !,
	experimenta_perm(Escolha, Perms_poss, Intermedias),
	simplifica(Intermedias, Simplificadas),
	resolve_aux(Simplificadas, Novas_perms_poss).
resolve_aux(Perms_poss, Perms_poss).

resolve(Puz) :-
	inicializa(Puz, Perms_poss),
	resolve_aux(Perms_poss, _).