:- [codigo_comum, puzzles_publicos].

combinacoes_soma(N, Els, Soma, Combs) :- 
    findall(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(X, (member(Comb, Combs), permutation(Comb, X)), UnsortedPerms),
    sort(UnsortedPerms, Perms).

espaco_fila(Fila, Esp, H_V) :-
    % Coloca em Somas os valores das somas
    findall(SOMA, (member(X, Fila), is_list(X), (H_V == h -> X = [_, SOMA]; H_V == v -> X = [SOMA, _])), Somas),
    % Ignora somas cujo valor = 0
    include(=\=(0), Somas, Somas_filtr),
    % Coloca em Indices os indices das listas
    bagof(IND, Y^(member(Y, Fila), is_list(Y), nth0(IND, Fila, Y)), Indices), !,
    % Tamanho = length da Fila
    length(Fila, Tamanho),
    % Adicionar Tamanho como Indice final
    append(Indices, [Tamanho], Indices_final),
    % Agrupa indices 2 a 2
    junta_dois(Indices_final, Pares_Indices),
    % Separa as vars em grupos de vars de acordo com os indices das listas
    bagof(VARS, Par^LimI^LimS^(member(Par, Pares_Indices), nth0(0, Par, LimI), nth0(1, Par, LimS), gera_vars_espaco(LimI, LimS, VARS, Fila), VARS \== []), Lista_vars),
    % Força unificação do indice 1 das vars quando Soma unifica com o indice 1 de Somas_filtr, same parra Vars
    nth0(Pos, Somas_filtr, Soma),
    nth0(Pos, Lista_vars, Vars),
    Esp = espaco(Soma, Vars).

junta_dois([], []).
junta_dois([_], []).
junta_dois([P1, P2|P3], [[P1, P2]|R]) :- junta_dois([P2|P3], R).

gera_vars_espaco(X, X, [], Fila).
gera_vars_espaco(Start, End, [], Fila) :-
    New_start is Start + 1,
    New_start =:= End, !.
gera_vars_espaco(Start, End, [P|R], Fila) :-
    New_start is Start + 1,
    nth0(New_start, Fila, P),
    gera_vars_espaco(New_start, End, R, Fila).