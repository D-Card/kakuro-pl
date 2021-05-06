:- [codigo_comum, puzzles_publicos].

combinacoes_soma(N, Els, Soma, Combs) :- 
    findall(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(X, (member(Comb, Combs), permutation(Comb, X)), UnsortedPerms),
    sort(UnsortedPerms, Perms).

espaco_fila(Fila, Esp, H_V) :-
    % Coloca em Somas os valores das somas
    findall(SOMA, 
        (member(X, Fila), 
        is_list(X), 
        (H_V == h -> X = [_, SOMA]; 
        H_V == v -> X = [SOMA, _])), Somas),
    % Ignora somas cujo valor = 0
    include(=\=(0), Somas, Somas_filtr),
    % Coloca em Indices os indices das listas
    bagof(IND, Y^(member(Y, Fila),
        is_list(Y), 
        nth0(IND, Fila, Y)), Indices), !,
    % Tamanho = length da Fila
    length(Fila, Tamanho),
    % Adicionar Tamanho como Indice final
    append(Indices, [Tamanho], Indices_final),
    % Agrupa indices 2 a 2
    junta_dois(Indices_final, Pares_Indices),
    % Separa as vars em grupos de vars de acordo com os indices das listas
    bagof(VARS, Par^LimI^LimS^(member(Par, Pares_Indices), 
        nth0(0, Par, LimI), 
        nth0(1, Par, LimS), 
        gera_vars_espaco(LimI, LimS, VARS, Fila), 
        VARS \== []), Lista_vars),
    % Força unificação do indice 1 das vars quando Soma unifica com o indice 1 de Somas_filtr, same parra Vars
    nth0(Pos, Somas_filtr, Soma),
    nth0(Pos, Lista_vars, Vars),
    Esp = espaco(Soma, Vars).

% agrupa elementos de uma lista 2 a 2 (elemento no indice x fica agrupado com elemento nos indices x-1 e x+1)
junta_dois([], []).
junta_dois([_], []).
junta_dois([P1, P2|P3], [[P1, P2]|R]) :- junta_dois([P2|P3], R).

gera_vars_espaco(X, X, [], _).
gera_vars_espaco(Start, End, [], _) :-
    New_start is Start + 1,
    New_start =:= End, !.
gera_vars_espaco(Start, End, [P|R], Fila) :-
    New_start is Start + 1,
    nth0(New_start, Fila, P),
    gera_vars_espaco(New_start, End, R, Fila).

espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).

espacos_puzzle(Puzzle, Espacos) :-
    bagof(X, Fila^Puzzle_transp^(member(Fila, Puzzle),
    espacos_fila(h, Fila, X);
    mat_transposta(Puzzle, Puzzle_transp),
    member(Fila, Puzzle_transp),
    espacos_fila(v, Fila, X)), Espacos_list),
    flatten(Espacos_list, Espacos).


aux_espacos_com_posicoes_comuns(Espacos, Esp, Espaco) :-
    member(Espaco, Espacos), 
    Espaco = espaco(_, Vars2),
    Esp = espaco(_, Vars),
    member(Var, Vars),
    any(==(Var), Vars2), 
    Espaco \== Esp.

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(X, aux_espacos_com_posicoes_comuns(Espacos, Esp, X), Esps_com).

any(Cond, [P|R]) :-
    call(Cond, P), !; 
    any(Cond, R). 

permutacoes_soma_espacos(Espacos, Perms_soma) :-
    bagof(X, aux_permutacoes_soma_espacos(Espacos, X), Perms_soma).

aux_permutacoes_soma_espacos(Espacos, [Espaco, Perms]) :-
    member(Espaco, Espacos),
    Espaco = espaco(Soma, Vars),
    length(Vars, Length),
    permutacoes_soma(Length, [1,2,3,4,5,6,7,8,9], Soma, Perms).


permutacao_possivel_espaco(Perm ,Esp, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    member(Perm_soma, Perms_soma),
    Perm_soma = [Esp, Perms_esp],
    member(Perm, Perms_esp),
    bagof(Pos, Var^(member(Var, Perm), nth0(Pos, Perm, Var)), Indices),
    aux_permutacao_possivel_espaco(Esps_com, Perms_soma, Indices, Perm).


aux_permutacao_possivel_espaco(_, _, [], _).
aux_permutacao_possivel_espaco(Esps_com, Perms_soma, [P|R], Perm) :-
    nth0(P, Perm, Var), 
    nth0(P, Esps_com, Esp_com),           
    member(Perm_soma, Perms_soma),
    Perm_soma = [Esp_com, Perms_esp_com],
    flatten(Perms_esp_com, Var_com),
    member(Var, Var_com),
    aux_permutacao_possivel_espaco(Esps_com, Perms_soma, R, Perm), !.

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Perms),
    Esp = espaco(_, Vars),
    Perms_poss = [Vars, Perms].

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perm, Espaco^(member(Espaco, Espacos), 
    permutacoes_possiveis_espaco(Espacos, Perms_soma, Espaco, Perm)), Perms_poss_esps).


numeros_comuns(Lst_perms, Numeros_comuns) :-
    findall(Num_com, numero_comum(Lst_perms, Num_com), Numeros_comuns).
    

numero_comum(Lst_perms, Numero_comum) :-
    mat_transposta(Lst_perms, Lst_perms_ind),
    member(Perm_ind, Lst_perms_ind),
    maplist(=(_), Perm_ind),
    nth1(Pos, Lst_perms_ind, Perm_ind),
    nth1(1, Perm_ind, Value),
    Numero_comum = (Pos, Value).



    
    
