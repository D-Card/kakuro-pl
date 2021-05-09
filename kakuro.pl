% :- [codigo_comum].

%---------------------------------------------------
% combinacoes_soma(N, Els, Soma, Combs), em que N eh um inteiro, Els eh uma
% lista de inteiros, e Soma eh um inteiro, significa que Combs eh a lista ordenada 
% cujos elementos sao as combinacoes N a N, dos elementos de Els cuja soma eh Soma .
%---------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :- 
    findall(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).


%---------------------------------------------------
% permutacoes_soma(N, Els, Soma, Perms), em que N eh um inteiro, Els eh uma
% lista de inteiros, e Soma eh um inteiro, significa que Perms eh a lista ordenada cujos elementos
% sao as permutacoes das combinacoes N a N, dos elementos de Els cuja soma eh
% Soma .
%---------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(X, (member(Comb, Combs), permutation(Comb, X)), UnsortedPerms),
    sort(UnsortedPerms, Perms).


%---------------------------------------------------
% espaco_fila(Fila, Esp, H_V), em que Fila eh uma fila (linha ou coluna) de um
% puzzle e H_V eh um dos atomos h ou v, conforme se trate de uma fila horizontal ou vertical,
% respectivamente, significa que Esp eh um espaco de Fila.
%---------------------------------------------------
espaco_fila(Fila, Esp, H_V) :-
    append([_, [P|R], Resto], Fila),
    is_list(P),
    (H_V == h -> P = [_, Soma]; 
    H_V == v -> P = [Soma, _]),
    R \== [],
    maplist(var, R),
    (Resto == [];
    nth1(1, Resto, X),
    is_list(X)),
    Esp = espaco(Soma, R).


%---------------------------------------------------
% espacos_fila(H_V, Fila, Espacos), em que Fila eh uma fila (linha ou coluna) de
% um Puzzle e e H_V eh um dos atomos h ou v, significa que Espacos eh a lista de todos os
% espacos de Fila, da esquerda para a direita.
%---------------------------------------------------
espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).


%---------------------------------------------------
% espacos_puzzle(Puzzle, Espacos), em que Puzzle eh um puzzle, significa que
% Espacos eh a lista de espacos de Puzzle.
%---------------------------------------------------
espacos_puzzle(Puzzle, Espacos) :-
    bagof(X, Fila^Puzzle_transp^(member(Fila, Puzzle),
    espacos_fila(h, Fila, X);
    mat_transposta(Puzzle, Puzzle_transp),
    member(Fila, Puzzle_transp),
    espacos_fila(v, Fila, X)), Espacos_list),
    flatten(Espacos_list, Espacos).


%---------------------------------------------------
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos
% eh uma lista de espacos e Esp eh um espaco, significa que Esps_com eh a lista 
% de espacos com variaveis em comum com Esp, exceptuando Esp. 
%---------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(X, aux_espacos_com_posicoes_comuns(Espacos, Esp, X), Esps_com).


aux_espacos_com_posicoes_comuns(Espacos, Esp, Espaco) :-
    member(Espaco, Espacos), 
    Espaco = espaco(_, Vars2),
    Esp = espaco(_, Vars),
    member(Var, Vars),
    any(==(Var), Vars2), 
    Espaco \== Esp.


%---------------------------------------------------
% any(Cond, Lst), em que Cond eh uma condicao e Lst eh uma lista, devolve true
% se algum elemento dentro da lista obedecer a condicao.
%---------------------------------------------------
any(Cond, [P|R]) :-
    call(Cond, P), !; 
    any(Cond, R). 


%---------------------------------------------------
% permutacoes_soma_espacos(Espacos, Perms_soma), em que Espacos eh uma
% lista de espacos, significa que Perms_soma eh a lista de listas de 2 elementos, em que
% o 1o elemento eh um espaco de Espacos e o 2o eh a lista ordenada de permutacoes cuja
% soma eh igual a soma do espaco.
%---------------------------------------------------
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    bagof(X, aux_permutacoes_soma_espacos(Espacos, X), Perms_soma).


aux_permutacoes_soma_espacos(Espacos, [Espaco, Perms]) :-
    member(Espaco, Espacos),
    Espaco = espaco(Soma, Vars),
    length(Vars, Length),
    permutacoes_soma(Length, [1,2,3,4,5,6,7,8,9], Soma, Perms).


%---------------------------------------------------
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), em que
% Perm eh uma permutacao, Esp eh um espaco, Espacos eh uma lista de espacos, e
% Perms_soma eh uma lista de listas tal como obtida pelo predicado anterior, significa que
% Perm eh uma permutacao possivel para o espaco Esp.
%---------------------------------------------------
permutacao_possivel_espaco(Perm ,Esp, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    findall(X, (member(Perm_soma, Perms_soma), Perm_soma = [Esp, X]), Perms_esp_temp),
    nth1(1, Perms_esp_temp, Perms_esp),
    member(Perm, Perms_esp),
    bagof(Pos, Var^(member(Var, Perm), nth0(Pos, Perm, Var)), Indices),
    forall(member(Indice, Indices), (
        nth0(Indice, Perm, X),
        nth0(Indice, Esps_com, Esp_com),
        member(Perm_soma, Perms_soma),
        Perm_soma = [Esp_com, Perms_esp_com],
        append(Perms_esp_com, Var_com),
        member(X, Var_com))).


%---------------------------------------------------
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,
% Perms_poss), em que Espacos eh uma lista de espacos, Perms_soma eh uma lista
% de listas tal como obtida pelo predicado permutacoes_soma_espacos, e Esp eh um
% espaco, significa que Perms_poss eh uma lista de 2 elementos em que o primeiro eh a
% lista de variaveis de Esp e o segundo eh a lista ordenada de permutacoes possiveis para o
% espaco Esp.
%---------------------------------------------------
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Perms),
    Esp = espaco(_, Vars),
    Perms_poss = [Vars, Perms].


%---------------------------------------------------
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps), em que
% Espacos eh uma lista de espacos, significa que Perms_poss_esps eh a lista de permutacoes
% possiveis.
%---------------------------------------------------
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perm, Espaco^(member(Espaco, Espacos), 
    permutacoes_possiveis_espaco(Espacos, Perms_soma, Espaco, Perm)), Perms_poss_esps).


%---------------------------------------------------
% numeros_comuns(Lst_Perms, Numeros_comuns), em que Lst_Perms eh uma lista
% de permutacoes, significa que Numeros_comuns eh uma lista de pares (pos, numero),
% significando que todas as listas de Lst_Perms contem o numero numero na posicao
% pos.
%---------------------------------------------------
numeros_comuns(Lst_perms, Numeros_comuns) :-
    findall(Num_com, numero_comum(Lst_perms, Num_com), Numeros_comuns).
    

numero_comum(Lst_perms, Numero_comum) :-
    mat_transposta(Lst_perms, Lst_perms_ind),
    member(Perm_ind, Lst_perms_ind),
    maplist(=(_), Perm_ind),
    nth1(Pos, Lst_perms_ind, Perm_ind),
    nth1(1, Perm_ind, Value),
    Numero_comum = (Pos, Value).

%---------------------------------------------------
% atribui_comuns(Perms_Possiveis), em que Perms_Possiveis eh uma lista de
% permutacoes possiveis, actualiza esta lista atribuindo a cada espaco numeros comuns
% a todas as permutacoes possiveis para esse espaco
%---------------------------------------------------
atribui_comuns([]).
atribui_comuns([P|R]) :-
    nth1(2, P, Perms),
    nth1(1, P, Vars),
    numeros_comuns(Perms, Numeros_comuns),
    aux_atribui_comuns(Vars, Numeros_comuns),
    atribui_comuns(R), !.

aux_atribui_comuns(_, []).
aux_atribui_comuns(Vars, [P|R]) :-
    P = (Pos, Num),
    nth1(Pos, Vars, Var),
    Var = Num,
    aux_atribui_comuns(Vars, R).


%---------------------------------------------------
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis), em que Perms_Possiveis
% eh uma lista de permutacoes possiveis, significa que Novas_Perms_Possiveis eh o 
% resultado de tirar permutacoes impossiveis de Perms_Possiveis.
%---------------------------------------------------
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    bagof(Nova_Perm_Possivel, aux_retira_impossiveis(Perms_Possiveis, Nova_Perm_Possivel), Novas_Perms_Possiveis).

aux_retira_impossiveis(Perms_Possiveis, Nova_Perm_Possivel) :-
    member(Perm, Perms_Possiveis),
    nth1(1, Perm, Vars),
    nth1(2, Perm, Perms),
    exclude(\=(Vars), Perms, Perms_fltr),
    Nova_Perm_Possivel = [Vars, Perms_fltr].
        

%---------------------------------------------------
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis), em que Perms_Possiveis eh
% uma lista de permutacoes possiveis, significa que Novas_Perms_Possiveis eh o 
% resultado de simplificar Perms_Possiveis.
%---------------------------------------------------
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis_temp),
    (Perms_Possiveis == Novas_Perms_Possiveis_temp, 
    Novas_Perms_Possiveis = Novas_Perms_Possiveis_temp, !;
    simplifica(Novas_Perms_Possiveis_temp, Novas_Perms_Possiveis)).


%---------------------------------------------------
% inicializa(Puzzle, Perms_Possiveis), em que Puzzle eh um puzzle, significa
% que Perms_Possiveis eh a lista de permutacoes possiveis simplificada para Puzzle.
%---------------------------------------------------
inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis_temp),
    simplifica(Perms_Possiveis_temp, Perms_Possiveis).


%---------------------------------------------------
% escolhe_menos_alternativas(Perms_Possiveis, Escolha), em que
% Perms_Possiveis eh uma lista de permutacoes possiveis, significa que Escolha
% eh o elemento de Perms_Possiveis escolhido segundo os criterios do enunciado.
% Se todos os espacos em Perms_Possiveis tiverem associadas listas de
% permutacoes unitarias, o predicado devolve "falso".
%---------------------------------------------------
escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    findall(Length, (member(Perms, Perms_Possiveis), nth1(2, Perms, Perm), length(Perm, Length)), Length_List),
    exclude(==(1), Length_List, Length_List_filtr),
    Length_List_filtr \== [],
    min_list(Length_List_filtr, Menor),
    nth1(Pos, Length_List, Menor), 
    nth1(Pos, Perms_Possiveis, Escolha), !.