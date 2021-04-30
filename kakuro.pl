:- [codigo_comum, puzzles_publicos].

combinacoes_soma(N, Els, Soma, Combs) :- 
    findall(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).

permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(X, (member(Comb, Combs), permutation(Comb, X)), UnsortedPerms),
    sort(UnsortedPerms, Perms).

espaco_fila(Fila, Esp, H_V) :-

    
    
    (H_V == h -> List = [_, SOMA];
    H_V == v -> List = [SOMA, _]),

    Esp = espaco(SOMA, vars).