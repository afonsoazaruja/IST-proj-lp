% Nome: Afonso Azaruja
% Numero: 103624

:- [codigo_comum].

/*##############################################################################
extrai_ilhas_linha/3 -> extrai_ilhas_linha(N_L, Linha, Ilhas), em que N_L e um 
inteiro positivo, correspondente ao numero de uma linha e Linha e uma lista 
correspondente a uma linha de um puzzle, significa que Ilhas e a lista ordenada 
(ilhas da esquerda para a direita) cujos elemento sao as ilhas da linha Linha.
##############################################################################*/

extrai_ilhas_linha(N_L, L, Ilhas) :-
    findall(ilha(N_P, (N_L, N_C)), 
        (member(N_P, L), N_P > 0, nth1(N_C, L, N_P)), 
        Lst),
    
    sort(2, @<, Lst, Ilhas).


/*##############################################################################
ilhas/2 -> ilhas(Puz, Ilhas), em que Puz e um puzzle, significa que Ilhas e a 
lista ordenada (ilhas da desquerda para a direita e de cima para baixo) cujos 
elementos sao as ilhas de Puz.
##############################################################################*/

ilhas(Puz, Ilhas_Puz) :-
    findall(Ilhas, 
        (member(L, Puz), nth1(N_L, Puz, L), extrai_ilhas_linha(N_L, L, Ilhas)), 
        Lst_Ilhas),
    
    append(Lst_Ilhas, Ilhas_No_Sort),
    sort(2, @<, Ilhas_No_Sort, Ilhas_Puz).


/*##############################################################################
vizinhas/3 -> vizinhas(Ilhas, Ilha, Vizinhas), em que Ilhas e a lista de ilhas 
de um puzzle e Ilha e a uma dessas ilhas, significa que Vizinhas e a lista 
ordenada (ilhas de cima para baixo e da esquerda para a direita) cujos elementos
sao as ilhas vizinhas de Ilha.
##############################################################################*/

vizinhas(Ilhas_Puz, ilha(_,(L, C)), Viz) :-
    findall(Poss_Viz, 
        (member(Poss_Viz, Ilhas_Puz), Poss_Viz = ilha(_, (L, N_C)), N_C > C),
        Viz_Dir), % lista com todas as ilhas a direita 

    findall(Poss_Viz, 
        (member(Poss_Viz, Ilhas_Puz), Poss_Viz = ilha(_, (L, N_C)), N_C < C), 
        Viz_Esq), % lista com todas as ilhas a esquerda

    findall(Poss_Viz, 
        (member(Poss_Viz, Ilhas_Puz), Poss_Viz = ilha(_, (N_L, C)), N_L > L), 
        Viz_Baixo), % lista com todas as ilhas a baixo

    findall(Poss_Viz, 
        (member(Poss_Viz, Ilhas_Puz), Poss_Viz = ilha(_, (N_L, C)), N_L < L), 
        Viz_Cima), % lista com todas as ilhas a cima

    reverse(Viz_Cima, Inv_L), % para a ilha vizinha ficar com indice 1
    reverse(Viz_Esq, Inv_C),
    Lst_Viz = [Inv_L, Inv_C, Viz_Dir, Viz_Baixo],

    findall(V, 
        (member(Lst, Lst_Viz), Lst \== [], nth1(1, Lst, V)), 
        Viz).
    % as ilhas vizinhas serao as primeiras de cada lista

    /*----------------------------------------------------------------------
    Este predicado cria 4 listas, Viz_Dir (ilhas a direita), Viz_Esq (ilhas 
    a esquerda), Viz_Baixo (ilhas a baixo), Viz_Cima (ilhas a cima) em rela-
    cao a ilha no 2. argumento. Nas listas Viz_Baixo e Viz_Dir, a ilha vizi-
    nha encontra-se no indice 1, enquanto em Viz_Cima e Viz_Esq encontra-se 
    no ultimo indice, por isso inverte-se estas duas. Neste momento e so a-
    grupar numa lista as ilhas que se encontram no indice 1 de cada uma das
    listas Viz_XXXX.
    ----------------------------------------------------------------------*/


/*##############################################################################
estado/2 -> estado(Ilhas, Estado), em que Ilhas e a lista de ilhas de um puzzle,
significa que Estado e a lista cujos elementos sao as entradas referentes a cada 
uma das ilhas de Ilhas.
##############################################################################*/

estado(Ilhas, Estado) :-
    findall([Ilha, Viz, []], 
        (member(Ilha, Ilhas), vizinhas(Ilhas, Ilha, Viz)), 
        Estado).


/*##############################################################################
posicoes_entre/3 -> posicoes_entre(Pos1, Pos2, Posicoes), em que Pos1 e Pos2 sao 
posicoes, significa que Posicoes e a lista ordenada de posicoes entre Pos1 e 
Pos2 (excluindo Pos1 e Pos2). Se Pos1 e Pos2 nao pertencerem a mesma linha ou a 
mesma coluna, o resultado e false.
##############################################################################*/

posicoes_entre((L1, C1), (L2, C2), Pos) :-
    (L1 == L2, % se linhas iguais entao as colunas variam
    Dist is abs(C1 - C2), Dist > 1, % distancia entre ilhas superior a 1
    Sup is max(C1, C2) - 1, % coordenada maior
    Inf is min(C1, C2) + 1, % coordenada menor
    findall((L1, C), between(Inf, Sup, C), Pos)), !
    ;
    (C1 == C2, % se colunas iguais entao as linhas variam
    Dist is abs(L1 - L2), Dist > 1, 
    Sup is max(L1, L2) - 1, 
    Inf is min(L1, L2) + 1,
    findall((L, C1), between(Inf, Sup, L), Pos)), !.


/*##############################################################################
cria_ponte/3 -> cria_ponte(Pos1, Pos2, Ponte), em que Pos1 e Pos2 sao 2 
posicoes, significa que Ponte e uma ponte entre essas 2 posicoes.
##############################################################################*/

cria_ponte(P1, P2, Ponte) :-
    sort([P1, P2], [POS1, POS2]),
    Ponte = ponte(POS1, POS2).


/*##############################################################################
caminho_livre/5 -> caminho_livre(Pos1, Pos2, Posicoes, I, Vz), em que Pos1 e 
Pos2 sao posicoes, Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2, I 
e uma ilha, e Vz e uma das suas vizinhas, significa que a adicao da ponte 
ponte(Pos1, Pos2) nao faz com que I e Vz deixem de ser vizinhas.
##############################################################################*/

caminho_livre(P1, P2, Pos_Entre, ilha(_, Pos_Ilha), ilha(_, Pos_Viz)) :-
    (posicoes_entre(Pos_Ilha, Pos_Viz, Pos_Entre_V),
    Pos_Entre \== Pos_Entre_V,
    \+ (member(P, Pos_Entre_V), member(P, Pos_Entre)), !)
    ;
    (subset([P1, P2], [Pos_Ilha, Pos_Viz]), !).
    % se a ponte criada e a que liga a Ilha e a Viz


/*##############################################################################
actualiza_vizinhas_entrada/5 -> actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, 
Entrada, Nova_Entrada), em que Pos1 e Pos2 sao as posicoes entre as quais ira 
ser adicionada uma ponte, Posicoes e a lista ordenada de posicoes entre Pos1 e 
Pos2, e Entrada e uma entrada, significa que Nova_Entrada e igual a Entrada, 
excepto no que diz respeito a lista de ilhas vizinhas; esta deve ser actualizada 
removendo as ilhas que deixaram de ser vizinhas, apos a adicao da ponte.
##############################################################################*/

actualiza_vizinhas_entrada(P1, P2, Pos_Entre, [Ilha, L_Viz, R], 
[Ilha, L_Nova_Viz, R]) :-
    findall(Nova_Viz, 
        (member(Nova_Viz, L_Viz), 
        caminho_livre(P1, P2, Pos_Entre, Ilha, Nova_Viz)), 
        L_Nova_Viz).


/*##############################################################################
actualiza_vizinhas_apos_pontes/4 -> actualiza_vizinhas_apos_pontes(Estado, Pos1, 
Pos2, Novo_estado) , em que Estado e um estado, Pos1 e Pos2 sao as posicoes 
entre as quais foi adicionada uma ponte, significa que Novo_estado e o estado 
que se obtem de Estado apos a actualizacao das ilhas vizinhas de cada uma das 
suas entradas.
##############################################################################*/

actualiza_vizinhas_apos_pontes(Estado, P1, P2, Novo_Estado) :-
    posicoes_entre(P1, P2, Pos_Entre),
    findall(Nova_Entrada, 
        (member(Entrada, Estado), 
        actualiza_vizinhas_entrada(P1, P2, Pos_Entre, Entrada, Nova_Entrada)), 
        Novo_Estado).


/*##############################################################################
ilhas_terminadas/2 -> ilhas_terminadas(Estado, Ilhas_term), em que Estado e um 
estado, significa que Ilhas_term e a lista de ilhas que ja tem todas as pontes 
associadas, designadas por ilhas terminadas. Se a entrada referente a uma ilha 
for [ilha(N_pontes, Pos), Vizinhas, Pontes], esta ilha esta terminada se 
N_pontes for diferente de 'X' e o comprimento da lista Pontes for N_pontes.
##############################################################################*/

ilhas_terminadas(Estado, Ilhas_Term) :-
    findall(ilha(N_Pontes, Pos), 
        (member(Entrada, Estado), 
        Entrada = [ilha(N_Pontes, Pos), _, Pontes], 
        N_Pontes \== 'X', 
        length(Pontes, N_Pontes)), 
        Ilhas_Term).


/*##############################################################################
tira_ilhas_terminadas_entrada -> tira_ilhas_terminadas_entrada(Ilhas_term, 
Entrada, Nova_entrada), em que Ilhas_term e uma lista de ilhas terminadas e 
Entrada e uma entrada, significa que Nova_entrada e a entrada resultante de 
remover as ilhas de Ilhas_term, da lista de ilhas vizinhas de entrada.
##############################################################################*/

tira_ilhas_terminadas_entrada(Ilhas_Term, [Ilha, Viz, Pontes], 
[Ilha, Viz_Sem_IT, Pontes]) :- 
    findall(Nova_Viz, 
        (member(Nova_Viz, Viz), \+ member(Nova_Viz, Ilhas_Term)), 
        Viz_Sem_IT).


/*##############################################################################
tira_ilhas_terminadas/3 -> tira_ilhas_terminadas(Estado, Ilhas_term, 
Novo_estado), em que Estado e um estado e Ilhas_term e uma lista de ilhas 
terminadas, significa que Novo_estado e o estado resultante de aplicar o 
predicado tira_ilhas_terminadas_entrada a cada uma das entradas de Estado. 
##############################################################################*/

tira_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado) :-
    findall(Nova_Entrada, 
        (member(Entrada, Estado), 
        tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada)), 
        Novo_Estado).


/*##############################################################################
marca_ilhas_terminadas_entrada/3 -> marca_ilhas_terminadas_entrada(Ilhas_term, 
Entrada, Nova_entrada), em que Ilhas_term e uma lista de ilhas terminadas e 
Entrada e uma entrada, significa que Nova_entrada e a entrada obtida de Entrada 
da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o numero de 
pontes desta e substituido por 'X'; em caso contrario Nova_entrada e igual a 
Entrada.
##############################################################################*/

marca_ilhas_terminadas_entrada(Ilhas_Term, [Ilha, Viz, Pontes], 
[ilha('X', Pos), Viz, Pontes]) :-
    subset([Ilha], Ilhas_Term),
    Ilha = ilha(_, Pos), !.

marca_ilhas_terminadas_entrada(Ilhas_Term, [Ilha, Viz, Pontes], 
[Ilha, Viz, Pontes]) :- 
    \+ subset([Ilha], Ilhas_Term).


/*##############################################################################
marca_ilhas_terminadas/3 -> marca_ilhas_terminadas(Estado, Ilhas_term, 
Novo_estado), em que Estado e um estado e Ilhas_term e uma lista de ilhas 
terminadas, significa que Novo_estado e o estado resultante de aplicar o 
predicado marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
##############################################################################*/

marca_ilhas_terminadas(Estado, Ilhas_Term, Novo_Estado) :-
    findall(Nova_Entrada, 
        (member(Entrada, Estado), 
        marca_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada)), 
        Novo_Estado).


/*##############################################################################
trata_ilhas_terminadas/2 -> trata_ilhas_terminadas(Estado, Novo_estado), em que 
Estado e um estado, significa que Novo_estado e o estado resultante de aplicar 
os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado.
##############################################################################*/

trata_ilhas_terminadas(Estado, Novo_Estado) :-
    ilhas_terminadas(Estado, Ilhas_Term),
    findall(Estado_Limpo, 
        (tira_ilhas_terminadas(Estado, Ilhas_Term, Estado_S_Ilhas), 
        marca_ilhas_terminadas(Estado_S_Ilhas, Ilhas_Term, Estado_Limpo)), 
        Novo_Estado_Quase),

    append(Novo_Estado_Quase, Novo_Estado).


/*##############################################################################
junta_pontes/5 -> junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado), 
em que Estado e um estado e Ilha1 e Ilha2 sao 2 ilhas, significa que Novo_estado 
e o estado que se obtem de Estado por adicao de Num_pontes pontes entre Ilha1 e 
Ilha2.
##############################################################################*/

junta_pontes(Estado, N_Pontes, Ilha1, Ilha2, Novo_Estado) :-
    Ilha1 = ilha(_, Pos1),
    Ilha2 = ilha(_, Pos2),

    cria_ponte(Pos1, Pos2, Ponte),
    length(L_Pontes, N_Pontes), % cria lista com tamanho N_Pontes
    maplist(=(Ponte), L_Pontes), % atribui Ponte a cada espaco da lista

    (member(E1, Estado), E1 = [Ilha1, V1, P1]), nth1(I1, Estado, E1),
    % encontra indice da entrada (E1) da Ilha1 
    (member(E2, Estado), E2 = [Ilha2, V2, P2]), nth1(I2, Estado, E2),
    % encontra indice da entrada (E2) da Ilha2

    append(L_Pontes, P1, NovaP1), NovaE1 = [Ilha1, V1, NovaP1], % insere ponte
    append(L_Pontes, P2, NovaP2), NovaE2 = [Ilha2, V2, NovaP2],

    nth1(I1, Estado, _, Estado_1), % eliminar E1, devolve Estado_1
    nth1(I1, Estado_2, NovaE1, Estado_1), % inserir NovaE1, devolve Estado_2

    nth1(I2, Estado_2, _, Estado_3), % eliminar E2, devolve Estado_3
    nth1(I2, Estado_4, NovaE2, Estado_3), % inserir NovaE2 devolve Estado_4

    actualiza_vizinhas_apos_pontes(Estado_4, Pos1, Pos2, Estado_5),
    trata_ilhas_terminadas(Estado_5, Novo_Estado).

    /*----------------------------------------------------------------------
    Cria lista com pontes a adicionar as entradas. Encontra o indice da en-
    trada da Ilha1 e Ilha2, e cria uma nova entrada identica a cada uma de-
    las, excepto a lista das pontes que e alterada com as novas pontes. Em 
    seguida elimina a entrada da Ilha1 do Estado e adiciona a nova entrada 
    no mesmo indice, o mesmo acontece com a entrada da Ilha2. Por fim apli-
    ca-se os predicados actualiza_vizinhas_apos_pontes e trata_ilhas_termi-
    nadas para atualizar o Estado. 
    ----------------------------------------------------------------------*/