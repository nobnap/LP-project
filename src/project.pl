% Projeto de Logica para Programacao

:- [codigo_comum].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% extrai_ilhas_linha/3 %
% Tem 3 argumentos, N_L, Linha e Ilhas, que representam respetivamente o numero da linha,
% a linha em si e as ilhas presentes na linha. Recorre a um predicado auxiliar: extrai_ilhas_linha/4.
extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, Ilhas, 1).

% extrai_ilhas_linha/4 %
extrai_ilhas_linha(_, [], [], _) :- !.

% O argumento Aux representa o numero da coluna que esta a ser "analisada"
extrai_ilhas_linha(N_L, [P | R], Ilhas, Aux) :-
    P =\= 0,
    !,
    Ilha =..[ilha, P, (N_L, Aux)],
    Aux_novo is Aux+1,
    extrai_ilhas_linha(N_L, R, Ilhas_novo, Aux_novo),
    append([Ilha], Ilhas_novo, Ilhas).

extrai_ilhas_linha(N_L, [_ | R], Ilhas, Aux) :-
    Aux_novo is Aux+1,
    extrai_ilhas_linha(N_L, R, Ilhas, Aux_novo).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% ilhas/2 %
% Tem 2 argumentos, Puz e Ilhas, que representam um puzzle e as ilhas nele presente
% respetivamente. Recorre a um predicado auxiliar ilhas_aux/2 que processa cada linha do puzzle
% com ajuda do predicado extrai_ilhas_linha/3. 
ilhas(Puz, Ilhas) :-
    setof([In, Linha], (member(Linha, Puz), nth1(In, Puz, Linha)), Puz_Aux),
    % Puz_Aux eh uma lista de listas, cada uma com o formato [numero da linha, linha]
    ilhas_aux(Puz_Aux, Ilhas).

% ilhas_aux/2 %
ilhas_aux([], []) :- !.

ilhas_aux([P | R], Ilhas) :-
    P = [Index, Linha],
    extrai_ilhas_linha(Index, Linha, Ilhas_aux),
    ilhas_aux(R, Ilhas_res),
    append(Ilhas_aux, Ilhas_res, Ilhas).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% vizinhas/3 %
% Tem 3 argumentos, Ilhas, Ilha e Vizinhas, que representam as ilhas existentes, uma ilha
% em particular e as suas vizinhas respetivamente. Recorre a 4 predicados auxiliares: 
% mesma_coluna/3, mesma_linha/3, encontra_ante/3 e encontra_apos/3.
vizinhas(Ilhas, Ilha, Vizinhas) :-
    % Cria duas listas Coluna e Linha, com todas as ilhas na coluna e na linha da ilha principal
    mesma_coluna(Ilha, Ilhas, Coluna),
    mesma_linha(Ilha, Ilhas, Linha),
    % Procura as ilhas imediatamente antes e apos a ilha principal em cada coluna e linha
    encontra_ante(Ilha, Coluna, Viz_cima),
    encontra_ante(Ilha, Linha, Viz_esq),
    encontra_apos(Ilha, Linha, Viz_dir),
    encontra_apos(Ilha, Coluna, Vir_baixo),
    append([Viz_cima, Viz_esq, Viz_dir, Vir_baixo], Vizinhas).

% mesma_coluna/3 %
% Tem 3 argumentos, Ilha, Ilhas e Coluna, que representam uma ilha, todas as ilhas existentes
% e as ilhas que estao na mesma coluna que a principal respetivamente.
mesma_coluna(_, [], []) :- !.

mesma_coluna(Ilha, [P | R], Coluna) :- 
    Ilha =..[_, _, (_, X_Ilha)],
    P =..[_, _, (_, X_Viz)],
    X_Viz =:= X_Ilha, !,
    mesma_coluna(Ilha, R, Coluna_Aux),
    Coluna = [P | Coluna_Aux].

mesma_coluna(Ilha, [_ | R], Coluna) :- mesma_coluna(Ilha, R, Coluna).

% mesma_linha/3 %
% Tem 3 argumentos, Ilha, Ilhas e Linha, que representam uma ilha, todas as ilhas existentes
% e as ilhas que estao na mesma linha que a principal respetivamente.
mesma_linha(_, [], []) :- !.

mesma_linha(Ilha, [P | R], Linha) :- 
    Ilha =..[_, _, (Y_Ilha, _)],
    P =..[_, _, (Y_Viz, _)],
    Y_Viz =:= Y_Ilha, !,
    mesma_linha(Ilha, R, Linha_Aux),
    Linha = [P | Linha_Aux].

mesma_linha(Ilha, [_ | R], Linha) :- mesma_linha(Ilha, R, Linha).

% encontra_ante/3 %
% Tem 3 argumentos, Ilha, Ilhas e Vizinha, que representam uma ilha, uma lista de ilhas e
% a ilha que se encontra diretamente antes da ilha principal na lista.
encontra_ante(_, [], []) :- !.

encontra_ante(Ilha, [Viz, Ilha | _], [Viz]) :- !.

encontra_ante(Ilha, [_ | R], Vizinha) :- encontra_ante(Ilha, R, Vizinha).

% encontra_apos/3 %
% Tem 3 argumentos, Ilha, Ilhas e Vizinha, que representam uma ilha, uma lista de ilhas e
% a ilha que se encontra diretamente apos a ilha principal na lista.
encontra_apos(_, [], []) :- !.

encontra_apos(Ilha, [Ilha, Viz | _], [Viz]) :- !.

encontra_apos(Ilha, [_ | R], Vizinha) :- encontra_apos(Ilha, R, Vizinha).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% estado/2 %
% Tem 2 argumentos, Ilhas e Estado, que representam todas as ilhas existentes e o estado do
% puzzle onde pertencem essas mesmas ilhas.
estado(Ilhas, Estado) :-
    % Cria uma lista de listas com o formato [Ilha, Vizinhas, Pontes] no qual Pontes = []
    findall([Ilha, Vizinhas, []], (member(Ilha, Ilhas), vizinhas(Ilhas, Ilha, Vizinhas)), Estado).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% posicoes_entre/3 %
% Tem 3 argumentos, Pos1, Pos2 e Posicoes, que representam 2 posicoes e as posicoes entre
% as mesmas.
posicoes_entre(Pos1, Pos2, Posicoes) :-
    Pos1 = (L1, C1),
    Pos2 = (L2, C2),
    % Caso as posicoes nao estejam na mesma linha ou coluna, o predicado falha
    (L1 =\= L2, C1 =\= C2 -> fail;
    % Caso as posicoes sejam iguais, nao existem posicoes entre elas
    L1 =:= L2, C1 =:= C2 -> Posicoes = [];
    % Nos restantes casos determina-se se esta na mesma coluna ou na mesma linha e
    % mantem-se igual o numero relativo a mesma, iterando sobre o outro
    L1 =:= L2, C1 =\= C2 -> max_member(Max, [C1, C2]),
        min_member(Min, [C1, C2]),
        findall(X, between(Min, Max, X), Cord),
        findall((L1, X), member(X, Cord), Posicoes_aux),
        subtract(Posicoes_aux, [Pos1, Pos2], Posicoes);
    L1 =\= L2, C1 =:= C2 -> max_member(Max, [L1, L2]),
        min_member(Min, [L1, L2]),
        findall(X, between(Min, Max, X), Cord),
        findall((X, C1), member(X, Cord), Posicoes_aux),
        subtract(Posicoes_aux, [Pos1, Pos2], Posicoes)).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% cria_ponte/3 %
% Tem 3 argumentos, Pos1, Pos2 e Ponte, que representam 2 posicoes e a ponte entre elas.
cria_ponte(Pos1, Pos2, Ponte) :-
    Pos1 = (L1, C1),
    Pos2 = (L2, C2),
    % Falha se as posicoes forem iguais ou tiverem coluna e linha diferentes
    (L1 =:= L2, C1 =:= C2 -> fail;
    L1 =\= L2, C1 =\= C2 -> fail;
    max_member(MaxC, [C1, C2]),
    min_member(MinC, [C1, C2]),
    max_member(MaxL, [L1, L2]),
    min_member(MinL, [L1, L2]),
    Ponte = ponte((MinL, MinC), (MaxL, MaxC))).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% caminho_livre/5 %
% Tem 5 argumentos, Pos1, Pos2, Posicoes, I e Vz, que representam 2 posicoes, as posicoes
% entre elas, uma ilha e a sua vizinha respetivamente. Recorre ao predicado posicoes_entre/3.
caminho_livre(_, _, Posicoes, I, Vz) :-
    I =..[_, _, PosI],
    Vz =..[_, _, PosVz],
    posicoes_entre(PosI, PosVz, Pos_geral),
    (Pos_geral == Posicoes;
    subtract(Pos_geral, Posicoes, Pos_aux),
    Pos_geral == Pos_aux).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% actualiza_vizinhas_entrada/5 %
% Tem 5 argumentos, Pos1, Pos2, Posicoes, Entrada e Nova_Entrada, que representam 2 posicoes,
% as posicoes entre as mesmas, uma entrada do puzzle, e a entrada depois de retirar-se as 
% ilhas que deixaram de ser vizinhas devido a ponte.
% Recorre ao predicado auxiliar actualiza_aux/5.
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada) :-
    Entrada = [Ilha, Viz, Pontes],
    actualiza_aux(Ilha, Viz, [Pos1, Pos2], Posicoes, Viz_novo),
    Nova_Entrada = [Ilha, Viz_novo, Pontes].

% actualiza_aux/5 %
% Tem 5 argumentos, Ilha, Viz, Pos, Posicoes e Viz_novo, que representam uma ilha, as suas
% ilhas vizinhas, as posicoes entre quais vao ser colocadas uma ponte, as posicoes da ponte
% e as ilhas vizinhas da ilha principal depois da ponte ser colocada.
actualiza_aux(_, [], _, _, []) :- !.

actualiza_aux(Ilha, Viz, Pos, Posicoes, Viz_novo) :-
    Viz = [P | R],
    Ilha =..[_, _, PosI],
    P =..[_, _, PosVz],
    % Uma ilha continua a ser vizinha se a ponte a ser colocada foi colocada entre essas ilhas
    % ou se nao existir nenhuma posicao entre as duas ilhas que fique ocupada pela ponte
    (member(PosI, Pos), member(PosVz, Pos);
    posicoes_entre(PosI, PosVz, Pos_aux),
    findall(X, (member(X, Pos_aux), member(X, Posicoes)), Ocupada),
    Ocupada = []), !,
    actualiza_aux(Ilha, R, Pos, Posicoes, Viz_aux),
    Viz_novo = [P | Viz_aux].
    
actualiza_aux(Ilha, [_ | R], Pos, Posicoes, Viz_novo) :-
    actualiza_aux(Ilha, R, Pos, Posicoes, Viz_novo).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% actualiza_vizinhas_apos_pontes/4 %
% Tem 4 argumentos, Estado, Pos1, Pos2 e Novo_Estado, que representam um estado do puzzle,
% 2 posicoes entre as quais vao ser colocadas uma ponte e o estado com as vizinhas de cada 
% ilha atualizadas apos essa ponte ser colocada.
% Recorre ao predicado actualiza_vizinhas_entrada/5.
actualiza_vizinhas_apos_pontes([], _, _, []) :- !.

actualiza_vizinhas_apos_pontes([Entrada | R], Pos1, Pos2, Novo_Estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada),
    actualiza_vizinhas_apos_pontes(R, Pos1, Pos2, Estado_Aux),
    Novo_Estado = [Nova_Entrada | Estado_Aux].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% ilhas_terminadas/2 %
% Tem 2 argumentos, Estado e Ilhas_Term, que representam um estado do puzzle e a lista de
% ilhas que ja tem todas as pontes associadas.
ilhas_terminadas([], []) :- !.

ilhas_terminadas([P | R], Ilhas_Term) :-
    P = [Ilha, _, Pontes],
    Ilha =..[_, N, _],
    length(Pontes, N),
    !,
    ilhas_terminadas(R, Ilhas_Aux),
    Ilhas_Term = [Ilha | Ilhas_Aux].

ilhas_terminadas([_ | R], Ilhas_Term) :- ilhas_terminadas(R, Ilhas_Term).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% tira_ilhas_terminadas_entrada/3 %
% Tem 3 argumentos, Ilhas_Term, Entrada e Nova_entrada, que representam a lista de ilhas
% que ja tem todas as pontes associadas, uma entrada do puzzle e a entrada apos serem retiradas
% das listas de vizinhas as ilhas em Ilhas_Term.
tira_ilhas_terminadas_entrada(Ilhas_Term, Entrada, Nova_Entrada) :-
    Entrada = [Ilha, Viz, Pontes],
    subtract(Viz, Ilhas_Term, Viz_novo),
    Nova_Entrada = [Ilha, Viz_novo, Pontes].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% tira_ilhas_terminadas/3 %
% Tem 3 argumentos, Estado, Ilhas_Term e Novo_Estado, que representam um estado do puzzle,
% a lista de ilhas que ja tem todas as pontes associadas e o estado do puzzle apos serem
% retiradas da lista de vizinhas de cada entrada as ilhas em Ilhas_Term.
tira_ilhas_terminadas([], _, []) :- !.

tira_ilhas_terminadas([P | R], Ilhas_Term, Novo_Estado) :-
    tira_ilhas_terminadas_entrada(Ilhas_Term, P, Nova_Entrada),
    tira_ilhas_terminadas(R, Ilhas_Term, Estado_Aux),
    Novo_Estado = [Nova_Entrada | Estado_Aux].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% marca_ilhas_terminadas_entrada/3 %
% Tem 3 argumentos, Ilhas_Term, Entrada e Nova_Entrada, que representam a lista de ilhas 
% que ja tem todas as pontes associadas, uma entrada do puzzle e essa entrada depois de 
% substituir na entrada de cada ilha terminada o numero de pontes por "X".
marca_ilhas_terminadas_entrada([], Entrada, Entrada).

marca_ilhas_terminadas_entrada([P | _], Entrada, Nova_Entrada) :-
    Entrada = [Ilha, Viz, Pontes],
    Ilha =..[A, _, B],
    % Se a ilha terminada for igual a ilha da entrada, o seu numero de pontes = "X"
    P == Ilha,
    !,
    Ilha_novo =..[A, "X", B],
    Nova_Entrada = [Ilha_novo, Viz, Pontes].

% Caso isso nao se verifique, procura-se a proxima ilha terminada na lista
marca_ilhas_terminadas_entrada([_ | R], Entrada, Nova_Entrada) :- 
    marca_ilhas_terminadas_entrada(R, Entrada, Nova_Entrada).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% marca_ilhas_terminadas/3 %
% Tem 3 argumentos, Estado, Ilhas_Term e Novo_Estado, que representam um estado do puzzle,
% a lista de ilhas que ja tem todas as pontes associadas e o estado depois de substituir
% nas ilhas terminadas o numero de pontes por "X".
% Recorre ao predicado marca_ilhas_terminadas_entrada/3.
marca_ilhas_terminadas([], _, []) :- !.

marca_ilhas_terminadas([P | R], Ilhas_Term, Novo_Estado) :-
    marca_ilhas_terminadas_entrada(Ilhas_Term, P, P_novo),
    marca_ilhas_terminadas(R, Ilhas_Term, Estado_Aux),
    Novo_Estado = [P_novo | Estado_Aux].

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% trata_ilhas_terminadas/2 %
% Tem 2 argumentos, Estado e Novo_Estado, que representam um estado do puzzle e esse estado
% depois de serem aplicados os predicados tira_ilhas_terminadas/3 e marca_ilhas_terminadas/3, 
% recorrendo tambem ao predicado ilhas_terminadas/2.
trata_ilhas_terminadas(Estado, Novo_Estado) :-
    ilhas_terminadas(Estado, Ilhas_Term),
    tira_ilhas_terminadas(Estado, Ilhas_Term, Estado_Aux),
    marca_ilhas_terminadas(Estado_Aux, Ilhas_Term, Novo_Estado).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%
% junta_pontes/5 %
% Tem 5 argumentos, Estado, N_pontes, Ilha1, Ilha2 e Novo_Estado, que representam um estado
% de um puzzle, o numero de pontes a adicionar, as ilhas entre quais as ponte serao adicionadas
% e o estado apos serem feitas as alteracoes relativas a adicao das pontes.
% Recorre aos predicados cria_ponte/3, adiciona_ponte/5, actualiza_vizinhas_apos_pontes/4
% e trata_ilhas_terminadas/2.
junta_pontes(Estado, N_pontes, Ilha1, Ilha2, Novo_Estado) :-
    Ilha1 =..[_, _, Pos1],
    Ilha2 =..[_, _, Pos2],
    cria_ponte(Pos1, Pos2, Ponte),
    adiciona_ponte(Estado, N_pontes, [Ilha1, Ilha2], Ponte, Estado_Aux1),
    actualiza_vizinhas_apos_pontes(Estado_Aux1, Pos1, Pos2, Estado_Aux2),
    trata_ilhas_terminadas(Estado_Aux2, Novo_Estado).

% adiciona_ponte/5 %
% Tem 5 argumentos, Estado, N_pontes, Ilhas, Ponte e Novo_estado, que representam um estado
% de um puzzle, o numero de pontes a adicionar, as ilhas entre quais as pontes serao adiconadas,
% a ponte a adiconar e o estado apos as pontes serem adiconadas as ilhas corretas.
adiciona_ponte([], _, _, _, []) :- !.

adiciona_ponte([Entrada | R], N_pontes, Ilhas, Ponte, Novo_Estado) :-
    Entrada = [Ilha, Viz, Pontes],
    % Apenas se adiciona a ponte a entrada se a Ilha pertencer a Ilhas
    member(Ilha, Ilhas), !,
    length(Ponte_Aux, N_pontes),
    maplist(=(Ponte), Ponte_Aux),
    append(Pontes, Ponte_Aux, Pontes_novo),
    Nova_Entrada = [Ilha, Viz, Pontes_novo],
    adiciona_ponte(R, N_pontes, Ilhas, Ponte, Estado_Aux),
    Novo_Estado = [Nova_Entrada | Estado_Aux].

% Caso contrario, a entrada mantem-se igual
adiciona_ponte([Entrada | R], N_pontes, Ilhas, Ponte, Novo_Estado) :- 
    adiciona_ponte(R, N_pontes, Ilhas, Ponte, Estado_Aux),
    Novo_Estado = [Entrada | Estado_Aux].
%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%