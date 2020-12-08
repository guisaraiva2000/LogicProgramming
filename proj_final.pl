:- [codigo_comum].

%-----------------------------------------------------------------------------
% Autor: Guilherme Saraiva
% Numero: 93717
% Disciplina: Logica para Programacao
%-----------------------------------------------------------------------------

%-----------------------------------------------------------------------------
%                                   CODIGO
%-----------------------------------------------------------------------------

%-----------------------------------------------------------------------------
% counter(List, Ele, Count):
% Count e' o numero de Ele que existem na Lst
%-----------------------------------------------------------------------------

counter([],_,0):-!.
counter([X|T],Num,N) :- 
   number(X), X == Num,!, 
   counter(T,Num,N1), 
   N is N1 + 1.

counter([X|T],Num,N) :- 
   number(X), 
   dif(X, Num),!, 
   counter(T,Num,N).

counter([_|T],Num,N) :- 
   counter(T,Num,N).

%-----------------------------------------------------------------------------
% count_vars(List, NumVars):
% NumVars e' o numero de variaveis que existem na Lst
%-----------------------------------------------------------------------------

count_vars([], 0):-!.
count_vars([X|T], N):- 
   var(X), !,
   count_vars(T,N1),
   N is N1 + 1.

count_vars([_|T], N):-
   count_vars(T,N).

%-----------------------------------------------------------------------------
% separaLista(N, List, N_List):
% N_List e' uma matriz cuja primeira lista tem N elementos
%-----------------------------------------------------------------------------

separaLista(N, List, [P|[R]]) :- 
   append(P, R, List), length(P, N).

%-----------------------------------------------------------------------------
% list_to_matrix(List, Len, Mat):
% Mat e' a matriz que resulta de separar a lista List em Len elementos
%-----------------------------------------------------------------------------

list_to_matrix([], _, []):-!.
list_to_matrix(List, Len, [Lin|Mat]):-
   list_to_matrix_row(List, Len, Lin, R),
   list_to_matrix(R, Len, Mat).

list_to_matrix_row(R, 0, [], R).
list_to_matrix_row([Item|List], Len, [Item|Lin], R):-
   NLen is Len-1,
   list_to_matrix_row(List, NLen, Lin, R).

%-----------------------------------------------------------------------------
% retira_vars(Mat, ListNoVars):
% ListNoVars e' a lista que resulta da matriz Mat em que as variaveis 
% unificam com o numero 2
%-----------------------------------------------------------------------------

retira_vars([], []):-!.
retira_vars([[P1|R]|R1], [2|R2]):-
   var(P1),!,
   (   compare(=, R, []) ->
       retira_vars(R1, R2)
   ;   append([R], R1, NewR1),
       retira_vars(NewR1, R2)
   ).

retira_vars([[P1|R]|R1], [P1|R2]):-
   not(var(P1)),!,
   (   compare(=, R, []) ->
       retira_vars(R1, R2)
   ;   append([R], R1, NewR1),
       retira_vars(NewR1, R2)
   ).

%-----------------------------------------------------------------------------
% mat_mod_index(Mat1, Mat2, Len, N_Mat):
% N_Mat e' a matriz que contem os indexes correspondentes aos elementos 
% que sao diferentes em Mat1 e em Mat2  
%-----------------------------------------------------------------------------

mat_mod_index([], [],_, []):-!.
mat_mod_index([P1|R1],[P2|R2],Len, [P3|R3]):-
   append([P1], R1, Mat1),
   retira_vars(Mat1, List1NoVars),
   list_to_matrix(List1NoVars, Len, Mat1NoVars),
   append([P2], R2, Mat2),
   retira_vars(Mat2, List2NoVars),
   list_to_matrix(List2NoVars, Len, Mat2NoVars),
   [N_P1|_] = Mat1NoVars,
   [N_P2|_] = Mat2NoVars,
   dif_el_pos(N_P1, N_P2, P3),
   mat_mod_index(R1, R2, Len, R3).

%-----------------------------------------------------------------------------
% devolve_vet_LinCol(Mat_Index_Lin, Mat_Index_Col, VetLin, VetCol):
% VetLin e VetCol sao as listas correspondes aos indexes das linhas e da
% colunas respetivamente
%-----------------------------------------------------------------------------

devolve_vet_LinCol([], [], [], []):-!.
devolve_vet_LinCol(Mat_Index_Lin, Mat_Index_Col, VetLin, VetCol):-
   flatten(Mat_Index_Lin, VetLin),
   flatten(Mat_Index_Col, VetCol).
  
%-----------------------------------------------------------------------------
% dif_el_pos(List1, List2, List3):
% List3 e' a lista que contem os indexes correspondentes aos elementos 
% que sao diferentes em List1 e em List2  
%-----------------------------------------------------------------------------

dif_el_pos(List1, List2, List3) :-
   findIndex(List1,List2,1, List3).

%-----------------------------------------------------------------------------
% findIndex(List1, List2, Count, List4):
% Predicado auxliar de dif_el_pos que encontra os indexes dos diferentes
% elementos nas listas List1 e List2 e os coloca em List4
%-----------------------------------------------------------------------------

findIndex([],_,_,[]) :- !.
findIndex(_,[],_,[]).

findIndex([P1|R1], [P2|R2], Count, List4):- 
   (   P1 \== P2 ->
       append([Count], FinalList, List4),
       NewCount is Count+1,
       findIndex(R1,R2,NewCount,FinalList)
   ;   NewCount is Count+1,
       findIndex(R1,R2,NewCount, List4)
   ).

%-----------------------------------------------------------------------------
% index(Matrix, Lin, Col, N):
% Lin e Col sao o numero da linha e da coluna, respetivamente, onde se 
% encontra o primeiro elemento N na matriz Matrix
%-----------------------------------------------------------------------------

index(Matrix, Lin, Col, N):-
   nth1(Lin, Matrix, MatrixRow),
   nth1(Col, MatrixRow, N).

%-----------------------------------------------------------------------------
% compare(List1, List2):
% Predicado que verifica se List1 esta contida em List2 
%-----------------------------------------------------------------------------
compare([],_):-!.
compare([P|Targets],[P|Checks]) :-
   compare(Targets,Checks).

compare(Targets,[_|Checks]) :-
   compare(Targets,Checks).

%-----------------------------------------------------------------------------
% aplica_R1_triplo(Triplo, N_Triplo):
% Predicado que conta o numero de uns e zeros de Triplo e em que N_Triplo 
% e' o resultado de colocar ou nao um 1 ou 0 no lugar duma variavel conforme 
% o numero de uns e zeros contado  
%-----------------------------------------------------------------------------

aplica_R1_triplo([], []):-!.
aplica_R1_triplo(L, RES):- 
   counter(L, 0, COUNT), 
   COUNT == 2,!, 
   aplica_R1_triplo_aux1(L, RES).

aplica_R1_triplo(L, L):- 
   counter(L, 0, COUNT), 
   COUNT == 1,!.

aplica_R1_triplo(L, _):- 
   counter(L, 0, COUNT), 
   COUNT == 3,!, 
   fail.

aplica_R1_triplo(L, RES):- 
   counter(L,1, COUNT), 
   COUNT == 2,!, 
   aplica_R1_triplo_aux2(L, RES).

aplica_R1_triplo(L, L):- 
   counter(L,1, COUNT), 
   COUNT == 1,!.

aplica_R1_triplo(L, _):- 
   counter(L,1, COUNT), 
   COUNT == 3,!, 
   fail.

aplica_R1_triplo(L, L):- 
   counter(L,1, COUNT), 
   COUNT == 0,!.

aplica_R1_triplo(L, L):- 
   counter(L,0, COUNT), 
   COUNT == 0,!.

%-----------------------------------------------------------------------------
% aplica_R1_triplo_aux1(Fila, N_Fila):
% Predicado que unifica a var de Triplo (se houver) com 1
%-----------------------------------------------------------------------------

aplica_R1_triplo_aux1([], []):-!.
aplica_R1_triplo_aux1([P1|R1], [1|R2]):- 
   var(P1),!, 
   aplica_R1_triplo_aux1(R1, R2).

aplica_R1_triplo_aux1([P1|R1], [P1|R2]):- 
   aplica_R1_triplo_aux1(R1, R2).

%-----------------------------------------------------------------------------
% aplica_R1_triplo_aux2(Fila, N_Fila):
% Predicado que unifica a var de Triplo (se houver) com 0
%-----------------------------------------------------------------------------

aplica_R1_triplo_aux2([], []):-!.
aplica_R1_triplo_aux2([P1|R1], [0|R2]):- 
   var(P1),!,
   aplica_R1_triplo_aux2(R1, R2).

aplica_R1_triplo_aux2([P1|R1], [P1|R2]):- 
   aplica_R1_triplo_aux2(R1, R2).

%-----------------------------------------------------------------------------
% aplica_R1_fila_aux(Fila, N_Fila):
% Predicado que itera Triplo de um em um e aplica o predicado
% aplica_R1_triplo/2 a sublistas com 3 elementos
%-----------------------------------------------------------------------------

aplica_R1_fila_aux([], []):-!.                            
aplica_R1_fila_aux([P1|R1], [P2|R2]):-
   append([P1], R1, Fila),
   length(Fila, NumEl),
   NumEl > 3,!,
   separaLista(3, Fila, SepList),
   nth1(1, SepList, L1),
   aplica_R1_triplo(L1, Res),
   [X,Y,Z] = Res, [A,B,C] = L1,
   (   X == A, Y == B, Z == C ->
       P2 = P1,
       aplica_R1_fila_aux(R1, R2)
   ;   X \== A ->
       P2 = X,
       aplica_R1_fila_aux(R1, R2)
   ;   Y \== B ->
       P2 = P1,
       [_| R1_aux] = R1,
       append([Y], R1_aux, NewR1),
       aplica_R1_fila_aux(NewR1, R2)
   ;   Z \== C ->
       P2 = P1,
       [_, _| R1_aux] = R1,
       append([B], [Z], K), append(K, R1_aux, NewR1),
       aplica_R1_fila_aux(NewR1, R2)
   ).

aplica_R1_fila_aux([P1|R1], Res):-
   append([P1], R1, Fila),
   length(Fila, NumEl),
   NumEl == 3,!,
   separaLista(3, Fila, SepList),
   nth1(1, SepList, L1),
   aplica_R1_triplo(L1, Res).

aplica_R1_fila_aux([P1|R1], [P1|R2]):-
   append([P1], R1, Fila),
   length(Fila, NumEl),
   NumEl < 3,!,
   aplica_R1_fila_aux(R1, R2).

%-----------------------------------------------------------------------------
% aplica_R1_fila(Fila, N_Fila):
% Predicado que aplica o predicado aplica_R1_fila_aux/2 a Fila ate que nao
% consiga mais, resultando dai N_Fila
%-----------------------------------------------------------------------------

aplica_R1_fila([],[]):-!.
aplica_R1_fila(Fila, N_Fila):-
   aplica_R1_fila_aux(Fila, L), 
   not(compare(=, L, Fila)),!,
   aplica_R1_fila_aux(L, L1), 
   aplica_R1_fila(L1, N_Fila).

aplica_R1_fila(Fila, N_Fila):- 
   aplica_R1_fila_aux(Fila, N_Fila), 
   compare(=, N_Fila, Fila).

%-----------------------------------------------------------------------------
% aplica_R2_fila(Fila, N_Fila):
% Predicado que conta o numero de uns e zeros de Fila e coloca (ou nao em 
% caso de falha) zeros ou uns no lugar das variaveis de Fila para que o numero 
% de zeros seja igual ao numero de uns
%-----------------------------------------------------------------------------

aplica_R2_fila([],[]):-!.
aplica_R2_fila(Fila, N_Fila):-
   length(Fila, NumEl), 
   counter(Fila, 0, NumZ),
   NumZ is NumEl/2, !, 
   aplica_R1_triplo_aux1(Fila, N_Fila).

aplica_R2_fila(Fila, N_Fila):-
   length(Fila, NumEl), 
   counter(Fila, 1, NumU),
   NumU is NumEl/2, !, 
   aplica_R1_triplo_aux2(Fila, N_Fila).

aplica_R2_fila(Fila, Fila):-
   length(Fila, NumEl), 
   counter(Fila, 0, NumZ),
   NumEl/2 > NumZ, !.

aplica_R2_fila(Fila, _):-
   length(Fila, NumEl), 
   counter(Fila, 0, NumZ),
   NumEl/2 < NumZ, !, 
   fail.

aplica_R2_fila(Fila, Fila):-
   length(Fila, NumEl), 
   counter(Fila, 1, NumU),
   NumEl/2 > NumU, !.

aplica_R2_fila(Fila, _):-
   length(Fila, NumEl), 
   counter(Fila, 1, NumU),
   NumEl/2 < NumU, !, 
   fail.

%-----------------------------------------------------------------------------
% aplica_R1_R2_fila(Fila, N_Fila):
% Predicado que aplica os predicados aplica_R1_fila/2 e aplica_R2_fila/2 
% a Fila, resultando dai N_Fila
%-----------------------------------------------------------------------------

aplica_R1_R2_fila([], []):-!.
aplica_R1_R2_fila(Fila, N_Fila):- 
   aplica_R1_fila(Fila, L), 
   aplica_R2_fila(L, N_Fila).

%-----------------------------------------------------------------------------
% aplica_R1_R2_puzzle(Puz, N_Puz):
% Predicado que aplica o predicado aplica_R1_R2_fila/2, as linhas e as colunas
% de Puz, resultando dai N_Fila
%-----------------------------------------------------------------------------

aplica_R1_R2_puzzle([], []):-!.
aplica_R1_R2_puzzle(Puz, N_Puz):- 
   aplica_R1_R2_puzzle_linha(Puz, L1),
   mat_transposta(L1, L1_Trans),
   aplica_R1_R2_puzzle_linha(L1_Trans, L2),
   mat_transposta(L2, N_Puz).

%-----------------------------------------------------------------------------
% aplica_R1_R2_puzzle_linha(Puz, N_Puz):
% Predicado que aplica o predicado aplica_R1_R2_fila/2, as linhas de Puz,
% resultando dai N_Fila
%-----------------------------------------------------------------------------

aplica_R1_R2_puzzle_linha([], []):-!.
aplica_R1_R2_puzzle_linha([P1|R1],[L|R2]):-
   aplica_R1_R2_fila(P1, L),
   aplica_R1_R2_puzzle_linha(R1,R2).

%-----------------------------------------------------------------------------
% inicializa(Puz, N_Puz):
% Predicado que aplica o predicado aplica_R1_R2_fila_aux/2 a Puz ate que nao 
% consiga mais, resultando dai N_Puz
%-----------------------------------------------------------------------------

inicializa([],[]):-!.
inicializa(Puz, N_Puz):-
   aplica_R1_R2_puzzle(Puz, L), 
   not(compare(=, L, Puz)),
   aplica_R1_R2_puzzle(L, L1), 
   inicializa(L1, N_Puz).

inicializa(Puz, N_Puz):- 
   aplica_R1_R2_puzzle(Puz, N_Puz), 
   compare(=, N_Puz, Puz).

%-----------------------------------------------------------------------------
% verifica_R3(Puz):
% Predicado que verifica se no Puz existem linhas e colunas iguais
%-----------------------------------------------------------------------------

verifica_R3([]).
verifica_R3(Puz):- 
   length(Puz, Len),
   retira_vars(Puz, PuzNoVars),
   list_to_matrix(PuzNoVars, Len, NewPuz),
   verifica_R3_linlin(NewPuz), 
   verifica_R3_colcol(NewPuz).

%-----------------------------------------------------------------------------
% verifica_R3_colcol(Puz):
% Predicado que verifica se no Puz existem colunas iguais
%-----------------------------------------------------------------------------

verifica_R3_colcol([]).
verifica_R3_colcol(Puz):-
   mat_transposta(Puz, PuzTrans),
   verifica_R3_linlin(PuzTrans).

%-----------------------------------------------------------------------------
% verifica_R3_linlin(Puz):
% Predicado que verifica se no Puz existem linhas iguais
%-----------------------------------------------------------------------------

verifica_R3_linlin([]).
verifica_R3_linlin([P1|R1]):-
   not(memberchk(2, P1)),!,
   (  memberchk(P1, R1) ->
      fail
   ;  verifica_R3_linlin(R1)
   ).

verifica_R3_linlin([P1|R1]):-
   memberchk(2, P1),!,
   verifica_R3_linlin(R1).

%-----------------------------------------------------------------------------
% propaga_posicoes(VetCoor, Puz, N_Puz):
% Predicado que a aplica o predicado aplica_R1_R2_fila/2 as coordenadas de
% VetCoor no Puz ate nao possam haver mais coordenadas tranformaveis
%-----------------------------------------------------------------------------

propaga_posicoes([], [], []):-!.
propaga_posicoes([P1|R1], Puz, N_NewPuz):-
   verifica_R3(Puz),
   length(Puz, Len),
   (IndexLin, IndexCol) = P1,                               %-----------------
   nth1(IndexLin, Puz, Lin),                                %
   aplica_R1_R2_fila(Lin, N_Lin),                           %      Linha
   mat_muda_linha(Puz, IndexLin, N_Lin, NewPuz),            %
   mat_mod_index(Puz, NewPuz,Len, Mat_Col_Pos),             %
   mat_transposta(NewPuz, NewPuzTrans),                     %-----------------   
   nth1(IndexCol, NewPuzTrans, Col),                        %-----------------
   aplica_R1_R2_fila(Col, N_Col),                           %     
   mat_muda_linha(NewPuzTrans, IndexCol, N_Col, NewPuz2),   %     Coluna
   mat_mod_index(NewPuzTrans, NewPuz2, Len, Mat_Lin_Pos),   %
   mat_transposta(NewPuz2, N_NewPuz),                       %-----------------       
   devolve_vet_LinCol(Mat_Lin_Pos, Mat_Col_Pos, VetLin, VetCol),
   propaga_posicoes_aux_lin(IndexLin, VetCol, VetCoorLin),
   propaga_posicoes_aux_col(IndexCol, VetLin, VetCoorCol),
   append(VetCoorLin, VetCoorCol, VetCoor),
   append(R1, VetCoor, RCoor),
   propaga_posicoes(RCoor, N_NewPuz, N_NewPuz).

propaga_posicoes(_ , Puz, Puz).

%-----------------------------------------------------------------------------
% propaga_posicoes_aux_lin(IndexLin, VetCol, VetCoorLin):
% VetCoorLin corresponde a lista com o index das coordenadas onde as
% posicoes da linha IndexLin foram alteradas
%-----------------------------------------------------------------------------

propaga_posicoes_aux_lin(_, [], []):-!.
propaga_posicoes_aux_lin(IndexLin, [P2|R2], [(IndexLin, P2)|R3]):-
   propaga_posicoes_aux_lin(IndexLin, R2, R3).

%-----------------------------------------------------------------------------
% propaga_posicoes_aux_col(IndexCol, VetLin, VetCoorCol):
% VetCoorCol corresponde a lista com o index das coordenadas onde as
% posicoes da coluna IndexCol foram alteradas
%-----------------------------------------------------------------------------

propaga_posicoes_aux_col(_, [], []):-!.
propaga_posicoes_aux_col(IndexCol, [P2|R2], [(P2, IndexCol)|R3]):-
   propaga_posicoes_aux_col(IndexCol, R2, R3).

%-----------------------------------------------------------------------------
% resolve(Puz, Sol):
% Predicado que incializa o Puz e o propaga, caso necessario, e coloca zeros
% ou uns nas restantes posicoes do Puz
%-----------------------------------------------------------------------------

resolve([], []):-!.
resolve(Puz, Puz):-
   verifica_R3(Puz),
   flatten(Puz , ListPuz),
   count_vars(ListPuz, NumVars),
   NumVars == 0.

resolve(Puz, Sol):-
   verifica_R3(Puz),
   inicializa(Puz, Sol),
   flatten(Sol , ListPuz),
   count_vars(ListPuz, NumVars),
   NumVars == 0.

resolve(Puz, Sol):-
   verifica_R3(Puz),
   inicializa(Puz, InitPuz),
   resolve_aux(InitPuz, Sol).

%-----------------------------------------------------------------------------
% resolve_aux(Puz, Sol_Aux):
% Predicado que coloca 0 ou 1 numa das posicoes de Puz onde exista variavel
%-----------------------------------------------------------------------------

resolve_aux(Puz, Sol_Aux):-
   length(Puz, Len),
   retira_vars(Puz, PuzListNoVars),                  
   list_to_matrix(PuzListNoVars, Len, PuzNoVars),
   index(PuzNoVars, Lin, Col, 2),
   mat_muda_posicao(Puz, (Lin, Col), 0, NewPuz),           % coloca 0
   nth1(Lin, NewPuz, Line),
   compare([0, 0, 0], Line),
   propaga_posicoes([(Lin, Col)], NewPuz, Sol_Aux),
   resolve(Sol_Aux, Sol_Aux). 

resolve_aux(Puz, Sol_Aux):-
   length(Puz, Len),
   retira_vars(Puz, PuzListNoVars),
   list_to_matrix(PuzListNoVars, Len, PuzNoVars),
   index(PuzNoVars, Lin, Col, 2),
   mat_muda_posicao(Puz, (Lin, Col), 1, NewPuz),           % coloca 1
   nth1(Lin, NewPuz, Line),
   compare([1, 1, 1], Line),
   propaga_posicoes([(Lin, Col)], NewPuz,Sol_Aux),
   resolve(Sol_Aux, Sol_Aux). 
