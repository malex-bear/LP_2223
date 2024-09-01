%ist1106120_Maria_Medvedeva
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.

eventosSemSalas(EventosSemSalas):-
    /*eventosSemSalas(-List)
     * Verdade se EventosSemSalas for uma lista, ordenada,
     dos eventos sem sala.
     */
    findall(ID,evento(ID,_,_,_,'semSala'),Eventos),
    sort(Eventos,EventosSemSalas).

intersecao([],_,[]):-!.%auxiliar
intersecao([H|T],El,[H|Res]):-
    /*intersecao(+List,+List,-List)
     * Verdade se a lista final for uma intersecao das
     * duas listas introduzidas.
     */
    member(H,El),
    !,
    intersecao(T,El,Res).

intersecao([_|T],El,Res):-
    intersecao(T,El,Res).

eventosSemSalasDiaSemana(DiaSemana,EventosSemSala):-
    /*eventosSemSalasDiaSemana(+Term,-List)
     * Verdade se EventosSemSala for uma lista ordenada de
     eventos sem sala que ocorrem no dia especificado.
     */
    findall(ID,horario(ID,DiaSemana,_,_,_,_),Eventos),
    sort(Eventos,Eventoslimpo),
    eventosSemSalas(EventosSemSalas),
    intersecao(Eventoslimpo,EventosSemSalas,EventosSemSala).

eventosSemSalasPeriodo([],[]):-!.
eventosSemSalasPeriodo(P,EventosSemSala):-
    /*eventosSemSalasPeriodo(+List,-List)
     *Verdade se EventosSemSala for uma lista ordenada
     *de eventos sem sala que ocorrem no
     *periodo especificado.
     */
    findall(ID,(horario(ID,_,_,_,_,Periodo),member(Periodo,P)),
            Eventos),
    findall(Id,horario(Id,_,_,_,_,'p1_2'),EventosSemestrais),
    append(Eventos,EventosSemestrais,Eventostds),
    sort(Eventostds,Eventoslimpo),
    eventosSemSalas(EventosSemSalas),
    intersecao(Eventoslimpo,EventosSemSalas,EventosSemSala).

organizaEventosAux([],_,[]):-!.
organizaEventosAux([H|Ids],Periodo,[H|RestoEventos]):-
    %Organizar por periodos
    Periodo \= 'p1_2',
    Periodo \= 'p3_4',
    (horario(H,_,_,_,_,Periodo);
    ((horario(H,_,_,_,_,'p1_2'),(Periodo == 'p1';Periodo == 'p2'));
    (horario(H,_,_,_,_,'p3_4'),(Periodo == 'p3';Periodo == 'p4')))),
    !,
    organizaEventosAux(Ids,Periodo,RestoEventos).
organizaEventosAux([_|Ids],Periodo,RestoEventos):-
    organizaEventosAux(Ids,Periodo,RestoEventos).

organizaEventos(ListaEventos,Periodo,EventosNoPeriodo):-
    /*organizaEventos(+List,+Term,-List)
     * Verdade se EventosNoPeriodo, a partir da lista introduzida,
     uma lista ordenada dos eventos que ocorrem no periodo especificado.
     */
    organizaEventosAux(ListaEventos,Periodo,Eventos),
    sort(Eventos,EventosNoPeriodo).

eventosMenoresQue(Duracao,EventosMenoresQue):-
    /*eventosMenoresQue(+Int,-List)
     * Verdade se EventosMenoresQue for uma lista de eventos
     de duracao menor ou igual a introduzida.
     */
    findall(ID,(horario(ID,_,_,_,Y,_),Y =< Duracao),ListaEventos),
    sort(ListaEventos,EventosMenoresQue).

eventosMenoresQueBool(ID,Duracao):-
    /*eventosMenoresQueBool(+Int,+Int)
     * Verdade se o evento pertencer a lista dos eventos
     * menores ou igual que a duracao especificada.
     */
    eventosMenoresQue(Duracao,ListaEventosMenores),
    member(ID,ListaEventosMenores).

encontraDisciplinas(ID,ListaD):- %auxiliar
    /*encontraDisciplinas(+Int,-List)
     * Verdade se ListaD for uma lista da(s) disciplina(s)
     * correspondentes ao evento indicado.
     */
    findall(Disciplina,evento(ID,Disciplina,_,_,_),LD),
    sort(LD,ListaD).

procuraDisciplinas(Curso,Disciplinas):-
    /*procuraDisciplinas(+Term,-List)
     * Verdade se Disciplinas for uma lista das disciplinas
     * constituintes do curso.
     */
    findall(ID,turno(ID,Curso,_,_),Idscurso),
    sort(Idscurso,ListaIDs),
    maplist(encontraDisciplinas,ListaIDs,ListaDisciplina1),
    sort(ListaDisciplina1,ListaDisciplina),
    append(ListaDisciplina,Disciplinas).

converteDisEmEventos(Dis,Id):-%auxiliar
    /*converteDisEmEventos(+Term,?Term)
     */
   evento(Y,Dis,_,_,_),Y = Id.

organizaDisciplinas([],_,[[],[]]).
organizaDisciplinas([Dis|Resto],Curso,[[Dis|R],Random]):-
    /*organizaDisciplinas(+List,+Term,-List)
     * Verdade se a lista final for uma lista das disciplinas
     *do respetivo curso organizadas de acordo com o
     *semestre em que ocorrem.
     */
    converteDisEmEventos(Dis,ID),
    turno(ID,Curso,_,_),
    %semestre 1.
    (horario(ID,_,_,_,_,'p1');horario(ID,_,_,_,_,'p2');
    horario(ID,_,_,_,_,'p1_2')),
    organizaDisciplinas(Resto,Curso,[R,Random]).
organizaDisciplinas([Dis|Resto],Curso,[Random,[Dis|R]]):-
    converteDisEmEventos(Dis,ID),
    turno(ID,Curso,_,_),
    %semestre 2
    (horario(ID,_,_,_,_,'p3');horario(ID,_,_,_,_,'p4');
    horario(ID,_,_,_,_,'p3_4')),
    organizaDisciplinas(Resto,Curso,[Random,R]).

organizaIdsEmHoras([],[]):-!.%auxiliar
organizaIdsEmHoras([ID|Outros],[Horas|Res]):-
    horario(ID,_,_,_,Horas,_),
    organizaIdsEmHoras(Outros,Res).

horasCurso(Periodo,Curso,Ano,TotalHoras):-
    /*horasCurso(+Term,+Term,+Int,-Int)
     * Verdadeiro se TotalHoras for o numero de horas dos
     * eventos associados ao curso no respetivo ano, no
     * respetivo periodo.
     */
    findall(ID,turno(ID,Curso,Ano,_),Idscurso),
    sort(Idscurso,ListaIDs),
    organizaEventos(ListaIDs,Periodo,IdsAtualizado),
    organizaIdsEmHoras(IdsAtualizado,ListaHoras),
    sum_list(ListaHoras,TotalHoras).

evolucaoHorasCurso(Curso,Evolucao):-
    /*evolucaoHorasCurso(+Term,-List)
     * Verdade se Evolucao for uma lista ordenada de tuplos
     * representando o numero de horas associadas ao curso,
     * em cada ano e periodo.
     */
    horasCurso(p1,Curso,1,TotalHoras1_1),
    horasCurso(p2,Curso,1,TotalHoras2_1),
    horasCurso(p3,Curso,1,TotalHoras3_1),
    horasCurso(p4,Curso,1,TotalHoras4_1),
    horasCurso(p1,Curso,2,TotalHoras1_2),
    horasCurso(p2,Curso,2,TotalHoras2_2),
    horasCurso(p3,Curso,2,TotalHoras3_2),
    horasCurso(p4,Curso,2,TotalHoras4_2),
    horasCurso(p1,Curso,3,TotalHoras1_3),
    horasCurso(p2,Curso,3,TotalHoras2_3),
    horasCurso(p3,Curso,3,TotalHoras3_3),
    horasCurso(p4,Curso,3,TotalHoras4_3),
    Evolucao = [(1,p1,TotalHoras1_1),(1,p2,TotalHoras2_1),
                (1,p3,TotalHoras3_1),(1,p4,TotalHoras4_1),
                (2,p1,TotalHoras1_2),(2,p2,TotalHoras2_2),
                (2,p3,TotalHoras3_2),(2,p4,TotalHoras4_2),
                (3,p1,TotalHoras1_3),(3,p2,TotalHoras2_3),
                (3,p3,TotalHoras3_3),(3,p4,TotalHoras4_3)].

/*ocupaSlot(+Int,+Int,+Int,+Int,-Int)
 * Verdade se Horas for o intervalo de tempo em que as horas
 * dadas e as horas dos eventos se sobrepoem.
 */
ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas):-
    HoraInicioDada == HoraInicioEvento,
    HoraFimDada == HoraFimEvento,
    Horas is HoraFimDada-HoraInicioDada.
ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas):-
    HoraInicioDada > HoraInicioEvento,
    HoraFimDada =< HoraFimEvento,
    Horas is HoraFimDada-HoraInicioDada,Horas > 0.
ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas):-
    HoraInicioDada =< HoraInicioEvento,
    HoraFimDada > HoraFimEvento,
    Horas is HoraFimEvento-HoraInicioEvento,Horas > 0.
ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas):-
    HoraInicioDada > HoraInicioEvento,
    HoraFimDada > HoraFimEvento,
    Horas is HoraFimEvento-HoraInicioDada,Horas > 0.
ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas):-
    HoraInicioDada < HoraInicioEvento,
    HoraFimDada < HoraFimEvento,
    Horas is HoraFimDada-HoraInicioEvento,Horas > 0.

numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras):-
    /*numHorasOcupadas(+Term,+Term,+Term,+Int,+Int,-Int)
     * Verdade se SomaHoras for o numero de horas ocupadas
     * num determinado tipo de sala, dia da semana, periodo
     * e entres as horas dadas.
     */
    salas(TipoSala,LSala),
    findall(Ids,(horario(Ids,DiaSemana,_,_,_,Periodo),
            evento(Ids,_,_,_,S),member(S,LSala)),LstEventos),
    %semestrais
    findall(I,(((horario(I,DiaSemana,_,_,_,'p1_2'),(
                     Periodo = 'p1';Periodo = 'p2'));
                (horario(I,DiaSemana,_,_,_,'p3_4'),(
                     Periodo = 'p3';Periodo = 'p4'))),
              evento(I,_,_,_,Sa),member(Sa,LSala)),LstIds),
    append(LstEventos,LstIds,ListaEventos),
    findall(Horas,(member(Id,ListaEventos),
                   horario(Id,_,HorasInicio,HorasFim,_,_),
                   ocupaSlot(HoraInicio,HoraFim,HorasInicio,HorasFim,Horas)),
            LstHoras),
    sum_list(LstHoras,SomaHoras).

ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max):-
    /*ocupacaoMax(+Term,+Int,+Int,-Int)
     * Verdade se Max for o numero de horas possiveis
     * de ser ocupadas naquele tipo de sala, entre as
     * horas especificadas.
     */
    findall(Salas,salas(TipoSala,Salas),ListaSalas),
    append(ListaSalas,ListaSalasAux),
    sort(ListaSalasAux,ListaSalasAux1),
    length(ListaSalasAux1,ComprimentoSala),
    Max is (HoraFim-HoraInicio)*ComprimentoSala.

percentagem(SomaHoras,Max,Percentagem):-
    /*percentagem(+Int,+Int,-Int)
     * Verdade se Percentagem for a divisao de SomaHoras
     * por Max, multiplicada por 100.
     */
    Percentagem is (SomaHoras/Max)*100.

horasOcupada([],[],[],_,_,[]).%auxiliar
horasOcupada([A|Anos],[S|Sala],[D|Dia],HoraInicio,HoraFim,
             [Termo|Resto]):-
    /*horasOcupada(+List,+List,+List,+Int,+Int,-List)
    Calcula recursivamente o numero de horas ocupadas em
    cada periodo,sala e dia.
    */
    numHorasOcupadas(A,S,D,HoraInicio,HoraFim,H),
    Termo = [D,S,H],
    horasOcupada(Anos,Sala,Dia,HoraInicio,HoraFim,Resto).

ocupacao([],_,_,[]):-!.%auxiliar
ocupacao([S|Sal],HoraInicio,HoraFim,[M|M1]):-
    /*ocupacao(+List,+Int,+Int,-List)
      Aplicar a ocupacaoMax a uma lista
     */
    ocupacaoMax(S,HoraInicio,HoraFim,M),
    ocupacao(Sal,HoraInicio,HoraFim,M1).

verificarThresholdAux([[]|[]],[],[[]|[]]):-!.%auxiliar
verificarThresholdAux([[casosCriticos|[_,_,H]]|R],[O|Oc],
                      [[casosCriticos|[_,_,Per]]|Res]):-
    percentagem(H,O,Per),
    verificarThresholdAux(R,Oc,[[]|Res]).

verificarThreshold([[]|[]],_,[[]|[]]):-!.%auxiliar
verificarThreshold([[Random|[Dia,Sala,Per]]|Rest],Threshold,
                   [[Random|[Dia,Sala,Per]]|Res]):-
    /*verificarThreshold(+List,+Int,-List)
     * Verifica recursivamente se a percentagem de
     * ocupacao for superior ao Threshold.
     */
    Per > Threshold,
    !,
    verificarThreshold(Rest,Threshold,[_|Res]).
verificarThreshold([_,Rest],Threshold,[_|Res]):-
    verificarThreshold(Rest,Threshold,Res).

transformaFunctores([[]|[]],[[]|[]]):-!.%auxiliar
transformaFunctores([El|Res],[F|F1]):-
    /*transformaFunctores(+List,-List)
     */
    F =.. El,
    transformaFunctores(Res,F1).


ocupacaoCritica(HoraInicio,HoraFim,Threshold,Resultados):-
    /*ocupacaoCritica(+Int,+Int,+Int,-List)
     * Verdade se Resultados for uma lista dos casos em que
     * a ocupacao das salas entre as horas dadas for
     * superior ao Threshold.
     */
    findall(Salas,salas(Salas,_),TipoSalas),
    Ano = [p1,p2,p1_2,p3,p4,p3_4],
    DiaSemana = ['segunda-feira','terca-feira','quarta-feira',
                 'quinta-feira','sexta-feira'],
    horasOcupada(Ano,TipoSalas,DiaSemana,HoraInicio,HoraFim,ResultadosAux),
    ocupacao(TipoSalas,HoraInicio,HoraFim,Maximo),
    verificarThresholdAux(ResultadosAux,Maximo,ResultadosAux1),
    verificarThreshold(ResultadosAux1,Threshold,ResultadosAux2),
    transformaFunctores(ResultadosAux2,Resultados).

exists(A,[A|_]).%auxiliar
exists(A,[_|B]):- exists(A,B).

%cab1 e cab2
cab1(NomePessoa):-
    exists(NomePessoa,[[_,_,_],[NomePessoa,_],[_,_,_]]).
cab2(NomePessoa):-
    exists(NomePessoa,[[_,_,_],[_,NomePessoa],[_,_,_]]).
%honra
honra(NomePessoa1,NomePessoa2):-
    exists(NomePessoa1,NomePessoa2,
          [[_,_,_],[NomePessoa1,_],[NomePessoa2,_,_]]).
honra(NomePessoa1,NomePessoa2,
          [[_,_,NomePessoa2],[_,NomePessoa1],[_,_,_]]).
%lado
lado(NomePessoa1,NomePessoa2,
         [[NomePessoa1,NomePessoa2,_],[_,_],[_,_,_]]).
lado(NomePessoa1,NomePessoa2,
         [[_,NomePessoa1,NomePessoa2],[_,_],[_,_,_]]).
lado(NomePessoa1,NomePessoa2,
         [[_,_,_],[_,_],[NomePessoa1,NomePessoa2,_]]).
lado(NomePessoa1,NomePessoa2,
         [[_,_,_],[_,_],[_,NomePessoa1,NomePessoa2]]).
lado(NomePessoa1,NomePessoa2,
         [[NomePessoa2,NomePessoa1,_],[_,_],[_,_,_]]).
lado(NomePessoa1,NomePessoa2,
         [[_,NomePessoa2,NomePessoa1],[_,_],[_,_,_]]).
lado(NomePessoa1,NomePessoa2,
         [[_,_,_],[_,_],[NomePessoa2,NomePessoa1,_]]).
lado(NomePessoa1,NomePessoa2,
         [[_,_,_],[_,_],[_,NomePessoa2,NomePessoa1]]).
%naoLado
naoLado(NomePessoa1,NomePessoa2,Mesa):-
    \+ lado(NomePessoa1,NomePessoa2,Mesa).
%frente
frente(NomePessoa1,NomePessoa2,
           [[NomePessoa1,_,_],[_,_],[NomePessoa2,_,_]]).
frente(NomePessoa1,NomePessoa2,
           [[_,NomePessoa1,_],[_,_],[_,NomePessoa2,_]]).
frente(NomePessoa1,NomePessoa2,
           [[_,_,NomePessoa1],[_,_],[_,_,NomePessoa2]]).
frente(NomePessoa1,NomePessoa2,
           [[NomePessoa2,_,_],[_,_],[NomePessoa1,_,_]]).
frente(NomePessoa1,NomePessoa2,
           [[_,NomePessoa2,_],[_,_],[_,NomePessoa1,_]]).
frente(NomePessoa1,NomePessoa2,
           [[_,_,NomePessoa2],[_,_],[_,_,NomePessoa1]]).
%naoFrente
naoFrente(NomePessoa1,NomePessoa2,Mesa):-
    \+ frente(NomePessoa1,NomePessoa2,Mesa).

ocupacaoMesa(_,[],[[_,_,_],[_,_],[_,_,_]]).
ocupacaoMesa(ListaPessoas,[R|Restricao],OcupacaoMesa):-
    /*ocupacaoMesa(+List,+List,?List)
     * Verdade se devolver a lista com o nome de pessoas
     * sentadas na mesa, de acordo com as restricoes introduzidas.
     */
    R,
    ocupacaoMesa(ListaPessoas,Restricao,OcupacaoMesa).

