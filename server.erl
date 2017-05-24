-module(server).
-compile(export_all).

%%=================================================%%
% Trabajo Practico Final - Sistemas Operativos I    %
% --------------------------------------------------%
% Integrantes: Ivan Ernandorena - Julio Guella -    %
%              Luis Dzikiewicz                      %
% --------------------------------------------------%
%         SERVIDOR DE JUEGOS DISTRIBUIDO            %
%%=================================================%%

%Comienza el sistema distribuido, con hacer ping a un solo nodo basta.
%Ya que se sincroniza con todo el sistema.

start(SName,Port) ->
	net_kernel:start([SName,shortnames]),
	spawn(?MODULE,server,[Port]).

%% para conectar dos servers en distintas PC's:
%% Inicializar ambos. En uno de ellos 
%% >gen_tcp:connect('ip donde esta el otro server','puerto con el que se inicio el nodo',[opciones]).
%% >net_kernel:connect_node('nombre del nodo').

%% para conectar dos servers en la misma PC:
%% Inicializar ambos.
%% >net_kernel:connect_node('nombre del nodo').

server(Port) ->
	Resolve = fun (Name,Pid1,Pid2) -> Pid1 end,
	{ok,LSock} = gen_tcp:listen(Port, [{packet, 0},{active,false}]), %%server escucha en port Port
	ClientPid = spawn(?MODULE,list_of_client,[[]]), %%creo el proceso que maneja la lista para los clientes
	yes = global:register_name(clients_pid,ClientPid,Resolve),
	GamesPid = spawn(?MODULE,lists_of_games,[[]]), %%crea el proceso que maneja la lista de juegos en curso.
	yes = global:register_name(games_pid,GamesPid,Resolve),
	PidBalance = spawn(?MODULE,pbalance,[1,node()]), %%statistics(total_active_tasks)
	register(pbalance,PidBalance),
	PidStat = spawn(?MODULE,pstat,[]),
	register(pstat,PidStat),
	dispatcher(LSock). %%llamo a dispatcher con el listen socket

%% Espera nuevas conexiones y crea un proceso para atender cada una.
dispatcher(LSock) ->
	  {ok, CSock} = gen_tcp:accept(LSock),
		Pid = spawn(?MODULE, psocket, [CSock]),
		ok = gen_tcp:controlling_process(CSock, Pid), %%Ahora a CSock lo controla Pid -- los mensajes a CSock llegan a Pid
		Pid ! ok,
		dispatcher(LSock).
	
%Atendera todos los pedidos del cliente hablando en CSock.
psocket(CSock) ->
	receive ok -> ok end, %%magia para que controlling process suceda antes que setopts.
	ok = inet:setopts(CSock	,[{active,true}]), %%recivo msjs con receive, no con gen_tcp:recv.	
	gen_tcp:send(CSock,"Bienvenido!~nRecuerda que primero debes registrarte con CON. -|- HELP para ayuda.~n~n"),
	psocket_loop(CSock).

%%Primer PSocket: Solo para registrarse.
psocket_loop(CSock) ->
	receive
		{tcp,CSock,Data} ->
			pbalance ! {req,self()},
			receive {send,Nodo} ->
				case string:tokens(lists:sublist(Data, length(Data)-2)," ") of
					["CON",Nombre] -> 
								spawn(Nodo,?MODULE,connect,[Nombre,CSock,self()]),
								receive {con,N} -> io:format("registro exitoso~n"),psocket_loop(CSock,N);
												{error} -> psocket_loop(CSock)
								end;
					["BYE"] -> gen_tcp:close(CSock),exit(normal);
					Otherwise -> gen_tcp:send(CSock,"Primero debes registrarte con CON 'id'.\n"),
								 io:format("Pid: ~p quiere ejecutar comandos sin registrarse~n",[self()]),
								 psocket_loop(CSock)
				end
			end;
		{error,Closed} -> io:format("Closed:~p~n",[Closed]),exit(normal) %%hace falta volver a llamar a psocketloop?
	end.


%%Segundo PSocket: Una vez registrado N tiene acceso a los demas comandos.
psocket_loop(CSock,N) ->
	{pbalance,node()} ! {req,self()},
	receive
		{send,Nodo} ->	receive 
											{tcp,CSock,Data} ->
												spawn(Nodo,?MODULE, pcommand, [Data,CSock,N]),
												psocket_loop(CSock,N);
											{print,Data} -> gen_tcp:send(CSock,Data),psocket_loop(CSock,N);
											{error,Closed} -> io:format("Closed:~p~n",[Closed]),gen_tcp:close(CSock),exit(normal)
						 				end
		
	end.	

pcommand(Data,CSock,N) ->
	case string:tokens(lists:sublist(Data, length(Data)-2)," ") of
		["CON",Nombre] -> gen_tcp:send(CSock,"Ya estas registrado\n");
		["LSG"] -> lsg(CSock);
		["NEW", NJuego] -> newgame(NJuego,CSock,N);
		["ACC", Juego] -> acc(Juego,CSock,N);
		["PLA", NJuego,Casilla] -> jugada(NJuego,Casilla,N);
		["OBS", NJuego] -> observa(NJuego,N);
		["LEA", NJuego] -> lea(N,NJuego);
		["BYE"] -> bye(N);
		["HELP"] -> game:help(N);
		Otherwise -> gen_tcp:send(CSock,"Comando Incorrecto. HELP para ayuda\n")
	end.

%Encargado de mandar la info de carga al resto de los nodos
pstat() -> 
	Carga = 1, %statistics(total_active_tasks),

	{pbalance,node()} ! {st,Carga,node()},
	lists:foreach(fun (X) -> {pbalance,X}!{st,Carga,X} end,nodes()),

	receive after 20000 -> ok end,
	pstat().

%Recibe lo de pstat y calcula en que nodo se debe ejecutar.
pbalance(Carga,Nodo) -> 
	receive
		{st,C,N} -> case Carga > C of
						true -> pbalance(C,N);
						false -> pbalance(Carga,Nodo)
					end;
		{req,Pid} -> Pid!{send,Nodo}, pbalance(Carga,Nodo) 
	end.

%Comando CON. Registra un usuario con Nombre.	
connect(Nombre, CSock, PSocketPid) ->
		N = list_to_atom(Nombre),
		case global:register_name(N,PSocketPid) of
			 yes -> global:send(clients_pid,{new_client,N}),
					    gen_tcp:send(CSock, "OK CON "++Nombre++"\n"),
					 		io:format("OK CON~n"),
					 		PSocketPid ! {con,N};
				no -> io:format("ERROR CON "++Nombre++"\n"),
				gen_tcp:send(CSock, "Ya estas registrado o el nombre está en uso\n"),
				PSocketPid ! {error}
		end.

%% Lista de los clientes conectados
list_of_client(L) ->
		receive
				{new_client,Client} -> 
						list_of_client(L++[Client]);
				{req,Pid} -> Pid ! {send,L},
							 list_of_client(L);
				{elim,N} -> list_of_client(lists:delete(N,L))
		end.

%% Lista de todos los juegos en curso.
lists_of_games(L) ->
	receive
		{new,Juego} -> lists_of_games(L++[Juego]);
		{req,Pid} -> Pid ! {send,L},
								 lists_of_games(L);
		{elim,Juego} -> lists_of_games(lists:delete(Juego,L))
	end.

%Comando NEW. N crea el juego NJuego.
newgame(NJuego,CSock,N) ->
	J = list_to_atom(NJuego),
	Game = spawn(?MODULE,game_init,[N,J]),
	case global:register_name(J,Game) of
		yes -> gen_tcp:send(CSock,"OK NEW\n"),
				 global:send(games_pid,{new,J});
		no -> gen_tcp:send(CSock,"ERROR Nombre de juego\n")
	end.
	
%Comando LSG. Para cada juego, su ID y participantes.
lsg(CSock) ->
	Imp_datos = fun (L) -> 	lists:foreach(fun (X) -> gen_tcp:send(CSock,atom_to_list(X)++" | ") end,L),
							 						gen_tcp:send(CSock,"\n") end,
	Obt_datos = fun (Game) -> global:send(Game,{datos,self()}),
														receive
														{send,Jugadores,Observadores} -> gen_tcp:send(CSock,"Jugadores:\n"),
																						 Imp_datos(lists:map(fun(X)-> element(2,X) end,Jugadores)),
																						 gen_tcp:send(CSock,"Observadores:\n"),
																						 Imp_datos(Observadores),
																						 gen_tcp:send(CSock,"_____________\n\n")
														end
							end,
	gen_tcp:send(CSock,"Lista de juegos:\n\n"),
	global:send(games_pid,{req,self()}),
	receive
		{send,L} -> lists:foreach(fun (X) -> gen_tcp:send(CSock,"Juego: "++atom_to_list(X)++"\n"),
											 Obt_datos(X) end, L)
	end.

%Comando ACC. N accede a Juego si es posible.
acc(Juego,CSock,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive
	{send,L} -> case lists:member(J,L) of
				false -> gen_tcp:send(CSock,"ERROR Juego inexistente\n");
				true  -> global:send(J,{datos,self()}),
						 receive {send,Jugadores,Observadores} ->
							if length(Jugadores) == 2 -> gen_tcp:send(CSock,"Juego en curso\n");
								 true -> case element(2,lists:nth(1,Jugadores)) == N of
								 				 true -> gen_tcp:send(CSock,"No puedes jugar contra ti mismo.\n");
								 				 false -> global:send(J,{update,Jugadores++[{2,N}],Observadores}),
											 	 gen_tcp:send(CSock,"OK ACC\n"),
											 	 global:send(J,{start})
											 	end
							end
						 end
				end
	end.

%%Inicializa el juego. Crea el tablero y parametros iniciales.
game_init(N,J) ->
	Tablero = game:inicializar_tablero(),
	Jugadores = [{1,N}],
	Observadores = [],
	Turno = random:uniform(2), %un numero aleatorio entre 1 y 2, para que no empiece siempre el mismo
	io:format("Turno: ~p~n",[Turno]),
	game(J,Tablero,Jugadores,Observadores,Turno).

%% Maneja el juego J.
%% Jugadores = [{1,J1},{2,J2}]
game(J,Tablero,Jugadores,Observadores,Turno) ->
	receive
		{datos,Pid} -> Pid ! {send,Jugadores,Observadores}, game(J,Tablero,Jugadores,Observadores,Turno);
		{datos2,Pid} -> Pid ! {send,Tablero,Jugadores,Observadores,Turno},game(J,Tablero,Jugadores,Observadores,Turno);
		{update,Jugadores_new,Observadores_new} -> game(J,Tablero,Jugadores_new,Observadores_new,Turno);
		{update2,TNew,TurnoNew} -> game:upd(J,Jugadores,TNew,Observadores,Turno),
															 case game:gano(TNew,"X") of
															 	true -> game:ganoJ1(Jugadores,Observadores),global:send(games_pid,{elim,J}),exit(normal);
															 	false -> case game:gano(TNew,"0") of
															 						true -> game:ganoJ2(Jugadores,Observadores),global:send(games_pid,{elim,J}),exit(normal);
															 						false -> case game:empate(TNew) of
															 											true -> lists:foreach(fun(X)->global:send(element(2,X),{print,"Han empatado.\n"}) end,Jugadores),
															 															lists:foreach(fun(X)->global:send(X,{print,"Han empatado.\n"}) end,Observadores),
															 															global:send(games_pid,{elim,J}),exit(normal);
															 											false -> X = game:es_turno(TurnoNew,Jugadores),
																				 										 global:send(X,{print,"-- Es tu turno "++atom_to_list(X)++" --\n"}), 
																				 										 game(J,TNew,Jugadores,Observadores,TurnoNew)
															 					 					 end
															 					 end
															 end; 
		{start} ->X = game:es_turno(Turno,Jugadores),
							global:send(X,{print,"-- Es tu turno "++atom_to_list(X)++" --\n"}),
							Aux = "+ "++atom_to_list(J)++" | "++atom_to_list(element(2,lists:nth(1,Jugadores)))++" (X) | "++atom_to_list(element(2,lists:nth(2,Jugadores)))++" (0) +\n\n",
							lists:foreach(fun(Y) -> global:send(element(2,Y),{print,Aux}),global:send(element(2,Y),{print,game:tprint(Tablero)}) end,Jugadores),
							lists:foreach(fun(Z) -> global:send(Z,{print,Aux}),global:send(Z,{print,game:tprint(Tablero)}) end,Observadores),
							game(J,Tablero,Jugadores,Observadores,Turno);
		bye -> global:send(games_pid,{elim,J}),exit(normal)
	end.


%%Comando PLA. Actualiza, si esta permitido, la Casilla del juego NJuego.	
jugada(NJuego,Casilla,N) ->
	J = list_to_atom(NJuego),
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		case lists:member(J,L) of
			  false -> global:send(N,{print,"Juego incorrecto.\n"}); %el juego no existe...
			  true -> 
			  	if 	Casilla == "BYE" -> %%se puede abandonar la partida con PLA.
						 		abandona(N,NJuego),
						 		exit(normal);
						 	true -> ok
					end,
					case (Casilla < "1") or (Casilla > "9") of
						true -> global:send(N,{print,"Comando incorrecto 2.\n"}),exit(normal);
						false -> ok
					end,
					C = list_to_integer(Casilla),
					global:send(J,{datos2,self()}),
					receive
						{send,T,Jugadores,Obs,Turno} ->
							case length(Jugadores) /= 2 of 
								 true -> global:send(N,{print,"ERROR PLA (Jugadores)\n"});
								 false -> A = element(2,lists:nth(1,Jugadores)), B = element(2,lists:nth(2,Jugadores)),
								 				 case lists:member(N,[A,B]) and (game:es_turno(Turno,Jugadores) == N) of 
								 	          	true -> %io:format("C:~p T: ~p A: ~p~n",[Casilla,find(Casilla,T)," " == find(Casilla,T)]),
								 	          					case game:find(C,T) == " " of 
								 	          						true -> TNew = game:update(C,T,Turno),
																								TurnoNew = game:turno(Turno),
																								global:send(J,{update2,TNew,TurnoNew});
														 						false -> global:send(N,{print,"ERROR PLA (Casilla)\n"})
																			end;
															false -> global:send(N,{print,"ERROR PLA (Jugador/Turno)\n"})
										     end
							end
					end
		end
	end.
	
abandona(N,NJuego) ->	
	J = list_to_atom(NJuego),
	% solo llamo a esta funcion en jugada, donde ya chequie si el juego existe.
	global:send(J,{datos,self()}),
	receive
		{send,Jugadores,Observadores} ->
			if 	length(Jugadores) /= 2 -> 
						game:send_msj_j1(Jugadores,"OK PLA "++NJuego++" BYE\n"),global:send(J,bye);
					true ->
					J1 = element(2,lists:nth(1,Jugadores)),
					J2 = element(2,lists:nth(2,Jugadores)),
					case N == J1 of
						true -> global:send(J1,{print,"OK PLA "++NJuego++" BYE\n"}),
										global:send(J2,{print,atom_to_list(J1)++" abandono "++NJuego++"\n"}),
										global:send(J,bye);
						false -> global:send(J2,{print,"OK PLA "++NJuego++" BYE\n"}),
										global:send(J1,{print,atom_to_list(J2)++" abandono "++NJuego++"\n"}),
										global:send(J,bye)
					end
			end
	end.

observa(Juego,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		case lists:member(J,L) of
			false -> global:send(N,{print,"Juego inexistente.\n"});
			true -> global:send(J,{datos2,self()}),
							receive
								{send,Tablero,Jugadores,Observadores,Turno} -> 	
									case (element(2,lists:nth(1,Jugadores)) == N) or (element(2,lists:nth(1,Jugadores)) == N) of
										true -> global:send(N,{print,"No puedes observar un juego en el que participas.\n"});
										false ->	case lists:member(N,Observadores) of
																true -> global:send(N,{print,"Ya estas observando "++Juego++"\n"});
																false ->  global:send(games_pid,{req,self()}),
																					receive {send,L} ->
																						case lists:member(J,L) of
																							false -> global:send(N,{print,"Juego inexistente\n"});
																							true -> io:format("Observadores: ~p~n",[Observadores++[N]]),
																											global:send(N,{print,"OK OBS "++Juego++"\n"}),
																											game:presentacion_o(J,Jugadores,Tablero,[N],Turno),
																											global:send(J,{update,Jugadores,Observadores++[N]})
																						end
																					end
															end
									end
							end
		end
	end.							

lea(N,NJuego) ->
	J = list_to_atom(NJuego),
	global:send(games_pid,{req,self()}),
	receive
		{send,L} -> 
			case lists:member(J,L) of
				false -> global:send(N,{print,"Juego inexistente\n"});
				true -> global:send(J,{datos,self()}),
								receive
									{send,Jugadores,Observadores} -> 
											case lists:member(N,Observadores) of
												false -> global:send(N,{print,"No estas observando "++NJuego++"\n"});
												true -> Obs_new = lists:delete(N,Observadores),
																global:send(N,{print,"OK LEA "++NJuego++"\n"}),
																global:send(J,{update,Jugadores,Obs_new})
											end
								end
			end
	end.


bye(N) ->
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		lists:foreach(fun (X) -> global:send(X,{datos,self()}),
								 receive
								 	{send,Jugadores,Observadores} ->
								 		J1 = element(2,lists:nth(1,Jugadores)),
								 		if (length(Jugadores) == 1) and (J1 == N)-> 
								 			global:send(X,bye);
								 			true -> 
										 		J2 = element(2,lists:nth(2,Jugadores)),
										 		case J1 == N of
										 			 true -> 
									 			 		 global:send(J2,{print,atom_to_list(J1)++" ha abandonado.\n"}),
									 			 		 lists:foreach( fun (X) -> global:send(X,{print,atom_to_list(J1)++" ha abandonado.\n"}) end,Observadores),
									 			 		 global:send(X,bye); 
										 			 false -> case J2 == N of
													 			 			true ->
													 			 				global:send(J1,{print,atom_to_list(J2)++" ha abandonado.\n"}),
								 						 			 		  lists:foreach( fun (X) -> global:send(X,{print,atom_to_list(J2)++" ha abandonado.\n"}) end,Observadores),
								 						 			 		  global:send(X,bye); 
								 						 			 		false -> 
								 						 			 			ok 	
								 						 			 	end
								 				end
								 		end
						 			end end, L),
		global:send(N,{error,normal})
	end.
 
