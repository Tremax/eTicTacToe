-module(main).

-export([s/1]).

s(Port)-> 
	spawn(
		fun () -> 
			{ok, Socket} = gen_tcp:listen(Port, [list,{active, false},{packet, http}]),  
			accept_loop(Socket) 
		end).
				
   	
accept_loop(Socket) ->
	
	{ok, CSocket} = gen_tcp:accept(Socket),

    Pid = spawn(
			fun () -> 
				client_socket() 
			end),
			
	gen_tcp:controlling_process(CSocket, Pid),
	
	Pid ! {take_socket, CSocket},
	
	
accept_loop(Socket).


client_socket() ->
	Socket = receive {take_socket, S} -> S end,
	client_loop(Socket, [], []).
	
	
client_loop(Socket, MovesX, MovesY) ->

	inet:setopts(Socket, [{active, once}]),

	receive

	   	{http, Socket, {http_request, 'GET', {abs_path, Path}, _Vers}} ->
		
			case http_uri2:parse_path_query(Path) of
				
				{"/", []} -> %index page
					
					{ok, Data} = file:read_file("../html/index.html"),
					gen_tcp:send(Socket, internal:response(200, Data)),
					client_loop(Socket, MovesX, MovesY);
					
				{"/newgame", []} -> 
				
					gen_tcp:send(Socket, internal:response(200, ["{\"ok\": 1}"])),
					client_loop(Socket, [], []);
				
				{"/play",[{"x",Xin}, {"y",Yin}, {"aggress", Ain}]} ->
					
					X = list_to_integer(Xin), Y = list_to_integer(Yin), 
					
					A = case string:to_float(Ain) of {F, []} -> F; {error, no_float} -> list_to_integer(Ain) end,

					Aggress = case  A < 0.1 orelse A > 3.6 of true -> 0.8; false -> A end, % проверка на корректность

					io:format("Aggress ~p~n", [Aggress]),
					
					TmpX = MovesX ++ [{X, Y}],
					
					{_,{X2,Y2}} = computer_logic:get_bot_move(TmpX, MovesY, Aggress),
										
					TmpY = MovesY ++ [{X2,Y2}],
				
				
				Res = case check_winner:cw(TmpX, TmpY, {X,Y}, {X2, Y2}, []) of
					
					bot_win -> lists:concat(["{\"lose\" : 1, \"x\":",X2, ", \"y\":", Y2, "}"]);
					player_win -> ["{\"win\": 1 }"];
					draw -> ["{\"draw\": 1 }"];
					next -> lists:concat(["{\"x\":",X2, ", \"y\":", Y2, "}"])

				end,
					
					gen_tcp:send(Socket, internal:response(200, Res)),
										
					client_loop(Socket, TmpX, TmpY);
					
				_ -> 
				
				client_loop(Socket, MovesX, MovesY)
					
			end;
		

		{tcp_closed, Socket} ->	
			gen_tcp:close(Socket);


		{tcp_error, Socket, _} ->
			gen_tcp:close(Socket);
		

		_ -> 
			client_loop(Socket, MovesX, MovesY)
			
			
	end.
