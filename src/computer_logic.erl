-module(computer_logic).

-export([get_bot_move/3]).


get_bot_move(PlayerMovesX, PlayerMovesY, Agress) ->

	PlayerMoves = PlayerMovesX ++ PlayerMovesY,

	AllVariants = [Element || Element <- get_all_empty_points(PlayerMoves), internal:exist_in_list(PlayerMoves, Element) == false], %удаляем точки в которых уже находится О или X

lists:max(calculate_point_gravity(PlayerMovesX , PlayerMovesY, AllVariants, Agress)).


calculate_point_gravity(_, _, [], _) -> [];
calculate_point_gravity(MovesX , MovesY, [Move | Rest], Aggress) -> 

	PGravityX = calculate(generate_point(horizontal, Move), MovesX ++ [Move], MovesY, [], Move, 1000) +
 				calculate(generate_point(vertical, Move), MovesX ++ [Move], MovesY, [], Move, 1000) + 
				calculate(generate_point(fdiagonal, Move), MovesX ++ [Move], MovesY, [], Move, 1000) + 
				calculate(generate_point(sdiagonal, Move), MovesX ++ [Move], MovesY, [], Move, 1000),


	PGravityY = calculate(generate_point(horizontal, Move), MovesY ++ [Move], MovesX, [], Move, 10000) +
 				calculate(generate_point(vertical, Move), MovesY ++ [Move], MovesX, [], Move, 10000) + 
				calculate(generate_point(fdiagonal, Move), MovesY ++ [Move], MovesX, [], Move, 10000) + 
				calculate(generate_point(sdiagonal, Move), MovesY ++ [Move], MovesX, [], Move, 10000),

	PGravity = PGravityY + Aggress * PGravityX,

[{PGravity, Move}] ++ calculate_point_gravity(MovesX, MovesY, Rest, Aggress). 


	
%генерация координт из одной клетки
generate_point (horizontal, {X, Y}) -> [{X2,Y} || X2<-lists:seq(X-4, X+4, 1), X2 > 0, X2 < 11];
generate_point (vertical, {X, Y}) -> [{X,Y2} || Y2<-lists:seq(Y-4, Y+4, 1), Y2 > 0, Y2 < 11];
generate_point (fdiagonal, {X, Y}) -> [{X2,Y2} || X2<-lists:seq(X-4, X+4, 1), Y2<-lists:seq(Y-4,Y+4, 1), X2 > 0, Y2 > 0, X2 < 11, Y2 < 11, X2-Y2 == X-Y];
generate_point (sdiagonal, {X, Y}) -> [{X2,Y2} || X2<-lists:seq(X-4, X+4, 1), Y2<-lists:seq(Y-4,Y+4, 1), X2 > 0, Y2 > 0, X2 < 11, Y2 < 11, X2+Y2 == X+Y].



loop(_,_,_,0, Counter) -> Counter;
loop([],_,_,_, Counter) -> Counter;
loop(_,_,_,_, opponent_point_find) -> 0;

loop([FC | RC], MovesX, MovesY, Num, Counter) ->

	Res = case internal:exist_in_list(MovesX, FC) of
		
			true -> Counter + 1; % текущая клетка
			false -> % пустая или соперника
		
				case  internal:exist_in_list(MovesY, FC) of
				
					true -> opponent_point_find; % соперника выход с цикла
					false -> Counter % пустая пропускаем
				
				end
			
		   end,
		
loop(RC ,MovesX, MovesY, Num - 1, Res).



calculate(CList, MovesX, MovesY, Current, BreakElement, W) when Current == BreakElement -> 0;
calculate(CList, MovesX, MovesY, Current, BreakElement, W) ->

	Res =  loop(CList, MovesX, MovesY, 5, 0),
	
	NewRes = case Res of 
				
				opponent_point_find -> 0;
				0 -> 0; 
				1 -> 0; % 1 своя клетка
				5 -> W; % собрана последовательность из 5, 
				_ -> math:pow(3, Res) % все остальное возводим в степень
		
			 end,	
	
	[Head | Rest] = CList,
	
NewRes + calculate(Rest, MovesX, MovesY, Head, BreakElement, W).
										

%возвращает все возможные варианты соседних  точек без дубликатов
get_all_empty_points([]) -> [];
get_all_empty_points([Head | Tail]) -> lists:usort(get_nearby_points(Head) ++ get_all_empty_points(Tail)).


%угловые координаты
get_nearby_points({X,Y}) when X =:= 1 andalso Y =:=1  -> [{X, Y+1}, {X+1, Y+1}, {X+1, Y}];
get_nearby_points({X,Y}) when X =:= 1 andalso Y =:=10  -> [{X+1, Y}, {X+1, Y-1}, {X, Y-1}];
get_nearby_points({X,Y}) when X =:= 10 andalso Y =:=10 -> [{X-1, Y}, {X-1, Y-1}, {X, Y-1}];
get_nearby_points({X,Y}) when X =:= 10 andalso Y =:=1 -> [{X, Y+1}, {X-1, Y+1}, {X-1, Y}];


%начальные и конечные X,Y
get_nearby_points({X,Y}) when X =:= 1  -> [{X, Y-1}, {X+1, Y-1}, {X+1, Y}, {X+1, Y+1}, {X, Y+1}];
get_nearby_points({X,Y}) when Y =:= 10 -> [{X+1, Y}, {X+1, Y-1}, {X, Y-1}, {X-1, Y-1}, {X-1, Y}];
get_nearby_points({X,Y}) when X =:= 10 -> [{X, Y-1}, {X-1, Y-1}, {X-1, Y}, {X-1, Y+1}, {X, Y+1}];
get_nearby_points({X,Y}) when Y =:= 1  -> [{X-1, Y}, {X-1, Y+1}, {X, Y+1}, {X+1, Y+1}, {X+1, Y}];

%остальные точки
get_nearby_points({X,Y}) -> [{X-1, Y+1}, {X, Y+1}, {X+1, Y+1}, {X+1,Y}, {X+1, Y-1}, {X, Y-1}, {X-1, Y-1}, {X-1, Y}].