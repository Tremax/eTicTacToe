-module(internal).

-export([exist_in_list/2, response/2]).


exist_in_list(List, Element) ->
	lists:any(fun(X)-> X == Element end, List).

response(200 ,Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(io_lib:fwrite("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nConnection: Keep-Alive\r\nKeep-Alive: timeout=25, max=100\r\nContent-Length: ~p\r\n\r\n~s", [size(B), B]));

response(404 ,Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(io_lib:fwrite("HTTP/1.1 404 Not Found\r\nContent-Type: text/html\r\nConnection: Keep-Alive\r\nKeep-Alive: timeout=25, max=100\r\nContent-Length: ~p\r\n\r\n~s", [size(B), B])).
