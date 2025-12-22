-module(server).
-export([start/0]).

start() ->
    Port = 8080,
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Сервер запущен на порту ~p. Ожидает подключения...~n", [Port]),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(ListenSocket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Request = binary_to_list(Data),
            case parse_request(Request) of
                {ok, FileName} ->
                    case file:read_file(FileName) of
                        {ok, Bin} ->
                            Header = io_lib:format("HTTP/1.1 200 OK\r\nContent-Length: ~p\r\nConnection: close\r\n\r\n", [byte_size(Bin)]),
                            gen_tcp:send(Socket, [Header, Bin]);
                        {error, _} ->
                            gen_tcp:send(Socket, "HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\nFile not found")
                    end;
                error ->
                    gen_tcp:send(Socket, "HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\n")
            end;
        {error, _} ->
            ok
    end,
    gen_tcp:close(Socket).

parse_request(Request) ->
    case string:tokens(Request, "\r\n") of
        [FirstLine | _] ->
            case string:tokens(FirstLine, " ") of
                ["GET", "/" ++ FileName | _] ->
                    CleanName = string:trim(FileName),
                    {ok, CleanName};
                _ ->
                    error
            end;
        _ ->
            error
    end.