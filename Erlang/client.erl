-module(client).
-export([main/0]).

main() ->
    Args = init:get_plain_arguments(),
    Input = get_arg(Args, "-i", "file3.txt"),
    Output = get_arg(Args, "-o", "received3.txt"),
    Host = "localhost",
    Port = 8080,

    FileName = filename:basename(Input),

    Request = "GET /" ++ FileName ++ " HTTP/1.1\r\nHost: localhost:8080\r\nConnection: close\r\n\r\n",

    case gen_tcp:connect(Host, Port, [binary, {active, false}], 10000) of
        {ok, Socket} ->
            ok = gen_tcp:send(Socket, Request),
            {ok, Response} = recv_all(Socket, <<>>),
            case binary:split(Response, <<"\r\n\r\n">>) of
                [_Header, Body] ->
                    file:write_file(Output, Body),
                    io:format("Файл успешно получен и записан в ~s~n", [Output]);
                _ ->
                    io:format("Некорректный ответ от сервера~n", [])
            end,
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("Не удалось подключиться: ~p~n", [Reason])
    end.

recv_all(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 10000) of
        {ok, Data} ->
            recv_all(Socket, <<Acc/binary, Data/binary>>);
        {error, closed} ->
            {ok, Acc};
        {error, _} ->
            {ok, Acc}
    end.

get_arg([], _, Default) -> Default;
get_arg([Flag, Value | _], Flag, _) -> Value;
get_arg([_ | Tail], Flag, Default) -> get_arg(Tail, Flag, Default).