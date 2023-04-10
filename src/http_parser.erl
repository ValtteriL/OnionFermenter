-module(http_parser).

-export([read_http_msg/1]).

-define(AcceptEncodingPolicy, <<"identity, *;q=0">>). % value to disable compression

%% module to help parsing http messages
%% disables compression on the fly

% read full http msg from Haysack
-type alldata() :: binary(). % data in alldata() that make a full http message
-type packet() :: binary(). % data in alldata() that make a full http message
-type rest() :: binary(). % data at end of alldata() that do not belong to full http message
-spec read_http_msg(alldata()) -> {packet(), rest()}
    ;(alldata()) -> more.
read_http_msg(Alldata) ->
    case read_command(Alldata) of
        more -> more;
        {HttpCommand, Rest} ->
            case read_headers(Rest) of
                more -> more;
                {normal, Headers, BodyLength, Remaining} ->
                    % body not chunked
                    case read_body(Remaining, BodyLength) of
                        more -> more;
                        {Body, Rem} -> {<<HttpCommand/binary, Headers/binary, Body/binary>>, Rem}
                    end;
                {chunked, Headers, Remaining} ->
                    % body chunked
                    case read_chunked_body(Remaining) of
                        more -> more;
                        {_Chunks, _TrailerHeaders, RawBody, Rem} ->
                            {<<HttpCommand/binary, Headers/binary, RawBody/binary>>, Rem}
                    end
            end
    end.


% read http command from Haysack
-type haysack() :: binary().
-type httpcommand() :: binary().
-spec read_command(haysack()) -> {httpcommand(), rest()}
    ;(haysack()) -> more.
read_command(Haysack) ->
    Ret = erlang:decode_packet(http_bin, Haysack, []),
    case Ret of
        {ok, _Packet, Rest} ->
            CommandLen = byte_size(Haysack) - byte_size(Rest),
            <<HttpCommand:CommandLen/binary, Rest/binary>> = Haysack,
            {HttpCommand, Rest};
        {more, _Length} -> more
    end.

% read all http headers from Haysack
-type headers() :: binary().
-type bodylength() :: integer().
-spec read_headers(haysack()) -> {normal, headers(), bodylength(), rest()}
    ;(haysack()) -> {chunked, headers(), rest()}
    ;(haysack()) -> more.
read_headers(Haysack) ->
    read_header(Haysack, <<>>, 0, false).

% allow max 10MB headers
read_header(Haysack, Headers, BodyLength, IsChunked) when byte_size(Headers) < 1024*1024*10 ->
    Ret = erlang:decode_packet(httph_bin, Haysack, []),
    case Ret of
        {ok, Header, Rest} ->
            Head = read_bytes(Haysack, byte_size(Haysack) - byte_size(Rest)), % read raw bytes from haysack
            NewHeaders = <<Headers/binary, Head/binary>>,
            case Header of
                {http_header, _Int, 'Content-Length', _UnmodifiedField, Value} ->
                    read_header(Rest, NewHeaders, binary_to_integer(Value), IsChunked);
                {http_header, _Int, 'Transfer-Encoding', _UnmodifiedField, Value} ->
                    case binary:match(Value, <<"chunked">>) of
                        nomatch -> read_header(Rest, NewHeaders, BodyLength, IsChunked);
                        _ -> read_header(Rest, NewHeaders, BodyLength, true) % body contains chunked content
                    end;
                {http_header, _Int, 'Accept-Encoding', _UnmodifiedField, Value} ->
                    NewHead = binary:replace(Head, Value, ?AcceptEncodingPolicy), % replace the Value with predefined policy to disable encoding
                    read_header(Rest, <<Headers/binary, NewHead/binary>>, BodyLength, IsChunked);
                {http_header, _Int, _HttpField, _UnmodifiedField, _Value} ->
                    read_header(Rest, NewHeaders, BodyLength, IsChunked);
                http_eoh ->
                    case IsChunked of
                        true -> {chunked, NewHeaders, Rest};
                        _ -> {normal, NewHeaders, BodyLength, Rest}
                    end
            end;
        {more, _Length} -> more
    end.

% read Length bytes from Haysack
% expects haysack to contain enough bytes
-type length() :: integer().
-type bytes() :: binary().
-spec read_bytes(haysack(), length()) -> bytes().
read_bytes(Haysack, Length) ->
    <<Bytes:Length/binary, _Rest/binary>> = Haysack,
    Bytes.

% read Length bytes from Haysack (http body) (at most 10MB)
-type body() :: binary().
-spec read_body(haysack(), length()) -> {body(), rest()}
    ;(haysack(), length()) -> more.
read_body(Haysack, Length) when Length =< 1024*1024*10 ->
    case Haysack of
        <<Body:Length/binary, Rest/binary>> -> {Body, Rest};
        _ -> more
    end.


% read chunked body
-type chunks() :: binary().
-type rawbody() :: binary().
-spec read_chunked_body(haysack()) -> {[chunks()], headers(), rawbody(), rest()}
    ;(haysack()) -> more.
read_chunked_body(Haysack) ->
    case read_chunked_body(Haysack, []) of
        more -> more;
        {Chunks, Headers, Rest} ->
            LenBody = byte_size(Haysack) - byte_size(Rest),
            <<RawBody:LenBody/binary, Rest/binary>> = Haysack,
            {Chunks, Headers, RawBody, Rest}
    end.

read_chunked_body(Haysack, Acc) ->
    %% read length (in hex, followed by \r\n)
    Ret = binary:split(Haysack, <<"\r\n">>),
    case Ret of
        [<<"0">>, Rest] ->
            case read_headers(Rest) of % this is last chunk, read headers
                more -> more;
                {normal, Headers, _BodyLength, Leftover} -> {Acc, Headers, Leftover}
            end;
        [HexLength, Rest] ->
            DecLength = list_to_integer(binary:bin_to_list(HexLength), 16), % convert hex length to dec
            case read_body(Rest, DecLength + 2) of % read next chunk with /r/n at the end
                more -> more;
                {Body, Leftover} ->
                    Delim = <<"\r\n">>,
                    <<BareBody:DecLength/binary, Delim/binary>> = Body, % remove /r/n from the end
                    read_chunked_body(Leftover, lists:append([Acc, [BareBody]]))
            end;
        _ -> more
    end.
