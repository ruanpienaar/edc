-module(edc_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    pool_name/3,
    tcpv4_child_spec/2
]).

-export([init/1]).

-include("edc.hrl").

%% ===================================================================
%% API functions
%% ===================================================================



%% TODO:

%% This app needs to connect and allow clients to forward data
%% EX: incoming-proto -> code
%% EX: incoming-proto -> outgoing-proto
%% EX: incoming-proto -> code -> outgoing-proto




start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% pool_name(Host, Port, _Type = tcpv4) ->
%     %% TODO: atom creation - ugh...
%     list_to_atom( Host ++ integer_to_list(Port) ++ atom_to_list(?TCPV4) );
% pool_name(Host, Port, _Type = udpv4) ->
%     %% TODO: atom creation - ugh...
%     list_to_atom( Host ++ integer_to_list(Port) ++ atom_to_list(?UDPV4) ).

% tcpv4_child_spec(Type, ClientOpts) ->
%     NumSockets = case lists:keyfind(num_sockets, 1, ClientOpts) of
%         {num_sockets, NS} when NS > 0 ->
%             NS;
%         false ->
%             1
%     end,
%     %% TODO: would poolboy even be useful if socket-count == 1 ?
%     {connect_opts, ConnectOpts} = lists:keyfind(connect_opts, 1, ClientOpts),
%     {socket_opts, SocketOpts} = lists:keyfind(socket_opts, 1, ClientOpts),
%     {host, Host} = lists:keyfind(host, 1, ConnectOpts),
%     {port, Port} = lists:keyfind(port, 1, ConnectOpts),
%     Name = pool_name(Host, Port, Type),
%     PoolArgs = [
%         {name, {local, Name}},
%         {worker_module, edc_tcpv4},
%         % Size args
%         {size, NumSockets},
%         {max_overflow, 0},
%         {strategy, fifo}
%     ],
%     WorkerArgs = {ConnectOpts, SocketOpts},
%     poolboy:child_spec(Name, PoolArgs, WorkerArgs).

% udpv4_child_spec(Type, ClientOpts) ->
%     poolboy_child_spec(edc_udpv4, Type, ClientOpts).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    StartupClients = application:get_env(edc, startup_clients, []),
    % ChildSpecs =
    %     lists:foldl(fun({_Client, ClientOpts}, Acc) ->
    %         case lists:keyfind(type, 1, ClientOpts) of
    %             {type, Type = ?TCPV4} ->
    %                 [ tcpv4_child_spec(Type, ClientOpts) | Acc ];
    %             {type, Type = ?UDPV4} ->
    %                 [ udpv4_child_spec(Type, ClientOpts) | Acc ];
    %             _ ->
    %                 Acc
    %         end
    %     end, [], StartupClients),
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.
