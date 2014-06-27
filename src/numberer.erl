-module(numberer).

-export([get/1,release/2]).
-export([n2i/2,i2n/2,name/3,number/2]).
-export([get/2]).
-export([is_named/2,is_numbered/2]).
-export([is_used/2]).
-export([reset/1,reset/2,list_bound/1]).
-export([rename/3]).
-export([count_occupied/1]).
-export([initiate/1,initiate/2]).

%%----------------------------------------------
%% @doc initates a new numbering server.
%% @spec (Server::string()|atom()|integer()) -> ok
%%       If server is already initiated, nothing is done.
%% @end
%%----------------------------------------------
initiate(Server) ->
  gen_server:cast(numberer_srv,{init,1,Server}).

%%----------------------------------------------
%% @doc initates a new numbering server that starts the numbering at N.
%%       If server is already initiated, nothing is done.
%% @spec (N::integer(),Server::string()|atom()|integer()) -> ok
%% @end
%%----------------------------------------------
initiate(N,Server) ->
  gen_server:cast(numberer_srv,{init,N,Server}).


%%----------------------------------------------
%% @doc counts the number of occupied numbers.
%% @spec (server()) -> non_neg_integer().
%% @end
%%----------------------------------------------
count_occupied(Server) ->
  gen_server:call(numberer_srv,{count,Server}).

%%----------------------------------------------
%% @doc Returns an unused integer.
%% @spec (server()) -> Number::unique_integer().
%% @end
%%----------------------------------------------
get(Server) ->
  gen_server:call(numberer_srv,{get,Server}).

%%----------------------------------------------
%% @doc On first run, assosicate a unique integer with Name.
%%     On sequential runs, return the integer associated with Name.
%% @spec (term(),server())-> Number::non_neg_integer().
%% @end
%%----------------------------------------------
get(Name,Server) ->
  case is_numbered(Name,Server) of
    false -> 
      Number=?MODULE:get(Server),
      gen_server:cast(numberer_srv,{register,Name,Number,Server}),
      Number;
    Number -> Number
  end.

%%----------------------------------------------
%% @doc Returns the number associated with Name.
%%     Crashes if no association exists.
%% @spec (term(),server())-> Number::non_neg_integer()|{error, {not_numbered,Name}}.
%% @end
%%----------------------------------------------
n2i(Name,Server) ->
  case is_numbered(Name,Server) of
    false ->
      {error, {not_numbered,Name}};
    True ->
      {ok,True}
  end.

%%----------------------------------------------
%% @doc Returns the number associated with Name.
%%     Crashes if no association exists.
%% @spec (term(),server())-> Name::Term()|{error, {not_named,Number}}.
%% @end
%%----------------------------------------------

i2n(Number,Server) ->
  case is_named(Number,Server) of
    false ->
      {error, {not_named,Number}};
    True ->
      {ok,True}
  end.


%%----------------------------------------------
%% @doc Creates a link between a name and a number.
%%     Crashes if the number is already bound.
%% @spec (term(),server())-> {ok, Number::non_neg_integer()} | { error, {is_numbered,Name}}.
%% @end
%%----------------------------------------------
number(Name,Server) ->
  case is_numbered(Name,Server) of
    false ->
      {ok,?MODULE:get(Name,Server)};
    True ->
      {error, {is_numbered, {Name,True}}}
  end.

%%----------------------------------------------
%% @doc Creates a link between a name and a number.
%%     Crashes if the number is already bound.
%% @spec (non_neg_integer(),term(),server())-> ok | {error, {is_named,Name,NamedNumber::non_neg_intege()},Number}.
%% @end
%%----------------------------------------------
name(Number,Name,Server) ->
  case is_named(Number,Server) of
    false ->
      gen_server:cast(numberer_srv,{register,Name,Number,Server});
    True ->
      {error,{is_named,{Number,True},Name}}
  end.

%%----------------------------------------------
%% @doc Checks whether Number has a named associated with it. Returs either false, or the associated name.
%% @spec (non_neg_integer(),server()) -> false|Name::term().
%% @end
%%----------------------------------------------
is_named(Number,Server) ->
  gen_server:call(numberer_srv,{is_named,Number,Server}).

%%----------------------------------------------
%% @doc Substitute the OldName-OldInode link with a NewName-OldInode link.
%% @spec (term(),server())-> ok
%% @end
%%----------------------------------------------
rename(OldName,NewName,Server) ->
  gen_server:cast(numberer_srv,{rename,OldName,NewName,Server}).

%%----------------------------------------------
%% @doc Checks whether Name has a number associated with it. Returs either false, or the associated number.
%% @spec (term(),server()) -> false|Name::uniqe_integer().
%% @end
%%----------------------------------------------
is_numbered(Name,Server) ->
  gen_server:call(numberer_srv,{is_numbered,Name,Server}).

%%----------------------------------------------
%% @doc Checks whether Number is currently in use. Returns either false or true.
%% @spec (term(),server()) -> bool().
%% @end
%%----------------------------------------------
is_used(Number,Server) ->
  gen_server:call(numberer_srv,{is_used,Number,Server}).

%%----------------------------------------------
%% @doc Makes the number Number no longer occupied.
%%      Releases any name association with Number, and makes Number free to get returned by a call to get/0 or get/1.
%% @spec (term(),server()) -> false|Name::unique_integer().
%% @end
%%----------------------------------------------
release(Number,Server) ->
  gen_server:cast(numberer_srv,{return,Number,Server}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings.
%% @spec (server()) -> ok.
%% @end
%%----------------------------------------------
reset(Server) ->
  gen_server:cast(numberer_srv,{reset,1,Server}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings. Starts anew at number Number.
%% @spec (Number,server()) -> ok.
%% @end
%%----------------------------------------------
reset(Number,Server) ->
  gen_server:cast(numberer_srv,{reset,Number,Server}).

%%----------------------------------------------
%% @doc Lists all integers bound to a term.
%% @spec (server()) -> [{term(),unique_integer()}].
%% @end
%%----------------------------------------------
list_bound(Server) ->
  gen_server:call(numberer_srv,{list,Server}).

