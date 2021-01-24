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

-type server_id() :: string() | atom() | integer.
-type unique_integer() :: non_neg_integer().
%%----------------------------------------------
%% @doc initates a new numbering server.
%%       If server is already initiated, nothing is done.
%% @end
%%----------------------------------------------
-spec initiate(string()|atom()|integer()) -> ok.
initiate(Server) ->
  gen_server:cast(numberer_srv,{init,1,Server}).

%%----------------------------------------------
%% @doc initates a new numbering server that starts the numbering at N.
%%       If server is already initiated, nothing is done.
%% @end
%%----------------------------------------------
-spec initiate(integer(),server_id()) -> ok.
initiate(N,Server) ->
  gen_server:cast(numberer_srv,{init,N,Server}).


%%----------------------------------------------
%% @doc counts the number of occupied numbers.
%% @end
%%----------------------------------------------
-spec count_occupied(server_id()) -> non_neg_integer().
count_occupied(Server) ->
  gen_server:call(numberer_srv,{count,Server}).

%%----------------------------------------------
%% @doc Returns an unused integer.
%% @end
%%----------------------------------------------
-spec get(server_id()) -> unique_integer().
get(Server) ->
  case gen_server:call(numberer_srv,{get,Server}) of
    undefined -> throw( {"not initiated",Server});
    N -> N
  end.

%%----------------------------------------------
%% @doc Associates a number with or gets the number associated with a thing.
%%     On first run, associate a unique integer with Name.
%%     On sequential runs, return the integer associated with Name.
%% @end
%%----------------------------------------------
-spec get(term(),server_id())->non_neg_integer().
get(Name,Server) ->
  case is_numbered(Name,Server) of
    false -> 
      Number=?MODULE:get(Server),
      gen_server:cast(numberer_srv,{register,Name,Number,Server}),
      Number;
    Number -> Number
  end.

%%----------------------------------------------
%% @doc Returns the number associated with Name, or crashes.
%%     Crashes if no association exists.
%% @end
%%----------------------------------------------
-spec n2i(Name,server_id()) -> non_neg_integer()|{error,{not_numbered,Name}}
    when Name::term().
n2i(Name,Server) ->
  case is_numbered(Name,Server) of
    false ->
      {error, {not_numbered,Name}};
    True ->
      {ok,True}
  end.

%%----------------------------------------------
%% @doc Returns the name associated with Number.
%%     Crashes if no association exists.
%% @end
%%----------------------------------------------
-spec i2n(Number,server_id())-> Name::term()|{error, {not_named,Number}}
    when Number::non_neg_integer().

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
%% @end
%%----------------------------------------------
-spec number(Name,server_id())-> {ok, unique_integer()} | { error, {is_numbered,Name}}
    when Name::term().
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
%% @end
%%----------------------------------------------
-spec name(Number,Name,server_id())-> ok | {error, {is_named,Name,NamedNumber},Number}
    when Number::unique_integer(),
    NamedNumber::unique_integer(),
    Name::term().
name(Number,Name,Server) ->
  case is_named(Number,Server) of
    false ->
      gen_server:cast(numberer_srv,{register,Name,Number,Server});
    True ->
      {error,{is_named,{Number,True},Name}}
  end.

%%----------------------------------------------
%% @doc Checks whether Number has a named associated with it. Returs either false, or the associated name.
%% @end
%%----------------------------------------------
-spec is_named(unique_integer(),server_id()) -> false | term().
is_named(Number,Server) ->
  gen_server:call(numberer_srv,{is_named,Number,Server}).

%%----------------------------------------------
%% @doc Substitute the OldName-OldInode link with a NewName-OldInode link.
%% @end
%%----------------------------------------------
-spec rename(term(),term(),server_id()) -> ok.
rename(OldName,NewName,Server) ->
  gen_server:cast(numberer_srv,{rename,OldName,NewName,Server}).

%%----------------------------------------------
%% @doc Checks whether Name has a number associated with it. Returs either false, or the associated number.
%% @end
%%----------------------------------------------
-spec is_numbered(term(),server_id()) -> false | {is_numbered,unique_integer()}.
is_numbered(Name,Server) ->
  gen_server:call(numberer_srv,{is_numbered,Name,Server}).

%%----------------------------------------------
%% @doc Checks whether Number is currently in use. Returns either false or true.
%% @end
%%----------------------------------------------

-spec is_used(unique_integer(),server_id()) -> boolean().
is_used(Number,Server) ->
  gen_server:call(numberer_srv,{is_used,Number,Server}).

%%----------------------------------------------
%% @doc Makes the number Number no longer occupied.
%%      Releases any name association with Number, and makes Number free to get returned by a call to get/0 or get/1.
%% @end
%%----------------------------------------------
-spec release(unique_integer(),server_id()) -> ok.
release(Number,Server) ->
  gen_server:cast(numberer_srv,{return,Number,Server}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings.
%% @end
%%----------------------------------------------
-spec reset(server_id()) -> ok.
reset(Server) ->
  gen_server:cast(numberer_srv,{reset,1,Server}).

%%----------------------------------------------
%% @doc Resets the counter. Forgets all name bindings. Starts anew at number Number.
%% @end
%%----------------------------------------------
-spec reset(StartingNumber::integer(),server_id()) -> ok.
reset(Number,Server) ->
  gen_server:cast(numberer_srv,{reset,Number,Server}).

%%----------------------------------------------
%% @doc Lists all integers bound to a term.
%% @end
%%----------------------------------------------
-spec list_bound(server_id()) -> [{Name::term(),unique_integer()}].
list_bound(Server) ->
  gen_server:call(numberer_srv,{list,Server}).

