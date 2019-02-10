-module(beam2nix).

%% API exports
-export([new/3]).

-import(prettypr, [above/2, text/1]).

-record(app, {name, vsn, src}).
%%====================================================================
%% API functions
%%====================================================================
-spec new(atom(), string(), string()) -> prettypr:document().
new(AppName, Vsn, Src) ->
    App = #app{name = AppName, vsn = Vsn, src = Src},
    above(
      header(),
      derivation(App)
     ).

%%====================================================================
%% Internal functions
%%====================================================================
-spec header() -> prettypr:document().
header() ->
    above(
      text("{ rebar3Relx }:"),
      text("")
     ).

-spec derivation(#app{}) -> prettypr:document().
derivation(App) ->
    above(
      text("rebar3Relx {"),
      above(
        nest(body(App)),
        text("}")
           )
     ).

-spec body(#app{}) -> prettypr:document().
body(App) ->
    Chunks = [
              kv("name", quote(atom_to_list(App#app.name))),
              kv("version", quote(App#app.vsn)),
              kv("src", App#app.src)
             ],
    lists:foldr(fun prettypr:above/2, prettypr:empty(), Chunks).

-spec quote(string()) -> prettypr:document().
quote(Value) ->
    prettypr:beside(text("\""), prettypr:beside(text(Value), text("\""))).

-spec kv(string(), string() | prettypr:document()) -> prettypr:document().
kv(Key, Value) when is_list(Value) ->
    prettypr:sep([text(Key), text("="), text(Value)]);
kv(Key, Value) ->
    prettypr:sep([text(Key), text("="), Value]).

-spec nest(prettypr:document()) -> prettypr:document().
nest(Document) ->
    prettypr:nest(2, Document).
