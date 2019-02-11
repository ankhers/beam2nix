-module(beam2nix).

%% API exports
-export([new/4]).

-import(prettypr, [above/2]).

-record(app, {name, vsn, src, deps}).
%%====================================================================
%% API functions
%%====================================================================
-spec new(atom(), string(), string(), [{binary(), binary()}]) -> prettypr:document().
new(AppName, Vsn, Src, Deps) ->
    App = #app{name = AppName, vsn = Vsn, src = Src, deps = Deps},
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
              kv("src", App#app.src),
              % kv("checkouts", deps(App#app.deps))
              deps_docs(App#app.deps)
             ],
    above(Chunks).
    %% lists:foldr(fun prettypr:above/2, prettypr:empty(), Chunks).

deps_docs(Deps) ->
    case deps(Deps) of
        [] ->
            kv("checkouts", "[ ]");
        DepsDocs ->
            above([
                   prettypr:sep([text("checkouts = [")]),
                   nest(above(DepsDocs)),
                   text("];")
                  ])
    end.

%% TODO: Properly format for large lists of values
-spec deps([{binary(), binary()}]) -> prettypr:document().
deps(Deps) ->
    lists:map(fun({Name, Vsn}) ->
                      prettypr:beside(
                        text(Name),
                        prettypr:beside(
                          text("_"),
                          text(fix_version(Vsn))
                         )
                       )
              end, Deps).

-spec quote(string()) -> prettypr:document().
quote(Value) ->
    prettypr:beside(text("\""), prettypr:beside(text(Value), text("\""))).

-spec kv(string(), string() | prettypr:document()) -> prettypr:document().
kv(Key, Value) when is_list(Value) ->
    prettypr:beside(prettypr:sep([text(Key), text("="), text(Value)]), text(";"));
kv(Key, Value) ->
    prettypr:beside(prettypr:sep([text(Key), text("="), Value]), text(";")).

-spec nest(prettypr:document()) -> prettypr:document().
nest(Document) ->
    prettypr:nest(2, Document).

-spec text(binary() | string()) -> prettypr:document().
text(Bin) when is_binary(Bin) ->
    prettypr:text(erlang:binary_to_list(Bin));
text(Str) when is_list(Str) ->
    prettypr:text(Str).

above(List) when is_list(List) ->
    lists:foldr(fun prettypr:above/2, prettypr:empty(), List).

fix_version(Vsn) ->
    re:replace(Vsn, "\\.", "_", [global, {return, list}]).
