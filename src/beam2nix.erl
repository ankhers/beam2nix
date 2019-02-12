-module(beam2nix).

%% API exports
-export([new/5]).

-import(prettypr, [above/2]).

-record(app, {name, vsn, src, deps, kind}).
%%====================================================================
%% API functions
%%====================================================================
-spec new(atom(), string(), string(), [{binary(), binary()}], atom()) -> prettypr:document().
new(AppName, Vsn, Src, Deps, Kind) ->
    App = #app{name = AppName, vsn = Vsn, src = Src, deps = Deps, kind = Kind},
    above(
      header(App#app.deps),
      derivation(App)
     ).

%%====================================================================
%% Internal functions
%%====================================================================
-spec header([{binary(), binary()}]) -> prettypr:document().
header([]) ->
    above(
      text("{ rebar3Relx }:"),
      text("")
     );
header(Deps) ->
    above([
           text("{ rebar3Relx, fetchHex }:"),
           text(""),
           text("let"),
           text(""),
           nest(deps(Deps)),
           text("in"),
           text("")
          ]
     ).

-spec deps([{binary(), binary()}]) -> prettypr:document().
deps(Deps) ->
    lists:foldr(fun(Dep, Acc) ->
                        prettypr:above(dep_doc(Dep), Acc)
                end, prettypr:empty(), Deps).

-spec dep_doc({binary(), binary()}) -> prettypr:document().
dep_doc(Dep) ->
    above([
           prettypr:beside(dep_name(Dep), text(" = fetchHex {")),
           nest(dep_attrs(Dep)),
           text("};"),
           text("")
          ]).

-spec dep_attrs({binary(), binary()}) -> prettypr:document().
dep_attrs({Name, Vsn}) ->
    above([
           kv("pkg", quote(binary_to_list(Name))),
           kv("version", quote(binary_to_list(Vsn))),
           kv("sha256", quote(sha256(Name, Vsn)))
          ]).

%% TODO: This is really slow. Probably want to async grab all the hashes and
%%       then try to build the document.
-spec sha256(binary(), binary()) -> string().
sha256(Name, Vsn) ->
    %% "https://repo.hex.pm/tarballs/${pkg}-${version}.tar";
    Output = os:cmd("nix-prefetch-url " ++ url(Name, Vsn)),
    [_, Sha, _] = string:split(Output, "\n", all),
    Sha.

-spec url(binary(), binary()) -> string().
url(Name, Vsn) ->
    "https://repo.hex.pm/tarballs/" ++ binary_to_list(Name) ++ "-" ++ binary_to_list(Vsn) ++ ".tar".

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
              deps_list(App#app.deps),
              kv("releaseType", quote(atom_to_list(App#app.kind)))
             ],
    above(Chunks).

deps_list(Deps) ->
    case deps_names(Deps) of
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
-spec deps_names([{binary(), binary()}]) -> [prettypr:document()].
deps_names(Deps) ->
    lists:map(fun dep_name/1, Deps).

-spec dep_name({binary(), binary()}) -> prettypr:document().
dep_name({Name, Vsn}) ->
    prettypr:beside(
      text(Name),
      prettypr:beside(
        text("_"),
        text(fix_version(Vsn))
       )
     ).

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
