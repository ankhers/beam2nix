-module(beam2nix).

%% API exports
-export([new/1]).

-import(prettypr, [above/2]).

-type(app() :: #{
                 name := atom(),
                 vsn := string(),
                 src := string(),
                 deps := [{binary(), binary()}],
                 release_type := atom(),
                 otp_vsn => string()
                }).
-export_type([app/0]).
%%====================================================================
%% API functions
%%====================================================================
-spec new(app()) -> prettypr:document().
new(#{name := AppName, vsn := Vsn} = App) ->
    Name = dep_name({AppName, Vsn}),
    above([
           header(),
           nest(builds(App)),
           text("in"),
           text(""),
           prettypr:sep([
                text("  beamPackages.callPackage"),
                Name,
                text("{ }")
               ])
          ]).

%%====================================================================
%% Internal functions
%%====================================================================
-spec builds(app()) -> prettypr:document().
builds(#{deps := Deps} = App) ->
    above([app(App), deps(Deps)]).

-spec app(app()) -> prettypr:document().
app(#{name := AppName, vsn := Vsn} = App) ->
    Name = dep_name({AppName, Vsn}),
    above([
           prettypr:beside(Name, text(" = { rebar3Relx }:")),
           nest(derivation(App)),
           text(" ")
          ]).

-spec header() -> prettypr:document().
header() ->
    above([
           text("{ pkgs ? import <nixpkgs> { } }:"),
           text(""),
           text("with pkgs;"),
           text(""),
           text("let"),
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

-spec derivation(app()) -> prettypr:document().
derivation(App) ->
    above(
      text("rebar3Relx {"),
      above(
        nest(body(App)),
        text("};")
           )
     ).

-spec body(app()) -> prettypr:document().
body(#{name := Name, vsn := Vsn, src := Src, deps := Deps, release_type := ReleaseType}) ->
    Chunks = [
              kv("name", quote(atom_to_list(Name))),
              kv("version", quote(Vsn)),
              kv("src", Src),
              deps_list(Deps),
              kv("releaseType", quote(atom_to_list(ReleaseType)))
             ],
    above(Chunks).

-spec deps_list([{binary(), binary()}]) -> prettypr:document().
deps_list(Deps) ->
    case deps_names(Deps) of
        [] ->
            prettypr:empty();
        DepsDocs ->
            above([
                   prettypr:sep([text("beamDeps = [")]),
                   nest(above(DepsDocs)),
                   text("];")
                  ])
    end.

-spec deps_names([{binary(), binary()}]) -> [prettypr:document()].
deps_names(Deps) ->
    lists:map(fun dep_name/1, Deps).

-spec dep_name({binary() | atom(), binary()}) -> prettypr:document().
dep_name({Name, Vsn}) when is_atom(Name) ->
    do_dep_name(atom_to_list(Name), Vsn);
dep_name({Name, Vsn}) ->
    do_dep_name(Name, Vsn).

-spec do_dep_name(binary() | string(), binary()) -> prettypr:document().
do_dep_name(Name, Vsn) ->
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

-spec above([prettypr:document()]) -> prettypr:document().
above(List) when is_list(List) ->
    lists:foldr(fun prettypr:above/2, prettypr:empty(), List).

-spec fix_version(binary()) -> string().
fix_version(Vsn) ->
    re:replace(Vsn, "\\.", "_", [global, {return, list}]).
