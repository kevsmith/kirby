-include_lib("eunit/include/eunit.hrl").

-define(FOREACH(Setup, Tests), {foreach, Setup, fun(_) -> ok, Tests}).
-define(FOREACH(Setup, Teardown, Tests), {foreach, Setup, Teardown, Tests}).
