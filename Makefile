ERL_RUN_ARGS:=+K true -name counter -boot start_sasl -s pn_counter_app -sasl errlog_type error

compile: get-deps
	rebar compile

get-deps:
	rebar get-deps

clean:
	rebar clean
	rm -f erl_crash.dump

test:	compile
	rebar eunit skip_deps=true

run:
	ERL_LIBS=deps erl $(ERL_RUN_ARGS)

background:
	ERL_LIBS=deps erl -detached $(ERL_RUN_ARGS)

d:
	dialyzer --src -I include src

etags:
	etags src/* 
