REBAR = ./rebar $(REBAR_ARGS)
REBAR_URL = http://cloud.github.com/downloads/basho/rebar/rebar

.PHONY: clean compile test

compile: rebar
	$(REBAR) get-deps compile

clean:
	$(REBAR) clean

test: compile
	$(REBAR) eunit -v skip_deps=true

rebar:
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar
