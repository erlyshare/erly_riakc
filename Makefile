.PHONY: all clean 
REBAR3 := rebar3

all:
	@$(REBAR3) compile

clean:
	@$(REBAR3) clean

