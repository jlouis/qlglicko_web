REBAR=rebar3

compile:
	${REBAR} compile

dialyzer:
	${REBAR} dialyzer | sed -e 's|/_build/default/lib/qlglicko_web||'


