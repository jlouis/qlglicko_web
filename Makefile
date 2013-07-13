PROJECT = qlglicko_web

DEPS = lager gproc jsx cowboy
dep_lager = https://github.com/basho/lager.git 2.0.0
dep_gproc = https://github.com/uwiger/gproc.git master
dep_jsx   = https://github.com/talentdeficit/jsx.git master
dep_cowboy = https://github.com/extend/cowboy.git master

include erlang.mk
