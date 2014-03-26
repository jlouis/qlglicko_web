PROJECT = qlglicko_web

DEPS = lager gproc jsx cowboy mimetypes
dep_mimetypes = https://github.com/spawngrid/mimetypes.git
dep_lager = https://github.com/basho/lager.git 2.0.3
dep_gproc = https://github.com/uwiger/gproc.git master
dep_jsx   = https://github.com/talentdeficit/jsx.git master
dep_cowboy = https://github.com/extend/cowboy.git master

include erlang.mk
