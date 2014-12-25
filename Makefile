PROJECT = qlglicko_web

DEPS = lager gproc jsx cowboy mimetypes
dep_jsx   = https://github.com/talentdeficit/jsx.git master
dep_cowboy = https://github.com/extend/cowboy.git master

include erlang.mk
