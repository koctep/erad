PROJECT = erad
PROJECT_DESCRIPTION = New project
#PROJECT_VERSION = 0.0.1

BUILD_DEPS 	= lager
DEPS				= _ jsxn whiskey4cowboy

dep__ 							= git https://github.com/koctep/_.git
dep_jsxn 						= git https://github.com/talentdeficit/jsxn.git
dep_whiskey4cowboy 	= git https://github.com/koctep/whiskey4cowboy.git

CONFIGS = $(foreach c, $(wildcard etc/*.config), -config $(c))

SHELL_ERL = erl $(CONFIGS) -args_file etc/vm.args

include erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
ERLC_OPTS += +'{parse_transform, lager_transform}'
