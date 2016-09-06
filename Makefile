PROJECT = erad
PROJECT_DESCRIPTION = New project
#PROJECT_VERSION = 0.0.1

BUILD_DEPS 	= lager
DEPS				= _

dep__ = git https://github.com/koctep/_.git

include erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
ERLC_OPTS += +'{parse_transform, lager_transform}'
