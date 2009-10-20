ERLCFLAGS = -pa ebin +debug_info -W -I include
.DEFAULT_GOAL = all
.PHONY: all clean

# [src/foo.erl, src/bar/tee.erl] -> [ebin/foo.beam, ebin/tee.beam]
define erl2beam
	$(foreach d, $(1), $(patsubst %.erl, ebin/%.beam, $(notdir $(wildcard $(d)/*.erl))))
endef

ebin/%.beam: %.erl | ebin
	erlc $(ERLCFLAGS) -o ebin $<

%.app: | ebin
	cp $< $@

######################################################################### deps
ERLYDTL_D = deps/erlydtl/src/erlydtl
MOCHIWEB_D = deps/mochiweb/src
vpath %.erl $(MOCHIWEB_D) $(ERLYDTL_D)

$(ERLYDTL_D)/erlydtl_parser.erl: $(ERLYDTL_D)/erlydtl_parser.yrl
	erlc -o $(ERLYDTL_D) $<
ebin/erlydtl_compiler.beam: ebin/erlydtl_parser.beam
ebin/mochiweb.app: $(MOCHIWEB_D)/mochiweb.app
ebin/erlydtl.app: $(ERLYDTL_D)/erlydtl.app

################################################################# playdar-core
DIRS = src src/behaviours
BEAM = $(call erl2beam, $(DIRS))
vpath %.erl $(wildcard $(DIRS))

ebin/script_resolver.beam: ebin/playdar_resolver.beam
ebin/playdar.app: src/playdar.app

############################################################## playdar-modules
TAGLIB_JSON_READER = playdar_modules/library/priv/taglib_driver/taglib_json_reader

define MODULE_template
$(1)/ebin:
	mkdir -p $$@
$(1)/ebin/%.beam: $(1)/src/%.erl $(call erl2beam, src/behaviours) | $(1)/ebin
	erlc $(ERLCFLAGS) -o $(1)/ebin $$<

BEAM += $(patsubst %.erl, $(1)/ebin/%.beam, $(notdir $(wildcard $(1)/src/*.erl)))
EBIN += $(1)/ebin
endef

$(foreach d, $(wildcard playdar_modules/*), $(eval $(call MODULE_template, $(d))) )

$(TAGLIB_JSON_READER): $(TAGLIB_JSON_READER).cpp
	g++ -I/opt/local/include `taglib-config --cflags` `taglib-config --libs` -o $@ $<

########################################################################## all
all: $(BEAM) $(TAGLIB_JSON_READER) ebin/playdar.app ebin/mochiweb.app ebin/erlydtl.app

clean:
	rm -rf ebin $(EBIN)
	rm -f $(ERLYDTL_PARSER)

$(BEAM): include/playdar.hrl $(call erl2beam, $(MOCHIWEB_D) $(ERLYDTL_D))

ebin:
	mkdir ebin
