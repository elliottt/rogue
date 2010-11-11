
GHC_DIR	= ghc
GHC	= ghc -i$(GHC_DIR) -odir $(GHC_DIR) -hidir $(GHC_DIR)

CC	= gcc
CFLAGS	= $(shell sdl-config --cflags)
LDFLAGS	= $(shell sdl-config --ldflags)

ifeq ($(V),)
	QUIET_CC	= @echo "  CC     $@";
	QUIET_GHC	= @echo "  GHC    $@";
	QUIET_CLEAN	= @echo "  CLEAN  $(subst clean-,,$@)";
	QUIET_DEPEND	= @echo "  DEPEND $@";
	Q		= @
else
	QUIET_CC	=
	QUIET_GHC	=
	QUIET_CLEAN	=
	QUIET_DEPEND	=
	Q		=
endif

SLASH_MODS	= $(subst src/,,$(basename $(shell find src -name '*.hs')))

HS_SOURCES	= $(addprefix src/,$(addsuffix .hs,$(SLASH_MODS)))
HS_OBJS		= $(addprefix $(GHC_DIR)/,$(addsuffix .o,$(SLASH_MODS)))
HS_LIBS		= -package OpenGL -package containers -package SDL \
		  -package SDL-image -package stm


all: test

test: cbits/sdl-opengl.o $(HS_OBJS)
	$(QUIET_GHC) $(GHC) -threaded $(HS_LIBS) -o $@ $^

cbits/sdl-opengl.o: cbits/sdl-opengl.c
	$(QUIET_CC) $(CC) $(CFLAGS) -c $< -o $@

ghci: $(GHC_DIR) $(HS_OBJS)
	$(GHC) --interactive -lGL -lSDL cbits/sdl-opengl.o -isrc

$(GHC_DIR):
	$(Q) mkdir $(GHC_DIR)

-include $(GHC_DIR)/depend

$(GHC_DIR)/depend: $(GHC_DIR)
	$(QUIET_DEPEND) $(GHC) -M -dep-makefile $@ $(HS_SOURCES)

$(GHC_DIR)/%.o: src/%.hs
	$(QUIET_GHC) $(GHC) -c $<

%.hi : %.o ;

clean: clean-cbits clean-$(GHC_DIR)
	$(Q) $(RM) test

clean-cbits:
	$(QUIET_CLEAN) $(RM) cbits/sdl-opengl.o

clean-$(GHC_DIR):
	$(QUIET_CLEAN) $(RM) -r $(GHC_DIR)
