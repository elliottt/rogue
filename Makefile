
profile		?= n

GHC_DIR	= ghc
GHC	= ghc -i$(GHC_DIR) -odir $(GHC_DIR) -hidir $(GHC_DIR)
HSFLAGS	= -O2

ifeq ($(profile),y)
HSFLAGS+= -prof -auto-all
endif

CC	= gcc
CFLAGS	= $(shell sdl-config --cflags)
LDFLAGS	= $(shell sdl-config --ldflags)

ifeq ($(V),)
	cmd	= @echo -e "  $1\t$@"; $($1)
	Q	= @
else
	cmd	= $($1)
	Q	=
endif

SLASH_MODS	:= $(subst src/,,$(basename $(shell find src -name '*.hs')))

HS_SOURCES	= $(addprefix src/,$(addsuffix .hs,$(SLASH_MODS)))
HS_OBJS		= $(addprefix $(GHC_DIR)/,$(addsuffix .o,$(SLASH_MODS)))
HS_LIBS		= -package OpenGL -package containers -package SDL \
		  -package SDL-image -package stm \
		  -package vector-0.7.0.1


all: test

test: cbits/sdl-opengl.o $(HS_OBJS)
	$(call cmd,GHC) $(HSFLAGS) -threaded $(HS_LIBS) -o $@ $^

cbits/sdl-opengl.o: cbits/sdl-opengl.c
	$(call cmd,CC) $(CFLAGS) -c $< -o $@

ghci: $(GHC_DIR) $(HS_OBJS)
	$(GHC) --interactive -lGL -lSDL cbits/sdl-opengl.o -isrc

$(GHC_DIR):
	$(Q) mkdir $(GHC_DIR)

-include $(GHC_DIR)/depend

$(GHC_DIR)/depend: $(GHC_DIR)
	$(Q) $(GHC) -M -dep-makefile $@ $(HS_SOURCES)

$(GHC_DIR)/%.o: src/%.hs
	$(call cmd,GHC) $(HSFLAGS) -c $<

%.hi : %.o ;

clean: clean-cbits clean-$(GHC_DIR)
	$(Q) $(RM) test

clean-cbits:
	$(call cmd,RM) cbits/sdl-opengl.o

clean-$(GHC_DIR):
	$(call cmd,RM) -r $(GHC_DIR)
