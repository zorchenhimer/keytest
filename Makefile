.PHONY: default env clean cleanall chr

CHRUTIL = go-nes/bin/chrutil
SCRCONV = convert-screen.go
NAME = keytest
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = main.asm \
		  controller.i \
		  keyboard.i
CHR = font.chr

default: env bin/$(NAME).nes
env: $(CHRUTIL) bin/
bin/:
	mkdir bin

clean:
	-rm bin/* *.chr *.i

cleanall: clean
	-rm images/*.bmp

bin/$(NAME).nes: bin/main.o
	ld65 $(LDFLAGS) -o $@ $^

bin/main.o: $(SOURCES) $(CHR)
	ca65 $(CAFLAGS) -o $@ main.asm

images/%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

font.chr: images/font.bmp
	$(CHRUTIL) -o $@ $<

controller.i: images/controller.tmx $(SCRCONV)
	go run convert-screen.go $< $@ --fill 32

keyboard.i: images/keyboard.tmx $(SCRCONV)
	go run convert-screen.go $< $@ --layer main --fill 32

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil$(EXT)
