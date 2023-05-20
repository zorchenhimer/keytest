.PHONY: default env clean cleanall chr

CHRUTIL = go-nes/bin/chrutil
SCRCONV = convert-screen.go
NAME = keytest
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = main.asm \
		  controller.i \
		  keyboard.i \
		  family-trainer-a.i \
		  family-trainer-b.i \
		  glasses-left.i \
		  glasses-right.i
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

family-trainer-a.i: images/family-trainer-split.tmx $(SCRCONV)
	go run convert-screen.go $< $@ --layer SideA --fill 32

family-trainer-b.i: images/family-trainer-split.tmx $(SCRCONV)
	go run convert-screen.go $< $@ --layer SideB --fill 32

glasses-left.i: images/glasses-left-right.tmx
	go run convert-screen.go $< $@ --layer left
glasses-right.i: images/glasses-left-right.tmx
	go run convert-screen.go $< $@ --layer right

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil$(EXT)
