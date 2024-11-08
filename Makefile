.PHONY: default env clean cleanall chr

CHRUTIL = go-nes/bin/chrutil
SCRCONV = convert-screen.go
NAME = keytest
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = main.asm \
		  3d-glasses.asm \
		  feet.asm \
		  keyboard.asm \
		  standard-controllers.asm \
		  trackball.asm \
		  tablet.asm \
		  hypershot.asm \
		  mahjong.asm \
		  controller.i \
		  keyboard.i \
		  titler.asm \
		  family-trainer-a.i \
		  family-trainer-b.i \
		  glasses-left.i \
		  glasses-right.i

CHR = font.chr font_lower.chr

default: env bin/$(NAME).nes
env: $(CHRUTIL) bin/
bin/:
	-mkdir bin

send: default
	./edlink bin/$(NAME).nes

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

font_lower.chr: images/font_lower.bmp
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
