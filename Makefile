.PHONY: default env clean cleanall chr

CHRUTIL = go-nes/bin/chrutil
SCRCONV = convert-screen/convert-screen
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
		  network.asm \
		  family-trainer-a.i \
		  family-trainer-b.i \
		  glasses-left.i \
		  glasses-right.i \
		  network.i

CHR = font.chr font_lower.chr

default: $(SCRCONV) env bin/$(NAME).nes
env: $(CHRUTIL) bin/
bin/:
	-mkdir bin

send: default
	./edlink bin/$(NAME).nes

clean:
	-rm bin/* *.chr *.i

cleanall: clean
	-rm images/*.bmp

convert-screen/convert-screen: convert-screen/*.go
	cd convert-screen && go build -o convert-screen

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

network.i: images/network-controller_lg.tmx $(SCRCONV)
	$(SCRCONV) $< $@ --fill 32

controller.i: images/controller.tmx $(SCRCONV)
	$(SCRCONV) $< $@ --fill 32

keyboard.i: images/keyboard.tmx $(SCRCONV)
	$(SCRCONV) $< $@ --layer main --fill 32

family-trainer-a.i: images/family-trainer-split.tmx $(SCRCONV)
	$(SCRCONV) $< $@ --layer SideA --fill 32

family-trainer-b.i: images/family-trainer-split.tmx $(SCRCONV)
	$(SCRCONV) $< $@ --layer SideB --fill 32

glasses-left.i: images/glasses-left-right.tmx
	$(SCRCONV) $< $@ --layer left
glasses-right.i: images/glasses-left-right.tmx
	$(SCRCONV) $< $@ --layer right

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil$(EXT)
