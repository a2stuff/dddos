
CAFLAGS := --target apple2 --list-bytes 0
LDFLAGS := --config apple2-asm.cfg

OUTDIR := out

TARGETS := $(OUTDIR)/double-dos.bin

MAKEFLAGS += --no-builtin-rules

.PHONY: clean all
all: $(OUTDIR) $(TARGETS)

$(OUTDIR):
	mkdir -p $(OUTDIR)

HEADERS := $(wildcard *.inc)

clean:
	rm -f $(OUTDIR)/*.o
	rm -f $(OUTDIR)/*.list
	rm -f $(TARGETS)

$(OUTDIR)/%.o: %.s $(HEADERS)
	ca65 $(CAFLAGS) $(DEFINES) --listing $(basename $@).list -o $@ $<

$(OUTDIR)/%.bin: $(OUTDIR)/%.o
	ld65 $(LDFLAGS) -o $@ $<
