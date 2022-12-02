OUTPUTDIR = ./build
DAY = Day01
FLAGS = -Wall -Werror -O3

$(OUTPUTDIR)/%.o: %.c
	@mkdir -p $(@D)
	gcc $(FLAGS) -o $@ -c $<

$(OUTPUTDIR)/Day%: Day%.c utils.h $(OUTPUTDIR)/utils.o
	@mkdir -p $(@D)
	gcc $(FLAGS) -o $@ $< $(OUTPUTDIR)/utils.o

all: run

.PHONY: run
run: $(OUTPUTDIR)/$(DAY)
	$<

.PHONY: sample
sample: $(OUTPUTDIR)/$(DAY)
	$< sample

clean:
	rm -rf $(OUTPUTDIR)
