OUTPUTDIR = ./build
DAY = Day01
FLAGS = -Wall -Werror -O3

$(OUTPUTDIR)/%.o: %.c
	@mkdir -p $(@D)
	gcc $(FLAGS) -o $@ -c $<

$(OUTPUTDIR)/Day%: Day%.c $(OUTPUTDIR)/utils.o
	gcc $(FLAGS) -o $@ $^

$(OUTPUTDIR)/Day%-sample: Day%.c $(OUTPUTDIR)/utils.o
	gcc $(FLAGS) -DSAMPLE -o $@ $^

all: $(OUTPUTDIR)/$(DAY)
	$<

.PHONY: sample
sample: $(OUTPUTDIR)/$(DAY)-sample
	$<

clean:
	rm -rf $(OUTPUTDIR)
