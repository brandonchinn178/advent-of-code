OUTPUTDIR = ./build
DAY = Day01

$(OUTPUTDIR)/%.o: %.c
	@mkdir -p $(@D)
	gcc -Werror -c $< -o $@

$(OUTPUTDIR)/Day%: Day%.c $(OUTPUTDIR)/utils.o
	gcc -Werror -O3 -o $@ $^

all: $(OUTPUTDIR)/$(DAY)
	$(OUTPUTDIR)/$(DAY)

clean:
	rm -rf $(OUTPUTDIR)
