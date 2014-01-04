RUSTC = time -p rustc
all:
	@echo "make debug|opt|prof|plots|clean"
	exit 1

debug:
	$(RUSTC) -Z extra-debug-info main.rs

opt:
	$(RUSTC) -O main.rs

prof: debug
	valgrind --tool=callgrind ./main --tasks=1 --samples=1000

plot:
	Rscript plots.R

data/%/meta.csv data/%/samples.csv: data/%/args.txt
	( \
	cd $$(dirname $@); \
	pwd; \
	touch -a args.txt; \
	../../main $$(cat args.txt); \
	)

.PRECIOUS: data/%/meta.csv
.PRECIOUS: data/%/samples.csv
.PRECIOUS: data/%/Rplots.svg

data/%/Rplots.svg: data/%/meta.csv data/%/samples.csv
	( \
	cd $$(dirname $@); \
	pwd; \
	Rscript ../../plots.R; \
	)

%.pdf: %.svg
	inkscape -T -z -A $@ $<

data/%/Rplots.pdf: data/%/Rplots.svg
	inkscape -T -z -A $@ $<


timings=$(patsubst data/%/args.txt, %, $(wildcard data/*/args.txt))

index.html:
	echo > index.html
	for t in $(timings); do \
		echo '<img src="data/'$$t'/Rplots.svg" style="border: 1px solid black;" />' >> index.html; \
	done

#plots: index.html $(addprefix data/, $(addsuffix /Rplots.svg, $(timings)))
plots: $(addprefix data/, $(addsuffix /Rplots.pdf, $(timings)))

lunch/timelines/%/timeline.svg:
	( \
	cd $$(dirname $@); \
	pwd; \
	Rscript tl.R; \
	)

timelines=$(patsubst lunch/timelines/%/tl.R, %, $(wildcard lunch/timelines/*/tl.R))
timelines: $(addprefix lunch/timelines/, $(addsuffix /timeline.pdf, $(timelines)))

clean:
	rm -f data/*/*.svg
	rm -f data/*/*.csv
	rm -f index.html
	rm -f main


# The following target is useful for debugging Makefiles; it
# prints the value of a make variable.
print-%:
	@echo $* = $($*)
