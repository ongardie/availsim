RUSTC = time -p rustc
all:
	@echo "make debug|opt|prof"
	exit 1

debug:
	$(RUSTC) -Z extra-debug-info main.rs

opt:
	$(RUSTC) -O main.rs

prof: debug
	valgrind --tool=callgrind ./main --tasks=1 --samples=1000

plot:
	Rscript plots.R
