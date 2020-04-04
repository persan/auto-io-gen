exec_dir=$(shell gprinfo -P ../auto_io_gen.gpr  --exec-dir)
export PATH:=${exec_dir}:${PATH}
export GPR_PROJECT_PATH=$(shell dirname ${CURDIR})
all: text_io


json text_io image:
	@rm -rf src/$@/gen
	mkdir -p results
	auto_io_gen -T -p src/simple.ads src/$@/gen --$@
	gprbuild -p -P  simple.gpr -XLANGUAGE=$@ -j0 -k
	bin/simple-$@-main > results/simple-$@.result.txt
	diff results/simple-$@.result.txt results/simple-$@.golden.txt
