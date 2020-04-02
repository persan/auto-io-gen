exec_dir=$(shell gprinfo -P ../auto_io_gen.gpr  --exec-dir)
export PATH:=${exec_dir}:${PATH}

all:
	@echo ${exec_dir}
	@rm -rf Auto.*
	auto_io_gen -p simple.ads Auto.Text_Io -d --text_io
	auto_io_gen -p simple.ads Auto.Image -d --image
	auto_io_gen -p simple.ads Auto.Json -d --json
	gprbuild -p -P  simple.gpr -j0 -k
	bin/simple-main > simple-main.result.txt
	diff simple-main.result.txt simple-main.golden.txt


