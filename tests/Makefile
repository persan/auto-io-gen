exec_dir=$(shell gprinfo -P ../auto_io_gen.gpr  --exec-dir)
export PATH:=${exec_dir}:${PATH}

all:
	@echo ${exec_dir}
	@rm -rf Auto
	auto_io_gen -T -p simple.ads Auto -d --text_io
#	auto_io_gen -p simple.ads Auto -d --image
#	auto_io_gen -p anotations.ads Auto -d  --text_io
	gprbuild -p -P  simple.gpr -j0 -k

