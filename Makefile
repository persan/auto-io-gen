export PATH:=${CURDIR}/bin:${PATH}

all: compile test

compile:
	gprbuild -P auto_io_gen.gpr -XASIS_BUILD=default -j0 -p -k
test:
	${MAKE} -C tests all
