export PATH:=${CURDIR}/bin:${PATH}

PREFIX?=${CURDIR}/_
all: compile test

compile:
	gprbuild -P auto_io_gen.gpr -XASIS_BUILD=default -j0 -p -k
test:
	${MAKE} -C tests all

install:
	sudo `which gprinstall` -f -p -r -P auto_io_gen.gpr --prefix=${PREFIX}
