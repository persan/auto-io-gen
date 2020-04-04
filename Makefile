export PATH:=${CURDIR}/bin:${PATH}

PREFIX?=${CURDIR}/_
all: compile test

compile:
	gprbuild -P auto_io_gen.gpr -XASIS_BUILD=static -j0 -p -k
test:
	${MAKE} -C tests all

install:
	sudo `which gprinstall` -f -p -r -P auto_io_gen.gpr --prefix=${PREFIX}

gps:
	gps -P auto_io_gen.gpr&
	gps -P tests/simple.gpr&
	
tag:compile test
	@if [ -n "`git status --porcelain`" ] ; then\
		echo "Folder is not clean";\
		git status;\
		exit 1;\
	fi
	
	@bin/check v`bin/auto_io_gen --version`-`date +%Y%m%d`
	@git tag v`bin/auto_io_gen --version`-`date +%Y%m%d`
	@git push
	@git push --tag			
