xvista: configure
	(cd source ; make -k xvista)

install: configure
	(cd source ; make -k install)

# comptar target for systems which support X option in tar
comptar: configure
	(cd source ; make -k comptar)

# comptar target for DEC systems which use -e flag to exclude files
dcomptar: configure
	(cd source ; make -k dcomptar)

clean: configure
	(cd source ; make -k clean)

configure:
	(cd source ; ./configure)
