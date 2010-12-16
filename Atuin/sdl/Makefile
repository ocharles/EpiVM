CC = gcc 
CFLAGS = `epic -includedirs`

INSTALLDIR = ${PREFIX}/lib/elogo

sdlrun.o : sdlrun.c sdlrun.h

install:
	mkdir -p ${INSTALLDIR}
	install sdlrun.o sdlrun.h ${INSTALLDIR}

clean:
	rm -f sdlrun.o

.PHONY:
