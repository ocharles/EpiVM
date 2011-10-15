CC = gcc
#CFLAGS = -Wall -g
CFLAGS = -Wall -O3 -DUSE_BOEHM
OBJS = closure.o stdfuns.o emalloc.o sparks.o
INSTALLDIR = ${PREFIX}/lib/evm

TARGET = libevm.a
INSTALLHDRS = closure.h stdfuns.h mainprog.c sparks.h

${TARGET} : ${OBJS}
	ar r ${TARGET} ${OBJS}
	ranlib ${TARGET}

install:
	mkdir -p ${INSTALLDIR}
	install libevm.a ${INSTALLHDRS} ${INSTALLDIR}
	ranlib ${INSTALLDIR}/libevm.a

clean:
	rm -f ${OBJS} ${TARGET}

closure.o : closure.h emalloc.h
stdfuns.o : stdfuns.h closure.h emalloc.h
emalloc.o : closure.h emalloc.h
sparks.o  : sparks.h closure.h