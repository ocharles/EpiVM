#ifndef _STDFUNS_H
#define _STDFUNS_H

# ifndef WIN32
#  include <pthread.h>
#  define GC_THREADS
# else
#  define GC_WIN32_THREADS
# endif

#include <gc/gc.h>
#include <gmp.h>
#include <stdio.h>
#include "closure.h"

// Some basic communication with the outside world

void putStr(char* str);
void printInt(int x);
void printBigInt(mpz_t x);
void printBig(VAL x);

// dump memory usage (from libgc)
void epicMemInfo();
// Force garbage collection
void epicGC();

int readInt();
char* readStr();
int streq(char* x, char* y);
int strlt(char* x, char* y);

void* fileOpen(char* name, char* mode);
void fileClose(void* h);
char* freadStr(void* h);
void* freadStrAny(void* h);
void fputStr(void* h, char* str);

int isNull(void* ptr);

int epic_numArgs();
char* epic_getArg(int i);

// IORefs
int newRef();
void* readRef(int r);
void writeRef(int r, void* val);

// Locks
int newLock(int sem);
void doLock(int lock);
void doUnlock(int lock);
void doFork(void* proc);
void* doWithin(int limit, void* proc, void* doOnFail);

int do_utime() ;

int strToInt(char* str);
char* intToStr(int x);

double strToFloat(char* str);
char* floatToStr(double x);

double intToFloat(int x);
int floatToInt(double x);

void* intToBigInt(int x);
int bigIntToInt(void* big);

mpz_t* strToBigInt(char* str);
char* bigIntToStr(mpz_t x);

VAL strToBig(char* str);
char* bigToStr(VAL x);

// get a native representation of a value
void* getNative(void * fn);

// String operations

int strIndex(char* str, int i);
int strHead(char* str);
char* strTail(char* str);
char* strCons(int h, char* str);
char* strrev(char* str);
char* substr(char* str, int start, int len);
int strFind(char* str, char c);

char* append(char* x, char* y);

// Big integer arithmetic

mpz_t* addBigInt(mpz_t x, mpz_t y);
mpz_t* subBigInt(mpz_t x, mpz_t y);
mpz_t* mulBigInt(mpz_t x, mpz_t y);
mpz_t* divBigInt(mpz_t x, mpz_t y);
mpz_t* modBigInt(mpz_t x, mpz_t y);

int eqBigInt(mpz_t x, mpz_t y);
int ltBigInt(mpz_t x, mpz_t y);
int gtBigInt(mpz_t x, mpz_t y);
int leBigInt(mpz_t x, mpz_t y);
int geBigInt(mpz_t x, mpz_t y);

// VAL versions, which can also cope with INT and promote to BIGINT if necessary

VAL addBig(VAL x, VAL y);
VAL subBig(VAL x, VAL y);
VAL mulBig(VAL x, VAL y);
VAL divBig(VAL x, VAL y);
VAL modBig(VAL x, VAL y);

int eqBig(VAL x, VAL y);
int ltBig(VAL x, VAL y);
int gtBig(VAL x, VAL y);
int leBig(VAL x, VAL y);
int geBig(VAL x, VAL y);

#endif

