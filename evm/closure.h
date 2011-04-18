#ifndef _CLOSURE_H
#define _CLOSURE_H

#include "gc_header.h"

//#include <emalloc.h>
#include <gmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef void*(*ALLOCATOR)(size_t);
typedef void*(*REALLOCATOR)(void*,size_t);

typedef struct {
    char* block;
    char* block_loc;
    char* block_end;
    ALLOCATOR allocate;
    REALLOCATOR reallocate;
    void* grow;
} pool_t;

extern ALLOCATOR allocate;
extern REALLOCATOR reallocate;
extern pool_t** pools;
extern pool_t* pool;

#define EMALLOC(x) pool->allocate(x)
#define EREADY(x) 
#define EREALLOC(ptr,x) pool->reallocate(ptr,x)
#define EFREE(x)

// Add a new memory pool to the pool stack, using the fixed size pool
// allocator
#define NEWFIXEDPOOL(x) \
    pool=malloc(sizeof(pool_t)); pools++; *pools = pool;		\
    pool->allocate=pool_malloc;						\
    pool->reallocate=pool_realloc;					\
    pool->block = malloc(GETINT(x));					\
    pool->block_loc = pool->block;					\
    pool->block_end = pool->block+GETINT(x);				\
    pool->grow = NULL;
// Add a new memory pool to the pool stack, using the fixed size pool
// allocator
#define NEWGROWABLEPOOL(x) \
    pool=malloc(sizeof(pool_t)); pools++; *pools = pool;		\
    pool->allocate=pool_grow_malloc;					\
    pool->reallocate=pool_grow_realloc;					\
    pool->block = malloc(GETINT(x));					\
    pool->block_loc = pool->block;					\
    pool->block_end = pool->block+GETINT(x);
// Go back to the previous pool, copying the value x into it
#define CLEARPOOL(x)							\
    pools--;								\
    pool = *pools;							\
    x=copy(x, *(pools+1));						\
    freePool(*(pools+1));

typedef intptr_t eint;

//#define EMALLOC malloc
//#define EREALLOC realloc
//#define EFREE free

#define MKCON (con*)EMALLOC(sizeof(con))
#define MKFUN (fun*)EMALLOC(sizeof(fun))
#define MKTHUNK (thunk*)EMALLOC(sizeof(thunk))
#define MKCLOSURE (Closure*)EMALLOC(sizeof(Closure))
#define MKUNIT (void*)0

#define INTOP(op,x,y) MKINT((((eint)x)>>1) op (((eint)y)>>1))
#define FLOATOP(op,x,y) MKFLOAT(((GETFLOAT(x)) op (GETFLOAT(y))))
#define FLOATBOP(op,x,y) MKINT(((GETFLOAT(x)) op (GETFLOAT(y))))
#define ADD(x,y) (void*)(((eint)x)+(((eint)y)-1))
#define MULT(x,y) (MKINT((((eint)x)>>1) * (((eint)y)>>1)))
#define CHECKEVALUATED(x) if(ISFUN(x) || ISTHUNK(x) \
    || ISFV(x)) return 0;

#define MKARGS(x) (void**)EMALLOC(sizeof(VAL)*(x));
#define MOREARGS(args,x) (void**)EREALLOC(args,sizeof(VAL)*(x));

typedef enum { 
    FUN, 
    THUNK, 
    CON, 
    INT, 
    BIGINT,
    FLOAT,
    BIGFLOAT,
    STRING, 
    UNIT, 
    PTR,
    FREEVAR,
    IND
} ClosureType;

typedef struct {
    int ty;
    void* info;
} Closure;

void dumpClosure(Closure* c);
void assertConR(Closure* c);
void assertIntR(Closure* c);

#define assertCon(x)  
#define assertInt(x) 
//assertConR(x)
//assertIntR(x)

typedef Closure* VAL;

#define GETTY(x) (ISINT(x) ? INT : ((ClosureType)(((x)->ty) >> 24)))
#define QGETTY(x) ((ClosureType)(((x)->ty) >> 24))
#define SETTY(x,t) (x)->ty = (((eint)t) << 24)
#define SETTY_STK(x,t) (x)->ty = (1 + (((eint)t) << 24))
#define ON_STK(x) (((x)->ty & 1) == 1)

#define REF(x) x
#define DEREF(x) 

typedef void*(*func)(void**);

typedef struct {
    func fn;
    void** args;
    void** arg_end;
    int arity;
} fun;

typedef struct {
    void* fn;
    void** args;
    int numargs;
} thunk;

typedef struct {
    int tag;
    void** args;
} con;

typedef struct {
    VAL* roots;
    VAL* start_roots;
    VAL* from_space;
    VAL* to_space;
    VAL* nursery;

    VAL* stack;
    VAL* stack_top;
    VAL* stack_limit;

    int heap_size;
    int next;
    int next_nursery;
} VMState;

#define STACK_INIT 8192

#define STACK(x) (*(vm->stack_top-x))
#define PUSH(x) vm->stack_top=x; vm->stack_top++;
#define DROP(x) vm->stack_top-=x;
#define SLIDE1(x) vm->stack_top[-(x+1)]=*(vm->stack_top-1); vm->stack_top-=x;
#define SLIDE(x,keep) slide(vm,x,keep);

void slide(VMState* vm, int lose, int keep);

extern void* e_malloc(VMState* vm, size_t size);
extern void* e_realloc(VMState* vm, void* ptr, size_t size);

// Allocate from a non-GCed region
void* pool_malloc(size_t size);
void* pool_realloc(void* ptr, size_t size);
void* pool_grow_malloc(size_t size);
void* pool_grow_realloc(void* ptr, size_t size);
// Copy value from the given pool into the currently active region
VAL copy(VAL x, pool_t* pool);
// Promote a value on the stack to the heap
VAL promote(VAL x);

void freePool(pool_t* pool);

#define INIT_HEAP_SIZE 1000000

#define UPDATE(x,res) if (ISINT(res)) { x = MKINT(GETINT(res)); } else { \
                      SETTY(x, GETTY(res)); x->info=res->info; }
#define TAG(x) (((con*)((Closure*)x)->info)->tag & 65535)
#define ARITY(x) (((con*)((Closure*)x)->info)->tag >> 16)

#define ISCON(x) (GETTY(((Closure*)(x)))==CON)
#define ISINT(x) ((((eint)x)&1) == 1)
#define ISTHUNK(x) (GETTY(((Closure*)(x)))==THUNK)
#define ISFUN(x) (GETTY(((Closure*)(x)))==FUN)
#define ISFV(x) (GETTY(((Closure*)(x)))==FREEVAR)

#define NEEDSEVAL(x) ((x) && GETTY((Closure*)(x))<CON)
#define NONEEDSEVAL(x) ((x) && GETTY((Closure*)(x))>=CON)

#ifdef TRACEON
  #define TRACE if(1)
#else
  #define TRACE if(0)
#endif

// Evaluate x to head normal form
VAL DO_EVAL(VAL x, int update);

//#define EVAL(x) DO_EVAL(x)
#define EVAL(x) (!ISINT(x) && NEEDSEVAL(x) ? DO_EVAL(x, 1) : x)
#define EVALINT(x) (!ISINT(x) ? DO_EVAL(x, 1) : x)
#define EVAL_NOUP(x) (!ISINT(x) && NEEDSEVAL(x) ? DO_EVAL(x, 0) : x)
#define EVALINT_NOUP(x) (!ISINT(x) ? DO_EVAL(x, 0) : x)

//#define EVAL(x) ((x && (ISFUN(x) || ISTHUNK(x))) ? DO_EVAL(x, 1) : x)
//#define EVAL_NOUP(x) ((x && (ISFUN(x) || ISTHUNK(x))) ? DO_EVAL(x, 0) : x)

#define CONSTRUCTOR(t,a,b) ((a)==0 && t<255 ? zcon[t] : CONSTRUCTORn(t,a,b))

// Return a new constructor
inline VAL CONSTRUCTORn(int tag, int arity, void** block);
inline VAL CONSTRUCTOR1(int tag, VAL a1);
inline VAL CONSTRUCTOR2(int tag, VAL a1, VAL a2);
inline VAL CONSTRUCTOR3(int tag, VAL a1, VAL a2, VAL a3);
inline VAL CONSTRUCTOR4(int tag, VAL a1, VAL a2, VAL a3, VAL a4);
inline VAL CONSTRUCTOR5(int tag, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5);

// Return a new function node
VAL CLOSURE(func x, int arity, int args, void** block);

// Add arguments to an already existing thunk
VAL CLOSURE_ADDN(VAL x, int args, void** block);
VAL CLOSURE_ADD1(VAL xin, VAL a1);
VAL CLOSURE_ADD2(VAL xin, VAL a1, VAL a2);
VAL CLOSURE_ADD3(VAL xin, VAL a1, VAL a2, VAL a3);
VAL CLOSURE_ADD4(VAL xin, VAL a1, VAL a2, VAL a3, VAL a4);
VAL CLOSURE_ADD5(VAL xin, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5);

// Apply a closure to some arguments
VAL CLOSURE_APPLY(VAL x, int args, void** block);
VAL CLOSURE_APPLY1(VAL x, VAL a1);
VAL CLOSURE_APPLY2(VAL x, VAL a1, VAL a2);

VAL CLOSURE_APPLY3(VAL x, VAL a1, VAL a2, VAL a3);
VAL CLOSURE_APPLY4(VAL x, VAL a1, VAL a2, VAL a3, VAL a4);
VAL CLOSURE_APPLY5(VAL x, VAL a1, VAL a2, VAL a3, VAL a4, VAL a5);

// Project an argument from a constructor
#define PROJECT(x,arg) (((con*)((x)->info))->args[arg])
//void* DO_PROJECT(VAL x, int arg);

#define ASSIGNINT(t, x) t=MKINT(x);

//extern VAL one; 

// array of zero arity constructors. We don't need more than one of each...
extern VAL* zcon;

// Roots for the garbage collector
extern VMState* vm;

#define MKINT(x) ((void*)((x)<<1)+1)
#define GETINT(x) ((eint)(x)>>1)
#define GETPTR(x) ((void*)(((VAL)(x))->info))
#define GETSTR(x) ((char*)(((VAL)(x))->info))

#define GROWROOT 
// VAL* rootbase = vm->roots;
#define ADDROOT(v) 
// *(vm->roots)=v; vm->roots++;
#define DROPROOTS 
// vm->roots=rootbase;

#define INTTOEINT(x) ((eint)(x))
#define EINTTOINT(x) ((int)(x))

//void* MKINT(int x);
mpz_t* NEWBIGINTI(int val);
void* NEWBIGINTVALI(int val);
void* NEWBIGINT(char* bigint);
void* MKBIGINT(mpz_t* bigint);

void* MKSTR(const char* str);
void* MKPTR(void* ptr);
void* MKFLOAT(double x);

// Get values from a closure
//int GETINT(void* x);

mpz_t* GETBIGINT(void* x);
double GETFLOAT(void* x);
//void* GETPTR(void* x);

void* MKFREE(int x);

// Exit with fatal error
void ERROR(char* msg);

VAL evm_getArg(int i);
int evm_numArgs();

// Initialise everything
VMState* init_evm(int argc, char* argv[]);
void close_evm(VMState* vm);

// Run the main program
void epic_main(int argc, char* argv[]);

void* FASTMALLOC(int size);

#define CONSTRUCTOR1m(c,t,x)		\
    c=EMALLOC(sizeof(Closure)+sizeof(con)+sizeof(VAL)); \
    ((con*)((VAL)c+1))->tag = t + (1 << 16);			\
    ((con*)((VAL)c+1))->args = (void*)c+sizeof(Closure)+sizeof(con); \
    ((con*)((VAL)c+1))->args[0] = x; \
    SETTY(((VAL)c),CON);	 \
    ((VAL)c)->info = (void*)((con*)((VAL)c+1)); \
    EREADY(c);

#define CONSTRUCTOR2m(c,t,x,y)		\
    c=EMALLOC(sizeof(Closure)+sizeof(con)+2*sizeof(VAL)); \
    ((con*)((VAL)c+1))->tag = t + (2 << 16);			\
    ((con*)((VAL)c+1))->args = (void*)c+sizeof(Closure)+sizeof(con); \
    ((con*)((VAL)c+1))->args[0] = x; \
    ((con*)((VAL)c+1))->args[1] = y; \
    SETTY(((VAL)c),CON);	 \
    ((VAL)c)->info = (void*)((con*)((VAL)c+1)); \
    EREADY(c);

#define CONSTRUCTOR3m(c,t,x,y,z)		\
    c=EMALLOC(sizeof(Closure)+sizeof(con)+3*sizeof(VAL)); \
    ((con*)((VAL)c+1))->tag = t + (3 << 16);			\
    ((con*)((VAL)c+1))->args = (void*)c+sizeof(Closure)+sizeof(con); \
    ((con*)((VAL)c+1))->args[0] = x; \
    ((con*)((VAL)c+1))->args[1] = y; \
    ((con*)((VAL)c+1))->args[2] = z; \
    SETTY(((VAL)c),CON);	 \
    ((VAL)c)->info = (void*)((con*)((VAL)c+1)); \
    EREADY(c);

#define CONSTRUCTOR4m(c,t,x,y,z,w)			  \
    c=EMALLOC(sizeof(Closure)+sizeof(con)+4*sizeof(VAL)); \
    ((con*)((VAL)c+1))->tag = t + (4<< 16);			\
    ((con*)((VAL)c+1))->args = (void*)c+sizeof(Closure)+sizeof(con); \
    ((con*)((VAL)c+1))->args[0] = x; \
    ((con*)((VAL)c+1))->args[1] = y; \
    ((con*)((VAL)c+1))->args[2] = z; \
    ((con*)((VAL)c+1))->args[3] = w; \
    SETTY(((VAL)c),CON);	 \
    ((VAL)c)->info = (void*)((con*)((VAL)c+1)); \
    EREADY(c);

#define CONSTRUCTOR5m(c,t,x,y,z,w,v)			  \
    c=EMALLOC(sizeof(Closure)+sizeof(con)+5*sizeof(VAL)); \
    ((con*)((VAL)c+1))->tag = t + (5 << 16);			\
    ((con*)((VAL)c+1))->args = (void*)c+sizeof(Closure)+sizeof(con); \
    ((con*)((VAL)c+1))->args[0] = x; \
    ((con*)((VAL)c+1))->args[1] = y; \
    ((con*)((VAL)c+1))->args[2] = z; \
    ((con*)((VAL)c+1))->args[3] = w; \
    ((con*)((VAL)c+1))->args[4] = v; \
    SETTY(((VAL)c),CON);	 \
    ((VAL)c)->info = (void*)((con*)((VAL)c+1)); \
    EREADY(c);

//    s = EMALLOC(sizeof(Closure)+strlen(x)+sizeof(char)+1);	

#define INITSTRING(var, str) \
    static Closure* var = NULL; \
    if (var==NULL) { var = MKSTR(str); }

#define MKSTRm(c,s) c = s;

//    SETTY((VAL)c, STRING);				   
//    ((VAL)(c))->info = ((void*)c)+sizeof(Closure);	   
//	    strcpy(((VAL)(c))->info,x);

#define MKPTRm(c, x) \
    c = MKCLOSURE; \
    SETTY((VAL)c, PTR);				\
    ((VAL)(c))->info = x; 

#endif
