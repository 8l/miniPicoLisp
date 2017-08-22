/* 27oct14abu
 * (c) Software Lab. Alexander Burger
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <setjmp.h>
#include <stdint.h>

#ifndef CELLS
#define CELLS (1024*1024/sizeof(cell))
#endif

#define WORD ((int)sizeof(long))
#define BITS (8*WORD)

typedef unsigned long word;
typedef unsigned char byte;
typedef unsigned char *ptr;

#undef bool
typedef enum {NO,YES} bool;

typedef struct cell {            // PicoLisp primary data type
   struct cell *car;
   struct cell *cdr;
} cell, *any;

typedef any (*fun)(any);

#include "sym.d"

typedef struct heap {
   cell cells[CELLS];
   struct heap *next;
} heap;

typedef struct bindFrame {
   struct bindFrame *link;
   int i, cnt;
   struct {any sym; any val;} bnd[1];
} bindFrame;

typedef struct inFrame {
   struct inFrame *link;
   void (*get)(void);
   FILE *fp;
   int next;
} inFrame;

typedef struct outFrame {
   struct outFrame *link;
   void (*put)(int);
   FILE *fp;
} outFrame;

typedef struct parseFrame {
   int i;
   word w;
   any sym, nm;
} parseFrame;

typedef struct stkEnv {
   cell *stack, *arg;
   bindFrame *bind;
   int next;
   any key, cls, *make, *yoke;
   inFrame *inFrames;
   outFrame *outFrames;
   parseFrame *parser;
   void (*get)(void);
   void (*put)(int);
   bool brk;
} stkEnv;

typedef struct catchFrame {
   struct catchFrame *link;
   any tag, fin;
   stkEnv env;
   jmp_buf rst;
} catchFrame;

/*** Macros ***/
#define Free(p)         ((p)->car=Avail, Avail=(p))

/* Number access */
#define num(x)          ((long)(x))
#define txt(n)          ((any)(num(n)<<1|1))
#define box(n)          ((any)(num(n)<<2|2))
#define unBox(n)        (num(n)>>2)
#define Zero            ((any)2)
#define One             ((any)6)

/* Symbol access */
#define symPtr(x)       ((any)&(x)->cdr)
#define val(x)          ((x)->car)
#define tail(x)         (((x)-1)->cdr)

/* Cell access */
#define car(x)          ((x)->car)
#define cdr(x)          ((x)->cdr)
#define caar(x)         (car(car(x)))
#define cadr(x)         (car(cdr(x)))
#define cdar(x)         (cdr(car(x)))
#define cddr(x)         (cdr(cdr(x)))
#define caaar(x)        (car(car(car(x))))
#define caadr(x)        (car(car(cdr(x))))
#define cadar(x)        (car(cdr(car(x))))
#define caddr(x)        (car(cdr(cdr(x))))
#define cdaar(x)        (cdr(car(car(x))))
#define cdadr(x)        (cdr(car(cdr(x))))
#define cddar(x)        (cdr(cdr(car(x))))
#define cdddr(x)        (cdr(cdr(cdr(x))))
#define cadddr(x)       (car(cdr(cdr(cdr(x)))))
#define cddddr(x)       (cdr(cdr(cdr(cdr(x)))))

#define data(c)         ((c).car)
#define Save(c)         ((c).cdr=Env.stack, Env.stack=&(c))
#define drop(c)         (Env.stack=(c).cdr)
#define Push(c,x)       (data(c)=(x), Save(c))
#define Pop(c)          (drop(c), data(c))

#define Bind(s,f)       ((f).i=0, (f).cnt=1, (f).bnd[0].sym=(s), (f).bnd[0].val=val(s), (f).link=Env.bind, Env.bind=&(f))
#define Unbind(f)       (val((f).bnd[0].sym)=(f).bnd[0].val, Env.bind=(f).link)

/* Predicates */
#define isNil(x)        ((x)==Nil)
#define isTxt(x)        (num(x)&1)
#define isNum(x)        (num(x)&2)
#define isSym(x)        (num(x)&WORD)
#define isSymb(x)       ((num(x)&(WORD+2))==WORD)
#define isCell(x)       (!(num(x)&(2*WORD-1)))

/* Evaluation */
#define EVAL(x)         (isNum(x)? x : isSym(x)? val(x) : evList(x))
#define evSubr(f,x)     (*(fun)(num(f) & ~2))(x)

/* Error checking */
#define NeedNum(ex,x)   if (!isNum(x)) numError(ex,x)
#define NeedSym(ex,x)   if (!isSym(x)) symError(ex,x)
#define NeedSymb(ex,x)  if (!isSymb(x)) symError(ex,x)
#define NeedPair(ex,x)  if (!isCell(x)) pairError(ex,x)
#define NeedAtom(ex,x)  if (isCell(x)) atomError(ex,x)
#define NeedLst(ex,x)   if (!isCell(x) && !isNil(x)) lstError(ex,x)
#define NeedVar(ex,x)   if (isNum(x)) varError(ex,x)
#define CheckVar(ex,x)  if ((x)>(any)Rom && (x)<=Quote) protError(ex,x)

/* Globals */
extern int Chr, Trace;
extern char **AV, *AV0, *Home;
extern heap *Heaps;
extern cell *Avail;
extern stkEnv Env;
extern catchFrame *CatchPtr;
extern FILE *InFile, *OutFile;
extern any TheKey, TheCls, Thrown;
extern any Intern[2], Transient[2];
extern any ApplyArgs, ApplyBody;
extern any const Rom[];
extern any Ram[];

/* Prototypes */
void *alloc(void*,size_t);
any apply(any,any,bool,int,cell*);
void argError(any,any) __attribute__ ((noreturn));
void atomError(any,any) __attribute__ ((noreturn));
void begString(void);
void brkLoad(any);
int bufNum(char[BITS/2],long);
int bufSize(any);
void bufString(any,char*);
void bye(int) __attribute__ ((noreturn));
void pairError(any,any) __attribute__ ((noreturn));
any circ(any);
long compare(any,any);
any cons(any,any);
any consName(word,any);
any consSym(any,word);
void newline(void);
any endString(void);
bool equal(any,any);
void err(any,any,char*,...) __attribute__ ((noreturn));
any evExpr(any,any);
any evList(any);
long evNum(any,any);
any evSym(any);
void execError(char*) __attribute__ ((noreturn));
int firstByte(any);
any get(any,any);
int getByte(int*,word*,any*);
int getByte1(int*,word*,any*);
void getStdin(void);
void giveup(char*) __attribute__ ((noreturn));
void heapAlloc(void);
any intern(any,any[2]);
bool isBlank(any);
any isIntern(any,any[2]);
void lstError(any,any) __attribute__ ((noreturn));
any load(any,int,any);
any loadAll(any);
any method(any);
any mkChar(int);
any mkChar2(int,int);
any mkSym(byte*);
any mkStr(char*);
any mkTxt(int);
any name(any);
int numBytes(any);
void numError(any,any) __attribute__ ((noreturn));
any numToSym(any,int,int,int);
void outName(any);
void outNum(long);
void outString(char*);
void pack(any,int*,word*,any*,cell*);
int pathSize(any);
void pathString(any,char*);
void popInFiles(void);
void popOutFiles(void);
any popSym(int,word,any,cell*);
void prin(any);
void print(any);
void protError(any,any) __attribute__ ((noreturn));
void pushInFiles(inFrame*);
void pushOutFiles(outFrame*);
void put(any,any,any);
void putByte(int,int*,word*,any*,cell*);
void putByte0(int*,word*,any*);
void putByte1(int,int*,word*,any*);
void putStdout(int);
void rdOpen(any,any,inFrame*);
any read1(int);
int secondByte(any);
void space(void);
int symBytes(any);
void symError(any,any) __attribute__ ((noreturn));
any symToNum(any,int,int,int);
void undefined(any,any);
void unintern(any,any[2]);
void unwind (catchFrame*);
void varError(any,any) __attribute__ ((noreturn));
void wrOpen(any,any,outFrame*);
long xNum(any,any);
any xSym(any);

/* List element access */
static inline any nCdr(int n, any x) {
   while (--n >= 0)
      x = cdr(x);
   return x;
}

static inline any nth(int n, any x) {
   if (--n < 0)
      return Nil;
   return nCdr(n,x);
}

static inline any getn(any x, any y) {
   if (isNum(x)) {
      long n = unBox(x);

      if (n < 0) {
         while (++n)
            y = cdr(y);
         return cdr(y);
      }
      if (n == 0)
         return Nil;
      while (--n)
         y = cdr(y);
      return car(y);
   }
   do
      if (isCell(car(y)) && x == caar(y))
         return cdar(y);
   while (isCell(y = cdr(y)));
   return Nil;
}

/* List length calculation */
static inline int length(any x) {
   int n;

   for (n = 0; isCell(x); x = cdr(x))
      ++n;
   return n;
}

/* Membership */
static inline any member(any x, any y) {
   any z = y;

   while (isCell(y)) {
      if (equal(x, car(y)))
         return y;
      if (z == (y = cdr(y)))
         return NULL;
   }
   return isNil(y) || !equal(x,y)? NULL : y;
}

static inline any memq(any x, any y) {
   any z = y;

   while (isCell(y)) {
      if (x == car(y))
         return y;
      if (z == (y = cdr(y)))
         return NULL;
   }
   return isNil(y) || x != y? NULL : y;
}

static inline int indx(any x, any y) {
   int n = 1;
   any z = y;

   while (isCell(y)) {
      if (equal(x, car(y)))
         return n;
      ++n;
      if (z == (y = cdr(y)))
         return 0;
   }
   return 0;
}

/* List interpreter */
static inline any prog(any x) {
   any y;

   do
      y = EVAL(car(x));
   while (isCell(x = cdr(x)));
   return y;
}

static inline any run(any x) {
   any y;
   cell at;

   Push(at,val(At));
   do
      y = EVAL(car(x));
   while (isCell(x = cdr(x)));
   val(At) = Pop(at);
   return y;
}
