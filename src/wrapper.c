#define CAT(a,b) XCAT(a,b)
#define XCAT(a,b) a ## b
#define STR(a) XSTR(a)
#define XSTR(a) #a
 
#include <HsFFI.h>
#include "Library_stub.h"
 
extern void CAT(__stginit_, MODULE)(void);
 
static void library_init(void) __attribute__((constructor));
static void library_init(void)
{
  static char *argv[] = { 0 }, **argv_ = argv;
  static int argc = 0;

  hs_init(&argc, &argv_);

  hs_add_root(CAT(__stginit_, MODULE));
}
 
static void library_exit(void) __attribute__((destructor));
static void library_exit(void)
{
  destruct();
  hs_exit();
}
