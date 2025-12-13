// comment 1

#define IDENT1 0
#define   IDENT2   1

/* comment 2 IDENT1 IDENT2 */

void ident3() {}

void ident4() {
  ;
}

void ident5()
{
  ;
}

void ident6(int a, float b)
{
  int ident3();
  IDENT1
  int ident7();
}

  void ident8() {}

#define IDENT9(x) 1
#define IDENT9fake(x) 1
#define IDENT1fake 1

static void ident10() {
  ;
}
