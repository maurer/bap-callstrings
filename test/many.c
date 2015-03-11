#define D(x) x x
#define DD(x) D(D(x))
#define DDD(x) DD(DD(x))
#define DDDD(x) DDD(DDD(x))
#define mf(f, g) \
void f(){\
  DDDD(g();)\
}

mf(a, b)
mf(b, c)
mf(c, d)
mf(d, e)

void f() {
  g();
}

void g() {
}

void e() {
  a();
  b();
  c();
  d();
  f();
}

int main(){
  a();
  return 0;
}
