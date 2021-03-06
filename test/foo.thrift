
const i32 n = 256;

struct Foo {
  2: i32 x = 1;
  1: string y;
  5: required bool z;
}

struct Loc {
  1: required double long;
  2: required double lat;
}

exception Ohno {}

exception Ohno2 { 1: bool really_bad }

exception Ohno3 {
  1: string why;
  2: Loc where;
}

// Bar defined below
union FooOrBarOrBool {
  1: Foo foo;
  2: Bar bar;
  3: bool b;
}

enum FooK {
  K1 = 1,
  K2 = 4,
  K3
}

struct Bar {
  1: list<list<Foo>> foos = [];
  5: optional FooK kind;
  3: required map<FooK, list<Foo>> fooM;
}

typedef list<Bar> Bar2;

service GiveKind {
  FooK get_kind(1: Foo foo);

  void send_bar(1: Bar bar) throws (2: Ohno o; 3: Ohno2 o2);

  oneway void send_whatev(1: required i32 how_many, 3: FooK k);
}

service Calculator {
  i32 add(1: required i32 x, 2: required i32 y);

  i32 mult(1: required i32 x, 2: required i32 y);
}
