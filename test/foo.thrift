
const i32 n = 256;

struct Foo {
  1: i32 x = 1;
  2: string y;
  3: bool z;
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
  2: optional FooK kind;
}

typedef list<Bar> Bar2;

service GiveKind {
  FooK get_kind(1: Foo foo);

  void send_bar(1: optional Bar bar) throws (2: Ohno o; 3: Ohno2 o2);

  oneway void send_whatev(3: FooK k);
}
