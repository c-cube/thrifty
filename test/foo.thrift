
const i32 n = 256;

struct Foo {
  1: i32 x = 1;
  2: string y;
  3: bool z;
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

  oneway void send_bar(1: optional Bar bar) throws (2: Foo foo);
}
