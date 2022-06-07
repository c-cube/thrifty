
const i32 n = 256;

struct Foo {
  1: i32 x = 1;
  2: string y;
  3: bool z;
}

struct Bar {
  1: list<list<Foo>> foos = [];
}
