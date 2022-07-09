
struct intlist {
  1: required list<i32> ints;
}

exception Div_by_zero {}


service S {
  i32 add(1: required i32 a, 2: required i32 b);

  i32 div(1: required i32 a, 2: required i32 b) throws (1: Div_by_zero e);

  i32 add_all(1: required intlist l);

  oneway void ping();

  i32 get_pings();
}
