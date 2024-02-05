
- debug compact protocol (using rust)

- change entirely how protocols work (replace `module type` with
    a record taking a `'a` argument to reduce the need for closures)

- framed transport as a `transport -> transport` combinator
- deflate transport?
- buffered transport?
