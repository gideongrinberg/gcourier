import gcourier
import gleeunit/should

pub fn main() -> Nil {
  gcourier.dev_server()
  Nil
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}
