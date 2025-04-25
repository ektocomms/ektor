import ektor.{type Target}
import gleeunit/should

type AResp {
  AResp
}

type A {
  A(a: Int, reply_to: Target(AResp))
}

type State {
  State(a: Int, b: Int)
}

fn handler(msg: A, state: State) {
  ektor.send(msg.reply_to, AResp)
  ektor.continue(State(..state, a: msg.a))
}

pub fn call_test() {
  let target = ektor.start_single(State(a: 0, b: 0), handler)
  let reply = ektor.call(target, A(1, _), within: 100)
  reply |> should.equal(AResp)
}
