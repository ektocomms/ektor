import ektor

type A {
  A(a: Int)
}

type State {
  State(a: Int, b: Int)
}

fn handler(msg: A, state: State) {
  ektor.continue(State(..state, a: msg.a))
}

pub fn start_single_test() {
  let #(pid, topic) = ektor.start_single(State(a: 0, b: 0), handler)
  ektor.send(pid, topic, A(1))
}
