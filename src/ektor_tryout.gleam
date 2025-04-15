import ektor.{type Inbox}
import gleam/erlang/process.{type Pid}
import gleam/int

type A {
  A(a: Int)
}

type B {
  B(b: Int)
}

type C {
  C(c: Int, reply_to: #(Pid, Inbox(Resp)))
}

pub type Resp {
  Resp(msg: String)
}

pub type State {
  State(a: Int, b: Int, c: Int)
}

fn handler_a(msg: A, state: State) {
  State(..state, a: msg.a)
}

fn handler_b(msg: B, state: State) {
  State(..state, b: msg.b)
}

fn handler_c(msg: C, state: State) {
  let #(pid, inbox) = msg.reply_to
  ektor.send(pid, inbox, Resp("Recibido " <> int.to_string(msg.c)))
  State(..state, c: msg.c)
}

pub fn main() {
  let handler = ektor.new_handler()
  let #(inbox_a, handler) = ektor.handling(handler, handler_a)
  let #(inbox_b, handler) = ektor.handling(handler, handler_b)
  let #(inbox_c, handler) = ektor.handling(handler, handler_c)
  let ekt = ektor.start(State(a: 0, b: 0, c: 0), handler)
  ektor.send(ekt, inbox_a, A(1))
  ektor.send(ekt, inbox_b, B(2))
  let pid = process.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt, inbox_c, C(3, #(pid, inbox)))
  let msg = ektor.receive(inbox, within: 200)
  echo msg
}
