import ektor.{type Inbox, type Pid}
import gleam/int
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

type A {
  A(a: Int)
}

type B {
  B(b: Int, reply_to: #(Pid, Inbox(Msg)))
}

type Msg {
  Msg(msg: String)
}

pub type State {
  State(a: Int, b: Int)
}

fn handler_a(msg: A, state: State) {
  ektor.continue(State(..state, a: msg.a))
}

fn handler_b(msg: B, state: State) {
  let #(pid, inbox) = msg.reply_to
  ektor.send(
    pid,
    inbox,
    Msg("Received " <> int.to_string(msg.b) <> " at B inbox"),
  )
  ektor.continue(State(..state, b: msg.b))
}

pub fn ektor_basic_usage_test() {
  let inbox_a = ektor.new_inbox()
  let inbox_b = ektor.new_inbox()
  let handlers =
    ektor.new_handler_map()
    |> ektor.handling(inbox_a, handler_a)
    |> ektor.handling(inbox_b, handler_b)
  let ekt_pid = ektor.start(State(a: 0, b: 0), handlers)
  ektor.send(ekt_pid, inbox_a, A(1))
  let my_pid = ektor.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt_pid, inbox_b, B(2, #(my_pid, inbox)))
  let msg = ektor.receive(inbox, within: 200)
  msg
  |> should.equal(Ok(Msg("Received 2 at B inbox")))
}
