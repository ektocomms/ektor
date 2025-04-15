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
  let inbox_a = ektor.new_inbox()
  let inbox_b = ektor.new_inbox()
  let inbox_c = ektor.new_inbox()
  let handler =
    ektor.new_handler()
    |> ektor.handling(inbox_a, handler_a)
    |> ektor.handling(inbox_b, handler_b)
    |> ektor.handling(inbox_c, handler_c)
  let ekt_pid = ektor.start(State(a: 0, b: 0, c: 0), handler)
  ektor.send(ekt_pid, inbox_a, A(1))
  ektor.send(ekt_pid, inbox_b, B(2))
  let my_pid = process.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt_pid, inbox_c, C(3, #(my_pid, inbox)))
  let msg = ektor.receive(inbox, within: 200)
  echo msg
}
