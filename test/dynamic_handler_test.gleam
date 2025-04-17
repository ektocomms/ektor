import ektor.{type Inbox}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleeunit/should

pub type State {
  State(a: Int, b: Int)
}

type A {
  A(a: Int, reply_to: #(Pid, Inbox(Msg)))
}

type B {
  B(b: Int, reply_to: #(Pid, Inbox(Msg)))
}

type Msg {
  Msg(msg: String)
  InboxMsg(inbox: Inbox(B))
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

fn handler_a(msg: A, state: State) {
  let #(pid, inbox) = msg.reply_to
  let inbox_b = ektor.new_inbox()
  ektor.send(pid, inbox, InboxMsg(inbox: inbox_b))

  let new_handler =
    ektor.new_handler_map()
    |> ektor.handling(inbox_b, handler_b)
  ektor.continue(State(..state, a: msg.a))
  |> ektor.with_handler_map(new_handler)
}

pub fn ektor_continue_with_new_handler_test() {
  let inbox_a = ektor.new_inbox()
  let handlers =
    ektor.new_handler_map()
    |> ektor.handling(inbox_a, handler_a)
  let ekt_pid = ektor.start(State(a: 0, b: 0), handlers)
  let my_pid = process.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt_pid, inbox_a, A(1, #(my_pid, inbox)))
  let msg = ektor.receive(inbox, within: 200)
  msg
  |> should.be_ok
  let assert Ok(InboxMsg(inbox_b)) = msg
  ektor.send(ekt_pid, inbox_b, B(2, reply_to: #(my_pid, inbox)))
  let msg = ektor.receive(inbox, within: 200)
  msg
  |> should.equal(Ok(Msg("Received 2 at B inbox")))
}
