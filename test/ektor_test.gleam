import ektor.{type Inbox}
import gleam/erlang/process.{type Pid}
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
  B(b: Int)
}

type C {
  C(c: Int, reply_to: #(Pid, Inbox(Resp)))
}

type Resp {
  Resp(msg: String)
  RespWithInbox(inbox: Inbox(C))
}

pub type State {
  State(a: Int, b: Int, c: Int)
}

fn handler_a(msg: A, state: State) {
  ektor.continue(State(..state, a: msg.a))
}

fn handler_b(msg: B, state: State) {
  ektor.continue(State(..state, b: msg.b))
}

fn handler_c(msg: C, state: State) {
  let #(pid, inbox) = msg.reply_to
  ektor.send(
    pid,
    inbox,
    Resp("Received " <> int.to_string(msg.c) <> " at C inbox"),
  )
  ektor.continue(State(..state, c: msg.c))
}

pub fn ektor_basic_usage_test() {
  let inbox_a = ektor.new_inbox()
  let inbox_b = ektor.new_inbox()
  let inbox_c = ektor.new_inbox()
  let handlers =
    ektor.new_handler_map()
    |> ektor.handling(inbox_a, handler_a)
    |> ektor.handling(inbox_b, handler_b)
    |> ektor.handling(inbox_c, handler_c)
  let ekt_pid = ektor.start(State(a: 0, b: 0, c: 0), handlers)
  ektor.send(ekt_pid, inbox_a, A(1))
  ektor.send(ekt_pid, inbox_b, B(2))
  let my_pid = process.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt_pid, inbox_c, C(3, #(my_pid, inbox)))
  let msg = ektor.receive(inbox, within: 200)
  msg
  |> should.equal(Ok(Resp("Received 3 at C inbox")))
}

type BPrime {
  BPrime(b: Int, reply_to: #(Pid, Inbox(Resp)))
}

fn handler_b_prime(msg: BPrime, state: State) {
  let #(pid, inbox) = msg.reply_to
  let inbox_c = ektor.new_inbox()
  ektor.send(pid, inbox, RespWithInbox(inbox: inbox_c))

  let new_handler =
    ektor.new_handler_map()
    |> ektor.handling(inbox_c, handler_c)
  ektor.continue(State(..state, b: msg.b))
  |> ektor.with_handler_map(new_handler)
}

pub fn ektor_continue_with_new_handler_test() {
  let inbox_b_prime = ektor.new_inbox()
  let handlers =
    ektor.new_handler_map()
    |> ektor.handling(inbox_b_prime, handler_b_prime)
  let ekt_pid = ektor.start(State(a: 0, b: 0, c: 0), handlers)
  let my_pid = process.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt_pid, inbox_b_prime, BPrime(3, #(my_pid, inbox)))
  let msg = ektor.receive(inbox, within: 200)
  msg
  |> should.be_ok
  Nil
  // let assert Ok(RespWithInbox(inbox_c)) = msg
  // ektor.send(ekt_pid, inbox_c, C(c: 3, reply_to: #(my_pid, inbox)))
  // let msg = ektor.receive(inbox, within: 200)
  // msg
  // |> should.equal(Ok(Resp("Received 3 at C inbox")))
}
