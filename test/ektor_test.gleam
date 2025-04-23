import ektor.{type Target}
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
  B(b: Int, reply_to: Target(Msg))
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
  ektor.send(
    msg.reply_to,
    Msg("Received " <> int.to_string(msg.b) <> " at handler_b"),
  )
  ektor.continue(State(..state, b: msg.b))
}

pub fn ektor_basic_usage_test() {
  let topic_a = ektor.new_topic()
  let topic_b = ektor.new_topic()
  let topic_router =
    ektor.new_topic_router()
    |> ektor.handling(topic_a, handler_a)
    |> ektor.handling(topic_b, handler_b)
  let ekt_pid = ektor.start(State(a: 0, b: 0), topic_router)
  ektor.send(ektor.Target(ekt_pid, topic_a), A(1))
  let my_target = ektor.new_target()
  ektor.send(ektor.Target(ekt_pid, topic_b), B(2, my_target))
  let msg = ektor.receive(my_target.topic, within: 200)
  msg
  |> should.equal(Ok(Msg("Received 2 at handler_b")))
}
