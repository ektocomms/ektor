import ektor.{type Pid, type Topic}
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
  B(b: Int, reply_to: #(Pid, Topic(Msg)))
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
  let #(pid, topic) = msg.reply_to
  ektor.send(
    pid,
    topic,
    Msg("Received " <> int.to_string(msg.b) <> " at handler_b"),
  )
  ektor.continue(State(..state, b: msg.b))
}

pub fn ektor_basic_usage_test() {
  let topic_a = ektor.new_topic()
  let topic_b = ektor.new_topic()
  let topics_router =
    ektor.new_topics_router()
    |> ektor.handling(topic_a, handler_a)
    |> ektor.handling(topic_b, handler_b)
  let ekt_pid = ektor.start(State(a: 0, b: 0), topics_router)
  ektor.send(ekt_pid, topic_a, A(1))
  let my_pid = ektor.self()
  let topic = ektor.new_topic()
  ektor.send(ekt_pid, topic_b, B(2, #(my_pid, topic)))
  let msg = ektor.receive(topic, within: 200)
  msg
  |> should.equal(Ok(Msg("Received 2 at handler_b")))
}
