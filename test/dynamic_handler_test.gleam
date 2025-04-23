import ektor.{type Pid, type Topic}
import gleam/int
import gleeunit/should

pub type State {
  State(a: Int, b: Int)
}

type A {
  A(a: Int, reply_to: #(Pid, Topic(Msg)))
}

type B {
  B(b: Int, reply_to: #(Pid, Topic(Msg)))
}

type Msg {
  Msg(msg: String)
  TopicMsg(topic: Topic(B))
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

fn handler_a(msg: A, state: State) {
  let #(pid, topic) = msg.reply_to
  let topic_b = ektor.new_topic()
  ektor.send(pid, topic, TopicMsg(topic: topic_b))

  let new_handler =
    ektor.new_topics_router()
    |> ektor.handling(topic_b, handler_b)
  ektor.continue(State(..state, a: msg.a))
  |> ektor.with_handler_map(new_handler)
}

pub fn ektor_continue_with_new_handler_test() {
  let topic_a = ektor.new_topic()
  let topics_router =
    ektor.new_topics_router()
    |> ektor.handling(topic_a, handler_a)
  let ekt_pid = ektor.start(State(a: 0, b: 0), topics_router)
  let my_pid = ektor.self()
  let topic = ektor.new_topic()
  ektor.send(ekt_pid, topic_a, A(1, #(my_pid, topic)))
  let msg = ektor.receive(topic, within: 200)
  msg
  |> should.be_ok
  let assert Ok(TopicMsg(topic_b)) = msg
  ektor.send(ekt_pid, topic_b, B(2, reply_to: #(my_pid, topic)))
  let msg = ektor.receive(topic, within: 200)
  msg
  |> should.equal(Ok(Msg("Received 2 at handler_b")))
}
