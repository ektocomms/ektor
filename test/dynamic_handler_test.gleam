import ektor.{type Topic}
import gleam/int
import gleeunit/should

pub type State {
  State(a: Int, b: Int)
}

type A {
  A(a: Int, reply_to: ektor.Target(Msg))
}

type B {
  B(b: Int, reply_to: ektor.Target(Msg))
}

type Msg {
  Msg(msg: String)
  TopicMsg(topic: Topic(B))
}

fn handler_b(msg: B, state: State) {
  ektor.send(
    msg.reply_to,
    Msg("Received " <> int.to_string(msg.b) <> " at handler_b"),
  )
  ektor.continue(State(..state, b: msg.b))
}

fn handler_a(msg: A, state: State) {
  let topic_b = ektor.new_topic()
  ektor.send(msg.reply_to, TopicMsg(topic: topic_b))

  let new_handler =
    ektor.new_topic_router()
    |> ektor.handling(topic_b, handler_b)
  ektor.continue(State(..state, a: msg.a))
  |> ektor.with_topic_router(new_handler)
}

pub fn ektor_continue_with_new_handler_test() {
  let topic_a = ektor.new_topic()
  let topic_router =
    ektor.new_topic_router()
    |> ektor.handling(topic_a, handler_a)
  let ekt_pid = ektor.start(State(a: 0, b: 0), topic_router)
  let my_target = ektor.new_target()
  ektor.send(ektor.Target(ekt_pid, topic_a), A(1, my_target))
  let msg = ektor.receive(my_target.topic, within: 200)
  msg
  |> should.be_ok
  let assert Ok(TopicMsg(topic_b)) = msg
  ektor.send(ektor.Target(ekt_pid, topic_b), B(2, reply_to: my_target))
  let msg = ektor.receive(my_target.topic, within: 200)
  msg
  |> should.equal(Ok(Msg("Received 2 at handler_b")))
}
