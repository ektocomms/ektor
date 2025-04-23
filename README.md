# ektor

[![Package Version](https://img.shields.io/hexpm/v/ektor)](https://hex.pm/packages/ektor)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/ektor/)

Typed Messages between actors handling multiple types with less code.


```gleam
import ektor.{type Pid, type Topic}
import gleam/int
import gleam/io

type A {
  A(a: Int)
}

type B {
  B(b: Int, reply_to: #(Pid, Topic(Msg)))
}

type Msg {
  Msg(msg: String)
}

type State {
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

pub fn main() {
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
  let assert Ok(Msg(msg)) = ektor.receive(topic, within: 200)
  io.println(msg)
}
```

Handling messages of different types in a single actor is useful to achieve separation of concerns. An actor may receive messages from a Supervisor and from other actors, and the expected interaction with each of them can be modeled using a different message type for each other. A supervisor would then provide a reference -let's say a subject owned by actor A- to an actor B, and the type of that subject would represent the kind of messages actor B is expected to send to actor A.

The actor implementation on `gleam_otp` library let us handle messages of different types in a single actor by using a `Selector`. A selector is a mapping of `Subject` instances to functions that transform messages from each Subject type to messages of the unique Selector type. A single `message_handler` function will then instruct the actor on how to handle messages from this unified type.

This approach has some drawbacks:
- It requires you to declare a selector and to add a mapping function for each different message type.
- It requires you to declare a constructor for each message type on the unified type.
- As subjects have to be created on the actor's process (when the new `Pid` is available), you can only get to know the individual subjects through message passing. So you need to declare and handle message constructors for this purpose.

Soon or later you would find your self spending more time and line of codes to achieve this plumbing than to your actual logic.

With ektor we wanted to overcome this situation by enabling multi-type message handling with minimal effort.

Instead of `Subject(type)`, we use `Topic(type)` which is not tied to a process `Pid` an so can be instantiated at the supervisor before spawning the child process.
Instead of a function to handle messages of a single type, in companion with a selector which maps multi-type messages to that single message type, we use a map of topics to handling functions called `TopicRouter(state)`. Each of them will instruct the actor (ektor) on how to process a message on that topic to produce the next state.

Basic features have been implemented so far following the original implementation at `gleam_erlang/process` and `gleam_otp/actor`:
  - Sending and receiving typed messages to and from topics.
  - Adding handlers to a `TopicRouter`, even dynamically.
  - Adding an anything/default handler with `handling_anything()`
  - Running an initial function  on spawned process with `start_spec()`

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
