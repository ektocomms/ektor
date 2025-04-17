# ektor

[![Package Version](https://img.shields.io/hexpm/v/ektor)](https://hex.pm/packages/ektor)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/ektor/)

```sh
gleam add ektor@1
```

```gleam
import ektor.{type Inbox}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/io

type A {
  A(a: Int)
}

type B {
  B(b: Int, reply_to: #(Pid, Inbox(Msg)))
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
  let #(pid, inbox) = msg.reply_to
  ektor.send(
    pid,
    inbox,
    Msg("Received " <> int.to_string(msg.b) <> " at B inbox"),
  )
  ektor.continue(State(..state, b: msg.b))
}

pub fn main() {
  let inbox_a = ektor.new_inbox()
  let inbox_b = ektor.new_inbox()
  let handlers =
    ektor.new_handler_map()
    |> ektor.handling(inbox_a, handler_a)
    |> ektor.handling(inbox_b, handler_b)
  let ekt_pid = ektor.start(State(a: 0, b: 0), handlers)
  ektor.send(ekt_pid, inbox_a, A(1))
  let my_pid = process.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt_pid, inbox_b, B(2, #(my_pid, inbox)))
  let assert Ok(Msg(msg)) = ektor.receive(inbox, within: 200)
  io.println(msg)
}
```

Further documentation can be found at <https://hexdocs.pm/ektor>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
