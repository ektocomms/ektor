import gleam/erlang.{type Reference}
import gleam/erlang/process.{type Pid}

type DoNotLeak

pub type Message(msg)

pub type Handler(state)

pub type Inbox(msg) {
  Inbox(ref: Reference)
}

pub fn new_inbox() {
  Inbox(ref: erlang.make_reference())
}

@external(erlang, "erlang", "send")
fn raw_send(a: Pid, b: message) -> DoNotLeak

pub fn send(pid: Pid, inbox: Inbox(msg), message: msg) -> Nil {
  raw_send(pid, #(inbox.ref, message))
  Nil
}

@external(erlang, "ektor_ffi", "receive")
pub fn receive(inbox: Inbox(msg), within timeout: Int) -> Result(msg, Nil)

@external(erlang, "ektor_ffi", "new_handler")
pub fn new_handler() -> Handler(state)

@external(erlang, "ektor_ffi", "insert_handler")
pub fn insert_handler(
  handler: Handler(state),
  inbox: Inbox(msg),
  handler_fn: fn(msg, state) -> state,
) -> Handler(state)

pub fn handling(
  handler: Handler(state),
  inbox: Inbox(msg),
  handler_fn: fn(msg, state) -> state,
) -> Handler(state) {
  handler |> insert_handler(inbox, handler_fn)
}

pub fn start(state: state, handler: Handler(state)) {
  process.start(fn() { initialize_ektor(state, handler) }, linked: True)
}

fn initialize_ektor(state, handler: Handler(state)) {
  loop(state, handler)
}

pub type State(state)

@external(erlang, "ektor_ffi", "receive_forever_with_handler")
fn receive_forever_with_handler(state: state, handler: Handler(state)) -> state

fn loop(state, handler: Handler(state)) {
  let new_state = receive_forever_with_handler(state, handler)
  loop(new_state, handler)
}
