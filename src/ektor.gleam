import gleam/erlang.{type Reference}
import gleam/erlang/process.{type Pid}

type DoNotLeak

pub type HandlerMap(state)

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

@external(erlang, "ektor_ffi", "new_handler_map")
pub fn new_handler_map() -> HandlerMap(state)

@external(erlang, "ektor_ffi", "insert_handler")
pub fn insert_handler(
  handler: HandlerMap(state),
  inbox: Inbox(msg),
  handler_fn: fn(msg, state) -> state,
) -> HandlerMap(state)

pub fn handling(
  handler: HandlerMap(state),
  inbox: Inbox(msg),
  handler_fn: fn(msg, state) -> state,
) -> HandlerMap(state) {
  handler |> insert_handler(inbox, handler_fn)
}

pub fn start(state: state, handler: HandlerMap(state)) {
  process.start(fn() { initialize_ektor(state, handler) }, linked: True)
}

fn initialize_ektor(state, handler: HandlerMap(state)) {
  loop(state, handler)
}

@external(erlang, "ektor_ffi", "receive_forever_with_handlers")
fn receive_forever_with_handlers(
  state: state,
  handler: HandlerMap(state),
) -> state

fn loop(state, handlers: HandlerMap(state)) {
  let new_state = receive_forever_with_handlers(state, handlers)
  loop(new_state, handlers)
}
