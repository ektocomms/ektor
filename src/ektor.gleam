import gleam/dynamic.{type Dynamic}
import gleam/erlang.{type Reference}
import gleam/erlang/charlist.{type Charlist}
import gleam/option.{type Option}
import gleam/string

pub type Pid

/// Get the `Pid` for the current process.
@external(erlang, "erlang", "self")
pub fn self() -> Pid

pub type ExitReason {
  Normal
  Killed
  Abnormal(reason: String)
}

type DoNotLeak

pub type HandlerMap(state)

pub type Inbox(msg) {
  Inbox(ref: Reference)
}

pub type InitResult(state) {
  Ready(state: state, handler: HandlerMap(state))
  Failed(String)
}

pub type Spec(state) {
  Spec(init: fn() -> InitResult(state))
}

pub type Next(state) {
  Continue(state: state, handler_map: Option(HandlerMap(state)))
  Stop(ExitReason)
}

pub fn continue(state: state) -> Next(state) {
  Continue(state, option.None)
}

pub fn with_handler_map(
  next: Next(state),
  handler_map: HandlerMap(state),
) -> Next(state) {
  case next {
    Continue(state, _) -> Continue(state, option.Some(handler_map))
    _ -> next
  }
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
  handlers: HandlerMap(state),
  inbox: Inbox(msg),
  handler: fn(msg, state) -> Next(state),
) -> HandlerMap(state)

@external(erlang, "ektor_ffi", "insert_anything_handler")
pub fn insert_anything_handler(
  handlers: HandlerMap(state),
  handler: fn(Dynamic, state) -> Next(state),
) -> HandlerMap(state)

pub fn handling_anything(
  handlers: HandlerMap(state),
  handler: fn(Dynamic, state) -> Next(state),
) -> HandlerMap(state) {
  insert_anything_handler(handlers, handler)
}

pub fn handling(
  handlers: HandlerMap(state),
  inbox: Inbox(msg),
  handler: fn(msg, state) -> Next(state),
) -> HandlerMap(state) {
  handlers |> insert_handler(inbox, handler)
}

pub fn start(state: state, handler: HandlerMap(state)) -> Pid {
  start_spec(Spec(init: fn() { Ready(state, handler) }))
}

pub fn start_spec(spec: Spec(state)) -> Pid {
  process_start(fn() { initialize_ektor(spec) }, linked: True)
}

pub fn process_start(
  running implementation: fn() -> anything,
  linked link: Bool,
) -> Pid {
  case link {
    True -> spawn_link(implementation)
    False -> spawn(implementation)
  }
}

@external(erlang, "erlang", "spawn")
fn spawn(a: fn() -> anything) -> Pid

@external(erlang, "erlang", "spawn_link")
fn spawn_link(a: fn() -> anything) -> Pid

fn initialize_ektor(spec: Spec(state)) -> ExitReason {
  let init_result = spec.init()
  case init_result {
    Ready(state, handlers) -> {
      loop(state, handlers)
    }
    Failed(reason) -> {
      Abnormal(reason)
    }
  }
}

@external(erlang, "ektor_ffi", "receive_forever_with_handlers")
fn receive_forever_with_handlers(
  state: state,
  handler: HandlerMap(state),
) -> Next(state)

@external(erlang, "ektor_ffi", "merge_handler_maps")
fn merge_handler_maps(
  a: HandlerMap(state),
  b: HandlerMap(state),
) -> HandlerMap(state)

@external(erlang, "logger", "warning")
fn log_warning(a: Charlist, b: List(Charlist)) -> Nil

fn default_handler(msg: Dynamic, state: state) -> Next(state) {
  log_warning(charlist.from_string("Ektor discarding unexpected message: ~s"), [
    charlist.from_string(string.inspect(msg)),
  ])
  continue(state)
}

fn loop(state, handlers: HandlerMap(state)) -> ExitReason {
  let catchall_handler =
    new_handler_map()
    |> handling_anything(default_handler)
  let handlers = merge_handler_maps(catchall_handler, handlers)
  let next = receive_forever_with_handlers(state, handlers)
  case next {
    Stop(reason) -> reason
    Continue(new_state, new_handlers) -> {
      case new_handlers {
        option.Some(new_handlers) -> {
          let new_handlers = merge_handler_maps(handlers, new_handlers)
          loop(new_state, new_handlers)
        }
        option.None -> {
          loop(new_state, handlers)
        }
      }
    }
  }
}
