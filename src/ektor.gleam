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

pub type TopicRouter(state)

pub type Topic(msg) {
  Topic(ref: Reference)
}

pub type InitResult(state) {
  Ready(state: state, handler: TopicRouter(state))
  Failed(String)
}

pub type Spec(state) {
  Spec(init: fn() -> InitResult(state))
}

pub type Next(state) {
  Continue(state: state, handler_map: Option(TopicRouter(state)))
  Stop(ExitReason)
}

pub fn continue(state: state) -> Next(state) {
  Continue(state, option.None)
}

pub fn with_handler_map(
  next: Next(state),
  handler_map: TopicRouter(state),
) -> Next(state) {
  case next {
    Continue(state, _) -> Continue(state, option.Some(handler_map))
    _ -> next
  }
}

pub fn new_topic() {
  Topic(ref: erlang.make_reference())
}

@external(erlang, "erlang", "send")
fn raw_send(a: Pid, b: message) -> DoNotLeak

pub fn send(pid: Pid, topic: Topic(msg), message: msg) -> Nil {
  raw_send(pid, #(topic.ref, message))
  Nil
}

@external(erlang, "ektor_ffi", "receive")
pub fn receive(topic: Topic(msg), within timeout: Int) -> Result(msg, Nil)

@external(erlang, "ektor_ffi", "new_topics_router")
pub fn new_topics_router() -> TopicRouter(state)

@external(erlang, "ektor_ffi", "insert_handler")
pub fn insert_handler(
  router: TopicRouter(state),
  topic: Topic(msg),
  handler: fn(msg, state) -> Next(state),
) -> TopicRouter(state)

@external(erlang, "ektor_ffi", "insert_anything_handler")
pub fn insert_anything_handler(
  router: TopicRouter(state),
  handler: fn(Dynamic, state) -> Next(state),
) -> TopicRouter(state)

pub fn handling_anything(
  router: TopicRouter(state),
  handler: fn(Dynamic, state) -> Next(state),
) -> TopicRouter(state) {
  insert_anything_handler(router, handler)
}

pub fn handling(
  router: TopicRouter(state),
  topic: Topic(msg),
  handler: fn(msg, state) -> Next(state),
) -> TopicRouter(state) {
  router |> insert_handler(topic, handler)
}

pub fn start(state: state, router: TopicRouter(state)) -> Pid {
  start_spec(Spec(init: fn() { Ready(state, router) }))
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
    Ready(state, router) -> {
      loop(state, router)
    }
    Failed(reason) -> {
      Abnormal(reason)
    }
  }
}

@external(erlang, "ektor_ffi", "receive_forever_with_router")
fn receive_forever_with_router(
  state: state,
  router: TopicRouter(state),
) -> Next(state)

@external(erlang, "ektor_ffi", "merge_topic_routers")
fn merge_topic_routers(
  a: TopicRouter(state),
  b: TopicRouter(state),
) -> TopicRouter(state)

@external(erlang, "logger", "warning")
fn log_warning(a: Charlist, b: List(Charlist)) -> Nil

fn default_handler(msg: Dynamic, state: state) -> Next(state) {
  log_warning(charlist.from_string("Ektor discarding unexpected message: ~s"), [
    charlist.from_string(string.inspect(msg)),
  ])
  continue(state)
}

fn loop(state, router: TopicRouter(state)) -> ExitReason {
  let catchall_router =
    new_topics_router()
    |> handling_anything(default_handler)
  let router = merge_topic_routers(catchall_router, router)
  let next = receive_forever_with_router(state, router)
  case next {
    Stop(reason) -> reason
    Continue(new_state, new_router) -> {
      case new_router {
        option.Some(new_router) -> {
          let new_router = merge_topic_routers(router, new_router)
          loop(new_state, new_router)
        }
        option.None -> {
          loop(new_state, router)
        }
      }
    }
  }
}
