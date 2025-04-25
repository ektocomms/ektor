import gleam/dynamic.{type Dynamic}
import gleam/erlang.{type Reference}
import gleam/erlang/charlist.{type Charlist}
import gleam/option.{type Option}
import gleam/string

pub type Pid

/// Get the `Pid` for the current process.
@external(erlang, "erlang", "self")
pub fn self() -> Pid

pub type Topic(msg) {
  Topic(ref: Reference)
}

pub fn new_topic() {
  Topic(ref: erlang.make_reference())
}

pub type Target(msg) {
  Target(pid: Pid, topic: Topic(msg))
}

pub fn new_target() {
  Target(self(), new_topic())
}

pub type ExitReason {
  Normal
  Killed
  Abnormal(reason: String)
}

pub type TopicRouter(state)

pub type InitResult(state) {
  Ready(state: state, router: TopicRouter(state))
  Failed(String)
}

pub type Spec(state) {
  Spec(init: fn() -> InitResult(state))
}

pub type Next(state) {
  Continue(state: state, topic_router: Option(TopicRouter(state)))
  Stop(ExitReason)
}

pub fn continue(state: state) -> Next(state) {
  Continue(state, option.None)
}

pub fn with_topic_router(
  next: Next(state),
  topic_router: TopicRouter(state),
) -> Next(state) {
  case next {
    Continue(state, _) -> Continue(state, option.Some(topic_router))
    _ -> next
  }
}

type DoNotLeak

@external(erlang, "erlang", "send")
fn raw_send(a: Pid, b: message) -> DoNotLeak

pub fn send(target: Target(msg), message: msg) -> Nil {
  raw_send(target.pid, #(target.topic.ref, message))
  Nil
}

@external(erlang, "ektor_ffi", "receive")
pub fn receive(topic: Topic(msg), within timeout: Int) -> Result(msg, Nil)

@external(erlang, "ektor_ffi", "new_topic_router")
pub fn new_topic_router() -> TopicRouter(state)

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

pub fn start_single(
  state: state,
  handler: fn(msg, state) -> Next(state),
) -> Target(msg) {
  let topic = new_topic()
  let router =
    new_topic_router()
    |> handling(topic, handler)
  let pid = start_spec(Spec(init: fn() { Ready(state, router) }))
  Target(pid, topic)
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
    new_topic_router()
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

pub type CallError {
  CallTimeout
}

pub fn try_call(
  target: Target(req),
  msg_factory: fn(Target(resp)) -> req,
  within timeout: Int,
) -> Result(resp, CallError) {
  let resp_target = new_target()
  send(target, msg_factory(resp_target))
  let result = receive(resp_target.topic, within: timeout)
  case result {
    Error(Nil) -> Error(CallTimeout)
    Ok(resp) -> Ok(resp)
  }
}

pub fn call(
  target: Target(resq),
  msg_factory: fn(Target(resp)) -> resq,
  within timeout: Int,
) -> resp {
  let assert Ok(resp) = try_call(target, msg_factory, timeout)
  resp
}
