import ektor.{type Pid, type Target}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleeunit/should

pub type EktorMsg {
  EktorMsg(msg: String)
}

pub type State {
  Initial
  Ready(reply_to: Target(EktorMsg))
}

pub type SupervisorMsg {
  ReplyTo(reply_to: Target(EktorMsg))
}

fn supervisor_msg_handler(msg: SupervisorMsg, state: State) {
  case state {
    Initial -> {
      ektor.continue(Ready(reply_to: msg.reply_to))
    }
    _ -> ektor.continue(state)
  }
}

fn anything_handler(msg: Dynamic, state: State) {
  case state {
    Ready(reply_to) -> {
      let res = decode.run(msg, decode.string)
      case res {
        Ok(decoded) -> {
          ektor.send(
            reply_to,
            EktorMsg("Received a dynamic string: " <> decoded),
          )
          ektor.continue(state)
        }
        Error(_) -> ektor.continue(state)
      }
    }
    Initial -> ektor.continue(state)
  }
}

type DoNotLeak

@external(erlang, "erlang", "send")
fn raw_send(a: Pid, b: message) -> DoNotLeak

pub fn ektor_handling_anything_test() {
  let supervisor_msg_topic = ektor.new_topic()
  let topic_router =
    ektor.new_topic_router()
    |> ektor.handling(supervisor_msg_topic, supervisor_msg_handler)
    |> ektor.handling_anything(anything_handler)
  let ekt_pid = ektor.start(Initial, topic_router)
  let target = ektor.new_target()
  ektor.send(ektor.Target(ekt_pid, supervisor_msg_topic), ReplyTo(target))
  raw_send(ekt_pid, "Test String")
  let msg = ektor.receive(target.topic, within: 200)
  msg
  |> should.equal(Ok(EktorMsg("Received a dynamic string: Test String")))
}
