import ektor.{type Pid, type Topic}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleeunit/should

pub type EktorMsg {
  EktorMsg(msg: String)
}

pub type State {
  Initial
  Ready(reply_to: #(Pid, Topic(EktorMsg)))
}

pub type SupervisorMsg {
  ReplyTo(reply_to: #(Pid, Topic(EktorMsg)))
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
      let #(pid, topic) = reply_to
      let res = decode.run(msg, decode.string)
      case res {
        Ok(decoded) -> {
          ektor.send(
            pid,
            topic,
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
  let topics_router =
    ektor.new_topics_router()
    |> ektor.handling(supervisor_msg_topic, supervisor_msg_handler)
    |> ektor.handling_anything(anything_handler)
  let ekt_pid = ektor.start(Initial, topics_router)
  let my_pid = ektor.self()
  let topic = ektor.new_topic()
  ektor.send(ekt_pid, supervisor_msg_topic, ReplyTo(#(my_pid, topic)))
  raw_send(ekt_pid, "Test String")
  let msg = ektor.receive(topic, within: 200)
  msg
  |> should.equal(Ok(EktorMsg("Received a dynamic string: Test String")))
}
