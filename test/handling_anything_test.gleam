import ektor.{type Inbox}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid}
import gleeunit/should

pub type EktorMsg {
  EktorMsg(msg: String)
}

pub type State {
  Initial
  Ready(reply_to: #(Pid, Inbox(EktorMsg)))
}

pub type SupervisorMsg {
  ReplyTo(reply_to: #(Pid, Inbox(EktorMsg)))
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
      let #(pid, inbox) = reply_to
      let res = decode.run(msg, decode.string)
      case res {
        Ok(decoded) -> {
          ektor.send(
            pid,
            inbox,
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
  let supervisor_msg_inbox = ektor.new_inbox()
  let handlers =
    ektor.new_handler_map()
    |> ektor.handling(supervisor_msg_inbox, supervisor_msg_handler)
    |> ektor.handling_anything(anything_handler)
  let ekt_pid = ektor.start(Initial, handlers)
  let my_pid = process.self()
  let inbox = ektor.new_inbox()
  ektor.send(ekt_pid, supervisor_msg_inbox, ReplyTo(#(my_pid, inbox)))
  raw_send(ekt_pid, "Test String")
  let msg = ektor.receive(inbox, within: 200)
  msg
  |> should.equal(Ok(EktorMsg("Received a dynamic string: Test String")))
}
