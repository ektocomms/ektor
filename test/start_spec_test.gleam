import ektor
import gleam/erlang/process
import gleeunit/should

type Msg {
  Msg
}

pub fn ektor_start_spec_test() {
  let my_pid = process.self()
  let my_inbox = ektor.new_inbox()
  let _ekt_pid =
    ektor.start_spec(
      ektor.Spec(init: fn() {
        ektor.send(my_pid, my_inbox, Msg)
        ektor.Ready(Nil, ektor.new_handler_map())
      }),
    )
  let msg = ektor.receive(my_inbox, within: 200)
  msg
  |> should.equal(Ok(Msg))
}
