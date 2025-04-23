import ektor
import gleeunit/should

type Msg {
  Msg
}

pub fn ektor_start_spec_test() {
  let my_target = ektor.new_target()
  let _ekt_pid =
    ektor.start_spec(
      ektor.Spec(init: fn() {
        ektor.send(my_target, Msg)
        ektor.Ready(Nil, ektor.new_topic_router())
      }),
    )
  let msg = ektor.receive(my_target.topic, within: 200)
  msg
  |> should.equal(Ok(Msg))
}
