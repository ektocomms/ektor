import ektor
import gleeunit/should

type Msg {
  Msg
}

pub fn ektor_start_spec_test() {
  let my_pid = ektor.self()
  let my_topic = ektor.new_topic()
  let _ekt_pid =
    ektor.start_spec(
      ektor.Spec(init: fn() {
        ektor.send(my_pid, my_topic, Msg)
        ektor.Ready(Nil, ektor.new_topic_router())
      }),
    )
  let msg = ektor.receive(my_topic, within: 200)
  msg
  |> should.equal(Ok(Msg))
}
