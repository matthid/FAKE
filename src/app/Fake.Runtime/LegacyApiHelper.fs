module internal Fake.Runtime.LegacyApiHelper

type NewApiMutableHelper<'a> = { Set : 'a -> unit; Get : unit -> 'a }

let ofRef r = { Set = (fun v -> r := v); Get = (fun () -> !r) }
let toGetSet { Set = s; Get = g } = g, s