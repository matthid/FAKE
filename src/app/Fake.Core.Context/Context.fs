/// This module contains function which allow to trace build output
module Fake.Core.Context

type ExecutionType =
  | Fake of isCached : bool
  | Other



#if NETSTANDARD1_6
open System.Threading
let private fake_data =
  let l = new AsyncLocal<System.Collections.Concurrent.ConcurrentDictionary<string, obj>>()
  l.Value <- new System.Collections.Concurrent.ConcurrentDictionary<string, obj>()
  l
let private getDataDict() = fake_data.Value
#endif

let private setContext (name:string) (o : obj) : unit =
#if NETSTANDARD1_6
  let d = getDataDict()
  d.AddOrUpdate(name, o, fun _ _ -> o) |> ignore
#else
  System.Runtime.Remoting.Messaging.CallContext.LogicalSetData(name, o)
#endif

let private getContext (name:string) : obj =
#if NETSTANDARD1_6
  let d = getDataDict()
  match d.TryGetValue(name) with
  | true, v -> v
  | false, _ -> null
#else
  System.Runtime.Remoting.Messaging.CallContext.LogicalGetData(name)
#endif

let private fake_ExecutionType = "fake_context_execution_type"

let getExecutionType () =
  match getContext fake_ExecutionType with
  | null -> Other
  | :? ExecutionType as e -> e
  | _ -> Other

let setExecutionType (e:ExecutionType) = setContext fake_ExecutionType e
