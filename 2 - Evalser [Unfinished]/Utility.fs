module Utility

let getAllOk (xs: Result<'T, 'E> list) : ('T list) option =
    let checkAll = xs |> List.forall Result.isOk
    if not checkAll then None
    else Some (xs |> List.choose (function | Ok v -> Some v | Error _ -> None))
