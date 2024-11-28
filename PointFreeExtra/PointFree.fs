namespace PointFreeExtra

module PointFree =
      let f'1 g l = List.map g (List.tail l)

      let f'2 g l = l |> List.tail |> List.map g

      let f'3 g = List.tail >> List.map g

      let f'4 g = (>>) List.tail (List.map g)

      let f'5 g = g |> List.map |> (>>) List.tail

      let f'6<'a, 'b> : (('a -> 'b) -> 'a list -> 'b list) = List.map >> (>>) List.tail
