module List = struct
  include List
  include BatList
end

module Array = struct
  include Array
  include BatArray
end

module Set = BatSet
module Map = BatMap
module Enum = BatEnum

let flip f x y =
  f y x

let (%) f g x =
  f (g x)
