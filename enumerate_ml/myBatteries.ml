module List = struct
  include List
  include BatList
end

module Set = BatSet
module Map = BatMap
module Enum = BatEnum

let flip f x y =
  f y x