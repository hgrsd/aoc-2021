module Common.Util

module Bit = 
  let getBitValue from idx =
    let mask = 1 <<< idx
    if (from &&& mask) <> 0 then 1 else 0