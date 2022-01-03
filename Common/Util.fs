module Common.Util

module Bit =
  let valueAt length i number =
    let mask = 1 <<< (length - 1 - i)
    if (number &&& mask) <> 0 then 1 else 0
