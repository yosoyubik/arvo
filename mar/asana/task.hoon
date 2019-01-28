::::  /mar/asana/task/hoon
  ::
/?    310
/-  asana
/+  asana-parse
::
|_  tk/task:asana
::
++  grab                                                ::  convert from
  |%
  ++  noun  task:asana
  ++  json                                              ::  from %json
    |=  a/^json
    ^-  task:asana
    (need (task:asana-parse a))
  --
::
++  grow                                                ::  convert to
  |%
  ++  json                                              ::  to %json
    *^json
  --
--
