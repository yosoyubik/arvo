::::  /mar/asana/task/hoon
  ::
/?    310
/-  asana
/+  asana-parse
::
|_  tk/(list task:asana)
::
++  grab                                                ::  convert from
  |%
  ++  noun  (list task:asana)
  ++  json                                              ::  from %json
    |=  a/^json
    ^-  (list task:asana)
    (need (tasks:asana-parse a))
  --
::
++  grow                                                ::  convert to
  |%
  ++  json                                              ::  to %json
    *^json
  --
--
