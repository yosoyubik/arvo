::
::::  /hoon/repl-json/mar
  ::
/?    310
  ::
::::  compute
  ::
=,  html
|_  jon/json
::
++  grow                                                ::  convert to
  |%
  ++  mime  [/text/json (as-octt:mimes (en-json jon))]              ::  convert to %mime
  --
++  grab
  |%                                                    ::  convert from
  ++  noun  json                                        ::  clam from %noun
  --
--
