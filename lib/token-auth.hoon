::  Basic authentication
::
::::  /hoon/basic-auth/lib
  ::
=,  eyre
|%
++  keys  @t
--
::
::::
  ::
|_  {bal/(bale keys) ~}
++  auth
  |%
  ++  header
    ^-  cord
    ?~  key.bal
      ~_  leaf+"Run |init-auth-token {<`path`dom.bal>}"
      ~|(%basic-auth-no-key !!)
    (cat 3 'Bearer ' key.bal)
  --
::
++  add-auth-header
  |=  a/hiss  ^-  hiss
  %_(a q.q (~(add ja q.q.a) %authorization header:auth))
::
++  standard
  |%
  ++  out-adding-header
    |=  a/hiss  ^-  sec-move
    [%send (add-auth-header a)]
  --
--
