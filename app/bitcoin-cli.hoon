::  minimum viable bitcoin app
::
::    first, run |init-auth-basic with bitcoin-rpc as the domain (no .tld)
::    to configure your rpc username and password.
::    then, poke this app with either an %app command or %rpc request.
::
/+  *bitcoin, basic-auth
::
|%
++  task
  $%  [%rpc req=request:rpc]
      [%app cmd=command]
  ==
::
++  command
  $%  [%select-wallet name=(unit @tas)]
  ==
::
++  state
  $:  wallet=(unit @tas)
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire (unit user:eyre) mark %hiss hiss:eyre]
  ==
::
--
::
|_  [=bowl:gall state]
++  prep
  |=  (unit *)
  ^-  (quip move _+>)
  [~ ..prep]
::
++  poke-noun
  |=  =task
  ^-  (quip move _+>)
  ?:  ?=(%app -.task)
    ?-  -.cmd.task
      %select-wallet  [~ +>.$(wallet name.cmd.task)]
    ==
  :_  +>.$
  :_  ~
  ^-  move
  :-  ost.bowl
  ^-  card
  :*  %hiss
      /some/wire
      [~ '']
      %json-rpc-response
      %hiss
      (auth-request /sec/bitcoin-rpc/atom req.task)
  ==
::
++  sigh-json-rpc-response
  |=  [wir=wire res=response:rpc:jstd]
  ^-  (quip move _+>)
  ~|  res
  ~&  (parse-response:rpc res)
  [~ +>.$]
::
++  request-to-hiss
  |=  req=request:rpc
  %+  request-to-hiss:rpc:jstd
    =-  (need (de-purl:html (crip -)))
    =+  url="http://localhost:18443/"
    ?.  ?=(?(%generate) -.req)  url
    ?~  wallet  url
    (weld url "wallet/{(trip u.wallet)}")
  (request-to-rpc:rpc req)
::
++  auth-request
  |=  [key=path req=request:rpc]
  ^-  hiss:eyre
  %.  (request-to-hiss req)
  ~(add-auth-header basic-auth (bale-from-path key) ~)
::
::TODO  into /lib/sec?
::
++  get-code
  ^-  @t
  =-  (crip +:(scow %p .^(@p %j -)))
  %-  en-beam:format
  =,  bowl
  [[our %code da+now] /(scot %p our)]
::
++  bale-from-path
  |=  =path
  ^-  (bale:eyre @t)
  :+  [our now eny byk]:bowl
    [*user:eyre domain=*(list @t)]
  ^-  @t
  %-  decode-atom
  %+  slav  %uw
  .^(@ %cx (weld =,(bowl /(scot %p our)/[q.byk]/(scot %da now)) path))
::
++  decode-atom
  |=  dat=@
  ^-  @
  %-  need
  (de:crub:crypto get-code dat)
--