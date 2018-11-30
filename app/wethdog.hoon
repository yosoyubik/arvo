::  "wet-dog": ethereum ceremony watchdog
::
/-  json-rpc
::
=,  ethereum
=,  ethe
::
|%
++  state
  $:  latest=@ud
      when=@da
      good=?
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%rest wire @da]
      [%wait wire @da]
  ==
--
::
|%
++  ceremony  0x740d.6d74.1711.163d.3fca.cecf.1f11.b867.9a7c.7964
::
++  parity    'http://104.198.35.227:8545'
--
::
|_  [bol=bowl:gall state]
::
++  prep
  |=  old=(unit *)
  [~ ..prep]
::
++  poke-noun
  |=  wat=@t
  ^-  [(list move) _+>]
  =-  [-^~ ..poke-noun]
  ?:  =('flow' wat)  ask-flow
  !!
::
++  ask-flow
  ^-  move
  %+  ask-node  /flow
  :+  %eth-get-transaction-count
    ceremony
  label+%latest
::
++  ask-node
  |=  [wir=wire req=request]
  ^-  move
  :-  ost.bol
  :^  %hiss  wir  ~
  :+  `mark`%json-rpc-response  %hiss
  ^-  hiss:eyre
  %+  json-request
    =+  (need (de-purl:html parity))
    -(p.p |)
  ~!  req
  (request-to-json `'no crash pls' req)
::
++  sigh-json-rpc-response
  |=  [wir=wire res=response:json-rpc]
  ^-  [(list move) _+>]
  ?>  ?=([%flow ~] wir)
  :-  [(wait /flow (add now.bol ~m1)) ~]
  ?:  ?=(%result -.res)
    =+  new=(parse-hex-result res.res)
    =?  when  (gth new latest)  now.bol
    =?  latest  (gth new latest)  new
    check-flow
  ~&  res
  +>
::
++  check-flow
  ^-  _..prep
  =+  new=(gth now.bol (add when ~m15))
  ~?  !=(good new)
    :_  latest
    ?:  good  'steadily confirming again...'
    'no new transactions confirmed in a little while...'
  ..check-flow(good new)
::
++  wait
  |=  [wir=wire wen=@da]
  ^-  move
  [ost.bol %wait wir wen]
::
++  wake-flow
  |=  [wir=wire ~]
  ^-  [(list move) _+>]
  [[ask-flow ~] +>]
--