::  "veriferreth": verify ethereum-side azimuth state
::  has naming gone too far?
::
/-  json-rpc
::
=,  ethereum
=,  ethe
::
|%
++  state
  $:  deeds=(map ship deed:eth-noun:constitution)
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
  ==
--
::
::
|%
++  azimuth  0x223c.067f.8cf2.8ae1.73ee.5caf.ea60.ca44.c335.fecb
::
++  parity   'http://104.198.35.227:8545'
--
::
::
|_  [bol=bowl:gall state]
::
++  prep
  |=  old=(unit *)
  ^-  [(list move) _+>]
  [~ +>]
::
++  poke-noun
  |=  a=@t
  ^-  [(list move) _+>]
  ?:  =('call' a)  [[initial-call ~] +>]
  ?:  =('file' a)  [~ write-file]
  ?:  =('show' a)  ~&  deeds  [~ +>]
  !!
::
++  write-file
  !!
::
++  initial-call
  (call (gulf ~zod ~syt) %hull) ::~fes))
::
++  call
  |=  [who=(list ship) wat=?(%hull %kids %deed)]
  ^-  move
  %+  ask-node  [wat ~]
  %+  turn  who
  |=  who=ship
  ^-  proto-read-request
  :+  `(scot %p who)
    azimuth
  :_  [%uint `@`who]~
  ?-  wat
    %hull  'points(uint32)'
    %kids  'getSpawned(uint32)'
    %deed  'rights(uint32)'
  ==
::
++  ask-node
  |=  [wir=wire req=(list proto-read-request)]
  ^-  move
  :-  ost.bol
  :^  %hiss  wir  ~
  :+  %json-rpc-response  %hiss
  ^-  hiss:eyre
  %+  json-request
    (need (de-purl:html parity))
  (batch-read-request req)
::
++  sigh-json-rpc-response
  |=  [wir=wire res=response:json-rpc]
  ^-  [(list move) _+>]
  ~|  res
  ?>  ?=(%batch -.res)
  %.  %+  turn  bas.res
      |=  r=response:json-rpc
      ^-  [ship json]
      ?>  ?=(%result -.r)
      [(slav %p id.r) res.r]
  ?+  wir  !!
    [%hull *]  hear-hulls
    [%deed *]  hear-deeds
    [%kids *]  hear-kids
  ==
::
++  hear-hulls
  |=  hus=(list [ship json])
  ^-  [(list move) _+>]
  =;  liv=(list ship)
    :_  +>.$
    ~&  ['active ships:' liv]
    ?~  liv  ~
    :~  (call liv %deed)
        (call liv %kids)
    ==
  %+  murn  hus
  |=  [who=ship hul=json]
  ^-  (unit ship)
  ~|  hul
  ?>  ?=(%s -.hul)
  =-  ?:(active `who ~)
  ^-  $:  crypt=octs
          auth=octs
          has-sponsor=?
          active=?
          escape-requested=?
          sponsor=@
          escape-requested-to=@
          crypto-suite-version=@
          key-revision-number=@
          continuity-number=@
      ==
  %+  decode-results  p.hul
  ~[[%bytes-n 32] [%bytes-n 32] %bool %bool %bool %uint %uint %uint %uint %uint]
::
++  hear-deeds
  |=  des=(list [ship json])
  ^-  [(list move) _+>]
  =,  constitution
  =-  [~ +>.$(deeds -)]
  %-  ~(gas in deeds)
  %+  turn  des
  |=  [who=ship ded=json]
  ^-  (pair ship deed:eth-noun)
  ~|  ded
  ?>  ?=(%s -.ded)
  =+  `deed:eth-noun`(decode-results p.ded deed:eth-type)
  ~|  [%wtf-no-owner who]
  ?<  =(0x0 owner)
  [who -]
::
++  hear-kids
  |=  kis=(list [ship json])
  ^-  [(list move) _+>]
  =;  lis=(list ship)
    :_  +>.$
    ~&  ['child ships:' (lent lis)]
    ::  doing them all in one request is likely to lead to a meme eventually
    ::  but doing individual request for each has too much overhead
    ::  so we send them out in groups of 255 instead
    =|  moz=(list move)
    |-
    ?:  =(~ lis)  moz
    =.  moz  [(call (scag 0xff lis) %hull) moz]
    $(lis (slag 0xff lis))
  %-  zing
  %+  turn  kis
  |=  [ship kid=json]
  ^-  (list ship)
  ?>  ?=(%s -.kid)
  %-  (list @)  ::NOTE  yes, i know this is bad, but output is (list)...
  %+  decode-results  `@t`p.kid
  [[%array %uint] ~]
::
::TODO  flow
::    read galaxy table
::    for each active galaxy, read deed, read spawned
::    for each spawned star, read hull, deed, spawned
::    for each spawned planet, read hull, deed
--