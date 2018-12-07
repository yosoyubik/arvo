::  "veriferreth": verify ethereum-side azimuth state
::  has naming gone too far?
::
/-  json-rpc
::
=,  ethereum
=,  ethe
=,  constitution
::
|%
++  state
  $:  deeds=(map ship deek)
  ==
::
+$  deek  [keys deed:eth-noun]
+$  keys  [crypt=octs auth=octs]
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%info wire ship desk nori:clay]
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
  |=  old=(unit state)
  ^-  [(list move) _+>]
  ?~  old  [~ +>]
  [~ +>.$(+<+ u.old)]
::
++  poke-noun
  |=  a=@t
  ^-  [(list move) _+>]
  ?:  =('call' a)  [initial-call +>]
  ?:  =('file' a)  [[write-file ~] +>]
  ?:  =('show' a)
    ~&  ^-  (map ship [[[@ud @ux] [@ud @ux]] deed:eth-noun])
        deeds
    [~ +>]
  !!
::
++  write-file
  :*  ost.bol
      %info
      /write
      our.bol
      %home
    ::
      =-  &+[/chain/txt -]~
      =-  %+  feel:space:userlib
            /(scot %p our.bol)/home/(scot %da now.bol)/chain/txt
          txt+!>(-)
      ^-  (list @t)
      =/  hout
        |=  num=@
        ?:  =(0x0 num)  "\"\""
        (address-to-hex num)
      =/  kout
        |=  key=octs
        ?:  =('' q.key)  "\"\""
        ((x-co:co 64) q.key)
      %+  murn  (sort ~(tap in ~(key by deeds)) lth)
      |=  who=ship
      ^-  (unit @t)
      =+  (~(got by deeds) who)
      ?.  =(%czar (clan:title who))  ~
      :-  ~
      %-  crip
      ;:  weld
        ((d-co:co 1) who)  ","        ::  ship,
      ::
        ?+  (clan:title who)  !!      ::  shipGlass (STAR, PLANET, GALAXY),
          %czar  "galaxy"
          %king  "star"
          %duke  "planet"
        ==
      ::
        ","  "id"                     ::  idCode
        ","  (hout owner)             ::  ownership,
        ","  (hout transfer-proxy)    ::  transfer,
        ","  (hout spawn-proxy)       ::  spawn,
        ","  (hout management-proxy)  ::  mgmt,
        ","  (hout voting-proxy)      ::  voting,
        ","  (kout auth)              ::  auth,
        ","  (kout crypt)             ::  crypt,
        ","  "code"                   ::  confirmationCode,
        ","  "timestamp"              ::  confirm
      ==
  ==
::
++  initial-call
  ^-  (list move)
  %+  turn  (gulf ~zod ~fes)
  |=  who=ship
  (call [who]~ %hull)
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
  ~|  ?:(?=(?(%error %fail) -.res) res -.res)
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
  =;  liv=(list [who=ship crypt=octs auth=octs])
    :_  =-  +>.$(deeds (~(gas in deeds) -))
        %+  turn  liv
        |=  [who=ship keys=[octs octs]]
        ^-  (pair ship [[octs octs] deed:eth-noun])
        [who keys *deed:eth-noun]
    ~&  ['active ships:' (lent liv)]
    ?:  =(~ liv)  ~  ::  ?~ is tmi reeeee
    :~  (call (turn liv head) %deed)
        (call (turn liv head) %kids)
    ==
  %+  murn  hus
  |=  [who=ship hul=json]
  ^-  (unit [ship octs octs])
  :: ~|  hul
  ?>  ?=(%s -.hul)
  =-  ?:(active `[who crypt auth] ~)
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
  %-  ~(gas by deeds)
  %+  turn  des
  |=  [who=ship ded=json]
  ^-  (pair ship [[octs octs] deed:eth-noun])
  :: ~|  ded
  ?>  ?=(%s -.ded)
  =+  `deed:eth-noun`(decode-results p.ded deed:eth-type)
  ~|  [%wtf-no-owner who]
  ?<  =(0x0 owner)
  [who [crypt auth]:(~(got by deeds) who) -]
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