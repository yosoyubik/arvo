::
=,  ethereum
=,  rpc
=,  key
::
=|  addr=address
=|  gas-price=@ud
=|  network=@ux
=|  now=@da
::
|_  $:  nonce=@ud                                       ::  next tx id
        transactions=(list transaction)                 ::  generated txs
        constitution=address                            ::  deployed address
    ==
::
++  this  .
::
+$  rights
  $:  own=address
      manage=(unit address)
      voting=(unit address)
      transfer=(unit address)
      spawn=(unit address)
      net=(unit [crypt=@ux auth=@ux])
  ==
::
+$  address-info
  $:  id=@ux
      owner=(unit address)
      transfer=(unit address)
      spawn=(unit address)
      management=(unit address)
      voting=(unit address)
      auth=(unit @ux)
      crypt=(unit @ux)
      conf-code=tape
      conf-date=tape
  ==
::
++  lockup-info
  $:  email=tape
      owner=tape
      group=tape
      backup-email=tape
      comment=tape
      type=?(%linear %conditional %direct)
      windup-years=(unit @ud)
      term=(unit @ud)      :: years
      rate=(unit @ud)      :: stars
      rate-unit=(unit @ud) :: seconds
      tranches=(unit @ud)
      extra=tape
  ==
::
++  parlock-info
  $:  b1=@ud
      b2=@ud
      b3=@ud
      extra=tape
  ==
::
++  conditional-recipient
  $:  b1=@ud
      b2=@ud
      b3=@ud
      rate=@ud
      rate-unit=@ud
  ==
::
++  linlock-info
  $:  star-count=@ud
      windup=@ud
      term=@ud
      stars=(list ,@p)
      extra=tape
  ==
::
++  linear-recipient
  $:  windup=@ud
      stars=@ud
      rate=@ud
      rate-unit=@ud
  ==
::
++  tape-to-ux
  |=  t=tape
  (scan t zero-ux)
::
++  zero-ux
  ;~(pfix (jest '0x') hex)
::
++  ship-and-rights
  |=  live=?
  ;~  (glue com)
    ;~(pfix sig fed:ag)
    (all-rights live)
  ==
::
++  all-rights
  |=  live=?
  ;~  (glue com)
    zero-ux
    (punt zero-ux)
    (punt zero-ux)
    (punt zero-ux)
    (punt zero-ux)
  ::
    =+  ;~(plug zero-ux ;~(pfix com zero-ux))
    ?.  live  (punt -)
    (stag ~ -)
    :: %.  ;~(plug zero-ux ;~(pfix com zero-ux))
    :: ^-  $-(rule rule)
    :: ?.  live  punt
    :: (cury stag ~)
  ==
::
++  get-file
  |=  pax=path
  ~|  pax
  .^  (list cord)  %cx
      (weld /(scot %p ~zod)/home/(scot %da now) pax)
  ==
::
++  parse-lines
  |*  [fil=knot par=rule]
  %+  murn
    [?~(- ~ t)]:(get-file /[fil]/txt)
  |=  c=cord
  ~|  c
  ?:  =('' c)
    ~
  ?:  =('::  ' (end 3 4 c))
    ~
  `u=(rash c par)
::
++  order-shiplist
  |=  [[a=ship *] [b=ship *]]
  (lth a b)
::
++  init
  |=  [n=@da g=@ud non=@ud addr=address]
  ^+  this
  %_  this
    now         n
    nonce       non
    gas-price   g
    addr        addr
  ==
::
++  address-or-quotes
  %+  cook  (unit address)
  ;~  pose
    (cook |=(* ~) (jest '""'))
    (stag ~ zero-ux)
    (stag ~ hex)
  ==
++  hex-or-quotes
  (cook (unit @p) ;~(pose (cook |=(* ~) (jest '""')) (stag ~ hex)))
++  no-comma :: [^,]*
  (cook tape (star ;~(pose (shim ' ' '+') (shim '-' '~'))))
++  na-or-num
  ;~  pose
    (stag ~ dum:ag)
    (cook |=(* ~) (jest 'N/A'))
    (cook |=(* ~) (jest '#VALUE!'))
    (easy ~)
  ==
++  no-quotes :: 0 or "[^"]*"
  %+  cook  tape
  ;~  pose
    ;~(plug (jest '0') (easy ~))
    %+  ifix  yel^yel
    (plus ;~(pose (shim ' ' '!') (shim '#' '~')))
  ==
++  get-all-addresses
  %-  malt
  ^-  (list [who=ship address-info])
  %-  skip  :_
    |=  [who=ship address-info]
    ?|  =("0" conf-date)  ::  addresses have not been confirmed yet
        (gth 5 (lent conf-date))
    ==
  ^-  (list [who=ship address-info])
  %+  parse-lines  'all-addresses'
  %+  cook  |=(* (,[who=ship address-info] +<))
  ;~  (glue com)
    (cook @p dum:ag)
    ;~(pfix no-comma com hex)
    address-or-quotes
    address-or-quotes
    address-or-quotes
    address-or-quotes
    address-or-quotes
    hex-or-quotes
    hex-or-quotes
    no-comma
    no-quotes
  ==
::
++  get-done-addresses
  %-  malt
  %+  turn
    ^-  (list ship)
    (parse-lines 'done' ;~(pfix sig fed:ag))
  |=(ship [+< *address-info])
::
++  get-lockups
  %-  malt
  ^-  (list [who=ship lockup-info])
  %+  parse-lines  'lockup'
  ;~  (glue com)
    ;~(pfix sig fed:ag)
    no-comma
    no-comma
    no-comma
    no-comma
    no-comma
    ;~  pose
      (cook |=(* %linear) (jest 'Linear'))
      (cook |=(* %conditional) (jest 'Conditional'))
      (cook |=(* %direct) (jest 'Direct'))
    ==
    na-or-num
    na-or-num
    na-or-num
    na-or-num
    na-or-num
    (star ;~(pose (jest '\0d') (shim ' ' '~')))
  ==
::
++  get-parlocks
  ^-  (list [id=@ux owner=address parlock-info])
  %+  parse-lines  'partial'
  ;~  (glue com)
    hex
    zero-ux
    dum:ag
    dum:ag
    dum:ag
    (star ;~(pose (jest '\0d') (shim ' ' '~')))
  ==
::
++  get-linlocks
  ^-  (list [owner=address linlock-info])
  %+  parse-lines  'linlock'
  ;~  (glue com)
    zero-ux
    dum:ag
    dum:ag
    dum:ag
    (most ace ;~(pfix sig fed:ag))
    (star ;~(pose (jest '\0d') (shim ' ' '~')))
  ==
::
++  get-direct-galaxies
  ^-  (list [who=ship rights])
  %+  parse-lines  'direct-galaxies'
  (ship-and-rights |)
::
++  get-direct-ships
  ^-  (list [who=ship rights])
  %+  parse-lines  'direct-ships'
  (ship-and-rights |)
::
++  get-linear-recipients
  ^-  (list [owner=address linear-recipient])
  %+  parse-lines  'linear-recipients'
  ;~  (glue com)
    zero-ux
    dum:ag
    dum:ag
    dum:ag
    dum:ag
  ==
::
++  get-conditional-recipients
  ^-  (list [owner=address conditional-recipient])
  %+  parse-lines  'conditional-recipients'
  ;~  (glue com)
    zero-ux
    dum:ag
    dum:ag
    dum:ag
    dum:ag
    dum:ag
  ==
::
++  get-locked-galaxies
  |=  type=@t
  ^-  (list [who=ship rights])
  ~|  type
  %+  parse-lines  (cat 3 type '-galaxies')
  (ship-and-rights &)
::
++  get-locked-stars
  |=  type=@t
  ^-  (list [who=ship recipient=address])
  ~|  type
  %+  parse-lines  (cat 3 type '-stars')
  ;~  (glue com)
    ;~(pfix sig fed:ag)
    zero-ux
  ==
::
++  write-tx
  |=  tx=transaction
  ^+  this
  this(transactions [tx transactions])
::
++  complete
  ~&  [%writing-transactions (lent transactions)]
  (flop transactions)
::
++  get-contract-address
  =+  dat=(encode-atoms:rlp:ethereum ~[addr nonce])
  =+  wid=(met 3 dat)
  %^  end  3  20
  (keccak-256:keccak:crypto wid (rev 3 wid dat))
::
++  do-deploy
  |=  [wat=cord arg=(list data)]
  ^-  [address _this]
  ~&  [`@ux`get-contract-address +(nonce)]
  :-  get-contract-address
  %^  do  0x0  600.000
  =+  cod=(get-file /contracts/[wat]/txt)
  ?>  ?=(^ cod)
  %-  tape-to-ux
  (weld (trip i.cod) (encode-args arg))
::
++  do
  ::TODO  maybe reconsider encode-call interface, if we end up wanting @ux
  ::      as or more often than we want tapes
  |=  [to=address gas=@ud dat=$@(@ux tape)]
  ^+  this
  %-  write-tx(nonce +(nonce))
  :*  nonce
      gas-price
      600.000
      to
      0
      `@`?@(dat dat (tape-to-ux dat))
      network
  ==
::
++  sequence
  |=  [won=@da net=?(%fake %main %ropsten) gasp=@ud non=@ud addr=address]
  =.  this  (init(now won) won gasp non addr)
  =.  network
    ?+  net  0x1
      %ropsten  0x3
      %fake     `@ux``@`1.337
    ==
  ::
  ::  data loading
  ::
  ::NOTE  we do these first so that we are sure we have sane files,
  ::      without waiting for that answer
  ::  =+  tlon-gal=get-direct-galaxies
  ::  =+  directs=get-direct-ships
  ::
  ::B =+  lin-rec=get-linear-recipients
  ::B =+  lin-gal=(get-locked-galaxies 'linear')
  =+  ships=(hex-to-num '0x223c067f8cf28ae173ee5cafea60ca44c335fecb')
  =+  polls=(hex-to-num '0x7fecab617c868bb5996d99d95200d2fa708218e4')
  =+  claims=(hex-to-num '0xe7e7f69b34d7d9bd8d61fb22c33b22708947971a')
  =+  linear-star-release=(hex-to-num '0x86cd9cd0992f04231751e3761de45cecea5d1801')
  ::  XX only if csr is deployed at nonce 8681!
  =+  conditional-star-release=(hex-to-num '0x8c241098c3d3498fe1261421633fd57986d74aea')
  =+  special-spawn-proxy=(hex-to-num '0xe86F3b9D53C4D660459Cb4deF9d2415117B767F9')
  =+  doc-0=(hex-to-num '0x1234567890000000000000000000000000000000000000000000002019010900')
  =+  doc-1=(hex-to-num '0xcb1f81e42b5e75f000f94fc71a3ea70cab4bfc6f236b91e717f1b9516e5596b5')
  =+  doc-2=(hex-to-num '0x0000000000000000000000000000000000000000000000000000000000000060')
  =.  constitution  (hex-to-num '0x6ac07b7c4601b5ce11de8dfe6335b871c7c4dd4d')
  ::  pre-upgrade '0xa23b5d8e86091ab6c14981f404c2864701aa2903'
  ::  pre-rescue: '0x12778371a6aa58b1dd623c126e09cd28fc5b9b5c'
  ::
  =/  linear-lockup-addresses  *(list address)
  =/  conditional-lockup-addresses  *(list address)
  ::
  =/  all-addr=(map ship address-info)
    get-all-addresses
  ~&  all-addr-wyt=~(wyt by all-addr)
  =/  done=(map ship address-info)
    get-done-addresses
  ~&  done-wyt=~(wyt by done)
  =/  addresses=(map ship address-info)
    (~(dif by all-addr) done)
  ~&  remaining-wyt=~(wyt by addresses)
  ~&  addresses
  ~&  dif=(~(dif by done) all-addr)
  =/  lockups=(map ship lockup-info)
    get-lockups
  ~&  lockups-wyt=~(wyt by lockups)
  =/  potential
    %+  skim
      ~(tap by addresses)
    |=  [who=ship address-info]
    ?.  (lth who ~marzod)
      |
    =+  lockup=(~(got by lockups) who)
    =(%linear type.lockup)
  =.  linear-lockup-addresses
    %+  weld  linear-lockup-addresses
    ^-  (list address)
    %+  murn  ~(tap by all-addr)
    |=  [who=ship address-info]
    ^-  (unit address)
    ~?  ?=(?(%~net %~wes %~sev) who)  hrm=+<
    =+  l=(~(get by lockups) who)
    ?~  l                     ~
    ?~  owner                 ~
    ?.  ?=(%linear type.u.l)  ~
    `u.owner
  ~&  lin=(lent linear-lockup-addresses)
  =.  conditional-lockup-addresses
    %+  weld  conditional-lockup-addresses
    ^-  (list address)
    %+  murn  ~(tap by all-addr)
    |=  [who=ship address-info]
    ^-  (unit address)
    =+  l=(~(get by lockups) who)
    ?~  l                                   ~
    ?~  owner                               ~
    ?:  ?=(?(%~fet %~nes %~rel %~rud) who)  ~
    ?.  ?=(%conditional type.u.l)           ~
    `u.owner
  ~&  con=(lent conditional-lockup-addresses)
  ::
  ::  Calculate linear lockups
  ::
  =/  lin-rec
    %+  roll  potential
    |=  [[who=ship address-info] m=(map address linear-recipient)]
    ~|  +<
    =/  l  (~(got by lockups) who)
    =/  tentative=linear-recipient
      =/  rate-unit  (div :(mul 60 60 24 365 (need term.l)) 255)
      ::L ?>  =(rate-unit.l :(mul 60 60 24 365 (need term.l)))
      :*  :(mul (need windup-years.l) 60 60 24 365)
          255
          (need rate.l)
          rate-unit
      ==
    =/  own  (need owner)
    =/  prev  (~(get by m) own)
    ?~  prev
      (~(put by m) own tentative)
    ::  ?:  &
    ::    ~&  'more than one galaxy per address, check rates-unit very carefully!'
    ::    !!
    ?>  =(windup.u.prev windup.tentative)
    ?>  =(rate.u.prev rate.tentative)
    ::XX  ?>  =(rate-unit.u.prev rate-unit.tentative)
    =/  stars  (add 255 stars.u.prev)
    =/  rate-unit  (div :(mul 60 60 24 365 (need term.l)) stars)
    %+  ~(put by m)  own
    :*  windup.u.prev
        stars
        rate.u.prev
        rate-unit
    ==
  =/  lin-gal=(list [who=ship rights])
    %+  turn  potential
    |=  [who=ship address-info]
    ~|  +<
    ::  linear galaxies must spawn
    ::
    =+  auth=?~(auth 0x0 u.auth)
    =+  crypt=?~(crypt 0x0 u.crypt)
    ^-  [who=ship rights]
    :*  who
        (need owner)
        management
        voting
        transfer
        spawn
        `[crypt auth]
    ==
  ~&  :-  %linear-galaxies
  lin-gal
  ~&  lin-rec
  ~&  %+  turn  potential
  |=  [who=ship address-info]
  :-  +<
  (~(got by lockups) who)
  ::
  ::  Calculate conditional lockups
  ::
  =/  potential-con
    %+  skim
      ~(tap by addresses)
    |=  [who=ship address-info]
    ?.  (lth who ~marzod)
      |
    =+  lockup=(~(got by lockups) who)
    =(%conditional type.lockup)
  =/  con-rec
    %+  roll  potential-con
    |=  [[who=ship address-info] m=(map address conditional-recipient)]
    ~|  +<
    ?:  =(who ~ryx)
      ~&  "not adding ~ryx to con-rec because they also have stars"
      m
    =/  l  (~(got by lockups) who)
    =/  t  (need tranches.l)
    =/  tentative=conditional-recipient
      ?>  ?=(?(%1 %3) t)
      =+  [b1 b2 b3]=?:(=(1 t) [255 0 0] [85 85 85])
      =+  rate-unit=(div :(mul 60 60 24 365 1) ?:(=(1 t) 255 85))
      :*  b1
          b2
          b3
          1   :: (need rate.l)
          rate-unit   :: (need rate-unit.l)
      ==
    =/  own  (need owner)
    =/  prev  (~(get by m) own)
    ?~  prev
      (~(put by m) own tentative)
    ?:  &
      ~&  'more than one galaxy per address, check rates-unit and tranches very carefully!'
      !!
    ?>  =(rate.u.prev rate.tentative)
    ?>  =(rate-unit.u.prev rate-unit.tentative)
    =/  stars  (add ?:(=(1 t) 255 85) :(add b1.u.prev b2.u.prev b3.u.prev))
    =/  rate-unit  (div :(mul 60 60 24 365 1) stars)
    %+  ~(put by m)  own
    :*  (add b1.tentative b1.u.prev)
        (add b2.tentative b2.u.prev)
        (add b3.tentative b3.u.prev)
        rate.u.prev
        rate-unit
      ==
  =/  con-gal=(list [who=ship rights])
    %+  turn  potential-con
    |=  [who=ship address-info]
    ~|  +<
    ::  conditional galaxies must spawn
    ::
    =+  auth=?~(auth 0x0 u.auth)
    =+  crypt=?~(crypt 0x0 u.crypt)
    ^-  [who=ship rights]
    :*  who
        (need owner)
        management
        voting
        transfer
        spawn
        `[crypt auth]
    ==
  ~&  :-  %conditional-galaxies
  con-gal
  ~&  con-rec
  ::C %+  turn  potential-con
  ::C |=  [who=ship address-info]
  ::C :-  +<
  ::C (~(got by lockups) who)
  ::Z =/  lin-gal=(list [ship rights[)
  ::Z   %+  murn
  ::Z     lin-rec
  ::Z   |=  [who=ship address-info]
  ::Z   =+  addr=(~(get by all-addr) who)
  ::Z   ?~  addr
  ::Z     ~
  ::Z   `[who `rights`[(need owner) management voting transfer spawn net]]
  ::
  ::  Calculate direct galaxies
  ::
  =/  potential-direct-galaxies
    %+  skim
      ~(tap by addresses)
    |=  [who=ship address-info]
    ?.  (lth who ~marzod)
      |
    =+  lockup=(~(got by lockups) who)
    =(%direct type.lockup)
  =/  direct-galaxies=(list [who=ship rights])
    %+  turn  potential-direct-galaxies
    |=  [who=ship address-info]
    ~|  +<
    :*  who
        (need owner)
        management
        voting
        transfer
        spawn
        ?:  |(?=(~ crypt) ?=(~ auth))
          ~
        `[u.crypt u.auth]
    ==
  ~&  [%direct-galaxies direct-galaxies]
  ~&  %+  turn  potential-direct-galaxies
      |=  [who=ship address-info]
      [+< (~(got by lockups) who)]
  ::
  ::  Calculate ~{,mar,wan,bin,sam}zod
  ::
  =/  tmp-points=(list [who=ship spawn=(unit address) keys=(unit [@ux @ux])])
    %+  turn
      ^-  (list ship)
      ~
      ::  :~  ::  ~dopzod
      ::      ~net
      ::      ~wel
      ::      ~wes
      ::      ~sev
      ::  ==
      ::  :~  ~feb
      ::  ==
      ::  :~  ~rel
      ::      ~rud
      ::      ~nes
      ::      ~fet
      ::  ==
      ::  :~  ~nus
      ::    ::
      ::      ~ten
      ::      ~pub
      ::      ~sud
      ::      ~pem
      ::      ~dev
      ::      ~lur
      ::      ~def
      ::      ~bus
      ::  ==
      ::  :~
      ::      ::  ~zod
      ::      ::  ~marzod
      ::      ::  ~binzod
      ::      ::  ~wanzod
      ::      ::  ~samzod
      ::  ==
      ::  XX what of dopzod?
    |=  who=ship
    [who ~ `[0x0 0x0]]
  ~&  [%tmp-points tmp-points]
  ::
  =/  old-tmp-points
    ^-  %-  list
        $:  who=ship
            net=(unit [crypt=@ux auth=@ux])
            $=  send
            $:  own=address
                manage=(unit address)
                voting=(unit address)
                spawn=(unit address)
                transfer=(unit address)
            ==
        ==
    %+  turn
      ^-  (list ship)
      ::  ~
      ::  :~  ::  ~dopzod
      ::      ~net
      ::      ~wel
      ::      ~wes
      ::      ~sev
      ::  ==
      ::  :~  ~feb
      ::  ==
      ::  :~  ~rel
      ::      ~rud
      ::      ~nes
      ::      ~fet
      ::  ==
      :~  ~nus
        ::
          ~ten
          ~pub
          ~sud
          ~pem
          ~dev
          ~lur
          ~def
          ~bus
      ==
      ::  :~
      ::      ::  ~zod
      ::      ::  ~marzod
      ::      ::  ~binzod
      ::      ::  ~wanzod
      ::      ::  ~samzod
      ::  ==
      ::  XX what of dopzod?
    |=  who=ship
    =+  (~(got by all-addr) who)
    :*  who
        ?:  |(?=(~ crypt) ?=(~ auth))
          ~
        `[u.crypt u.auth]
        (need owner)
        management
        voting
        ::  ?~  spawn  nope because we manually set these guys
        ::    `*address
        spawn
        transfer
    ==
  ~&  [%old-tmp-points old-tmp-points]
  ::
  ::  Calculate conditional stars
  ::
  =/  potential-conditional-stars
    %+  skim
      ~(tap by addresses)
    |=  [who=ship address-info]
    ?&  (gte who ~marzod)
        (lth who 0x1.0000)
        ?=  :: is a conditional partial galaxy
          $?  %~rel
              %~rud
              %~nes
              %~fet
          ==
        (^sein:title who)
    ==
  =/  parlocks  get-parlocks
  =/  con-stars-rec=(list [own=address conditional-recipient])
    %+  turn  parlocks
    |=  [id=@ux own=address parlock-info]
    ~|  +<
    =+  rate-unit=(div :(mul 60 60 24 365 1) :(max b1 b2 b3))
    :*  own
        b1
        b2
        b3
        1
        rate-unit
    ==
  =/  con-stars=(list [who=ship recipient=address])
    =+  parlock-map=(malt parlocks)
    %+  turn  potential-conditional-stars
    |=  [who=ship address-info]
    ~|  +<
    ?>  (~(has by parlock-map) id)
    :*  who
        owner:(~(got by parlock-map) id)
    ==
  ~&  [%conditional-stars (lent potential-conditional-stars)]
  ~&  potential-conditional-stars
  ~&  con-stars
  ~&  con-stars-rec
  =.  conditional-lockup-addresses
    %+  weld  conditional-lockup-addresses
    ^-  (list address)
    %+  turn  con-stars-rec
    |=  [a=address *]
    a
  ::
  ::  Calculate linear stars
  ::
  ::P =/  potential-linear-stars
  ::P   %+  skim
  ::P     ~(tap by addresses)
  ::P   |=  [who=ship address-info]
  ::P   ?&  (gte who ~marzod)
  ::P       (lth who 0x1.0000)
  ::P       ?=  :: is a conditional partial galaxy
  ::P         $?  %~feb
  ::P         ==
  ::P       (^sein:title who)
  ::P   ==
  ::P ~&  [%potential-linear-stars potential-linear-stars]
  =/  linlocks  get-linlocks :: XXX
  =/  lin-stars-rec=(list [own=address linear-recipient])
    ::  ~
    %+  turn  linlocks
    |=  [own=address linlock-info]
    ~|  +<
    ?>  =(star-count (lent stars))
    =+  rate-unit=(div :(mul 60 60 24 365 term) star-count)
    :*  own
        windup
        star-count
        1
        rate-unit
    ==
  =/  lin-stars=(list [who=ship recipient=address])
    %-  zing
    ^-  (list (list [who=ship recipient=address]))
    %+  turn  linlocks
    |=  [own=address linlock-info]
    ^-  (list [who=ship recipient=address])
    ~|  +<
    %+  turn  stars
    |=  who=@p
    [who own]
  ~&  [%linear-stars (lent lin-stars)]
  ~&  lin-stars
  ~&  lin-stars-rec
  =.  linear-lockup-addresses
    %+  weld  linear-lockup-addresses
    =-  ~&  a=-  -
    ^-  (list address)
    %+  turn  lin-stars-rec
    |=  [a=address *]
    a
  ::  Calculate direct stars
  ::
  =/  potential-direct-stars
    %+  skim
      ~(tap by addresses)
    |=  [who=ship address-info]
    ::  ?:  =(id (hex-to-num '0xafd568600afb25596b90ff155d15c5b6799c0b680c4cc0fab2032893ca68044a'))
    ::    ~&  "say it ain't so, joe!"
    ::    !!
    ?:  =(id (hex-to-num '0xdffe59eb14574213d33ddab0f9db5eef9766395f51af53a4cbf38fa849f9100e'))
      ~&  "i'm afraid it is"
      !!
    ?&  (gte who ~marzod)
        (lth who 0x1.0000)
        ?=  :: is a crowdsale galaxy
          ::
          ::  %~nus
          ::
          $?  %~zod
            ::
              %~feb
            ::
              %~nus
            ::
              %~ten
              %~pub
              %~sud
              %~pem
              %~dev
              %~lur
              %~def
              %~bus
          ==
          ::
          ::  %~feb
          ::
          ::  $?  %~rel
          ::      %~rud
          ::      %~nes
          ::      %~fet
          ::  ==
        (^sein:title who)
    ==
  =/  direct-stars=(list [who=ship rights])
    %+  turn  potential-direct-stars
    |=  [who=ship address-info]
    ~|  +<
    :*  who
        (need owner)
        management
        voting
        transfer
        spawn
        ?:  |(?=(~ crypt) ?=(~ auth))
          ~
        `[u.crypt u.auth]
    ==
  ~&  [%direct-stars (lent potential-direct-stars)]
  ~&  %+  turn  direct-stars
      |=  [=ship *]
      ship
  ::  ~&  %+  turn  potential-direct-stars
  ::      |=  [who=ship address-info]
  ::      +<
  ::  =+  lin-sar=(get-locked-stars 'linear')
  ::
  ::  Calculate direct planets
  ::
  =/  potential-direct-planets
    %+  skim
      ~(tap by addresses)
    |=  [who=ship address-info]
    ?&  (gte who 0x1.0000)
        (lth who 0x1.0000.0000.0000)
        ?=
          $?  %~marzod
              %~binzod
              %~wanzod
              %~samzod
          ==
        (^sein:title who)
    ==
  =/  direct-planets=(list [who=ship rights])
    %+  turn  potential-direct-planets
    |=  [who=ship address-info]
    ~|  +<
    :*  who
        (need owner)
        management
        voting
        transfer
        spawn
        ?:  |(?=(~ crypt) ?=(~ auth))
          ~
        `[u.crypt u.auth]
    ==
  ~&  [%direct-planets (lent direct-planets)]
  ~&  %+  turn  direct-planets
      |=  [=ship *]
      ship
  ::  ::
  ::  =+  con-rec=get-conditional-recipients
  ::  =+  con-gal=(get-locked-galaxies 'conditional')
  ::  =+  con-sar=(get-locked-stars 'conditional')
  ::  ::
  ::  ~&  'Deed data sanity check...'
  ::  =/  tlon-map=(map ship rights)
  ::    (~(gas by *(map ship rights)) tlon-gal)
  ::  =/  deed-map=(map ship rights)
  ::    (~(gas by *(map ship rights)) directs)
  ::  =/  star-map=(map ship (set ship))
  ::    %+  roll  directs
  ::    |=  [[who=ship *] smp=(map ship (set ship))]
  ::    ^+  smp
  ::    =+  par=(^sein:title who)
  ::    ~|  [%need-parent par %for who]
  ::    ?>  ?&  ?|  (~(has by tlon-map) par)
  ::                (~(has by deed-map) par)
  ::            ==
  ::          ::
  ::            ?=  ^
  ::            =<  net
  ::            %+  fall
  ::              (~(get by deed-map) par)
  ::            %+  fall
  ::              (~(get by tlon-map) par)
  ::            *rights
  ::        ==
  ::    %-  ~(put by smp)
  ::    ^-  [ship (set ship)]
  ::    ?+  (clan:title who)  !!
  ::      %king  [who (fall (~(get by smp) who) ~)]
  ::      %duke  :-  par
  ::             =+  sm=(fall (~(get by smp) par) ~)
  ::             (~(put in sm) who)
  ::    ==
  ::
  ::  contract deployment
  ::
  ::  ~&  'Deploying ships...'
  ::  =^  ships  this
  ::    (do-deploy 'azimuth' ~)
  ::  ~&  'Deploying polls...'
  ::  =^  polls  this
  ::    %+  do-deploy  'polls'
  ::    ~[uint+2.592.000 uint+2.592.000]
  ::  ~&  'Deploying claims...'
  ::  =^  claims  this
  ::    %+  do-deploy  'claims'
  ::    ~[address+ships]
  ::  ~&  'Deploying constitution-ceremony...'
  ::  =^  constit  this
  ::    %+  do-deploy  'ecliptic-ceremony'
  ::    :~  [%address 0x0]
  ::        [%address ships]
  ::        [%address polls]
  ::        [%address claims]
  ::    ==
  ::  =.  constitution  constit
  ::  ~&  'Transferring contract ownership...'
  ::  =.  this
  ::    %^  do  ships  50.000
  ::    (transfer-ownership:dat constit)
  ::  =.  this
  ::    %^  do  polls  50.000
  ::    (transfer-ownership:dat constit)
  ::  ~&  'Deploying linear-star-release...'
  ::  =^  linear-star-release  this
  ::    %+  do-deploy  'linear-star-release'
  ::    ~[address+ships]
  ::  ~&  'Deploying conditional-star-release...'
  ::B =^  conditional-star-release  this
  ::B   %+  do-deploy  'conditional-star-release'
  ::B   :~  [%address ships]
  ::B     ::
  ::B       :-  %array
  ::B       :~  [%bytes 32^`@`0x0]
  ::B         ::
  ::B           :+  %bytes  32
  ::B           ^-  @
  ::B           0xecd4.0bbe.04fd.f2a6.307d.9ec7.0b65.195b.
  ::B             cb4d.2f80.75b7.7e39.53d9.95c2.9fed.e2af
  ::B         ::
  ::B           :+  %bytes  32
  ::B           ^-  @
  ::B           0x1fac.fda9.4a86.63d5.6eb3.2a00.da16.5912.
  ::B             6d76.dbb4.f88a.0f27.6476.bde0.c115.ec13
  ::B       ==
  ::B     ::
  ::B       :-  %array
  ::B       :~  [%uint 1.516.089.540]  ::  2018-01-15 23:59:00 PST
  ::B           [%uint 1.547.625.540]  ::  2019-01-15 23:59:00 PST
  ::B           [%uint 1.579.161.540]  ::  2020-01-15 23:59:00 PST
  ::B       ==
  ::B     ::
  ::B       :-  %array
  ::B       :~  [%uint 1.547.625.540]  ::  2019-01-15 23:59:00 PST
  ::B           [%uint 1.579.161.540]  ::  2020-01-15 23:59:00 PST
  ::B           [%uint 1.610.783.940]  ::  2021-01-15 23:59:00 PST
  ::B       ==
  ::B   ==
  ::  ~&  'Deploying censures...'
  ::  =^  censures  this
  ::    %+  do-deploy  'censures'
  ::    ~[address+ships]
  ::  ~&  'Deploying delegated-sending...'
  ::  =^  delegated-sending  this
  ::    %+  do-deploy  'delegated-sending'
  ::    ~[address+ships]
  ::  ~&  'Deploying constitution-resolver...'
  ::  =^  constitution-resolver  this
  ::    %+  do-deploy  'ecliptic-resolver'
  ::    ~[address+ships]
  ::
  ::  tlon galaxy booting
  ::
  ::B ~&  ['Booting Tlon galaxies...' +(nonce)]
  ::B =/  galaxies  (sort tlon-gal order-shiplist)
  ::B |-
  ::B ?^  galaxies
  ::B   =.  this
  ::B     (create-ship [who ~ net]:i.galaxies)
  ::B   $(galaxies t.galaxies)
  ::
  ::  direct deeding
  ::
  ::B ~&  ['Directly deeding assets...' +(nonce)]
  ::B =/  stars  (sort ~(tap by star-map) order-shiplist)
  ::B |-
  ::B ?^  stars
  ::B   =*  star  p.i.stars
  ::B   ~&  [star=star nonce=nonce]
  ::B   =+  star-deed=(~(got by deed-map) star)
  ::B   =.  this
  ::B     (create-ship star ~ net.star-deed)
  ::B   ::
  ::B   =+  planets=(sort ~(tap in q.i.stars) lth)
  ::B   |-
  ::B   ?^  planets
  ::B     =*  planet  i.planets
  ::B     ~&  [planet=planet nonce=nonce]
  ::B     =+  plan-deed=(~(got by deed-map) planet)
  ::B     =.  this
  ::B       (create-ship planet ~ net.plan-deed)
  ::B     ::
  ::B     =.  this
  ::B       (send-ship planet [own manage voting spawn transfer]:plan-deed)
  ::B     $(planets t.planets)
  ::B   ::
  ::B   =.  this
  ::B     (send-ship star [own manage voting spawn transfer]:star-deed)
  ::B   ^$(stars t.stars)
  ::
  ::  linear release registration and deeding
  ::
  ~&  ['Registering linear release recipients...' +(nonce)]
  =+  lin-rec-list=~(tap by lin-rec)
  |-
  ?^  lin-rec-list
    =.  this
      %^  do  linear-star-release  350.000
      (register-linear:dat i.lin-rec-list)
    $(lin-rec-list t.lin-rec-list)
  ::
  ~&  ['Depositing linear release galaxies...' +(nonce)]
  =.  this
    (deposit-galaxies linear-star-release lin-gal)
  ::
  ::B ~&  ['Depositing linear release stars...' +(nonce)]
  ::B =.  this
  ::B   (deposit-stars linear-star-release lin-sar)
  ::B ::
  ::B ::  conditional release registration and deeding
  ::B ::
  ~&  ['Registering conditional release recipients...' +(nonce)]
  =+  con-rec-list=~(tap by con-rec)
  |-
  ?^  con-rec-list
    =.  this
      %^  do  conditional-star-release  350.000
      (register-conditional:dat i.con-rec-list)
    $(con-rec-list t.con-rec-list)
  ::
  ::  Register conditional stars
  ::
  ::CS  ~&  ['Registering conditional release stars...' +(nonce)]
  ::CS  |-
  ::CS  ?^  con-stars-rec
  ::CS    =.  this
  ::CS      %^  do  conditional-star-release  350.000
  ::CS      (register-conditional:dat i.con-stars-rec)
  ::CS    $(con-stars-rec t.con-stars-rec)
  ::CS  ::
  ::CS  ~&  ['Depositing conditional release galaxies...' +(nonce)]
  ::CS  =.  this
  ::CS    (deposit-galaxies conditional-star-release con-gal)
  ::
  ::  Deploy directly-deeded galaxies
  ::
  ~&  ['Booting directly-deeded galaxies...' +(nonce)]
  =/  galaxies  (sort direct-galaxies order-shiplist)
  |-
  ?^  galaxies
    =.  this
      (create-ship [who ~ net]:i.galaxies)
    =.  this
      (send-ship who.i.galaxies [own manage voting spawn transfer]:i.galaxies)
    $(galaxies t.galaxies)
  ::
  ::  Deploy ~{,mar,wan,bin,sam}zod
  ::
  |-
  ?^  tmp-points
    =.  this
      (create-ship i.tmp-points)
    $(tmp-points t.tmp-points)
  ::O ?^  old-tmp-points
  ::O   =?  this  ?=(^ net.i.old-tmp-points)
  ::O     %^  do  constitution  300.000
  ::O     (configure-keys:dat [who u.net]:i.old-tmp-points)
  ::O   =.  this
  ::O     (send-ship [who send]:i.old-tmp-points)
  ::O   $(old-tmp-points t.old-tmp-points)
  ::
  ::  Deploy conditional stars
  ::
  ::CS  ~&  ['Deploying conditional stars...' +(nonce)]
  =/  conditional-star-gals=(list @p)
    :~  ~pub
        ~sud
        ~pem
        ~dev
        ~lur
        ~ten
        ~bus
        ~rel
        ~rud
        ~nes
        ~nus
        ~feb
        ~def
        ~fet
    ==
    ::  :~  ~rel
    ::      ~rud
    ::      ~nes
    ::      ~fet
    ::  ==
  ::SS  |-
  ::SS  ?^  conditional-star-gals
  ::SS    =.  this
  ::SS      %^  do  constitution  300.000
  ::SS      %+  set-spawn-proxy:dat  i.conditional-star-gals
  ::SS      special-spawn-proxy
  ::SS    $(conditional-star-gals t.conditional-star-gals)
  ::CS  =.  this
  ::CS    (deposit-stars conditional-star-release con-stars)
  ::
  ::  Register linear stars
  ::
  ::LS  ~&  ['Registering linear release stars...' +(nonce)]
  ::LS  |-
  ::LS  ?^  lin-stars-rec
  ::LS    =.  this
  ::LS      %^  do  linear-star-release  350.000
  ::LS      (register-linear:dat i.lin-stars-rec)
  ::LS    $(lin-stars-rec t.lin-stars-rec)
  ::LS  ::
  ::LS  ::  Deploy linear stars
  ::LS  ::
  ::LS  ~&  ['Deploying linear stars...' +(nonce)]
  ::LS  =/  linear-star-gals=(list @p)
  ::LS    :~  ~ten
  ::LS        ~pub
  ::LS        ~sud
  ::LS        ~pem
  ::LS        ~dev
  ::LS        ~lur
  ::LS        ~def
  ::LS        ~bus
  ::LS    ==
  ::LS    ::  ~
  ::LS    ::  ~[~feb]
  ::LS  |-
  ::LS  ?^  linear-star-gals
  ::LS    =.  this
  ::LS      %^  do  constitution  300.000
  ::LS      %+  set-spawn-proxy:dat  i.linear-star-gals
  ::LS      linear-star-release
  ::LS    $(linear-star-gals t.linear-star-gals)
  ::LS  =.  this
  ::LS    (deposit-stars linear-star-release lin-stars)
  ::
  ::  Deploy direct stars
  ::
  ~&  ['Deploying direct stars...' +(nonce)]
  =+  stars=(sort direct-stars |=([[@ *] [@ *]] (lth +<-< +<+<)))
  |-
  ?^  stars
    =*  star  i.stars
    ~&  [star=star nonce=nonce]
    =.  this
      (create-ship [who ~ net]:star)
    ::
    =.  this
      (send-ship [who own manage voting spawn transfer]:star)
    $(stars t.stars)
  ::
  ::  Deploy direct planets
  ::
  ~&  ['Deploying direct planets...' +(nonce)]
  =+  planets=(sort direct-planets |=([[@ *] [@ *]] (lth +<-< +<+<)))
  |-
  ?^  planets
    =*  planet  i.planets
    ~&  [planet=planet nonce=nonce]
    =.  this
      (create-ship [who ~ net]:planet)
    ::
    =.  this
      (send-ship [who own manage voting spawn transfer]:planet)
    $(planets t.planets)
  ::
  =/  zod-net
    :-  (hex-to-num '0xf9cfce9e358657c2f2a1ea36f76ac88891ae70ffb14aff9a032f4e645ac13cb2')
        (hex-to-num '0xf67337ea88a5ef905ab65ea29a5ba3858519dc297e6df10bf52649df057c7c5a')
  =/  marzod-net
    :-  (hex-to-num '0xf68b1ef787a6e35af4b62866f663c75f729dc3f3eed90997ffd272d5abe04a0d')
        (hex-to-num '0xe6acb4951c84d3cd1bff58acad5bd70e67ce00c228ed60c13470229b4a6db052')
  =/  samzod-net
    :-  (hex-to-num '0x679ef9fcf5448b49c4603fdb57dbb8817a8f132321ecb780c02cad74b423cf89')
        (hex-to-num '0x74d6e0960db8196809edfe9a999eaf5e2f3b1e34e9ed65266f1bf735d8dc4dc9')
  =/  wanzod-net
    :-  (hex-to-num '0x9170fe0f4f42a7ce1af7688df88100bd40c080ee962b59e57b26ac7bf974b9d5')
        (hex-to-num '0x7b91da6fe334b10ccf11a7b5cba2f8af7d65f4f017bf19a5f7f3c41dfcb2a257')
  =/  binzod-net
    :-  (hex-to-num '0xe3cab27e170f469ae41b73335e527085fb5aa970c35677c87e31442703854518')
        (hex-to-num '0xa9f9aef56cb492111049e0ee86fe1e916742c67989c106d1c81ee80d081eac81')
  =.  this
    %^  do  constitution  300.000
    (configure-keys:dat ~zod zod-net)
  =.  this
    %^  do  constitution  300.000
    (configure-keys:dat ~marzod marzod-net)
  =.  this
    %^  do  constitution  300.000
    (configure-keys:dat ~samzod samzod-net)
  =.  this
    %^  do  constitution  300.000
    (configure-keys:dat ~wanzod wanzod-net)
  =.  this
    %^  do  constitution  300.000
    (configure-keys:dat ~binzod binzod-net)
  ::
  ::  Voting
  ::
  =/  vote-gals=(list ship)
    :~  ::  ~zod
        ::  ~bus
        ::  ~def
        ::  ~dev
        ::  ~lur
        ::  ~pem
        ::  ~pub
        ::  ~sud
        ::  ~ten
        ::  ~nus
        ::  ~feb
        ::  ~fet
        ::  ~nes
        ::  ~rel
        ::  ~rud
        ~wel
    ==
  ::V0  =.  this
  ::V0    %^  do  constitution  300.000
  ::V0    (start-document-poll:dat ~zod doc-0)
  ::V0  |-
  ::V0  ?^  vote-gals
  ::V0    =.  this
  ::V0      %^  do  constitution  300.000
  ::V0      (cast-document-vote:dat i.vote-gals doc-0 &)
  ::V0    $(vote-gals t.vote-gals)
  ::V1  =.  this
  ::V1    %^  do  constitution  300.000
  ::V1    (start-document-poll:dat ~zod doc-1)
  |-
  ?^  vote-gals
    =.  this
      %^  do  constitution  300.000
      (cast-document-vote:dat i.vote-gals doc-1 &)
    $(vote-gals t.vote-gals)
  ::V2  =.  this
  ::V2    %^  do  constitution  300.000
  ::V2    (start-document-poll:dat ~zod doc-2)
  ::V2  |-
  ::V2  ?^  vote-gals
  ::V2    =.  this
  ::V2      %^  do  constitution  300.000
  ::V2      (cast-document-vote:dat i.vote-gals doc-2 &)
  ::V2    $(vote-gals t.vote-gals)
  =/  tlon-vote-gals=(list ship)
    :~  ~bec  ~bel  ~bep  ~ber  ~bet  ~bex  ~byl  ~byr  ~byt  ~deb
        ~deg  ~dem  ~des  ~det  ~duc  ~dun  ~dus  ~dut  ~dux  ~dyl
        ~dyn  ~dyt  ~fel  ~fen  ~fex  ~fur  ~fyl  ~hul  ~hus  ~hut
        ~lec  ~len  ~lep  ~ler  ~lev  ~luc  ~lud  ~lyn  ~lyr  ~lys
        ~lyx  ~meb  ~mec  ~med  ~meg  ~mel  ~mes  ~mex  ~mug  ~mun
        ~mur  ~myl  ~myn  ~myr  ~neb  ~ned  ~nel  ~ner  ~nev  ~nub
        ~nux  ~nyd  ~nyl  ~nys  ~pec  ~pel  ~pex  ~rec  ~rem  ~ren
        ~res  ~ret  ~rev  ~rux  ~ryc  ~ryd  ~ryl  ~rym  ~ryn  ~seb
        ~sed  ~seg  ~sen  ~set  ~sug  ~sur  ~syl  ~syp  ~tec  ~ted
        ~teg  ~tel  ~ter  ~tes  ~tex  ~tuc  ~tud  ~tun  ~tus  ~tyn
        ~wed  ~weg  ~wer  ~wet  ~wyt  ::  ~wel
    ==
  ::TV1 |-
  ::TV1 ?^  tlon-vote-gals
  ::TV1   =.  this
  ::TV1     %^  do  constitution  300.000
  ::TV1     (cast-document-vote:dat i.tlon-vote-gals doc-1 &)
  ::TV1   $(tlon-vote-gals t.tlon-vote-gals)
  ::B ~&  ['Depositing conditional release stars...' +(nonce)]
  ::B =.  this
  ::B   (deposit-stars conditional-star-release con-sar)
  ::B ::
  ::B ::  tlon galaxy sending
  ::B ::
  ::B ~&  ['Sending Tlon galaxies...' +(nonce)]
  ::B =/  galaxies  (sort tlon-gal order-shiplist)
  ::B |-
  ::B ?^  galaxies
  ::B   =.  this
  ::B     (send-ship [who own manage voting spawn transfer]:i.galaxies)
  ::B   $(galaxies t.galaxies)
  ::B ::
  ::B ::  concluding ceremony
  ::B ::
  ::B ~&  ['Deploying constitution-final...' +(nonce)]
  ::B =^  constit-final  this
  ::B   %+  do-deploy  'ecliptic-final'
  ::B   :~  [%address constitution]
  ::B       [%address ships]
  ::B       [%address polls]
  ::B       [%address claims]
  ::B   ==
  ::B =.  this
  ::B   ::NOTE  currently included bytecode has on-upgrade ens functionality
  ::B   ::      stripped out to make this not fail despite 0x0 dns contract
  ::B   %^  do  constitution  300.000
  ::B   (upgrade-to:dat constit-final)
  ::B ::
  ::B =.  this
  ::B   %^  do  constit-final  300.000
  ::B   (set-dns-domains:dat "urbit.org" "urbit.org" "urbit.org")
  ::
  ::LL  ~&  linear-lockup-addresses=linear-lockup-addresses
  ::LL  ~&  conditional-lockup-addresses=conditional-lockup-addresses
  complete
::
::  sign pre-generated transactions
++  sign
  |=  [won=@da in=path key=path gas=(unit @ud)]
  ^-  (list cord)
  =.  now  won
  ?>  ?=([@ @ @ *] key)
  =/  pkf  (get-file t.t.t.key)
  ?>  ?=(^ pkf)
  =/  pk  (rash i.pkf ;~(pfix (jest '0x') hex))
  =/  txs  .^((list transaction) %cx in)
  =/  enumerated
    =/  n  1
    |-  ^-  (list [@ud transaction])
    ?~  txs
      ~
    [[n i.txs] $(n +(n), txs t.txs)]
  %+  turn  enumerated
  |=  [n=@ud tx=transaction]
  ~?  =(0 (mod n 100))  [%signing n]
  =?  gas-price.tx  ?=(^ gas)  u.gas
  (crip '0' 'x' ((x-co:co 0) (sign-transaction:key:ethereum tx pk)))
::
::  create or spawn a ship, configure its spawn proxy and pubkeys
++  create-ship
  |=  $:  who=ship
          spawn=(unit address)
          keys=(unit [@ux @ux])
      ==
  ^+  this
  =+  wat=(clan:title who)
  =*  do-c  (cury (cury do constitution) 300.000)
  =.  this
    ?:  |(=(~teb who) =(~sibfus-dosryp who))  ::  rescued
      this
    ?:  ?=(%czar wat)
      (do-c (create-galaxy:dat who))
    (do-c (spawn:dat who))
  =?  this  &(?=(^ spawn) !?=(%duke wat))
    (do-c (set-spawn-proxy:dat who u.spawn))
  =?  this  ?=(^ keys)
    (do-c (configure-keys:dat who u.keys))
  this
::
::  transfer a ship to a new owner, set a transfer proxy
++  send-ship
  |=  $:  who=ship
          own=address
          manage=(unit address)
          voting=(unit address)
          spawn=(unit address)
          transfer=(unit address)
      ==
  ^+  this
  =+  wat=(clan:title who)
  =*  do-c  (cury (cury do constitution) 300.000)
  =?  this  ?=(^ manage)
    (do-c (set-management-proxy:dat who u.manage))
  =?  this  &(?=(^ voting) ?=(%czar wat))
    (do-c (set-voting-proxy:dat who u.voting))
  =?  this  &(?=(^ spawn) !?=(%duke wat))
    (do-c (set-spawn-proxy:dat who u.spawn))
  =?  this  ?=(^ transfer)
    (do-c (set-transfer-proxy:dat who u.transfer))
  =.  this
    (do-c (transfer-point:dat who own))
  this
::
::  deposit a whole galaxy into a star release contract
++  deposit-galaxies
  |=  [into=address galaxies=(list [gal=ship rights])]
  ^+  this
  =.  galaxies  (sort galaxies order-shiplist)
  |-
  ?~  galaxies  this
  ~&  [(lent galaxies) 'galaxies remaining' nonce]
  =*  galaxy  gal.i.galaxies
  ~&  `@p`galaxy
  =*  gal-deed  i.galaxies
  ::
  ::  create the galaxy, with spawn proxy set to the lockup contract
  =.  this
    ~|  [%locked-galaxy-needs-network-keys galaxy]
    ~!  net.gal-deed
    ?>  ?=(^ net.gal-deed)
    (create-ship galaxy `into net.gal-deed)
  ::
  ::  deposit all its stars
  =+  stars=(gulf 1 255)
  |-
  ?^  stars
    =?  this  =(~teb galaxy)  ::  rescued
      %^  do  constitution  300.000
      %-  set-transfer-proxy:dat
      [(cat 3 galaxy i.stars) into]
    =.  this
      %^  do  into  350.000
      %-  deposit:dat
      [own.gal-deed (cat 3 galaxy i.stars)]
    $(stars t.stars)
  ::
  ::  send the galaxy to its owner, with spawn proxy at zero
  ::  because it can't spawn anymore
  =.  this
    (send-ship galaxy [own manage voting ?~(spawn `0x0 spawn) transfer]:gal-deed)
  ^$(galaxies t.galaxies)
::
::  deposit a list of stars
++  deposit-stars
  |=  [into=address stars=(list [who=ship recipient=address])]
  ^+  this
  =.  stars  (sort stars order-shiplist)
  =|  gals=(set ship)
  |-
  ?~  stars  this
  =*  star  who.i.stars
  =*  to  recipient.i.stars
  =.  this
    %^  do  into  550.000
    (deposit:dat to star)
  $(stars t.stars)
::
::  call data generation
::TODO  most of these should later be cleaned and go in ++constitution
::
++  dat
  |%
  ++  enc
    |*  cal=$-(* call-data)
    (cork cal encode-call)
  ::
  ++  create-galaxy           (enc create-galaxy:cal)
  ++  spawn                   (enc spawn:cal)
  ++  configure-keys          (enc configure-keys:cal)
  ++  set-spawn-proxy         (enc set-spawn-proxy:cal)
  ++  transfer-point          (enc transfer-point:cal)
  ++  set-management-proxy    (enc set-management-proxy:cal)
  ++  set-voting-proxy        (enc set-voting-proxy:cal)
  ++  set-transfer-proxy      (enc set-transfer-proxy:cal)
  ++  set-dns-domains         (enc set-dns-domains:cal)
  ++  upgrade-to              (enc upgrade-to:cal)
  ++  transfer-ownership      (enc transfer-ownership:cal)
  ++  register-linear         (enc register-linear:cal)
  ++  register-conditional    (enc register-conditional:cal)
  ++  deposit                 (enc deposit:cal)
  ++  start-document-poll     (enc start-document-poll:cal)
  ++  cast-document-vote      (enc cast-document-vote:cal)
  --
::
++  cal
  |%
  ++  create-galaxy
    |=  gal=ship
    ^-  call-data
    ?>  =(%czar (clan:title gal))
    :-  'createGalaxy(uint8,address)'
    ^-  (list data)
    :~  [%uint `@`gal]
        [%address addr]
    ==
  ::
  ++  spawn
    |=  who=ship
    ^-  call-data
    ?>  ?=(?(%king %duke) (clan:title who))
    :-  'spawn(uint32,address)'
    :~  [%uint `@`who]
        [%address addr]
    ==
  ::
  ++  configure-keys
    |=  [who=ship crypt=@ auth=@]
    ?>  (lte (met 3 crypt) 32)
    ?>  (lte (met 3 auth) 32)
    :-  'configureKeys(uint32,bytes32,bytes32,uint32,bool)'
    :~  [%uint `@`who]
        [%bytes-n 32^crypt]
        [%bytes-n 32^auth]
        [%uint 1]
        [%bool |]
    ==
  ::
  ++  set-management-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setManagementProxy(uint32,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  set-voting-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setVotingProxy(uint8,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  set-spawn-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setSpawnProxy(uint16,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  transfer-point
    |=  [who=ship to=address]
    ^-  call-data
    :-  'transferPoint(uint32,address,bool)'
    :~  [%uint `@`who]
        [%address to]
        [%bool |]
    ==
  ::
  ++  set-transfer-proxy
    |=  [who=ship proxy=address]
    ^-  call-data
    :-  'setTransferProxy(uint32,address)'
    :~  [%uint `@`who]
        [%address proxy]
    ==
  ::
  ++  set-dns-domains
    |=  [pri=tape sec=tape ter=tape]
    ^-  call-data
    :-  'setDnsDomains(string,string,string)'
    :~  [%string pri]
        [%string sec]
        [%string ter]
    ==
  ::
  ++  upgrade-to
    |=  to=address
    ^-  call-data
    :-  'upgradeTo(address)'
    :~  [%address to]
    ==
  ::
  ::
  ++  transfer-ownership  ::  of contract
    |=  to=address
    ^-  call-data
    :-  'transferOwnership(address)'
    :~  [%address to]
    ==
  ::
  ::
  ++  register-linear
    |=  $:  to=address
            windup=@ud
            stars=@ud
            rate=@ud
            rate-unit=@ud
        ==
    ^-  call-data
    ~&  [%register-linear stars to]
    :-  'register(address,uint256,uint16,uint16,uint256)'
    :~  [%address to]
        [%uint windup]
        [%uint stars]
        [%uint rate]
        [%uint rate-unit]
    ==
  ::
  ++  register-conditional
    |=  $:  to=address
            b1=@ud
            b2=@ud
            b3=@ud
            rate=@ud
            rate-unit=@ud
        ==
    ^-  call-data
    :-  'register(address,uint16[],uint16,uint256)'
    :~  [%address to]
        [%array ~[uint+b1 uint+b2 uint+b3]]
        [%uint rate]
        [%uint rate-unit]
    ==
  ::
  ++  deposit
    |=  [to=address star=ship]
    ^-  call-data
    :-  'deposit(address,uint16)'
    :~  [%address to]
        [%uint `@`star]
    ==
  ::
  ++  start-document-poll
    |=  [as=ship document=@]
    ^-  call-data
    :-  'startDocumentPoll(uint8,bytes32)'
    :~  [%uint `@`as]
        [%bytes-n 32^document]
    ==
  ::
  ++  cast-document-vote
    |=  [as=ship document=@ vote=?]
    ^-  call-data
    :-  'castDocumentVote(uint8,bytes32,bool)'
    :~  [%uint `@`as]
        [%bytes-n 32^document]
        [%bool vote]
    ==
  --
--
