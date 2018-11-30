!:
=,  ethe
=,  ethereum
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
      extra=tape
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
  (cook (unit address) ;~(pose (cook |=(* ~) (jest '""')) (stag ~ zero-ux)))
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
  ^-  %-  list
      $:  recipient=address
          windup=@ud
          stars=@ud
          rate=@ud
          rate-unit=@ud
      ==
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
  ^-  %-  list
      $:  recipient=address
          b1=@ud
          b2=@ud
          b3=@ud
          rate=@ud
          rate-unit=@ud
      ==
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
  =+  tlon-gal=get-direct-galaxies
  =+  directs=get-direct-ships
  ::
  =+  lin-rec=get-linear-recipients
  =+  lin-gal=(get-locked-galaxies 'linear')
  =+  ships=(hex-to-num '0x223c067f8cf28ae173ee5cafea60ca44c335fecb')
  =+  polls=(hex-to-num '0x7fecab617c868bb5996d99d95200d2fa708218e4')
  =+  claims=(hex-to-num '0xe7e7f69b34d7d9bd8d61fb22c33b22708947971a')
  =+  linear-star-release=(hex-to-num '0x86cd9cd0992f04231751e3761de45cecea5d1801')
  =.  constitution  (hex-to-num '0x12778371a6aa58b1dd623c126e09cd28fc5b9b5c')
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
  =/  lockups=(map ship lockup-info)
    get-lockups
  ~&  lockups-wyt=~(wyt by lockups)
  ~&  lockups=lockups
  ::  ~&  :-  %galaxies
  ::  %+  skim
  ::    ~(tap by addresses)
  ::  |=  [who=ship address-info]
  ::  (lth who ~marzod)
  ::Z =/  lin-gal=(list [ship rights[)
  ::Z   %+  murn
  ::Z     lin-rec
  ::Z   |=  [who=ship address-info]
  ::Z   =+  addr=(~(get by all-addr) who)
  ::Z   ?~  addr
  ::Z     ~
  ::Z   `[who `rights`[(need owner) management voting transfer spawn net]]
  =+  lin-sar=(get-locked-stars 'linear')
  ::
  =+  con-rec=get-conditional-recipients
  =+  con-gal=(get-locked-galaxies 'conditional')
  =+  con-sar=(get-locked-stars 'conditional')
  ::
  ~&  'Deed data sanity check...'
  =/  tlon-map=(map ship rights)
    (~(gas by *(map ship rights)) tlon-gal)
  =/  deed-map=(map ship rights)
    (~(gas by *(map ship rights)) directs)
  =/  star-map=(map ship (set ship))
    %+  roll  directs
    |=  [[who=ship *] smp=(map ship (set ship))]
    ^+  smp
    =+  par=(^sein:title who)
    ~|  [%need-parent par %for who]
    ?>  ?&  ?|  (~(has by tlon-map) par)
                (~(has by deed-map) par)
            ==
          ::
            ?=  ^
            =<  net
            %+  fall
              (~(get by deed-map) par)
            %+  fall
              (~(get by tlon-map) par)
            *rights
        ==
    %-  ~(put by smp)
    ^-  [ship (set ship)]
    ?+  (clan:title who)  !!
      %king  [who (fall (~(get by smp) who) ~)]
      %duke  :-  par
             =+  sm=(fall (~(get by smp) par) ~)
             (~(put in sm) who)
    ==
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
  =^  conditional-star-release  this
    %+  do-deploy  'conditional-star-release'
    :~  [%address ships]
      ::
        :-  %array
        :~  [%bytes 32^`@`0x0]
          ::
            :+  %bytes  32
            ^-  @
            0xecd4.0bbe.04fd.f2a6.307d.9ec7.0b65.195b.
              cb4d.2f80.75b7.7e39.53d9.95c2.9fed.e2af
          ::
            :+  %bytes  32
            ^-  @
            0x1fac.fda9.4a86.63d5.6eb3.2a00.da16.5912.
              6d76.dbb4.f88a.0f27.6476.bde0.c115.ec13
        ==
      ::
        :-  %array
        :~  [%uint 1.516.089.540]  ::  2018-01-15 23:59:00 PST
            [%uint 1.547.625.540]  ::  2019-01-15 23:59:00 PST
            [%uint 1.579.161.540]  ::  2020-01-15 23:59:00 PST
        ==
      ::
        :-  %array
        :~  [%uint 1.547.625.540]  ::  2019-01-15 23:59:00 PST
            [%uint 1.579.161.540]  ::  2020-01-15 23:59:00 PST
            [%uint 1.610.783.940]  ::  2021-01-15 23:59:00 PST
        ==
    ==
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
  ~&  ['Booting Tlon galaxies...' +(nonce)]
  =/  galaxies  (sort tlon-gal order-shiplist)
  |-
  ?^  galaxies
    =.  this
      (create-ship [who ~ net]:i.galaxies)
    $(galaxies t.galaxies)
  ::
  ::  direct deeding
  ::
  ~&  ['Directly deeding assets...' +(nonce)]
  =/  stars  (sort ~(tap by star-map) order-shiplist)
  |-
  ?^  stars
    =*  star  p.i.stars
    ~&  [star=star nonce=nonce]
    =+  star-deed=(~(got by deed-map) star)
    =.  this
      (create-ship star ~ net.star-deed)
    ::
    =+  planets=(sort ~(tap in q.i.stars) lth)
    |-
    ?^  planets
      =*  planet  i.planets
      ~&  [planet=planet nonce=nonce]
      =+  plan-deed=(~(got by deed-map) planet)
      =.  this
        (create-ship planet ~ net.plan-deed)
      ::
      =.  this
        (send-ship planet [own manage voting spawn transfer]:plan-deed)
      $(planets t.planets)
    ::
    =.  this
      (send-ship star [own manage voting spawn transfer]:star-deed)
    ^$(stars t.stars)
  ::
  ::  linear release registration and deeding
  ::
  ::A~&  ['Registering linear release recipients...' +(nonce)]
  ::A|-
  ::A?^  lin-rec
  ::A  =.  this
  ::A    %^  do  linear-star-release  350.000
  ::A    (register-linear:dat i.lin-rec)
  ::A  $(lin-rec t.lin-rec)
  ::A::
  ::A~&  ['Depositing linear release galaxies...' +(nonce)]
  ::A=.  this
  ::A  (deposit-galaxies linear-star-release lin-gal)
  ::
  ~&  ['Depositing linear release stars...' +(nonce)]
  =.  this
    (deposit-stars linear-star-release lin-sar)
  ::
  ::  conditional release registration and deeding
  ::
  ~&  ['Registering conditional release recipients...' +(nonce)]
  |-
  ?^  con-rec
    =.  this
      %^  do  conditional-star-release  350.000
      (register-conditional:dat i.con-rec)
    $(con-rec t.con-rec)
  ::
  ~&  ['Depositing conditional release galaxies...' +(nonce)]
  =.  this
    (deposit-galaxies conditional-star-release con-gal)
  ::
  ~&  ['Depositing conditional release stars...' +(nonce)]
  =.  this
    (deposit-stars conditional-star-release con-sar)
  ::
  ::  tlon galaxy sending
  ::
  ~&  ['Sending Tlon galaxies...' +(nonce)]
  =/  galaxies  (sort tlon-gal order-shiplist)
  |-
  ?^  galaxies
    =.  this
      (send-ship [who own manage voting spawn transfer]:i.galaxies)
    $(galaxies t.galaxies)
  ::
  ::  concluding ceremony
  ::
  ~&  ['Deploying constitution-final...' +(nonce)]
  =^  constit-final  this
    %+  do-deploy  'ecliptic-final'
    :~  [%address constitution]
        [%address ships]
        [%address polls]
        [%address claims]
    ==
  =.  this
    ::NOTE  currently included bytecode has on-upgrade ens functionality
    ::      stripped out to make this not fail despite 0x0 dns contract
    %^  do  constitution  300.000
    (upgrade-to:dat constit-final)
  ::
  =.  this
    %^  do  constit-final  300.000
    (set-dns-domains:dat "urbit.org" "urbit.org" "urbit.org")
  ::
  complete
::
::  sign pre-generated transactions
++  sign
  |=  [won=@da in=path key=path]
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
  (crip '0' 'x' ((x-co:co 0) (sign-transaction tx pk)))
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
  ::
  ::  if the parent galaxy hasn't made the target contracts
  ::  a spawn proxy yet, do so now
  =+  par=(^sein:title star)
  =?  this  !(~(has in gals) par)
    =.  gals  (~(put in gals) par)
    %^  do  constitution  300.000
    %+  set-spawn-proxy:dat  par
    into
  ::
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
  --
--
