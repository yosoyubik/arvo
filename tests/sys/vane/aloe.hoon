/+  *test
::
/=  aloe-raw  /:  /===/sys/vane/aloe
              /!noun/
::
=/  test-pit=vase  !>(..zuse)
=/  aloe-gate  (aloe-raw test-pit)
::  some common test fixtures
::
=/  fix
  =/  our  ~nul
  =/  our-life=life  2
  =/  our-crub  (pit:nu:crub:crypto 512 (shaz 'Alice'))
  =/  our-private-key=ring  sec:ex:our-crub
  =/  our-public-key=pass  pub:ex:our-crub
  ::
  =/  her  ~doznec-doznec
  =/  her-life=life  3
  =/  her-crub  (pit:nu:crub:crypto 512 (shaz 'Bob'))
  =/  her-private-key=ring  sec:ex:her-crub
  =/  her-public-key=pass  pub:ex:her-crub
  ::
  :*  now=~2222.2.2
      eny=0xdead.beef
      ::
      our=our
      our-life=our-life
      our-crub=our-crub
      our-private-key=our-private-key
      our-public-key=our-public-key
      our-sponsors=~
      ::
      her=her
      her-life=her-life
      her-crub=her-crub
      her-private-key=her-private-key
      her-public-key=her-public-key
      her-sponsors=~[~marzod ~zod]
      ::
      her-public-keys=(my [her-life her-public-key]~)
  ==
::
=/  aloe  (aloe-gate our.fix now.fix `@`eny.fix scry=*sley)
::
|%
++  test-packet-encoding  ^-  tang
  ::
  =/  =packet:aloe
    [[to=~nec from=~doznec-doznec] encoding=%none payload=(jam [42 43])]
  ::
  =/  encoded  (encode-packet:aloe packet)
  =/  decoded  (decode-packet:aloe encoded)
  ::
  %+  expect-eq
    !>  packet
    !>  decoded
::
++  test-interpret-packet-none  ^-  tang
  ::
  =/  test-meal=meal:aloe  [%bond [1 1] /remote-route [%foo %bar]]
  ::
  =/  formatted=none:packet-format:aloe  raw-payload=(jam test-meal)
  ::
  =/  interpreted
    %.  [%none formatted]
    %-  interpret-packet:aloe  :*
      her.fix
      our-crub.fix
      ^-  pipe:aloe
      [fast-key=~ `her-life.fix her-public-keys.fix her-sponsors.fix]
    ==
  ::
  %+  expect-eq
    !>  [gifts=~ authenticated=%.n meal=test-meal]
    !>  interpreted
::
++  test-interpret-packet-open-no-deed  ^-  tang
  ::
  =/  test-meal=meal:aloe  [%bond [1 1] /remote-route [%foo %bar]]
  =/  jammed-meal=@        (jam test-meal)
  =/  signed-payload=@     (sign:as:her-crub.fix jammed-meal)
  ::
  =/  formatted=open:packet-format:aloe
    [from=her-life.fix deed=~ signed-payload]
  ::
  =/  packet-interpreter
    %-  interpret-packet:aloe  :*
      her.fix
      our-crub.fix
      ^-  pipe:aloe
      [fast-key=~ `her-life.fix her-public-keys.fix her-sponsors.fix]
    ==
  ::
  =/  interpreted
    %-  packet-interpreter
    [%open (jam formatted)]
  ::
  %+  expect-eq
    !>  [gifts=~ authenticated=%.y meal=test-meal]
    !>  interpreted
::
++  test-interpret-packet-fast  ^-  tang
  ::
  =/  test-meal=meal:aloe            [%bond [1 1] /remote-route [%foo %bar]]
  =/  jammed-meal=@                  (jam test-meal)
  =/  =symmetric-key:aloe            `@uvI`0xbeef.cafe
  =/  hashed-key=key-hash:aloe       (shaf %hand symmetric-key)
  ::
  =/  encrypted=@  (en:crub:crypto `@J`symmetric-key jammed-meal)
  =/  formatted=@  (cat 7 hashed-key encrypted)
  ::
  =/  packet-interpreter
    %-  interpret-packet:aloe  :*
      her.fix
      our-crub.fix
      ^-  pipe:aloe
      :*  :-  ~
          :+  key-hash=`@uvH`hashed-key
            expiration-date=`@da`(add ~d1 now.fix)
          value=symmetric-key
      ::
          `her-life.fix
          her-public-keys.fix
          her-sponsors.fix
      ==
    ==
  ::
  =/  interpreted
    %-  packet-interpreter
    [%fast formatted]
  ::
  %+  expect-eq
    !>  [gifts=~ authenticated=& meal=test-meal]
    !>  interpreted
::
++  test-interpret-packet-full  ^-  tang
  ::
  =/  test-meal=meal:aloe  [%bond [1 1] /remote-route [%foo %bar]]
  =/  jammed-meal=@        (jam test-meal)
  =/  =symmetric-key:aloe  (shaz %symmetric-key-foo)
  =/  jammed-message=@     (jam symmetric-key jammed-meal)
  =/  encrypted=@  (seal:as:her-crub.fix our-public-key.fix jammed-message)
  ::
  =/  formatted=full:packet-format:aloe
    [[to=our-life.fix from=her-life.fix] deed=~ encrypted]
  ::
  =/  packet-interpreter
    %-  interpret-packet:aloe  :*
      her.fix
      our-crub.fix
      ^-  pipe:aloe
      [fast-key=~ `her-life.fix her-public-keys.fix her-sponsors.fix]
    ==
  ::
  =/  interpreted
    %-  packet-interpreter
    [%full (jam formatted)]
  ::
  %+  expect-eq
    !>  :+  ^=  gifts
            :~  [%symmetric-key symmetric-key]
                [%meet her her-life her-public-key]:fix
            ==
          authenticated=%.y
        meal=test-meal
    ::
    !>  interpreted
::
::  +encode-meal tests
::
++  test-encode-meal-carp  ^-  tang
  ::
  =/  encoder
    %-  encode-meal:aloe  :*
      our.fix
      our-life.fix
      our-crub.fix
      her.fix
      ^-  pipe:aloe
      [fast-key=~ `her-life.fix her-public-keys.fix her-sponsors.fix]
    ==
  ::
  =/  packet      %packet-foo
  =/  =meal:aloe  [%carp *message-descriptor:aloe 42 packet]
  =/  result1
    %-  encoder
    [now.fix eny.fix meal]
  ::
  =/  spat  (encode-packet:aloe [our.fix her.fix] %none (jam meal))
  ::
  %+  expect-eq
    !>  [~ ~[spat]]
    !>  result1
::
++  test-encode-meal-bond-full  ^-  tang
  ::
  =/  encoder
    %-  encode-meal:aloe  :*
      our.fix
      our-life.fix
      our-crub.fix
      her.fix
      ^-  pipe:aloe
      [fast-key=~ `her-life.fix her-public-keys.fix her-sponsors.fix]
    ==
  ::
  =/  message     [%message (gulf 1 2.000)]
  =/  =meal:aloe  [%bond [0 0] /remote/route message]
  =/  result1
    %-  encoder
    [now.fix eny.fix meal]
  ::
  =/  actual-gifts=(list gift:encode-meal:aloe)  gifts.result1
  =/  actual-fragments=(list @)                  fragments.result1
  ::
  =/  sit  (sign:as:our-crub.fix (jam meal))
  =/  wrapper-meal=meal:aloe
    [%carp [[0 0] 2 1] 0 sit]
  ::
  =/  maj  (jam wrapper-meal)
  ::
  =/  spat  (encode-packet:aloe [our.fix her.fix] %open maj)
  ::
  ;:  weld
    %+  expect-eq
      !>  6
      !>  (lent actual-fragments)
  ::
    ::  checking against 1.024 fails because header brings it just over that
    ::
    %+  expect-eq
      !>  %.y
      !>  (levy actual-fragments |=(@ (lte (met 3 +<) 1.100)))
  ::  make sure all but the last packet are actually large enough
  ::
    %+  expect-eq
      !>  %.y
      !>  (levy (tail (flop actual-fragments)) |=(@ (gte (met 3 +<) 1.000)))
  ::
    %+  expect-eq
      !>  1
      !>  (lent actual-gifts)
  ::
    %+  expect-eq
      !>  %symmetric-key
      !>  ?>(?=(^ actual-gifts) -.i.actual-gifts)
  ==
::
::  TODO reinstate when handling moons and comets
::++  test-encode-meal-full-emit-gift  ^-  tang
::  ::  set the foreign life to 3 so we can test the %full case
::  ::
::  =.  cur.pipe.fix  `1
::  ?>  ?=(^ cur.pipe.fix)
::  ::
::  =/  encoder
::    %-  encode-meal:xmas-gate
::    [our.fix her.fix life.fix rings.fix pipe.fix]
::  ::
::  =/  message          [%message %foo %bar]
::  =/  =meal:xmas-gate  [%bond *flea:xmas-gate /chan/foo message]
::  ::
::  =/  result1
::    %-  encoder
::    [now=now.fix eny=eny.fix ham=meal]
::  ::
::  =/  key  (shaz :(mix (mug meal) now.fix eny.fix))
::  ::  TODO: do we really want to triple-jam the message?
::  ::
::  =/  sit  (seal:as:crub.fix 'b' (jam key (jam meal)))
::  =/  maj  (jam [u.cur.pipe.fix life.fix] [[her.fix pub.pipe.fix] ~ ~] sit)
::  ::
::  =/  spat  (spit:xmas-gate [our.fix her.fix] %full maj)
::  ::  TODO: why does the date always come out as ~2018.1.1?
::  ::
::  %+  expect-eq
::    !>  :-  [%line ~2018.1.1 key]~
::        [spat]~
::    !>  result1
::
++  aloe-call
  |=  $:  aloe-gate=_aloe-gate
          now=@da
          call-args=[=duct wrapped-task=(hypo (hobo task:able:aloe-gate))]
          expected-moves=(list move:aloe-gate)
      ==
  ^-  [tang _aloe-gate]
  ::
  =/  aloe  (aloe-gate our=~nul now=now eny=`@`0xdead.beef scry=*sley)
  ::
  =^  moves  aloe-gate
    %-  call:aloe  call-args
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output aloe-gate]
--
