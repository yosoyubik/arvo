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
  =/  test-meal=meal:aloe  [%'TODO' ~]
  ::
  =/  formatted=none:packet-format:aloe  raw-payload=(jam test-meal)
  ::
  =/  interpreted
    %.  [%none formatted]
    %-  interpret-packet:aloe  :*
      her.fix
      our-private-key.fix
      ^-  pipe:aloe
      [fast-key=~ her-life.fix her-public-keys.fix her-sponsors.fix]
    ==
  ::
  %+  expect-eq
    !>  [gifts=~ authenticated=%.n meal=test-meal]
    !>  interpreted
::
++  test-interpret-packet-open-no-deed  ^-  tang
  =/  test-meal=meal:aloe  [%'TODO' ~]
  =/  jammed-meal=@        (jam test-meal)
  =/  signed-payload=@  (sign:as:her-crub.fix jammed-meal)
  ::
  =/  formatted=open:packet-format:aloe
    [from=her-life.fix deed=~ signed-payload]
  ::
  =/  packet-interpreter
    %-  interpret-packet:aloe  :*
      her.fix
      our-private-key.fix
      ^-  pipe:aloe
      [fast-key=~ her-life.fix her-public-keys.fix her-sponsors.fix]
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
++  notest-interpret-packet-fast  ^-  tang
  !!
::
++  notest-interpret-packet-full  ^-  tang
  !!
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
