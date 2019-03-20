/+  *test
::
/=  aloe-raw  /:  /===/sys/vane/aloe
              /!noun/
::
=/  test-pit=vase  !>(..zuse)
=/  aloe-gate  (aloe-raw test-pit)
=/  aloe  (aloe-gate our=~nul now=~2000.1.1 eny=`@`0xdead.beef scry=*sley)
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
      her=~doznec-doznec
      our-private-key=*private-key:aloe
      ^=  pipe
      [fast-key=~ her-life=1 her-public-keys=~ her-sponsors=~[~marzod ~zod]]
    ==
  ::
  %+  expect-eq
    !>  [gifts=~ authenticated=%.n meal=test-meal]
    !>  interpreted
::
++  notest-interpret-packet-open  ^-  tang
  !!
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
