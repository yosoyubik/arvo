/+  *test
::
/=  aloe-raw  /:  /===/sys/vane/aloe
              /!noun/
::
=/  test-pit=vase  !>(..zuse)
=/  aloe-gate  (aloe-raw test-pit)
::
|%
++  test-packet-encoding  ^-  tang
  =/  =packet:aloe-gate
    [[to=~nec from=~doznec-doznec] encoding=%none payload=(jam [42 43])]
  ::
  =/  encoded  (encode-packet:aloe-gate packet)
  =/  decoded  (decode-packet:aloe-gate encoded)
  ::
  %+  expect-eq
    !>  packet
    !>  decoded
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
