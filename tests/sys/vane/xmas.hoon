/+  *test
/=  xmas-raw  /:  /===/sys/vane/xmas  /!noun/
::
=/  test-pit=vase  !>(..zuse)
=/  xmas-gate  (xmas-raw test-pit)
::  test fixtures
::
=/  fix
  :*  our=~nul
      her=~nec
      life=1
      rings=`(map life ring:xmas-gate)`[[1 *ring:xmas-gate] ~ ~]
      pipe=*pipe:xmas-gate
  ==
::
|%
++  test-trivial  ^-  tang
  %+  expect-eq
    !>  ~[%take %load %scry %neon %call %doze %stay]
    !>  (sloe -:!>(*xmas-gate))
::
++  test-nose  ^-  tang
  ~
++  test-hose  ^-  tang
  ~
++  test-pump  ^-  tang
  ~
++  test-knit-carp  ^-  tang
  ::
  =/  knit
    %-  knit:xmas-gate
    [our.fix her.fix life.fix rings.fix pipe.fix]
  ::
  =/  packet           %packet-foo
  =/  =meal:xmas-gate  [%carp *moan:xmas-gate 42 packet]
  =/  result1
    %-  knit
    [now=~2000.1.1 eny=0xdead.beef ham=meal]
  ::
  =/  spat  (spit:xmas-gate [our.fix her.fix] %none (jam meal))
  ::
  %+  expect-eq
    !>  [~ ~[spat]]
    !>  result1
::
++  test-rail  ^-  tang
  ~
--

