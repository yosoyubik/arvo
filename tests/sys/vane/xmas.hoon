/+  *test
/=  xmas-raw  /:  /===/sys/vane/xmas  /!noun/
::
=/  test-pit=vase  !>(..zuse)
=/  xmas-gate  (xmas-raw test-pit)
::  test fixtures
::
=/  fix
  =/  crub   (pit:nu:crub:crypto 512 (shaz 'Alice'))
  =/  =ring  sec:ex:crub
  =|  =cert:xmas-gate
  =.  pub.dat.cert  'b'
  ::
  =/  =colt:xmas-gate  zeal:rail:xmas-gate
  =/  =mini:xmas-gate  myn.colt
  =/  mup  (yawn:pump:xmas-gate mini)
  ::
  :*  our=~nul
      her=~nec
      now=~2019.1.1
      eny=0xdead.beef
      life=1
      crub=crub
      ring=ring
      rings=`(map life ring:xmas-gate)`[[1 ring] ~ ~]
      ^=  pipe  ^-  pipe:xmas-gate
      :*  out=~
          inn=~
          cur=~
          sax=~
          pub=[[1 cert] ~ ~]
      ==
      ::
      colt=colt
      mini=mini
      mup=mup
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
::
::  |pump core tests
::
++  test-pump-back  ^-  tang
  ~
++  test-pump-cull  ^-  tang
  ~
++  test-pump-pack  ^-  tang
  =/  =task:pump:xmas-gate
    :-  %pack
    :~  [& [0 0] 0v0 (jam %foo)]
        [& [1 1] 0v1 (jam %bar)]
        [& [2 2] 0v2 (jam %qux)]
    ==
  ::
  =/  result1  (work:mup.fix now.fix task)
  ::  TODO is this really supposed to completely no-op on the third packet?
  ::  TODO is it supposed to emit the packets in reverse order?
  ::
  %+  expect-eq
    !>  %~  .  zu:pump:xmas-gate
        :-  ^-  fex=(list gift:pump:xmas-gate)
            :~  [%send `@uvH`0v1 [1 1] `@uvO`(jam %bar)]
                [%send `@uvH`0v0 [0 0] `@uvO`(jam %foo)]
            ==
        =/  lad0=@da  (add now.fix (mul 2 rtt.saw.mini.fix))
        =/  lad1=@da  +(lad0)
        ::
        %_  mini.fix
          las.saw  +(now.fix)
          lad.saw  lad1
          cur.saw  2
          liv      %-  ~(gas to liv.mini.fix)
                   ^-  (list coal:xmas-gate)
                   :~  [now.fix lad0 [& [0 0] 0v0 (jam %foo)]]
                       [+(now.fix) lad1 [& [1 1] 0v1 (jam %bar)]]
                   ==
        ==
    !>  result1
::
++  test-pump-wake  ^-  tang
  ~
++  test-pump-wait  ^-  tang
  ~
::
::  |knit core tests
::
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
    [now=now.fix eny=eny.fix ham=meal]
  ::
  =/  spat  (spit:xmas-gate [our.fix her.fix] %none (jam meal))
  ::
  %+  expect-eq
    !>  [~ ~[spat]]
    !>  result1
::
++  test-knit-bond  ^-  tang
  ::
  =/  knit
    %-  knit:xmas-gate
    [our.fix her.fix life.fix rings.fix pipe.fix]
  ::
  =/  message           [%message %foo %bar]
  =/  =meal:xmas-gate  [%bond *flea:xmas-gate /chan/foo message]
  =/  result1
    %-  knit
    [now=now.fix eny=eny.fix ham=meal]
  ::
  =/  sit  (sign:as:crub.fix (jam meal))
  =/  maj  (jam [~ life.fix] [[her.fix pub.pipe.fix] ~ ~] sit)
  ::
  =/  spat  (spit:xmas-gate [our.fix her.fix] %open maj)
  ::
  %+  expect-eq
    !>  [~ ~[spat]]
    !>  result1
::
++  test-knit-emit-line  ^-  tang
  ::  set the foreign life to 3 so we can test the %full case
  ::
  =.  cur.pipe.fix  `1
  ?>  ?=(^ cur.pipe.fix)
  ::
  =/  knit
    %-  knit:xmas-gate
    [our.fix her.fix life.fix rings.fix pipe.fix]
  ::
  =/  message          [%message %foo %bar]
  =/  =meal:xmas-gate  [%bond *flea:xmas-gate /chan/foo message]
  ::
  =/  result1
    %-  knit
    [now=now.fix eny=eny.fix ham=meal]
  ::
  =/  key  (shaz :(mix (mug meal) now.fix eny.fix))
  ::  TODO: do we really want to triple-jam the message?
  ::
  =/  sit  (seal:as:crub.fix 'b' (jam key (jam meal)))
  =/  maj  (jam [u.cur.pipe.fix life.fix] [[her.fix pub.pipe.fix] ~ ~] sit)
  ::
  =/  spat  (spit:xmas-gate [our.fix her.fix] %full maj)
  ::  TODO: why does the date always come out as ~2018.1.1?
  ::
  %+  expect-eq
    !>  :-  [%line ~2018.1.1 key]~
        [spat]~
    !>  result1
::
++  test-rail  ^-  tang
  ~
--

