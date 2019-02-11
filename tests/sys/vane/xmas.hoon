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
  :*  our=~nul
      her=~nec
      now=~2000.1.1
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

