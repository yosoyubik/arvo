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
::  TODO: remove +neon and +doze
::
++  test-vane-interface  ^-  tang
  %+  expect-eq
    !>  ~[%take %load %scry %neon %call %doze %stay]
    !>  (sloe -:!>(*xmas-gate))
::
::  |nose core tests
::
++  test-nose-none  ^-  tang
  =/  =meal:xmas-gate  [%bond [0 0] /foo/bar [%message %foo]]
  =/  message  [%none (jam meal)]
  ::
  =/  noser
    %-  nose:xmas-gate
    [him=her.fix wyr=rings.fix det=pipe.fix]
  ::
  =/  result1
    %-  noser
    message
  ::
  %+  expect-eq
    !>  :-  ^-  (list gift:nose:xmas-gate)
            ~
        [aut=| ham=meal]
    !>  result1
::
++  test-nose-fast  ^-  tang
  =.  inn.pipe.fix
    ^-  (map hand:xmas-gate bill:xmas-gate)
    :+  :-  `hand:xmas-gate`0v1
        `bill:xmas-gate`[~2020.1.1 `@J`%key]
    ~  ~
  =/  =meal:xmas-gate  [%bond [0 0] /foo/bar [%message %foo]]
  =/  message  [%fast (cat 7 0v1 (en:crub:crypto `@J`%key (jam meal)))]
  ::
  =/  noser
    %-  nose:xmas-gate
    [him=her.fix wyr=rings.fix det=pipe.fix]
  ::
  =/  result1
    %-  noser
    message
  ::
  %+  expect-eq
    !>  [~ & meal]
    !>  result1
::
++  test-nose-full  ^-  tang
  ::  generate a packet the same way as +test-knit-full
  ::
  ::  set the foreign life to 3 so we can test the %full case
  ::
  =.  cur.pipe.fix  `1
  ?>  ?=(^ cur.pipe.fix)
  ::
  =/  message          [%message %foo %bar]
  =/  =meal:xmas-gate  [%bond *flea:xmas-gate /chan/foo message]
  ::
  =/  liz   `[life:xmas-gate life:xmas-gate]`[u.cur.pipe.fix life.fix]
  =/  key   (shaz :(mix (mug meal) now.fix eny.fix))
  =.  inn.pipe.fix
    ^-  (map hand:xmas-gate bill:xmas-gate)
    :+  :-  `hand:xmas-gate`0v1
        `bill:xmas-gate`[~2020.1.1 key]
    ~  ~
  =/  sit   (seal:as:crub.fix 'b' (jam key (jam meal)))
  =/  ger   `gree:xmas-gate`[[her.fix pub.pipe.fix] ~ ~]
  =/  maj   (jam liz ger sit)
  =/  spat  (spit:xmas-gate [our.fix her.fix] %full maj)
  ::
  =/  noser
    %-  nose:xmas-gate
    [him=her.fix wyr=rings.fix det=pipe.fix]
  ::
  =/  result1
    %-  noser
    [%full maj]
  ::
  %+  expect-eq
    !>  :-  ^-  (list gift:nose:xmas-gate)
            :~  `gift:nose:xmas-gate`[%link ~2018.1.1 key]
                `gift:nose:xmas-gate`[%meet ger]
            ==
        [& meal]
    !>  result1
::
++  test-nose-open  ^-  tang
  [>"not implemented"<]~
::
::  |hose core tests
::
++  test-hose  ^-  tang
  [>"not implemented"<]~
::
::  |pump core tests
::
++  test-pump-back  ^-  tang
  ::  use same +mini as the end of +test-pump-pack
  ::
  =/  lad0=@da  (add now.fix (mul 2 rtt.saw.mini.fix))
  =/  lad1=@da  +(lad0)
  ::
  =.  mini.fix
    %_  mini.fix
      las.saw  +(now.fix)
      lad.saw  lad1
      cur.saw  2
      liv      %-  ~(gas to `_liv.mini.fix`~)
               ^-  (list coal:xmas-gate)
               :~  [now.fix lad0 [& [0 0] 0v0 (jam %foo)]]
                   [+(now.fix) lad1 [& [1 1] 0v1 (jam %bar)]]
               ==
    ==
  ::
  =/  result1
    (work:(yawn:pump:xmas-gate mini.fix) (add ~s13 now.fix) [%back 0v0 ~ ~s13])
  ::
  %+  expect-eq
    !>  =<  +<
        %~  .  zu:pump:xmas-gate
        :-  ^-  fex=(list gift:pump:xmas-gate)
            :~  [%good 0v0 [0 0] ~s13 ~]
            ==
        %_  mini.fix
          cur.saw  1
          rey.saw  0
          liv      %-  ~(gas to `_liv.mini.fix`~)
                   ^-  (list coal:xmas-gate)
                   :~  [+(now.fix) lad1 [& [1 1] 0v1 (jam %bar)]]
                   ==
        ==
    !>  =<  +<
        result1
::
++  test-pump-cull  ^-  tang
  ::  use same +mini as the end of +test-pump-pack
  ::
  =/  lad0=@da  (add now.fix (mul 2 rtt.saw.mini.fix))
  =/  lad1=@da  +(lad0)
  ::
  =.  mini.fix
    %_  mini.fix
      las.saw  +(now.fix)
      lad.saw  lad1
      cur.saw  2
      liv      %-  ~(gas to `_liv.mini.fix`~)
               ^-  (list coal:xmas-gate)
               :~  [now.fix lad0 [& [0 0] 0v0 (jam %foo)]]
                   [+(now.fix) lad1 [& [1 1] 0v1 (jam %bar)]]
               ==
    ==
  ::
  =/  result1
    (work:(yawn:pump:xmas-gate mini.fix) (add ~s1 now.fix) [%cull 0])
  ::
  %+  expect-eq
    !>  =<  +<
        %~  .  zu:pump:xmas-gate
        :-  ^-  fex=(list gift:pump:xmas-gate)
            ~
        %_  mini.fix
          liv      %-  ~(gas to `_liv.mini.fix`~)
                   ^-  (list coal:xmas-gate)
                   :~  [+(now.fix) lad1 [& [1 1] 0v1 (jam %bar)]]
                   ==
        ==
    !>  =<  +<
        result1
::
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
++  test-pump-wait  ^-  tang
  ::  use same +mini as the end of +test-pump-pack
  ::
  =/  lad0=@da  (add now.fix (mul 2 rtt.saw.mini.fix))
  =/  lad1=@da  +(lad0)
  ::
  =.  mini.fix
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
  ::
  =/  result1  next-wakeup:(yawn:pump:xmas-gate mini.fix)
  ::
  %+  expect-eq
    !>  `lad0
    !>  result1
::
++  test-pump-wake  ^-  tang
  ::  use same +mini as the end of +test-pump-pack
  ::
  =/  lad0=@da  (add now.fix (mul 2 rtt.saw.mini.fix))
  =/  lad1=@da  +(lad0)
  ::
  =.  mini.fix
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
  ::  let's time out the first packet by setting now to lad0
  ::
  =/  result1  (work:(yawn:pump:xmas-gate mini.fix) lad0 [%wake ~])
  ::
  %+  expect-eq
    !>  %~  .  zu:pump:xmas-gate
        :-  ~
        %_  mini.fix
          cur.saw  1
          rey.saw  1
          liv      %-  ~(gas to `_liv.mini.fix`~)
                   ^-  (list coal:xmas-gate)
                   :~  [+(now.fix) lad1 [& [1 1] 0v1 (jam %bar)]]
                   ==
          lop      %-  ~(gas to `_lop.mini.fix`~)
                   ^-  (list clue:xmas-gate)
                   :~  [& [0 0] 0v0 (jam %foo)]
                   ==
        ==
    !>  result1
::
::  +encode-meal tests
::
++  test-encode-carp  ^-  tang
  ::
  =/  encoder
    %-  encode-meal:xmas-gate
    [our.fix her.fix life.fix rings.fix pipe.fix]
  ::
  =/  packet           %packet-foo
  =/  =meal:xmas-gate  [%carp *moan:xmas-gate 42 packet]
  =/  result1
    %-  encoder
    [now=now.fix eny=eny.fix ham=meal]
  ::
  =/  spat  (spit:xmas-gate [our.fix her.fix] %none (jam meal))
  ::
  %+  expect-eq
    !>  [~ ~[spat]]
    !>  result1
::
++  test-encode-bond  ^-  tang
  ::
  =/  encoder
    %-  encode-meal:xmas-gate
    [our.fix her.fix life.fix rings.fix pipe.fix]
  ::
  =/  message           [%message %foo %bar]
  =/  =meal:xmas-gate  [%bond *flea:xmas-gate /chan/foo message]
  =/  result1
    %-  encoder
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
++  test-encode-full-emit-gift  ^-  tang
  ::  set the foreign life to 3 so we can test the %full case
  ::
  =.  cur.pipe.fix  `1
  ?>  ?=(^ cur.pipe.fix)
  ::
  =/  encoder
    %-  encode-meal:xmas-gate
    [our.fix her.fix life.fix rings.fix pipe.fix]
  ::
  =/  message          [%message %foo %bar]
  =/  =meal:xmas-gate  [%bond *flea:xmas-gate /chan/foo message]
  ::
  =/  result1
    %-  encoder
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
  [>"not implemented"<]~
--

