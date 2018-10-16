/+  *test
/=  hoon-gate  /:  /===/sys/hoon  /!noun/
::
!:
::
|%
++  test-ntcb-bare-hoon  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %ntcb %wing ~[%foo]]
    !>  (scan "foo" tall-top:rage:vast:hoon-gate)
::
++  test-ntbn  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        [%gear %ntbn [%ntdt [%sand %da ~2000.1.1]] %ntcb [%cnts [%& 1]~ ~]]
    !>  (ream:hoon-gate '#>  #.  ~2000.1.1  .')
::
++  test-ntbs  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        [%gear %ntbs [%ntdt %wing ~[%foo]] [%ntdt %wing ~[%bar]]]
    !>  (ream:hoon-gate '#$  #.  foo  #.  bar')
::
++  test-ntdt  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %ntdt %bust %null]
    !>  (ream:hoon-gate '#.  ~')
::
++  test-ntkt  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        [%gear %ntkt [%ntdt %wing ~[%foo]] [%ntdt %wing ~[%bar]]]
    !>  (ream:hoon-gate '#^  #.  foo  #.  bar')
::
++  test-ntls  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        [%gear %ntls [%ntdt %wing ~[%foo]] [%ntdt %wing ~[%bar]]]
    !>  (ream:hoon-gate '#+  #.  foo  #.  bar')
::
++  test-ntnt  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        [%gear %ntnt [%ntdt %wing ~[%foo]] [%ntdt %wing ~[%bar]]]
    !>  (ream:hoon-gate '#/  #.  foo  #.  bar')
::
++  test-ntpd  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %ntpd %ntdt [%sand %da ~2000.1.1]]
    !>  (ream:hoon-gate '#&  #.  ~2000.1.1')
::
++  test-nttr  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %nttr %cx %ntdt [%wing ~[%foo]]]
    !>  (ream:hoon-gate '#*  %cx  #.  foo')
::
++  test-ntts  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %ntts %foo %ntdt [%wing ~[%bar]]]
    !>  (ream:hoon-gate '#=  foo  #.  bar')
::
++  test-ntvt  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %ntvt [%wing ~[%date]] %ntdt [%wing ~[%foo]]]
    !>  (ream:hoon-gate '#@  date  #.  foo')
::
++  test-ntwt  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        :+  %gear  %ntwt
        [[%ntdt %wing ~[%foo]] [%ntdt %wing ~[%bar]] [%ntdt %wing ~[%qux]]]
    !>  (ream:hoon-gate '#?  #.  foo  #.  bar  #.  qux')
--
