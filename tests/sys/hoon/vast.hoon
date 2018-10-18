/+  *test
/=  hoon-gate  /:  /===/sys/hoon  /!noun/
::
!:
::
|%
++  test-ntcb-bare-hoon  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %ntcb %wing ~[%foo]]
    !>  (scan "foo" tall-top:(rage:vast:hoon-gate allow-bare-hoon=&))
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
++  test-ntdt-path-literal  ^-  tang
  %+  expect-eq
    !>  `hoon:hoon-gate`[%gear %ntdt (ream:hoon-gate '/some/path')]
    !>  (ream:hoon-gate '#.  /some/path')
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
        [%gear %ntls [%ntts %foo [%ntdt %wing ~[%foo]]] [%ntdt %wing ~[%bar]]]
    !>  (ream:hoon-gate '#+  #=  foo  #.  foo  #.  bar')
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
++  test-ntpd-dynamic-path  ^-  tang
  =/  =path  /~zod/home/~1111.1.1/app/hood/hoon
  ::
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        :-  %gear
        [%ntdt (rain:hoon-gate path '/===/sole/lib/hoon')]
    !>  (rain:hoon-gate path '#.  /===/sole/lib/hoon')
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
        [[%ntdt %wing ~[%foo]] [%ntdt %wing ~[%bar]] [%ntdt %wing ~[%baz]]]
    !>  (ream:hoon-gate '#?  #.  foo  #.  bar  #.  baz')
::
++  test-gear-clhp  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        :-  %gear
        [[%ntdt %wing ~[%foo]] [%ntdt %wing ~[%bar]]]
    !>  (scan ":-  #.  foo  #.  bar" tall-top:(rage:vast:hoon-gate |))
::
++  test-gear-clkt  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        :-  %gear
        :^    [%ntdt %wing ~[%foo]]
            [%ntdt %wing ~[%bar]]
          [%ntdt %wing ~[%baz]]
        [%ntdt %wing ~[%quz]]
    ::
    !>  %+  scan
          ":^  #.  foo  #.  bar  #.  baz  #.  quz"
        tall-top:(rage:vast:hoon-gate |)
::
++  test-gear-clls  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        :-  %gear
        :+  [%ntdt %wing ~[%foo]]
          [%ntdt %wing ~[%bar]]
        [%ntdt %wing ~[%baz]]
    ::
    !>  %+  scan
          ":+  #.  foo  #.  bar  #.  baz"
        tall-top:(rage:vast:hoon-gate |)
::
++  test-gear-clsg  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        :-  %gear
        :+  [%ntdt %wing ~[%foo]]
          [%ntdt %wing ~[%bar]]
        [%ntdt %bust %null]
    ::
    !>  %+  scan
          ":~  #.  foo  #.  bar  =="
        tall-top:(rage:vast:hoon-gate |)
::
++  test-gear-cltr  ^-  tang
  %+  expect-eq
    !>  ^-  hoon:hoon-gate
        :-  %gear
        :+  [%ntdt %wing ~[%foo]]
          [%ntdt %wing ~[%bar]]
        [%ntdt %wing ~[%baz]]
    ::
    !>  %+  scan:hoon-gate
          ":*  #.  foo  #.  bar  #.  baz  =="
        tall-top:(rage:vast:hoon-gate |)
::
++  test-file-header  ^-  tang
  =/  =path  /~zod/home/~1111.1.1/app/hood/hoon
  ::
  ~&  %+  scan:hoon-gate
        """
        #+  #=  here-disc
          ::=/  her=path  /==
          =/  her=path  /foo/bar
          ?>  ?=([* * *] her)
          [(slav %p i.her) (slav %tas i.t.her)]
        ::
        #+  #=  sole  #&  :-  here-disc  #.  /hoon/sole/lib
        ::
        sole
        """
      tall-top:(rage:(vang:hoon-gate | path) &)
  ::
  %+  expect-eq
    !>  :-  %gear
        :+  %ntls
          [%ntts %sole [%ntpd [%ntdt (ream:hoon-gate '/some/path')]]]
        [%ntcb %wing ~[%sole]]
    ::
    !>  %+  scan:hoon-gate
          "#+  #=  sole  #&  #.  /some/path  sole"
        tall-top:(rage:(vang:hoon-gate | path) &)
--
