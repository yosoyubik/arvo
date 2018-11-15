#+  #=  here-disc
  ^-  disc:ford
  !:
  =/  her=path  /==
  ~&  [%loading %]
  ?>  ?=([* * *] her)
  [(slav %p i.her) (slav %tas i.t.her)]
::
#+  #=  test  #&  :-  here-disc  #.  /hoon/test/lib
::
#+  #=  hoon-rail  :-  here-disc  #.  /hoon/hoon/sys
#+  #=  hoon-gate  #&  hoon-rail
#+  #=  hoon-text
  #^  @t
  #>  #*  %cx  hoon-rail
  +
::
=,  test
::
~&  %compiling-hoon
::
=/  hoon-comp                 %-  ~(mint ut:hoon-gate %noun)
                              [%noun (rain /===/sys/hoon/hoon hoon-text)]
::
=/  hoon-inflated             .*(0 q.hoon-comp)
=/  hoon-vase=vase:hoon-gate  [p.hoon-comp hoon-inflated]
::
~&  %running-open-tests
::
!:
::
|%
++  test-open-ntdt  ^-  tang
  %+  expect-eq
    !>  [[%rock %tas %ntdt] %zpbn [%bust %null]]
    !>  ~(open ap.hoon-gate [%gear %ntdt [%bust %null]])
::
++  test-open-ntcb  ^-  tang
  %+  expect-eq
    !>  [[%rock %tas %ntcb] %zpcm [%kttr %like ~[%hoon] ~] %bust %null]
    !>  ~(open ap.hoon-gate [%gear %ntcb [%bust %null]])
::
++  test-slap-ntdt  ^-  tang
  %+  expect-eq
    !>  [%ntdt [%atom %tas `%foo] %foo]
    :-  -:!>([%ntdt [%atom %tas `%foo] %foo])
    q:(slap:hoon-gate hoon-vase (ream:hoon-gate '#.  %foo'))
::
++  test-slap-ntpd  ^-  tang
  %+  expect-eq
    !>  [%ntpd [[%ntcb %wing ~[%here-disc]] /hoon/foo/lib]]
    ::
    :-  -:!>([%ntpd [[%ntcb %wing ~[%here-disc]] *path]])
    =/  parsed
      %+  rain:hoon-gate  /~nul/home/0/lib/foo
      '#&  :-  here-disc  #.  /hoon/foo/lib'
    ::
    =-  ~&  `*`-  -
    q:(slap:hoon-gate hoon-vase parsed)
--
