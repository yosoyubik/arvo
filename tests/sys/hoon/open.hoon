/+  *test
/=  hoon-gate  /:  /===/sys/hoon  /!noun/
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
::++  test-slap-ntdt  ^-  tang
::  %+  expect-eq
::    !>  [%ntdt [%atom %tas `%foo] %foo]
::    :-  -:!>([%ntdt [%atom %tas `%foo] %foo])
::    q:(slap:hoon-gate !>(hoon-gate) (ream:hoon-gate '#.  %foo'))
--
