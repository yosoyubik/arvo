/+  *test
/=  hoon-gate  /:  /===/sys/hoon  /!noun/
::
!:
::
|%
++  test-slap-ntdt  ^-  tang
  %+  expect-eq
    !>  [%ntdt [%atom %tas `%foo] %foo]
    :-  -:!>([%ntdt [%atom %tas `%foo] %foo])
    (slap:hoon-gate [[%atom %n `~] ~] (ream:hoon-gate '#.  %foo'))
--
