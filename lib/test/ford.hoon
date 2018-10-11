/+  *test, *marker  ::  TODO: fix with zuse
|%
::  +expect-schematic: assert a +schematic:ford is what we expect
::
::    Since Ford requests contain types, we can't do simple
::    equality checking. This function handles all the different
::    kinds of +schematic:ford, dealing with types as necessary.
::
++  expect-schematic
  |=  [expected=schematic:ford actual=schematic:ford]
  ^-  tang
  ::
  ~&  [%expect-schematic ?^(-.expected '^' -.expected)]
  ::
  ?-    -.expected
      ^
    ?.  ?=(^ -.actual)
      [%leaf "expected autocons, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected head.expected, actual head.actual)
    $(expected tail.expected, actual tail.actual)
  ::
      %ntbn
    ::
    ?.  ?=(%ntbn -.actual)
      [%leaf "expected %ntbn, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected subject.expected, actual subject.actual)
    $(expected rest.expected, actual rest.actual)
  ::
      %ntbs
    ::
    ?.  ?=(%ntbs -.actual)
      [%leaf "expected %ntbs, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected gate.expected, actual gate.actual)
    $(expected sample.expected, actual sample.actual)
  ::
      %ntcb  (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %ntdt
    ::
    ?.  ?=(%ntdt -.actual)
      [%leaf "expected %ntdt, but got {<-.actual>}"]~
    ::
    (expect-eq literal.expected literal.actual)
  ::
      %ntkt
    ::
    ?.  ?=(%ntkt -.actual)
      [%leaf "expected %ntkt, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected spec.expected, actual spec.actual)
    $(expected rest.expected, actual rest.actual)
  ::
      %ntls
    ::
    ?.  ?=(%ntls -.actual)
      [%leaf "expected %ntls, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected head.expected, actual head.actual)
    $(expected rest.expected, actual rest.actual)
  ::
      %ntnt
    ::
    ?.  ?=(%ntnt -.actual)
      [%leaf "expected %ntnt, but got {<-.actual>}"]~
    ::
    %+  weld
      $(expected subject.expected, actual subject.actual)
    $(expected schematic.expected, actual schematic.actual)
  ::
      %ntpd  (expect-eq [schematic-type expected] [schematic-type actual])
      %nttr  (expect-eq [schematic-type expected] [schematic-type actual])
  ::
      %ntts
    ::
    ?.  ?=(%ntts -.actual)
      [%leaf "expected %ntts, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq !>(face.expected) !>(face.actual))
    $(expected rest.expected, actual rest.actual)
  ::
      %ntvt
    ::
    ?.  ?=(%ntvt -.actual)
      [%leaf "expected %ntvt, but got {<-.actual>}"]~
    ::
    %+  weld
      (expect-eq !>(date.expected) !>(date.actual))
    $(expected rest.expected, actual rest.actual)
  ::
      %ntwt
    ::
    ?.  ?=(%ntwt -.actual)
      [%leaf "expected %ntwt, but got {<-.actual>}"]~
    ::
    ;:  weld
      $(expected if.expected, actual if.actual)
      $(expected then.expected, actual then.actual)
      $(expected else.expected, actual else.actual)
    ==
  ==
::  +schematic-type: the +type for +schematic:ford
::
++  schematic-type  ^~  `type`-:!>(*schematic:ford)
--
