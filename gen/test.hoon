#+  #=  here-disc
  ^-  disc:ford
  !:
  =/  her=path  /==
  ~&  [%loading %]
  ?>  ?=([* * *] her)
  [(slav %p i.her) (slav %tas i.t.her)]
::
#+  #=  test-runner  #&  :-  here-disc  #.  /hoon/runner/test/lib
::
!:
=>
|%
++  run-test
  ::  executes an individual test.
  |=  [pax=path test=test-func:test-runner]
  ^-  tang
  =+  name=(spud pax)
  !:
  ~&  [%running-test name]
  =+  run=(mule test)
  ?-  -.run
    %|  ::  the stack is already flopped for output?
        ;:  weld
          p.run
          `tang`[[%leaf (weld "CRASHED " name)] ~]
        ==
    %&  ?:  =(~ p.run)
          [[%leaf (weld "OK      " name)] ~]
        ::  Create a welded list of all failures indented.
        %-  flop
        ;:  weld
          `tang`[[%leaf (weld "FAILED  " name)] ~]
          ::TODO indent
          :: %+  turn  p:run
          ::   |=  {i/tape}
          ::   ^-  tank
          ::   [%leaf (weld "  " i)]
          p.run
        ==
  ==
::
++  load-test-files
  |=  [~ =mark paths=(list path)]
  ^-  schematic:ford
  ::
  !:
  ~&  [%test-paths-unfiltered paths]
  ::
  %-  build-list:forder
  %+  murn  paths
  |=  =path
  ^-  (unit schematic:ford)
  ::
  ~&  [%checking-test-path path]
  ::
  ?~  spur=(flop path)
    ~
  ?.  =(%hoon i.spur)
    ~
  :-  ~
  :-  #.  path
  #$  #.  get-test-arms:test-runner
  #>  #&  :-  here-disc  #.  spur
  !>(.)
--
::
:-  %bud
|=  $:  [now=@da eny=@uvJ bec=beak]
        [filter=$?(~ [pax=path ~])]
        [defer=_& seed=?(~ @uvJ)]
    ==
^-  schematic:ford
::  bring some variables into the build subject scope
::
#+  #=  here-disc    #.  here-disc
#+  #=  run-test     #.  run-test
#+  #=  test-runner  #.  test-runner
::  use empty path prefix if unspecified
::
#+  #=  prefix  #.  ?~(filter ~ pax.filter)
#>  !:
    ~&  [%test-prefix prefix]
    .
::  load tests from source files, filtered by :prefix
::
#+  #=  tests
  #$  #.  resolve-test-paths:test-runner
  ::#^  #.  (list ,[=path =test-arm:test-runner])
  #+  #/  .
      #$  #.  load-test-files
      #*  %ct  :-  here-disc  (flop `path`[%tests prefix])
  !:
  ~&  [%tests -]
  -
::  run each test in a separate build for granular caching
::
::    TODO handle :defer
::
#^  #.  tang
#$  #.  zing
#/  ..zuse
::
%-  build-list:forder
%+  turn  tests
|=  =test:test-runner
^-  schematic:ford
::
#+  #.  path=path
#!  [%ford-test-fail path]
#$  #.  run-test
#.  test
