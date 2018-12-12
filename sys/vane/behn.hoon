::  ::  %behn, just a timer
!?  164
::::
=,  behn
|=  pit/vase
=>  =~
|%
+$  timer  [date=@da =duct]
+$  coke  (list timer)
++  get
  |=  c=coke
  ^-  (unit timer)
  ?~  c  ~
  `i.c
++  put
  |=  [t=timer c=coke]
  %+  sort  [t c]
  |=  [a=timer b=timer]
  (lte date.a date.b)
++  pop
  |=  c=coke
  ^-  [(unit timer) coke]
  ?~  c  [~ ~]
  [`i.c t.c]
++  crank
  |=  [state=coke now=@da]
  =|  moves=(list move)
  |-  ^+  [moves state]
  ?~  state  [moves state]
  ?.  (lte date.i.state now)
    [moves state]
  %_  $
    state  t.state
    moves  [[duct.i.state %give %doze date.i.state] moves]
  ==
--
.  ==
=|  coke                                                ::  persistent state
=*  state  -                                            ::
|=  [our=ship now=@da eny=@uvJ ski=sley]                ::  current invocation
^?
|%                                                      ::  poke+peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          type=*
          wrapped-task=(hobo task:able)
      ==
  ::
  =/  req=task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::
  ::
  =|  moves=(list move)
  ?-    -.req
      %crud
    [[[hen %slip %d %flog req] ~] ..^^$]
  ::
      %born
    =.  gad  hen
    ?~  p.tym
      [~ ..^^$]
    =/  nex  ~(get up p.tym)
    ?:  (lte now p.nex)
      [[gad %give %doze `p.nex]~ ..^^$]
    $(req [%wake ~])
  ::
      $rest
    =.  state 
      |-  ^+  state
      ?~  state  state
      ?:  =(i.state [p.req hen])
        t.state
      [i.state $(state t.state)]
    (crank state now)
  ::
      $wait
    =^  moves  state  (crank state now)
    =.  state  (put [p.req hen] state)
    [moves state]
  ::
      $wake
    =|  moves=(list move)
    |-  ^+  [moves state]
    ?~  state  [moves state]
    ?.  (lte date.i.state now)
      [moves state]
    %_  $
      state  t.state
      moves  [[duct.i.state %give %wake ~] moves]
    ==
  ::
      $wegh
    :_  tym  :_  ~
    :^  hen  %give  %mass
    :-  %behn
    :-  %|
    :~  tym+[%& tym]
    ==
  ==
  [mof ..^^$]
::
++  load
  |=  old=coke
  ^+  ..^$
  ..^$(state old)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=(%& -.why)  ~
  =*  who  p.why
  =+  ^=  liz
      |-  ^-  (list {@da duct})
      =.  tym  (raze tym)
      ?~  p.tym  ~
      [~(get up p.tym) $(p.tym ~(pop up p.tym))]
  [~ ~ %tank !>(>liz<)]
::
++  stay  state
++  take                                                ::  process move
  |=  {tea/wire hen/duct hin/(hypo sign)}
  ^+  [*(list move) ..^$]
  !!
--
