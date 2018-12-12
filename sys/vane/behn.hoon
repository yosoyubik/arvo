::  %behn, just a timer
::
!?  164
::
=,  behn
|=  pit=vase
=>  |%
    +$  move  [p=duct q=(wind note:able gift:able)]
    +$  sign  ~
    ::
    +$  behn-state
      $:  timers=(list timer)
          unix-duct=duct
          next-wake=(unit @da)
      ==
    ::
    +$  timer  [date=@da =duct]
    --
::
=|  behn-state
=*  state  -
|=  [our=ship now=@da eny=@uvJ ski=sley]                ::  current invocation
^?
|%                                                      ::  poke+peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^-  [(list move) _..^$]
  ::
  =/  =task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::
  |^  =^  moves  state
        ::
        ?-    -.task
            %crud
          [[hen %slip %d %flog task]~ state]
        ::
            %born
          =.  unix-duct  hen
          ::  unset :next-wake to make sure we set it again ourselves
          ::
          =.  next-wake  ~
          =^  moves  timers  notify-clients
          (set-wake moves)
        ::
            %rest
          =.  timers  (unset-timer [p.task hen])
          (set-wake ~)
        ::
            %wait
          =^  moves  timers  notify-clients
          =.  timers  (set-timer [p.task hen])
          (set-wake moves)
        ::
            %wake
          =.  next-wake  ~
          =^  moves  timers  notify-clients
          (set-wake moves)
        ::
            %wegh
          :_  state  :_  ~
          :^  hen  %give  %mass
          :-  %behn
          :-  %|
          :~  timers+[%& timers]
          ==
        ==
      ::
      [moves ..^^$]
  ::  +set-timer: set a timer, maintaining the sort order of the :timers list
  ::
  ++  set-timer
    |=  t=timer
    ^+  timers
    ::
    ?~  timers
      ~[t]
    ::
    ?:  (lte date.t date.i.timers)
      [t timers]
    ::
    [i.timers $(timers t.timers)]
  ::  +unset-timer: cancel a timer; if it already expired, no-op
  ::
  ++  unset-timer
    |=  [t=timer]
    ^+  timers
    ::
    ?~  timers
      ~
    ?:  =(i.timers t)
      t.timers
    ::
    [i.timers $(timers t.timers)]
  ::  +notify-clients: wake up vanes whose timers have expired
  ::
  ++  notify-clients
    =|  moves=(list move)
    |-  ^+  [moves timers]
    ::
    ?~  timers
      [moves timers]
    ::
    ?:  (gth date.i.timers now)
      [moves timers]
    ::
    %_  $
      timers  t.timers
      moves  [[duct.i.timers %give %wake ~] moves]
    ==
  ::  +set-wake: set or unset a unix timer to wake us when next timer expires
  ::
  ++  set-wake
    |=  moves=(list move)
    ^+  [moves state]
    ::  if no timers, don't wake us up ever; if a wake timer was set, unset it
    ::
    ?~  timers
      ?~  next-wake
        [~ state]
      :_  state(next-wake ~)
      [[unix-duct %give %doze ~] moves]
    ::  set an earlier wake timer if :next-wake isn't soon enough
    ::
    ?^  next-wake
      ?:  &((gte date.i.timers u.next-wake) (lte now u.next-wake))
        [~ state]
      :_  state(next-wake `date.i.timers)
      [[unix-duct %give %doze `date.i.timers] moves]
    ::  there was no wakeup timer; set one
    ::
    :_  state(next-wake `date.i.timers)
    [[unix-duct %give %doze `date.i.timers] moves]
  --
::
++  load
  |=  old=*
  ^+  ..^$
  ?^  new=((soft behn-state) old)
    ~&  %behn-load-new
    ..^$(state u.new)
  ~&  %behn-load-wipe
  ..^$(state *behn-state)
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ::
  ?.  ?=(%& -.why)
    ~
  [~ ~ %tank !>(>timers<)]
::
++  stay  state
++  take                                                ::  process move
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^+  [*(list move) ..^$]
  ~|  %behn-take
  !!
--
