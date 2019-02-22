::  %abel, message transceiver
::
::    Implements the Ames network protocol for messaging among ships.
::
!:
!?  141
|=  pit=vase
=>
::  type definitions
::
|%
::  |able: public move interfaces
::
++  able
  |%
  ::  +task: requests to this vane
  ::
  +$  task
    $%  ::  %send: encode and send message over the wire
        ::
        [%send =ship =path message=*]
        ::  %ward: send a message to :ship to be forwarded to another ship
        ::
        ::    This sends :ship a message that has been wrapped in an envelope
        ::    containing the address of the intended recipient.
        ::
        [%ward =ship =path message=*]
        ::  %hear: receive a packet from unix
        ::
        [%hear =lane packet=@]
        ::  %hole: receive notification from unix that packet crashed
        ::
        [%hole =lane packet=@]
        ::  %sunk: receive notification that a ship has reincarnated
        ::
        [%sunk =ship =life]
        ::  %nuke: reset all sequence numbers between :our and :ship
        ::
        [%nuke =ship]
        ::  %born: urbit process restarted
        ::
        [%born ~]
        ::  %crud: previous unix event errored
        ::
        [%crud tag=@tas =tang]
        ::  %vega: kernel reset notification
        ::
        [%vega ~]
        ::  %wegh: request for memory usage report
        ::
        [%wegh ~]
    ==
  ::  +gift: responses from this vane
  ::
  +$  gift
    $%  ::  %mack: tell unix to send ack or nack on packet
        ::
        [%mack error=(unit [tag=@tas =tang])]
        ::  %send: tell unix to send a packet to another ship
        ::
        ::    Emitted in response to a %send +task.
        ::
        [%send =lane packet=@]
        ::  %pons: relay response message to another vane
        ::
        ::    Emitted upon hearing a message from Unix (originally from
        ::    another ship) in response to a message the vane
        ::    asked us to send.
        ::
        [%pons response=*]
        ::  %turf: tell unix which domains to bind
        ::
        ::    We learn this from Jael, then relay it out to Unix.
        ::
        [%turf domains=(list domain)]
        ::  %mass: memory usage report
        ::
        [%mass =mass]
    ==
  --
::  +move: output effect
::
+$  move  [=duct card=(wind note gift:able)]
::  +note: request from us to another vane
::
+$  note
  $%  $:  %b
          $%  [%wait date=@da]
              [%rest date=@da]
  ==  ==  ==
::  +sign: response to us from another vane
::
+$  sign
  $%  $:  %b
          $%  [%wake ~]
  ==  ==  ==
::  +abel-state: all persistent state
::
+$  abel-state  ~
::  +domain: an HTTP domain, as list of '.'-delimited segments
::
+$  domain  (list @t)
::  +lane: TODO WTF
::
+$  lane
  $%  [%if expiration-date=@da port=@ud ipv4=@if]
      [%is port=@ud lane=(unit lane) ipv6=@is]
      [%ix expiration-date=@da port=@ud ipv4=@if]
  ==
--
=<
::  vane interface core
::
=|  abel-state
=*  state  -
|=  [our=ship now=@da eny=@uvJ scry-gate=sley]
=*  abel-gate  .
|%
::  +call: handle a +task:able:abel request
::
++  call
  |=  $:  =duct
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^-  [(list move) _abel-gate]
  ::  unwrap :task, coercing to valid type if needed
  ::
  =/  =task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    (task:able wrapped-task)
  ::
  =/  event-core  (per-event [our now eny scry-gate duct] state)
  ::
  =^  moves  state
    =<  abet
    ?-  -.task
      %wegh  wegh:event-core
      *      ~|  [%abel-not-implemented -.task]  !!
    ==
  ::
  [moves abel-gate]
::  +load: migrate an old state to a new abel version
::
++  load
  |=  old=*
  ^+  abel-gate
  ::
  ~|  %abel-load-fail
  abel-gate(state (abel-state old))
::  +scry: handle scry request for internal state
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
  ^-  (unit (unit cage))
  ::
  ~
::
++  stay  state
::  +take: receive response from another vane
::
++  take
  |=  [=wire =duct sign-type=type =sign:able]
  ^-  [(list move) _abel-gate]
  ::
  =/  event-core  (per-event [our now eny scry-gate duct] state)
  ::
  =^  moves  state
    =<  abet
    ?-  sign
      [%b %wake ~]  (wake:event-core wire)
    ==
  ::
  [moves abel-gate]
--
::  implementation core
::
|%
++  per-event
  =|  moves=(list move)
  |=  [[our=ship now=@da eny=@ scry-gate=sley =duct] state=abel-state]
  |%
  +|  %entry-points
  ::  +wake: handle elapsed timer from behn
  ::
  ++  wake
    |=  =wire
    ^+  event-core
    !!
  ::  +wegh: report memory usage
  ::
  ++  wegh
    ^+  event-core
    %-  emit
    :^  duct  %give  %mass
    ^-  mass
    :+  %abel  %|
    :~  dot+&+state
    ==
  ::
  +|  %utilities
  ::
  ++  event-core  .
  ::  +abet: finalize, producing [moves state] with moves in the right order
  ::
  ++  abet  [(flop moves) state]
  ::  +emit: enqueue an output move to be emitted at the end of the event
  ::
  ::    Prepends the move to :moves.event-core, which is reversed
  ::    at the end of an event.
  ::
  ++  emit  |=(=move event-core(moves [move moves]))
  --
--
