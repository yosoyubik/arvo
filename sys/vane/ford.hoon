!:
::  ford: build system vane
::
::    Ford is a functional reactive build system.
::
::    A Ford build is a function of the Urbit namespace and a date that
::    produces marked, typed data or an error.
::
::    The function in the definition of a build is called a "schematic,"
::    and it's represented by a Hoon data structure with twenty-five sub-types.
::    A schematic is a (possibly trivial) DAG of sub-builds to be performed.
::    The different schematic sub-types transform the results of their
::    sub-builds in different ways.
::
::    We call the date in the definition of a build the "formal date" to
::    distinguish it from the time at which the build was performed.

::    Each build is referentially transparent with respect to its formal date:
::    ask to run that function on the namespace and a particular formal date,
::    and Ford will always produce the same result.
::
::    We can now say Ford is a functional build system, since each build is a
::    function. We have not yet explained how it's a functional reactive build
::    system. With Ford, you can subscribe to results of a build. Ford tracks
::    the result of a "live" build consisting of a static schematic and the
::    ever-changing current date. Whenever this live build's result changes,
::    Ford sends you the new result and the formal date of the build (the date
::    which would cause the same result if you asked Ford to build that
::    schematic again). This is a push-based FRP paradigm.
::
::    The implementation is event-driven, like the rest of Urbit. While
::    performing a build, Ford registers each namespace access as a dependency
::    and also notes whether the dependency is "live," meaning the path within
::    the namespace updates with time. For example a live Clay dependency would
::    update the +case within the +beam over time.
::
::    A request to perform a build without subscribing to its future changes is
::    called a "once build."
::
::    After finishing a build, Ford subscribes to updates on the build's
::    dependencies. For now, this just means it subscribes to Clay for file
::    changes. Whenever any of the files in the subscription have new contents,
::    Clay will notify Ford, which will then rerun any live builds that depend
::    on any of the changed files and send its subscribers the new results.
::
::    This matches the semantics of live builds defined above. If someone had
::    asked for a build of the schematic with a formal date d2 just before the
::    changed Clay files, Ford would respond with the result of the previous
::    build with formal date d1, which would still be an accurate
::    representation of the schematic's result at d2, since Ford knows none of
::    its dependencies changed between d1 and d2.
::
::    Note that Ford can only calculate dependencies after running a build,
::    not before. This is because Ford can be thought of as an interpreter for
::    schematics, rather than a compiler, in the sense that it can't have a
::    dependency-gathering step followed by a build step. The dependencies of
::    some schematics must be calculated based on results, e.g. the %alts
::    schematic, which tries a sequence of sub-builds until one succeeds. If
::    the first sub-build succeeds, the build depends only on that first
::    sub-build, but if the first fails and the second succeeds, the build
::    depends on both.
::
::    This dynamicity implies we don't know what we depend on until we depend
::    on it. Most build systems have this property, but this part of Ford's
::    job is easier than for most Unix-based build systems: Ford draws all
::    resources from an immutable namespace, and it can track every access of
::    that namespace.
::
::    Ford might produce a build's result asynchronously, in a subsequent Arvo
::    event. This happens when accessing the namespace doesn't complete
::    synchronously, such as when grabbing a file from another ship. Ford
::    guarantees it will respond with build results in chronological order
::    using the formal date, not the order in which the builds completed.
::
::    Ford does not guarantee it will notify a subscriber of a changed build
::    only once per change. In common usage it will not send duplicate
::    notifications, but it might if its cache was recently wiped.
::
::    Ford uses dependency tracking, caching, and results of previous builds
::    to eliminate excess work. When rerunning a live build, Ford "promotes"
::    previous results to the new time if the build's dependencies hvaen't
::    changed since the previous build's formal date. Ford does this check
::    for each build in a tree of sub-builds under the "root build," which
::    is the build that was requested directly.
::
::    In addition to the main %build +task sub-type, Ford also supports
::    four other commands:
::
::    %kill: cancel a build
::
::      A once build in progress will be canceled, including all of its
::      sub-builds that aren't part of any other builds.
::
::      A live build's subscriptions will be canceled, its completed results
::      will be deleted, and its dependency tracking information will be
::      deleted. If a rebuild is in progress, it will be canceled.
::
::    %keep: resize caches
::
::      Ford maintains two caches: a :compiler-cache that stores
::      content-addressed compiler operations, such as parsing, compiling,
::      and type inference; and a :build-cache that stores previously
::      completed build trees along with their results and dependency tracking.
::
::      The %keep command resets the maximum sizes of these caches, deleting
::      entries if necessary.
::
::    %wipe: decimate storage
::
::      The %wipe command removes build results from storage to free memory.
::      It deletes the specified percentage of build results, in LRU
::      (Least Recently Used) order. It also removes entries from the compiler
::      cache. It does not remove dependency tracking information.
::
::    %wegh: report memory usage
::
::      Like all vanes, Ford can also be asked to produce a human-readable
::      report of its memory usage. Nock cannot calculate its own memory use
::      directly, so instead we produce the nouns themselves, which the runtime
::      "weighs" based on its memory model.
::
::    For details on Ford's implementation, consult Ford's vane interface core
::    near the bottom of the file.
::
::  pit: a +vase of the hoon+zuse kernel, which is a deeply nested core
::
|=  pit=vase
::
=,  contain
=,  ford
::  ford internal data structures
::
=>  =~
|%
  ::  TODO: move back to zuse
  ++  able
    |%
    ::  +task:able:ford: requests to ford
    ::
    +$  task
      $%  ::  %build: perform a build, either live or once
          ::
          $:  %build
              ::  our: who our ship is (remove after cc-release)
              ::
              our=@p
              ::  live: whether we run this build live
              ::
              ::    A live build will subscribe to further updates and keep the
              ::    build around.
              ::
              live=?
              ::  plan: the schematic to build
              ::
              =schematic
          ==
          ::  %keep: reset cache sizes
          ::
          [%keep compiler-cache-size=@ud]
          ::  %kill: stop a build; send on same duct as original %build request
          ::
          $:  %kill
              ::  our: who our ship is (remove after cc-release)
              ::
              our=@p
          ==
          ::  %wegh: produce memory usage information
          ::
          [%wegh ~]
          ::  %wipe: wipes stored builds
          ::
          [%wipe percent-to-remove=@ud]
      ==
    ::  +gift:able:ford: responses from ford
    ::
    +$  gift
      $%  ::  %mass: memory usage; response to %wegh +task
          ::
          [%mass p=mass]
          ::  %made: build result; response to %build +task
          ::
          $:  %made
              ::  date: formal date of the build
              ::
              date=@da
              ::  result: result of the build; either complete build, or error
              ::
              result=made-result
      ==  ==
    --
  ::  +made-result: the main payload for a %made +gift
  ::
  +$  made-result
    $%  ::  %complete: contains the result of the completed build
        ::
        [%complete result=(each vase tang)]
        ::  %incomplete: couldn't finish build; contains error message
        ::
        [%incomplete =tang]
    ==
  ::  +schematic: ford build request, as a function of time
  ::
  +$  schematic
    $~  [%ntdt !>(~)]
    ::    If the head of the +schematic is a pair, it's an auto-cons
    ::    schematic. Its result will be the pair of results of its
    ::    sub-schematics.
    ::
    $^  [head=schematic tail=schematic]
    ::
    $%  ::  %ntbn: /> with :subject as subject, evaluate :rest
        ::
        $:  %ntbn
            ::  subject: schematic that becomes new subject
            ::
            subject=schematic
            ::  rest: schematic to evaluate against result of :subject
            ::
            rest=schematic
        ==
        ::  %ntbs: /$ call a gate on a sample
        ::
        $:  %ntbs
            ::  gate: schematic whose result is a gate
            ::
            gate=schematic
            ::  sample:  schematic whose result will be the gate's sample
            ::
            sample=schematic
        ==
        ::  %ntdt: /. literal value. Produces its input unchanged.
        ::
        $:  %ntdt
            ::  literal: the value to be produced by the build
            ::
            literal=vase
        ==
        ::  %ntkt: /^ cast the result of a schematic using ^-
        ::
        $:  %ntkt
            ::  spec: schematic that produces a +spec to cast to
            ::
            spec=schematic
            ::  rest: schematic whose result will be cast to :spec
            ::
            rest=schematic
        ==
        ::  %ntcb: /_ compile and evaluate a hoon against the current subject
        ::
        $:  %ntcb
            ::  hoon: a hoon to be evaluated against the subject
            ::
            =hoon
        ==
        ::  %ntls: /+ prepend result of :head schematic to subject
        ::
        $:  %ntls
            ::  head: schematic that produces new head of subject
            ::
            head=schematic
            ::  rest: schematic to evaluate against augmented subject
            ::
            rest=schematic
        ==
        ::  %ntnt: // eval new schematic against new subject, like Nock 2
        ::
        $:  %ntnt
            ::  subject: schematic that produces new subject
            ::
            subject=schematic
            ::  schematic: schematic that produces new schematic
            ::
            =schematic
        ==
        ::  %ntpd: /& load and compile a hoon file against the standard library
        ::
        ::    Schematics can only be resolved when specifying a time,
        ::    which will turn the +rail into a +beam (full absolute path).
        ::
        $:  %ntpd
            ::  rail: schematic that evaluates to a hoon filepath to load
            ::
            rail=schematic
        ==
        ::  %ntts: /= wrap a face around result
        ::
        $:  %ntts
            ::  face: the name of the face to wrap around the result
            ::
            face=term
            ::  rest: a sub-schematic whose result will get labeled
            ::
            rest=schematic
        ==
        ::  %nttr: /* lookup a value from the urbit namespace (do a scry)
        ::
        $:  %nttr
            ::  term: request type, e.g. %cx or %cz
            ::
            =term
            ::  rail: schematic that evaluates to a :rail to load
            ::
            ::    Schematics can only be resolved when specifying a time,
            ::    which will convert this +resource into a +scry-request.
            ::
            rail=schematic
        ==
        ::  %ntvt: /@ pins a sub-schematic to a date
        ::
        ::    There is a difference between live builds and once builds. In
        ::    live builds, we produce results over and over again and aren't
        ::    pinned to a specifc time. In once builds, we want to specify a
        ::    specific date, which we apply recursively to any sub-schematics
        ::    contained within :schematic.
        ::
        ::    If a build has a %pin at the top level, we consider it to be a
        ::    once build. Otherwise, we consider it to be a live build. We do
        ::    this so schematics which depend on the result of a once build can
        ::    be cached, giving the client explicit control over the caching
        ::    behaviour.
        ::
        $:  %ntvt
            ::  date: time at which to perform the build
            ::
            date=@da
            ::  schematic: wrapped schematic of pinned time
            ::
            rest=schematic
        ==
        ::  %ntwt: /? asynchronous conditional
        ::
        $:  %ntwt
            ::  if: schematic that evaluates to a conditional
            ::
            if=schematic
            ::  then: schematic to evaluate if conditional was true
            ::
            then=schematic
            ::  else: schematic to evaluate if conditional was false
            ::
            else=schematic
    ==  ==
::  +move: arvo moves that ford can emit
::
+$  move
  ::
  $:  ::  duct: request identifier
      ::
      =duct
      ::  card: move contents; either a %pass with +note or a %give with +gift
      ::
      card=(wind note gift)
  ==
+$  gift
  $%  ::  %mass: memory usage; response to %wegh +task
      ::
      [%mass p=mass]
      ::  %meta: produce a metavase so arvo can do its untyped magic
      ::
      $:  %meta
          ::  p: the type of the %made card
          ::
          ::    The type that :p represents should have a vase where
          ::    :q has a `[p=* q=*]`. We can't statically guarantee that
          ::    we produce a vase, but as long as it nests, Arvo will
          ::    figure out what to do with it.
          ::
          p=type
          ::  q: value in the metavase, containing actual build result
          ::
          $=  q
          $:  %made
              date=@da
              $=  made-result
              $%  [%complete result=(each [p=* q=*] tang)]
                  [%incomplete =tang]
  ==  ==  ==  ==
::  +note: private request from ford to another vane
::
+$  note
  $%  ::  %c: to clay
      ::
      $:  %c
          ::  %warp: internal (intra-ship) file request
          ::
          $%  $:  %warp
                  ::  sock: pair of requesting ship, requestee ship
                  ::
                  =sock
                  ::  riff: clay request contents
                  ::
                  riff=riff:clay
  ==  ==  ==  ==
::  +sign: private response from another vane to ford
::
+$  sign
  $%  ::  %c: from clay
      ::
      $:  %c
          ::  %writ: internal (intra-ship) file response
          ::
          $%  $:  %writ
                  ::  riot: response contents
                  ::
                  riot=riot:clay
              ==
              ::  %wris: response to %mult; many changed files
              ::
              $:  %wris
                  ::  case: case of the new files
                  ::
                  ::    %wris can only return dates to us.
                  ::
                  case=[%da p=@da]
                  ::  care-paths: the +care:clay and +path of each file
                  ::
                  care-paths=(set [care=care:clay =path])
  ==  ==  ==  ==
--
|%
::  +axle: overall ford state
::
+=  axle
  $:  ::  date: date at which ford's state was updated to this data structure
      ::
      date=%~2018.6.28
      ::  state-by-ship: storage for all the @p's this ford has been
      ::
      ::    Once the cc-release boot sequence lands, we can remove this
      ::    mapping, since an arvo will not change @p identities. until
      ::    then, we need to support a ship booting as a comet before
      ::    becoming its adult identity.
      ::
      state-by-ship=(map ship ford-state)
  ==
::  +ford-state: all state that ford maintains for a @p ship identity
::
+=  ford-state
  $:  ::  ducts: per-duct state machine for all incoming ducts (build requests)
      ::
      ::    Ford tracks every duct that has requested a build until it has
      ::    finished dealing with that request.
      ::
      ::    For live ducts, we store the duct while we repeatedly run new
      ::    versions of the live build it requested until it is explicitly
      ::    canceled by the requester.
      ::
      ::    A once (non-live) duct, on the other hand, will be removed
      ::    as soon as the requested build has been completed.
      ::
      ducts=(map duct duct-status)
      ::  pending-scrys: outgoing requests for static resources
      ::
      pending-scrys=(request-tracker scry-request)
      ::  pending-subscriptions: outgoing subscriptions on live resources
      ::
      pending-subscriptions=(request-tracker subscription)
      ::  compiler-cache: clock based cache of build results
      ::
      compiler-cache=*  ::  (clock compiler-cache-key (each vase tang))
  ==
::  +duct-status: information relating a build to a duct
::
::    TODO: convert %live and %once to %& and %|
::    TODO: reorganize for convenience
::
+=  duct-status
  $:  ::  live: whether this duct is being run live
      ::
      $=  live
      $%  $:  %once
              ::  date: formal date of the in-progress build
              ::
              date=@da
              ::  scry-results: stored results of /* sub-builds
              ::
              =scry-results
          ==
          $:  %live
              ::  in-progress: unfinished live build, if we've started one
              ::
              in-progress=(unit [date=@da =scry-results])
              ::  last-completed: the last build we completed
              ::
              ::    If there is no :subscription, then the last build did not
              ::    depend on any live resources.
              ::
              ::    NOTE: Only allowing a single subscription implies that a
              ::    single live build can only depend on live resources from a
              ::    single disc. We don't have a working plan for fixing this
              ::    and will need to think very hard about the future.
              ::
              $=  last-completed
              (unit [date=@da =scry-results subscription=(unit subscription)])
      ==  ==
      ::  root-schematic: the requested build for this duct
      ::
      root-schematic=schematic
  ==
+$  scry-results  (map scry-request (unit (unit cage)))
::
+$  real-product  (unit (each vase tang))
+$  product       (unit (each [p=* q=*] tang))
::
+$  real-progress
  $:  blocks=(set scry-request)
      live-resources=(set [=term =rail])
      cache=(clock compiler-cache-key (each vase tang))
  ==
+$  progress  [blocks=* live-resources=* cache=*]
::  +build: a referentially transparent request for a build
::
::    Each unique +build will always produce the same +build-result
::    when run (if it completes). A live build consists of a sequence of
::    instances of +build with the same :schematic and increasing :date.
::
+=  build
  $:  ::  date: the formal date of this build; unrelated to time of execution
      ::
      date=@da
      ::  schematic: the schematic that determines how to run this build
      ::
      =schematic
  ==
::  +request-tracker: generic tracker and multiplexer for pending requests
::
++  request-tracker
  |*  request-type=mold
  %+  map  request-type
  $:  ::  waiting: ducts blocked on this request
      ::
      waiting=(set duct)
      ::  originator: the duct that kicked off the request
      ::
      originator=duct
  ==
::  +subscription: a single subscription to changes on a set of resources
::
+=  subscription
  $:  ::  date: date this was made
      ::
      date=@da
      ::  disc: ship and desk for all :resources
      ::
      =disc
      ::  resources: we will be notified if any of these resources change
      ::
      resources=(set resource)
  ==
::  +scry-request: parsed arguments to a scry operation
::
+=  scry-request
  $:  ::  vane: the vane from which to make the request
      ::
      ::    If we add other vanes in the future, this will become a fork type.
      ::    For now, though, Ford only knows how to make asynchronous scry
      ::    requests to Clay.
      ::
      vane=%c
      ::  care: type of request
      ::
      care=care:clay
      ::  beam: request path
      ::
      =beam
  ==
::  +compiler-cache-key: content addressable build definitions
::
+=  compiler-cache-key
  $%  [%call gate=vase sample=vase]
      [%ride formula=hoon subject=vase]
      [%slim subject-type=type formula=hoon]
      [%slit gate=type sample=type]
  ==
--
=,  format
|%
::  +current-status: retrieve current build status from :duct-status
::
++  current-status
  |=  =duct-status
  ^-  [date=@da =scry-results]
  ::
  ?:  ?=(%once -.live.duct-status)
    +.live.duct-status
  ::
  ?<  ?=(~ in-progress.live.duct-status)
  u.in-progress.live.duct-status
::  +complete-scrys: filter a +scry-results for completed requests
::
++  complete-scrys
  |=  =scry-results
  ^+  scry-results
  ::
  %-  ~(gas in *_scry-results)
  %+  skim  ~(tap by scry-results)
  |=  [=scry-request scry-result=(unit (unit cage))]
  ::
  ?=(^ scry-result)
::  +incomplete-scrys: filter a +scry-results for pending requests
::
++  incomplete-scrys
  |=  =scry-results
  ^+  scry-results
  ::
  %-  ~(gas in *_scry-results)
  %+  skip  ~(tap by scry-results)
  |=  [=scry-request scry-result=(unit (unit cage))]
  ::
  ?=(^ scry-result)
::  +path-to-scry-request: parse :path's components into :vane, :care, and :rail
::
++  path-to-scry-request
  |=  =path
  ^-  (unit scry-request)
  ::
  ?~  path
    ~
  ?~  vane=((soft ,%c) (end 3 1 i.path))
    ~
  ?~  care=((soft care:clay) (rsh 3 1 i.path))
    ~
  ?~  beam=(de-beam t.path)
    ~
  ?.  ?=(%da -.r.u.beam)
    ~
  `[u.vane u.care u.beam]
::  +get-request-ducts: all ducts waiting on this request
::
++  get-request-ducts
  |*  [tracker=(request-tracker) request=*]
  ^-  (list duct)
  ::
  ~(tap in waiting:(~(got by tracker) request))
::  +put-request: associates a +duct with a request
::
++  put-request
  |*  [tracker=(request-tracker) request=* =duct]
  ::
  %+  ~(put by tracker)  request
  ?~  existing=(~(get by tracker) request)
    [(sy duct ~) duct]
  u.existing(waiting (~(put in waiting.u.existing) duct))
::  +del-request: remove a duct and produce the originating duct if empty
::
++  del-request
  |*  [tracker=(request-tracker) request=* =duct]
  ^-  [(unit ^duct) _tracker]
  ::  remove :duct from the existing :record of this :request
  ::
  =/  record  (~(got by tracker) request)
  =.  waiting.record  (~(del in waiting.record) duct)
  ::  if no more ducts wait on :request, delete it
  ::
  ?^  waiting.record
    [~ (~(put by tracker) request record)]
  [`originator.record (~(del by tracker) request)]
::  +per-event: per-event core; main build engine
::
::    This arm produces a gate that when called with state and event
::    information produces the core of Ford's main build engine.
::
::    The main build engine core has the following entry points:
::
::      +start-build  start performing a build
::      +rebuild      rerun a live build at a new date
::      +unblock      continue a build that was waiting on a resource
::      +cancel       stop trying to run a build and delete its tracking info
::      +wipe         wipe the build storage to free memory
::      +keep         resize caches, deleting entries if necessary
::
::    The main internal arm isevent-core-loop, which is called from +start-build,
::    +rebuild, and +unblock.event-core defines Ford's build loop.
::
++  per-event
  ::  moves: the moves to be sent out at the end of this event, reversed
  ::
  =|  moves=(list move)
  ::  gate that produces the +per-event core from event information
  ::
  ::    Produces a core containing Ford's main build engine.
  ::
  ~%  %f  ..is  ~
  |=  [[our=@p =duct now=@da scry=sley] state=ford-state]
  ::
  ~%  %per-event  +  ~
  |%
  ++  event-core  .
  ::  +finalize: extract moves and state from the +per-event core
  ::
  ::    Run once at the end of processing an event.
  ::
  ++  finalize
    ^-  [(list move) ford-state]
    [(flop moves) state]
  ::  |entry-points: externally fired arms
  ::
  ::+|  entry-points
  ::
  ::  +start-build: perform a fresh +build, either live or once
  ::
  ::    This might complete the build, or the build might block on one or more
  ::    requests for resources. Calls +run-root-build.
  ::
  ++  start-build
    ~/  %start-build
    |=  [=build live=?]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::  associate :duct with :build in :ducts.state
    ::
    =.  ducts.state
      %+  ~(put by ducts.state)  duct
      :_  schematic.build
      ?:  live
        [%live in-progress=`[date.build scry-results=~] last-completed=~]
      [%once date.build scry-results=~]
    ::  runevent-core on :build in a loop until it completes or blocks
    ::
    (run-root-build build duct live)
  ::  +rebuild: rebuild a live build based on +resource updates
  ::
  ::    TODO: more detailed docs once this stabilizes
  ::
  ++  rebuild
    ~/  %rebuild
    |=  $:  =subscription
            new-date=@da
            =disc
            care-paths=(set [care=care:clay =path])
        ==
    ^-  [(list move) ford-state]
    ::
    ~|  [%rebuilding new-date disc]
    ::
    =<  finalize
    ::  mark this subscription as complete now that we've heard a response
    ::
    =.  pending-subscriptions.state
      +:(del-request pending-subscriptions.state subscription duct)
    ::
    =/  changed-scry-requests=(set scry-request)
      %-  ~(gas in *(set scry-request))
      %+  turn  ~(tap in care-paths)
      |=  [care=care:clay =path]
      ^-  scry-request
      ::
      [vane=%c care `beam`[[ship.disc desk.disc %da new-date] (flop path)]]
    ::  sanity check; only rebuild live builds, not once builds
    ::
    =/  =duct-status  (~(got by ducts.state) duct)
    ?>  ?=(%live -.live.duct-status)
    ::  sanity check; only rebuild once we've completed the previous one
    ::
    ?>  ?=(~ in-progress.live.duct-status)
    ?>  ?=(^ last-completed.live.duct-status)
    ::
    =/  previous-subscription=^subscription
      %+  fall  subscription.u.last-completed.live.duct-status
      [date.u.last-completed.live.duct-status disc ~]
    ::
    =/  previous-scry-results=(list [scry-request (unit (unit cage))])
      %+  skim
        %~  tap  by
        %-  incomplete-scrys
        scry-results.u.last-completed.live.duct-status
      ::
      |=  [=scry-request scry-result=(unit (unit cage))]
      ^-  ?
      ::
      ?.  =([%da date.previous-subscription] r.beam.scry-request)
        %.n
      ::
      %-  ~(has in resources.previous-subscription)
      `resource`[vane care [[p q] s]:beam]:scry-request
    ::  if we subscribed to a resource that didn't update, it's unchanged
    ::
    =/  unchanged-scry-results=scry-results
      %-  ~(gas by *scry-results)
      %+  murn  previous-scry-results
      |=  [=scry-request scry-result=(unit (unit cage))]
      ^-  (unit [^scry-request (unit (unit cage))])
      ::
      ?:  (~(has in changed-scry-requests) scry-request)
        ~
      `[scry-request scry-result]
    ::  set the in-progress date for this rebuild and copy scry results
    ::
    =.  ducts.state
      %+  ~(put by ducts.state)  duct
      duct-status(in-progress.live `[new-date unchanged-scry-results])
    ::
    (run-root-build [new-date root-schematic.duct-status] duct live=%.y)
  ::  +unblock: continue builds that had blocked on :resource
  ::
  ::    A build can be stymied temporarily if it depends on a resource
  ::    that must be fetched asynchronously. +unblock is called when
  ::    we receive a response to a resource request that blocked a build.
  ::
  ::    We pick up the build from where we left off, starting with the
  ::    %scry build that blocked on this resource last time we tried it.
  ::
  ::    TODO: rewrite from scratch
  ::
  ++  unblock
    ~/  %unblock
    |=  [=scry-request scry-result=(unit cage)]
    ^-  [(list move) ford-state]
    ::
    =<  finalize
    ::  place :scry-result in :scry-results.per-event
    ::
    ::    We don't want to call the actual +scry function again,
    ::    because we already tried that in a previous event and it
    ::    had no synchronous answer. This +unblock call is a result
    ::    of the response to the asynchronous request we made to
    ::    retrieve that resource from another vane.
    ::
    =/  =duct-status  (~(got by ducts.state) duct)
    ::
    =.  duct-status
      ?:  ?=(%once -.live.duct-status)
        %_    duct-status
            scry-results.live
          (~(put by scry-results.live.duct-status) scry-request `scry-result)
        ==
      ::
      ?<  ?=(~ in-progress.live.duct-status)
      ::
      %_    duct-status
          scry-results.u.in-progress.live
        %-  ~(put by scry-results.u.in-progress.live.duct-status)
        [scry-request `scry-result]
      ==
    ::
    =.  ducts.state  (~(put by ducts.state) duct duct-status)
    ::  mark this +scry-request as complete now that we have a response
    ::
    =.  pending-scrys.state
      +:(del-request pending-scrys.state scry-request duct)
    ::
    =/  date=@da               date:(current-status duct-status)
    =/  unblocked-build=build  [date root-schematic.duct-status]
    =/  live=?                 ?=(%live -.live.duct-status)
    ::
    (run-root-build unblocked-build duct live)
  ::  +wipe: forcibly decimate build results from the state
  ::
  ++  wipe
    ~/  %wipe
    |=  percent-to-remove=@ud
    ^+  state
    ::  removing 0% is the same as doing nothing, so do nothing
    ::
    ?:  =(0 percent-to-remove)
      ~&  %wipe-no-op
      state
    ::
    ~|  [%wipe percent-to-remove=percent-to-remove]
    ?>  (lte percent-to-remove 100)
    ::  determine how many builds should remain after decimation
    ::
    ::    This formula has the property that repeated applications
    ::    of +wipe with anything other than 100% retention rate will
    ::    always eventually remove every build.
    ::
    =/  num-completed-builds=@ud
      %-  (hard @ud)
      %+  run-gate
        |=((clock * *) size)
      compiler-cache.state
    ::
    =/  percent-to-keep=@ud  (sub 100 percent-to-remove)
    =/  num-to-keep=@ud  (div (mul percent-to-keep num-completed-builds) 100)
    =/  num-to-remove=@ud  (sub num-completed-builds num-to-keep)
    ::
    =.  compiler-cache.state
      %-  run-gate
      :_  compiler-cache.state
      ::
      |=  cache=(clock compiler-cache-key (each vase tang))
      =/  by-clock  (by-clock compiler-cache-key (each vase tang))
      ::
      ?:  =(num-to-remove size.cache)
        ~(purge by-clock cache)
      (~(trim by-clock cache) num-to-remove)
    ::
    state
  ::  +keep: resize caches
  ::
  ::    Ford maintains two caches: a :build-cache for caching previously
  ::    completed build trees, and a :compiler-cache for caching various
  ::    compiler operations that tend to be shared among multiple builds.
  ::
  ::    To handle this command, we reset the maximum sizes of both of
  ::    these caches, removing entries from the caches if necessary.
  ::
  ::    TODO: remove second cache argument
  ::
  ++  keep
    ~/  %keep
    |=  compiler-cache-size=@ud
    ^+  state
    ::  resize the :compiler-cache
    ::
    %_    state
        compiler-cache
      =/  resize-gate  ~(resize (by-clock * *) *(clock))
      %+  run-gate  resize-gate(+>+< compiler-cache.state)
      compiler-cache-size
    ==
  ::  +cancel: cancel a build
  ::
  ::    When called on a live build, removes all tracking related to the live
  ::    build, and no more %made moves will be sent for that build.
  ::
  ::    When called on a once build, removes all tracking related to the once
  ::    build, and that build will never be completed or have a %made sent.
  ::
  ::    When called on a build that isn't registered in :state, such as a
  ::    completed once build, or a build that has already been canceled,
  ::    prints and no-ops.
  ::
  ++  cancel  ^+  [moves state]
    ::
    =<  finalize
    ::
    ?~  duct-status=(~(get by ducts.state) duct)
      ~&  [%no-build-for-duct duct]
      event-core
    ::  :duct is being canceled, so remove it unconditionally
    ::
    =.  ducts.state  (~(del by ducts.state) duct)
    ::  if the duct was not live, cancel any in-progress builds
    ::
    ?:  ?=(%once -.live.u.duct-status)
      ::
      %-  cancel-scrys
      [date.live root-schematic]:u.duct-status
    ::  if the duct was live and has an unfinished build, cancel it
    ::
    =?  event-core  ?=(^ in-progress.live.u.duct-status)
      ::
      %-  cancel-scrys
      [date.u.in-progress.live root-schematic]:u.duct-status
    ::  if there is no completed build for the live duct, we're done
    ::
    ?~  last-completed=last-completed.live.u.duct-status
      event-core
    ::  cancel the pending subscription if there is one
    ::
    ?~  subscription.u.last-completed
      event-core
    (cancel-clay-subscription u.subscription.u.last-completed)
  ::  +cancel-scrys: cancel all blocked %scry sub-builds of :root-builds
  ::
  ++  cancel-scrys
    |=  root-build=build
    ^+  event-core
    ::
    =/  blocked-scry-requests=(list scry-request)
      %~  tap  in
      %~  key  by
      %-  incomplete-scrys
      =<  scry-results
      %-  current-status
      (~(got by ducts.state) duct)
    ::
    |-  ^+  event-core
    ?~  blocked-scry-requests  event-core
    ::
    =.  event-core  (cancel-scry-request i.blocked-scry-requests)
    ::
    $(blocked-scry-requests t.blocked-scry-requests)
  ::  |construction: arms for performing builds
  ::
  ::+|  construction
  ::
  ::  +run-root-build: run a requested build, performing all indicated effects
  ::
  ::    TODO: use :duct from :event-core?
  ::
  ++  run-root-build
    |=  [root-build=build =^duct live=?]
    ^+  event-core
    ::  ~&  %run-root-build
    ::
    =+  [product progress]=(run-build root-build live)
    ::
    =.  compiler-cache.state  cache.progress
    ::
    ?~  product
      (on-build-blocked root-build duct blocks.progress)
    ::
    ?:  live
      %-  on-live-build-completed
      [root-build u.product duct live-resources.progress]
    ::
    (on-once-build-completed root-build u.product duct)
  ::  +run-build: run a build recursively, without updating permanent state
  ::
  ++  run-build
    =/  =progress
      [blocks=`*`~ live-resources=`*`~ cache=`*`compiler-cache.state]
    ::
    =/  subject=vase  pit
    =/  depth=@ud  0
    ::
    |=  [[date=@da =schematic] live=?]
    ^-  [product ^progress]
    ::
    |^  ^-  [product ^progress]
        =.  depth  +(depth)
        ::
        ::~&  (pad ?@(-.schematic `tape`['%' (trip `@t`-.schematic)] "^"))
        ::
        ?-    -.schematic
            ^
          ::~&  (pad "^ head start")
          =^  head  progress  $(schematic -.schematic)
          ::~&  (pad "^ head end")
          ?:  ?=([~ %| *] head)
            (wrap-error p.u.head [%leaf "ford: head of cell build failed:"]~)
          ::
          ::~&  (pad "^ tail start")
          =^  tail  progress  $(schematic +.schematic)
          ::~&  (pad "^ tail end")
          ?:  ?=([~ %| *] tail)
            (wrap-error p.u.tail [%leaf "ford: tail of cell build failed:"]~)
          ::
          ?~  head  block
          ?~  tail  block
          ::
          (succeed [[%cell p.p.u.head p.p.u.tail] q.p.u.head q.p.u.tail])
        ::
            %ntbn
          ::~&  (pad "%ntbn subject start")
          =^  new-subject  progress  $(schematic subject.schematic)
          ::~&  (pad "%ntbn subject end")
          ?~  new-subject
            block
          ?:  ?=([~ %| *] new-subject)
            (wrap-error p.u.new-subject [%leaf "ford: /> subject failed:"]~)
          ::
          ::~&  (pad "%ntbn rest start")
          =/  raw-gate  ..^$(subject p.u.new-subject, schematic rest.schematic)
          ::
          ::=-  ~&  (pad "%ntbn rest end")  -
          %-  cast-raw-result
          .*(raw-gate [9 2 0 1])
        ::
            %ntbs
          ::~&  (pad "%ntbs start gate")
          =^  gate  progress  $(schematic gate.schematic)
          ::~&  (pad "%ntbs end gate")
          ::
          ?:  ?=([~ %| *] gate)
            (wrap-error p.u.gate [%leaf "ford: /$ gate build failed"]~)
          ::
          ::~&  (pad "%ntbs start sample")
          =^  sample  progress  $(schematic sample.schematic)
          ::~&  (pad "%ntbs end sample")
          ::
          ?:  ?=([~ %| *] sample)
            (wrap-error p.u.sample [%leaf "ford: /$ sample build failed"]~)
          ::
          ?~  gate    block
          ?~  sample  block
          ::
          |^  ^-  [product ^progress]
              ::
              ::~&  (pad "%ntbs about to infer")
              =^  inferred-product-type  progress
                %-  cast-raw-result
                (run-gate infer-product-type p.p.u.gate p.p.u.sample)
              ::~&  (pad "%ntbs inferred")
              ::
              ?<  ?=(~ inferred-product-type)
              ::
              ?:  ?=([~ %| *] inferred-product-type)
                (fail p.u.inferred-product-type)
              ::
              ::=-  ~&  (pad "%ntbs done")  -
              %-  cast-raw-result
              %+  run-gate  perform-call
              [p.p.u.inferred-product-type q.p.u.gate q.p.u.sample]
          ::
          ++  infer-product-type
            |=  [gate-type=type sample-type=type]
            ^-  [product ^progress]
            ::
            =/  cache-key  [%slit gate-type sample-type]
            =^  cache-result  progress  (access-cache cache-key)
            ::
            ?^  cache-result
              [cache-result progress]
            ::
            =/  product=(each type tang)
              (mule |.((slit gate-type sample-type)))
            ::
            ?:  ?=(%| -.product)
              ::~&  (pad "%ntbs slit failed")
              ::  flesh out error message if failed
              ::
              =.  p.product
                :*  (~(dunk ut sample-type) %have)
                    (~(dunk ut (~(peek ut gate-type) %free 6)) %want)
                    leaf+"ford: %call: slit failed:"
                    p.product
                ==
              ::
              =.  cache.progress  (put-in-cache cache-key product)
              ::
              [`product progress]
            ::~&  (pad "%ntbs slit succeeded")
            ::  contrive a vase out of the slit result; we'll only use the type
            ::
            ::    This is a bit ugly, but it means we don't need a special
            ::    type of cache line for +slit.
            ::
            =/  product-vase=vase  [p.product 0]
            ::~&  (pad "%ntbs slit product {<-.p.product>}")
            ::
            =.  cache.progress  (put-in-cache cache-key [%& product-vase])
            ::
            (succeed product-vase)
          ::
          ++  perform-call
            |=  [product-type=type gate=vase sample=vase]
            ^-  [product ^progress]
            ::
            =/  cache-key  [%call gate sample]
            =^  cache-result  progress  (access-cache cache-key)
            ::
            ?^  cache-result
              [cache-result progress]
            ::
            =/  product  (mong [gate sample] intercepted-scry)
            ::~&  (pad "%ntbs-ran-product")
            ?-    -.product
                %0
              =/  success=vase  [product-type p.product]
              =.  cache.progress  (put-in-cache cache-key [%& success])
              ::
              (succeed success)
            ::
                %1
              =/  blocked-paths=(list path)  ((hard (list path)) p.product)
              =.  blocks.progress
                %+  run-gate
                  =/  gate  ~(gas in *(set [=term =beam]))
                  gate(+>+< a=blocks.progress)
                (turn blocked-paths path-to-scry-request)
              ::
              block
            ::
                %2
              =/  error=tang
                (welp p.product [%leaf "ford: /$ execution failed"]~)
              ::
              =.  cache.progress  (put-in-cache cache-key [%| error])
              ::
              (fail error)
            ==
          --
        ::
            %ntcb
          ::
          =/  ride-key  [%ride hoon.schematic subject]
          =^  ride-result  progress  (access-cache ride-key)
          ::
          ?^  ride-result
            [ride-result progress]
          ::
          |^  =^  slim-result  progress  (run-slim hoon.schematic p.subject)
              ?<  ?=(~ slim-result)
              ?:  ?=([~ %| *] slim-result)
                ::~&  (pad "%ntcb slim fail")
                (wrap-error p.u.slim-result [%leaf "ford: /_ slim failed:"]~)
              ::~&  (pad "%ntcb-ran-slim")
              ::
              =^  mock-result  progress  (run-mock q.subject q.p.u.slim-result)
              ::~&  (pad "%ntcb-ran-mock")
              ?~  mock-result
                block
              ::
              =/  full-result=(each [p=* q=*] tang)
                ?:  ?=([~ %| *] mock-result)
                  [%| [[%leaf "ford: /_ failed:"] p.u.mock-result]]
                [%& p.p.u.slim-result q.p.u.mock-result]
              ::
              =.  cache.progress  (put-in-cache ride-key full-result)
              ::
              %-  cast-raw-result
              [`full-result progress]
          ::
          ++  run-slim
            |=  [=hoon subject-type=type]
            ^-  [product ^progress]
            ::
            =/  slim-key  [%slim hoon.schematic p.subject]
            =^  slim-result  progress  (access-cache slim-key)
            ?^  slim-result
              [slim-result progress]
            ::
            =/  compiled=(each (pair type nock) tang)
              (mule |.((~(mint ut subject-type) [%noun hoon])))
            ::
            =.  cache.progress  (put-in-cache slim-key compiled)
            ::
            [`compiled progress]
          ::
          ++  run-mock
            |=  [raw-subject=* raw-formula=*]
            ^-  [product ^progress]
            ::
            =/  mock-result  (mock [raw-subject raw-formula] intercepted-scry)
            ::
            ?-  -.mock-result
                %0
              (succeed -:!>(**) p.mock-result)
            ::
                %1
              =/  blocked-paths=(list path)  ((hard (list path)) p.mock-result)
              =.  blocks.progress
                %+  run-gate
                  =/  gate  ~(gas in *(set [=term =beam]))
                  gate(+>+< a=blocks.progress)
                (turn blocked-paths path-to-scry-request)
              ::
              block
            ::
                %2
              (fail p.mock-result)
            ==
          --
        ::
            %ntdt
          (succeed literal.schematic)
        ::
            %ntkt
          ::  macro-expand to a %ride to apply the cast
          ::
          ::  />  :-  /=  spec  spec
          ::          /=  rest  rest
          ::  ^-(spec rest)
          ::
          =/  new-schematic=^schematic
            :+  %ntbn
              [[%ntts %spec spec.schematic] [%ntts %rest rest.schematic]]
            :-  %ntcb
            ^-  hoon
            [%kthp [%like ~[%spec] ~] [%limb %rest]]
          ::
          $(schematic new-schematic)
      ::
          %ntls
        $(schematic [%ntbn [head.schematic [%ntdt subject]] rest.schematic])
      ::
          %ntnt
        =^  new-subject  progress  $(schematic subject.schematic)
        ::~&  (pad "%ntnt-ran-subject")
        ::  ensure type safety inside the build so we get a real schematic
        ::
        =^  new-schematic  progress
          $(schematic [%ntkt [%ntdt !>(^schematic)] schematic.schematic])
        ::~&  (pad "%ntnt-ran-schematic")
        ::
        ?~  new-subject    block
        ?~  new-schematic  block
        ::
        ?:  ?=(%| -.u.new-subject)
          (wrap-error p.u.new-subject [%leaf "ford: // subject failed:"]~)
        ?:  ?=(%| -.u.new-schematic)
          (wrap-error p.u.new-schematic [%leaf "ford: // schematic failed:"]~)
        ::  we can't coerce :q.new-schematic to a vase, so run raw nock
        ::
        ::    We also don't virtualize with +mock, since Ford should never
        ::    error out; if the build fails, the error will be contained
        ::    in the product of the function.
        ::
        =/  raw-run-build-gate=*
          ..^$(subject p.u.new-subject, schematic q.p.u.new-schematic)
        ::
        =/  raw-product=*  .*(raw-run-build-gate [9 2 0 1])
        ::~&  (pad "%ntnt-ran-product")
        ::
        (cast-raw-result raw-product)
      ::
          %ntpd
        ::  TODO: define these helper functions outside this arm
        ::
        =/  parse-at-rail
          |=  [=rail source=@t]
          ^-  hoon
          !:
          ::~&  (pad "%parse-at-rail {<rail>}")
          =/  parse-path=path  (en-beam [[ship.disc desk.disc %ud 0] spur]:rail)
          (rain parse-path source)
        ::
        =/  extract-source
          |=  [=rail result=(unit [mark=term data=@t])]
          ^-  @t
          !:
          ::~&  (pad "%ntpd extracting")
          ?~  result
            ~|  "ford: /& failed: file not found at {<rail>}"  !!
          ?.  =(%hoon mark.u.result)
            ~|  "ford: /& failed: bad mark {<mark.u.result>} at {<rail>}"  !!
          ::~&  (pad "%ntpd extracted")
          ::
          data.u.result
        ::
        =/  new-schematic=^schematic
          ::  load source file
          ::
          :+  %ntls
            :+  %ntts  %rail
            :+  %ntkt  [%ntdt !>(rail)]
            rail.schematic
          ::  parse source file's contents to a +hoon
          ::
          :+  %ntls
            :+  %ntts  %parsed-hoon
            :+  %ntbs
              [%ntdt !>(parse-at-rail)]
            :-  [%ntcb [%limb %rail]]
            :+  %ntbs
              [%ntdt !>(extract-source)]
            :-  [%ntcb [%limb %rail]]
            [%nttr %cx [%ntcb [%limb %rail]]]
          ::  ride :parsed-hoon against the standard library to get a +schematic
          ::
          :+  %ntls
            :+  %ntts  %source-schematic
            :+  %ntnt
              [%ntdt pit]
            :-  %ntcb
            ^-  hoon
            :+  %clhp
              [%rock %tas %ntcb]
            [%limb %parsed-hoon]
          ::  evaluate :source-schematic with the standard library as subject
          ::
          :+  %ntnt
            [%ntdt pit]
          [%ntcb %limb %source-schematic]
        ::
        $(schematic new-schematic)
      ::
          %nttr
        ::  TODO cleanup
        ::
        =^  rail-result  progress
          $(schematic [%ntkt [%ntdt !>(rail)] rail.schematic])
        ::
        ?~  rail-result
          block
        ?:  ?=([~ %| *] rail-result)
          (wrap-error p.u.rail-result [%leaf "ford: /* rail build failed:"]~)
        ::
        =/  =rail  ((hard rail) q.p.u.rail-result)
        ::
        =?    live-resources.progress
            live
          =/  put-in  ~(put in *(set [=term =^rail]))
          %+  run-gate  put-in(+>+< live-resources.progress)
          [term.schematic rail]
        ::
        =/  =beam  [[ship.disc.rail desk.disc.rail %da date] spur.rail]
        ::
        =/  vane=(unit %c)         ((soft ,%c) (end 3 1 term.schematic))
        ?~  vane
          =/  scry-result=(unit (unit))  ((sloy scry) term.schematic beam)
          ?~  scry-result
            ::  TODO: figure out how to handle %incomplete
            ::
            %-  fail
            [%leaf "ford: scry request failed: {<[term.schematic beam]>}"]~
          ::
          (handle-scry-result u.scry-result)
        ::
        =/  care=(unit care:clay)  ((soft care:clay) (rsh 3 1 term.schematic))
        ?~  care
          =/  scry-result=(unit (unit))  ((sloy scry) term.schematic beam)
          ?~  scry-result
            ::  TODO: figure out how to handle %incomplete
            ::
            %-  fail
            [%leaf "ford: scry request failed: {<[term.schematic beam]>}"]~
          ::
          (handle-scry-result u.scry-result)
        ::
        =/  =scry-request  [u.vane u.care beam]
        ::
        =/  local-result=(unit (unit (unit cage)))
          %.  scry-request
          %~  get  by
          %-  complete-scrys
          =<  scry-results
          %-  current-status
          (~(got by ducts.state) duct)
        ::
        ?~  local-result
          =/  scry-result
            (scry [%141 %noun] ~ term.schematic beam)
          ::
          ?~  scry-result
            ::
            =.  blocks.progress
              =/  put-in  ~(put in *(set ^scry-request))
              %+  run-gate  put-in(+>+< blocks.progress)
              scry-request
            ::
            block
          ::
          (handle-scry-result u.scry-result)
        ::  we already filtered out blocked local results
        ::
        ?~  u.local-result  !!
        ::
        (handle-scry-result u.u.local-result)
      ::
          %ntts
        ::~&  (pad "%ntts-face {<face.schematic>}")
        =^  sub-result  progress  $(schematic rest.schematic)
        ?~  sub-result
          block
        ::
        ?:  ?=([~ %| *] sub-result)
          %+  wrap-error  p.u.sub-result
          [%leaf "ford: /= {<face.schematic>} failed:"]~
        ::  wrap :face.schematic around product type
        ::
        =.  p.p.u.sub-result  [%face face.schematic p.p.u.sub-result]
        ::
        (succeed p.u.sub-result)
      ::
          %ntvt
        $(date date.schematic, schematic rest.schematic, live %.n)
      ::
          %ntwt
        =^  if  progress  $(schematic [%ntkt [%ntdt !>(?)] if.schematic])
        ::
        ?~  if
          block
        ::
        ?:  ?=(%| -.u.if)
          (wrap-error p.u.if [%leaf "ford: /? conditional failed:"]~)
        ::
        ?+  q.p.u.if  !!
          %&  $(schematic then.schematic)
          %|  $(schematic else.schematic)
        ==
      ==
    ++  pad
      |=  a=tape
      ^-  tape
      (weld `tape`(reap depth ' ') a)
    ::  +cast-raw-result: runtime cast to placate the type system
    ::
    ++  cast-raw-result
      |=  raw-product=*
      ^-  [product ^progress]
      ::
      =/  result    -.raw-product
      =/  progress  +.raw-product
      ::
      ?>  ?=([blocks=* live-resources=* cache=*] progress)
      ::
      ?+    result  !!
          ::  succeed
          ::
          [~ %& success-vase=^]
        ::  assert we didn't block on anything
        ::
        ?>  ?=(~ blocks.progress)
        [[~ %& success-vase.result] progress]
      ::
          ::  fail
          ::
          [~ %| error-tang=*]
        ::
        [[~ %| ((hard tang) error-tang.result)] progress]
      ::
          ::  block
          ::
          ~
        [~ progress]
      ==
    ::  +wrap-error: wrap a failed sub-build's error with a message
    ::
    ++  wrap-error
      |=  [wrapped=tang wrapper=tang]
      ^-  [product ^progress]
      ::
      =/  combined-message  (weld wrapper wrapped)
      [[~ %| combined-message] progress]
    ::  +fail: produce an error with a message
    ::
    ++  fail
      |=  =tang
      ^-  [product ^progress]
      ::
      [[~ %| tang] progress]
    ::  +succeed: produce successful "vase"
    ::
    ++  succeed
      |=  success=*
      ^-  [product ^progress]
      ::  :success is really a vase, but we can't prove that
      ::
      ?>  ?=(^ success)
      ::
      [[~ %& success] progress]
    ::  +block: halt, producing a "set" of blocked resource requests
    ::
    ++  block
      ^-  [product ^progress]
      ::
      ?<  ?=(~ blocks.progress)
      ::
      [~ progress]
    ::  +handle-scry-result: convert complete scry result to vase product
    ::
    ++  handle-scry-result
      %+  corl  cast-raw-result
      |=  raw-scry-result=(unit *)
      %-  run-gate
      :_  raw-scry-result
      |=  scry-result=(unit cage)
      ^-  [product ^progress]
      ::
      ?~  scry-result
        (succeed !>(~))
      ::
      =/  =cage  u.scry-result
      =/  result-vase=vase  :(slop !>(~) !>(p.cage) q.cage)
      ::
      (succeed result-vase)
    ::  +put-in-cache: place :product in :cache.progress
    ::
    ++  put-in-cache
      |=  [compiler-cache-key=* product=*]
      ^+  cache.progress
      ::
      =/  put-gate  ~(put (by-clock * *) *(clock))
      %+  run-gate  put-gate(+>+< cache.progress)
      [compiler-cache-key product]
    ::  +access-cache: retrieve and freshen a cache entry
    ::
    ++  access-cache
      %+  corl  cast-raw-result
      |=  raw-cache-key=*
      ^-  [* *]
      ::
      =+  ^=  [entry updated-cache]
          =/  get-gate  ~(get (by-clock * *) *(clock))
          (run-gate get-gate(+>+< cache.progress) compiler-cache-key)
      ::
      ?~  entry
        [~ progress]
      ::
      [entry progress(cache updated-cache)]
    --
  ::  +on-build-blocked: perform effects of a build blocking on async resources
  ::
  ++  on-build-blocked
    |=  [=build =^duct blocks=*]
    ^+  event-core
    ::~&  %on-build-blocked
    ::
    =>  .(blocks ((hard (set scry-request)) blocks))
    ::
    =/  block-list=(list scry-request)  ~(tap in blocks)
    ::
    |-  ^+  event-core
    ?~  block-list  event-core
    ::
    =.  event-core  (start-scry-request i.block-list duct)
    ::
    $(block-list t.block-list)
  ::  +on-once-build-completed: perform effects of finishing a non-live build
  ::
  ++  on-once-build-completed
    |=  [=build result=(each [p=* q=*] tang) =^duct]
    ^+  event-core
    ::~&  %on-once-build-completed
    ::
    =.  ducts.state  (~(del by ducts.state) duct)
    ::
    (send-complete date.build result duct)
  ::  +on-live-build-completed: perform effects of finishing a live build
  ::
  ++  on-live-build-completed
    |=  $:  =build
            result=(each [p=* q=*] tang)
            =^duct
            live-resources=*
        ==
    ^+  event-core
    ::~&  %on-live-build-completed
    ::  cast :live-resources to a usable type
    ::
    =>  .(live-resources ((hard (set ,[=term =rail])) live-resources))
    ::  group :live-resources by disc
    ::
    =/  resource-list=(list [=term =rail])  ~(tap in live-resources)
    ::
    =|  resources-by-disc=(jug disc resource)
    =.  resources-by-disc
      |-  ^+  resources-by-disc
      ?~  resource-list  resources-by-disc
      ::
      =/  vane  ((hard ,%c) (end 3 1 term.i.resource-list))
      =/  care  ((hard care:clay) (rsh 3 1 term.i.resource-list))
      ::
      =/  =resource  [vane care rail.i.resource-list]
      ::
      =.  resources-by-disc
        (~(put ju resources-by-disc) disc.rail.resource resource)
      ::
      $(resource-list t.resource-list)
    ::  if :build depends on multiple discs, send an %incomplete and cancel
    ::
    ?:  (lth 1 ~(wyt by resources-by-disc))
      =.  ducts.state  (~(del by ducts.state) duct)
      ::
      =/  reason=tang  :~
        [%leaf "root build"]
        ::  TODO: [%leaf (build-to-tape build)]
        [%leaf "on duct:"]
        [%leaf "{<duct>}"]
        [%leaf "tried to subscribe to multiple discs:"]
        [%leaf "{<resources-by-disc>}"]
      ==
      ::
      (send-incomplete date.build reason duct)
    ::  TODO: don't send move if result is same as previous result
    ::
    =.  event-core  (send-complete date.build result duct)
    ::
    =/  subscription=(unit subscription)
      ?~  resources-by-disc
        ~
      `[date.build n.resources-by-disc]
    ::
    =?  event-core  ?=(^ subscription)
      (start-clay-subscription u.subscription)
    ::
    =.  ducts.state
      %+  ~(jab by ducts.state)  duct
      |=  =duct-status
      ^+  duct-status
      ::
      ?>  ?=(%live -.live.duct-status)
      ::
      %_    duct-status
          in-progress.live
        ~
      ::
          last-completed.live
        ?<  ?=(~ in-progress.live.duct-status)
        `[date.build scry-results.u.in-progress.live.duct-status subscription]
      ==
    ::
    event-core
  ::  |utilities:per-event: helper arms
  ::
  ::+|  utilities
  ::
  ::  +run-gate: run a gate using raw nock, untyped and unvirtualized
  ::
  ++  run-gate
    |=  [gate=* sample=*]
    ^-  *
    .*(gate(+< sample) [9 2 0 1])
  ::  +send-complete: send a move to respond with a completed build
  ::
  ++  send-complete
    |=  [date=@da raw-product=(each [p=* q=*] tang) =^duct]
    ^+  event-core
    ::
    =.  moves  :_  moves
      ^-  move
      :-  duct
      :-  %give
      :+  %meta
        p=-:!>([%made *@da %complete *(each vase tang)])
      q=[%made date %complete raw-product]
    ::
    event-core
  ::  +send-incomplete: send a move to indicate a build could not complete
  ::
  ++  send-incomplete
    |=  [date=@da =tang =^duct]
    ^+  event-core
    ::
    =.  moves  :_  moves
      ^-  move
      :-  duct
      :-  %give
      :+  %meta
        p=-:!>([%made date %incomplete tang])
      q=[%made date %incomplete tang]
    ::
    event-core
  ::  +intercepted-scry: augment real scry with local %scry build results
  ::
  ::    Try to deduplicate requests for possibly remote resources by looking up
  ::    the result in local state if the real scry has no synchronous
  ::    answer (it produced `~`).
  ::
  ++  intercepted-scry
    %-  sloy  ^-  slyd
    ~/  %intercepted-scry
    |=  [ref=* (unit (set monk)) =term =beam]
    ^-  (unit (unit (cask)))
    !:
    ::  if the actual scry produces a value, use that value; otherwise use local
    ::
    =/  scry-response  (scry +<.$)
    ::
    ?^  scry-response
      scry-response
    ::
    =/  vane=(unit %c)  ((soft ,%c) (end 3 1 term))
    ?~  vane
      ~
    =/  care=(unit care:clay)  ((soft care:clay) (rsh 3 1 term))
    ?~  care
      ~
    ?.  ?=(%da -.r.beam)
      ~
    ::  look up the scry result from our local state
    ::
    =/  =scry-request  [u.vane u.care beam]
    =/  =duct-status   (~(got by ducts.state) duct)
    ::
    =/  local-result=(unit (unit (unit cage)))
      %.  scry-request
      %~  get  by
      %-  complete-scrys
      =<  scry-results
      %-  current-status
      (~(got by ducts.state) duct)
    ::
    ?~  local-result
      ~
    ?~  u.local-result
      ~
    ?~  u.u.local-result
      [~ ~]
    ::
    =/  local-cage=cage  u.u.u.local-result
    ::  if :local-result does not nest in :type, produce an error
    ::
    ?.  -:(nets:wa +.ref p.q.local-cage)
      [~ ~]
    ::
    [~ ~ local-cage]
  ::  +start-clay-subscription: listen for changes in the filesystem
  ::
  ++  start-clay-subscription
    ~/  %start-clay-subscription
    |=  =subscription
    ^+  event-core
    ::
    =/  already-subscribed=?
      (~(has by pending-subscriptions.state) subscription)
    ::
    =.  pending-subscriptions.state
      (put-request pending-subscriptions.state subscription duct)
    ::  don't send a duplicate move if we're already subscribed
    ::
    ?:  already-subscribed
      event-core
    ::
    =/  =wire  (clay-subscription-wire [date disc]:subscription)
    ::
    =/  =note
      ::  request-contents: the set of [care path]s to subscribe to in clay
      ::
      =/  request-contents=(set [care:clay path])
        %-  sy  ^-  (list [care:clay path])
        %+  murn  ~(tap in `(set resource)`resources.subscription)
        |=  =resource  ^-  (unit [care:clay path])
        ::
        `[care.resource (flop spur.rail.resource)]
      ::  if :request-contents is `~`, this code is incorrect
      ::
      ?<  ?=(~ request-contents)
      ::  their: requestee +ship
      ::
      =+  [their desk]=disc.subscription
      ::
      :^  %c  %warp  sock=[our their]
      ^-  riff:clay
      [desk `[%mult `case`[%da date.subscription] request-contents]]
    ::
    =.  moves  [`move`[duct [%pass wire note]] moves]
    ::
    event-core
  ::  +cancel-clay-subscription: remove a subscription on :duct
  ::
  ++  cancel-clay-subscription
    ~/  %cancel-clay-subscription
    |=  =subscription
    ^+  event-core
    ::
    =^  originator  pending-subscriptions.state
      (del-request pending-subscriptions.state subscription duct)
    ::  if there are still other ducts on this subscription, don't send a move
    ::
    ?~  originator
      event-core
    ::
    =/  =wire  (clay-subscription-wire [date disc]:subscription)
    ::
    =/  =note
      =+  [their desk]=disc.subscription
      [%c %warp sock=[our their] `riff:clay`[desk ~]]
    ::
    =.  moves  [`move`[u.originator [%pass wire note]] moves]
    ::
    event-core
  ::  +clay-sub-wire: the wire to use for a clay subscription
  ::
  ::    While it is possible for two different root builds to make
  ::    subscriptions with the same wire, those wires will always be associated
  ::    with different ducts, so there's no risk of duplicates.
  ::
  ++  clay-subscription-wire
    |=  [date=@da =disc]
    ^-  wire
    ::
    =+  [their desk]=disc
    ::
    /(scot %p our)/clay-sub/(scot %p their)/[desk]/(scot %da date)
  ::  +start-scry-request: kick off an asynchronous request for a resource
  ::
  ++  start-scry-request
    |=  [=scry-request =^duct]
    ^+  event-core
    ::  if we are the first block depending on this scry, send a move
    ::
    =/  already-started=?  (~(has by pending-scrys.state) scry-request)
    ::
    =.  pending-scrys.state
      (put-request pending-scrys.state scry-request duct)
    ::  mark the request as pending in :duct-status
    ::
    =.  ducts.state
      %+  ~(jab by ducts.state)  duct
      |=  =duct-status
      ^+  duct-status
      ::
      =/  new-scry-results=scry-results
        %.  [scry-request ~]
        %~  put  in
        =<  scry-results
        (current-status duct-status)
      ::
      ?:  ?=(%once -.live.duct-status)
        duct-status(scry-results.live new-scry-results)
      ::
      ?<  ?=(~ in-progress.live.duct-status)
      duct-status(scry-results.u.in-progress.live new-scry-results)
    ::  don't send a duplicate move if we've already sent one
    ::
    ?:  already-started
      event-core
    ::
    =/  =wire  (scry-request-wire scry-request)
    ::
    =/  =note
      =,  scry-request
      =/  =disc  [p q]:beam
      :*  %c  %warp  sock=[our their=ship.disc]  desk.disc
          `[%sing care case=r.beam (flop s.beam)]
      ==
    ::
    =.  moves  [`move`[duct [%pass wire note]] moves]
    ::
    event-core
  ::  +cancel-scry-request: cancel a pending asynchronous scry request
  ::
  ++  cancel-scry-request
    |=  =scry-request
    ^+  event-core
    ::
    =^  originator  pending-scrys.state
      (del-request pending-scrys.state scry-request duct)
    ::  if there are still other ducts on this subscription, don't send a move
    ::
    ?~  originator
      event-core
    ::
    =/  =wire  (scry-request-wire scry-request)
    ::
    =/  =note
      =+  [their desk]=[p q]:beam.scry-request
      [%c %warp sock=[our their] `riff:clay`[desk ~]]
    ::
    =.  moves  [`move`[u.originator [%pass wire note]] moves]
    ::
    event-core
  ::  +scry-request-wire: wire identifying a +scry-request
  ::
  ++  scry-request-wire
    |=  =scry-request
    ^-  wire
    =/  =term  (cat 3 [vane care]:scry-request)
    (weld /(scot %p our)/scry-request/[term] (en-beam beam.scry-request))
  --
--
::
::  end the =~
::
.  ==
::
::::  vane interface
  ::
::  begin with a default +axle as a blank slate
::
=|  ax=axle
::  a vane is activated with current date, entropy, and a namespace function
::
|=  [now=@da eny=@ scry-gate=sley]
::  allow jets to be registered within this core
::
~%  %ford  ..is  ~
|%
::  +call: handle a +task:able from arvo
::
::    Ford can be tasked with:
::
::      %build: perform a build
::      %keep: resize caches
::      %kill: cancel a build
::      %wipe: clear memory
::
::    The general procedure is for Ford to determine the `our` identity
::    for this +task and operate on the :ship-state for that identity.
::
::    Most requests get converted into operations to be performed inside
::    the +per-event core, which is Ford's main build engine. The %keep
::    and %wipe requests work across all identities stored in Ford, though.
::
++  call
  |=  [=duct type=* task=(hobo task:able)]
  ^-  [p=(list move) q=_ford-gate]
  ::  make sure :task is not a %soft +hobo
  ::
  ?:  ?=(%soft -.task)
    ~&  %ford-cannot-soft-task
    [~ ford-gate]
  ::
  ?-    -.task
      ::  %build: request to perform a build
      ::
      %build
    ::  perform the build indicated by :task
    ::
    ::    First, we find or create the :ship-state for :our.task,
    ::    modifying :state-by-ship as necessary. Then we dispatch to the |ev
    ::    by constructing :event-args and using them to create :start-build,
    ::    which performs the build. The result of :start-build is a pair of
    ::    :moves and a mutant :ship-state. We update our :state-by-ship map
    ::    with the new :ship-state and produce it along with :moves.
    ::
    =^  ship-state  state-by-ship.ax  (find-or-create-ship-state our.task)
    =/  =build  [now schematic.task]
    =*  event-args  [[our.task duct now scry-gate] ship-state]
    =*  start-build  start-build:(per-event event-args)
    =^  moves  ship-state  (start-build build live.task)
    =.  state-by-ship.ax  (~(put by state-by-ship.ax) our.task ship-state)
    ::
    [moves ford-gate]
  ::
      ::  %keep: keep :count cache entries
      ::
      %keep
    ::
    =/  ship-states=(list [ship=@p state=ford-state])
      ~(tap by state-by-ship.ax)
    ::
    =.  state-by-ship.ax
      |-  ^+  state-by-ship.ax
      ?~  ship-states  state-by-ship.ax
      ::
      =,  i.ship-states
      =*  event-args   [[ship duct now scry-gate] state]
      ::
      =.  state-by-ship.ax
        %+  ~(put by state-by-ship.ax)  ship
        (keep:(per-event event-args) compiler-cache-size.task)
      ::
      $(ship-states t.ship-states)
    ::
    [~ ford-gate]
  ::
      ::  %kill: cancel a %build
      ::
      %kill
    ::
    =/  ship-state  ~|(our+our.task (~(got by state-by-ship.ax) our.task))
    =*  event-args  [[our.task duct now scry-gate] ship-state]
    =^  moves  ship-state  cancel:(per-event event-args)
    =.  state-by-ship.ax  (~(put by state-by-ship.ax) our.task ship-state)
    ::
    [moves ford-gate]
  ::
      ::  %wipe: wipe stored builds, clearing :percent-to-remove of the entries
      ::
      %wipe
    ::
    =/  ship-states=(list [ship=@p state=ford-state])
      ~(tap by state-by-ship.ax)
    ::
    =.  state-by-ship.ax
      |-  ^+  state-by-ship.ax
      ?~  ship-states  state-by-ship.ax
      ::
      =,  i.ship-states
      =*  event-args   [[ship duct now scry-gate] state]
      ::
      =.  state-by-ship.ax
        %+  ~(put by state-by-ship.ax)  ship
        (wipe:(per-event event-args) percent-to-remove.task)
      ::
      $(ship-states t.ship-states)
    ::
    [~ ford-gate]
  ::
      %wegh
    :_  ford-gate
    :_  ~
    :^  duct  %give  %mass
    ^-  mass
    :-  %ford
    :-  %|
    %+  turn  ~(tap by state-by-ship.ax)     :: XX single-home
    |=  [our=@ ford-state]  ^-  mass
    :+  (scot %p our)  %|
    ::
    :~  [%compiler-cache [%& compiler-cache]]
    ==
  ==
::  +take: receive a response from another vane
::
::    A +take is a response to a request that Ford made of another vane.
::
::    Ford decodes the type of response based on the +wire in the +take.
::    The possibilities are:
::
::      %clay-sub: Clay notification of an update to a subscription
::
::        If Ford receives this, it will rebuild one or more live builds,
::        taking into account the new date and changed resources.
::
::      %scry-request: Clay response to a request for a resource
::
::        If Ford receives this, it will continue building one or more builds
::        that were blocked on this resource.
::
::    The general procedure is for Ford to determine the `our` identity
::    for this +task and operate on the :ship-state for that identity.
::
::    The +sign gets converted into operations to be performed inside
::    the +per-event core, which is Ford's main build engine.
::
++  take
  |=  [=wire =duct wrapped-sign=(hypo sign)]
  ^-  [p=(list move) q=_ford-gate]
  ::  unwrap :sign, ignoring unneeded +type in :p.wrapped-sign
  ::
  =/  =sign  q.wrapped-sign
  ::  :wire must at least contain :our and a tag for dispatching
  ::
  ?>  ?=([@ @ *] wire)
  ::  :parse our from the head of :wire
  ::
  =/  our=@p  (slav %p i.wire)
  ::
  =/  ship-state
    ::  we know :our is already in :state-by-ship because we sent this request
    ::
    ~|  [%take-our our]
    (~(got by state-by-ship.ax) our)
  ::
  |^  ^-  [p=(list move) q=_ford-gate]
      ::
      =^  moves  ship-state
        ?+  i.t.wire     ~|([%bad-take-wire wire] !!)
          %clay-sub      take-rebuilds
          %scry-request  take-unblocks
        ==
      ::
      =.  state-by-ship.ax  (~(put by state-by-ship.ax) our ship-state)
      ::
      [moves ford-gate]
    ::  +take-rebuilds: rebuild all live builds affected by the Clay changes
    ::
    ++  take-rebuilds
      ^-  [(list move) ford-state]
      ::
      ?>  ?=([%c %wris *] sign)
      =+  [ship desk date]=(raid:wired t.t.wire ~[%p %tas %da])
      =/  disc  [ship desk]
      ::
      =/  =subscription
        ~|  [%ford-take-bad-clay-sub wire=wire duct=duct]
        =/  =duct-status  (~(got by ducts.ship-state) duct)
        ?>  ?=(%live -.live.duct-status)
        ?>  ?=(^ last-completed.live.duct-status)
        ?>  ?=(^ subscription.u.last-completed.live.duct-status)
        u.subscription.u.last-completed.live.duct-status
      ::
      =/  ducts=(list ^duct)
        ~|  [%ford-take-missing-subscription subscription]
        (get-request-ducts pending-subscriptions.ship-state subscription)
      ::
      =|  moves=(list move)
      |-  ^+  [moves ship-state]
      ?~  ducts  [moves ship-state]
      ::
      =*  event-args  [[our i.ducts now scry-gate] ship-state]
      =*  rebuild  rebuild:(per-event event-args)
      =^  duct-moves  ship-state
        (rebuild subscription p.case.sign disc care-paths.sign)
      ::
      $(ducts t.ducts, moves (weld moves duct-moves))
    ::  +take-unblocks: unblock all builds waiting on this scry request
    ::
    ++  take-unblocks
      ^-  [(list move) ford-state]
      ::
      ?>  ?=([%c %writ *] sign)
      ::  scry-request: the +scry-request we had previously blocked on
      ::
      =/  =scry-request
        ~|  [%ford-take-bad-scry-request wire=wire duct=duct]
        (need (path-to-scry-request t.t.wire))
      ::  scry-result: parse a (unit cage) from :sign
      ::
      ::    If the result is `~`, the requested resource was not available.
      ::
      =/  scry-result=(unit cage)
        ?~  riot.sign
          ~
        `r.u.riot.sign
      ::
      =/  ducts=(list ^duct)
        ~|  [%ford-take-missing-scry-request scry-request]
        (get-request-ducts pending-scrys.ship-state scry-request)
      ::
      =|  moves=(list move)
      |-  ^+  [moves ship-state]
      ?~  ducts  [moves ship-state]
      ::
      =*  event-args  [[our i.ducts now scry-gate] ship-state]
      ::  unblock the builds that had blocked on :resource
      ::
      =*  unblock  unblock:(per-event event-args)
      =^  duct-moves  ship-state  (unblock scry-request scry-result)
      ::
      $(ducts t.ducts, moves (weld moves duct-moves))
  --
::  +load: migrate old state to new state (called on vane reload)
::
++  load
  |=  old=axle
  ^+  ..^$
  ::
  ~!  %loading
  ..^$(ax old)
::  +stay: produce current state
::
++  stay  `axle`ax
::  +scry: request a path in the urbit namespace
::
++  scry
  |=  *
  [~ ~]
::  %utilities
::
::+|
::
++  ford-gate  ..$
::  +find-or-create-ship-state: find or create a ford-state for a @p
::
::    Accesses and modifies :state-by-ship.
::
++  find-or-create-ship-state
  |=  our=@p
  ^-  [ford-state _state-by-ship.ax]
  ::
  =/  existing  (~(get by state-by-ship.ax) our)
  ?^  existing
    [u.existing state-by-ship.ax]
  ::
  =|  new-state=ford-state
  =.  compiler-cache.new-state  *(clock compiler-cache-key (each [p=* q=*] tang))
  [new-state (~(put by state-by-ship.ax) our new-state)]
--
