=>
|%
++  ford
  |%
  +$  disc  [=ship =desk]
  +$  rail  [=disc =spur]
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
  --
--
  =>
  |%
  +$  mark-descriptor
    $:  grows=(set mark)
        grabs=(set mark)
        grad=(each delegate=mark form=mark)
    ==
  --
  |%
  ::  +bunt: produce the default value for a :mark on a :disc
  ::
  ++  bunt
    |=  [=mark disc=disc:ford]
    ^-  schematic:ford
    ::
    :+  %ntbn
      (build-mark-loader mark disc)
    [%ntcb ^~((ream '*_+<'))]
  ::  +cast: convert :data from mark :start to mark :end
  ::
  ++  cast
    |=  [data=vase start=[=mark disc=disc:ford] end=[=mark disc=disc:ford]]
    ^-  schematic:ford
    ::
    :+  %ntbs
      (build-mark-converter start end)
    [%ntdt data]
  ::  +build-mark-converter: build a gate to transform :start data to :end
  ::
  ++  build-mark-converter
    |=  [start=[=mark disc=disc:ford] end=[=mark disc=disc:ford]]
    ^-  schematic:ford
    ::
    :+  %ntls  [%ntts %start (build-mark-analyzer start)]
    :+  %ntls  [%ntts %end (build-mark-analyzer end)]
    ::  make sure we produce a gate with the right type signature
    ::
    :+  %ntkt
      :-  %ntcb
      ^~((ream '$-(_+<.core.start _+<.core.end)'))
    ::  can we grab from the end, or do we have to grow from the start?
    ::
    :^  %ntwt  :-  %ntcb
               ^-  hoon
               :+  %cncl
                 ^~((ream '~(has in grabs.descriptor.end)'))
               [%rock %tas mark.start]~
      ::  we can grab, so run +grab from :end
      ::
      [%ntcb ^~((ream (cat 3 mark.start ':grab:core.end')))]
    ::  no grab available; fall back to running +grow on :start
    ::
    :-  %ntcb
    ^-  hoon
    ^~  %-  ream  %-  crip
    "|=(data=_+<.core.start {(trip mark.end)}:~(grow core.start data))"
  ::  +diff: produce a diff between two nouns of mark :mark
  ::
  ::    The result of the build will be (a vase of) a pair of
  ::    the mark of the diff and the noun representing the diff.
  ::
  ++  diff
    |=  [start=vase end=vase =mark disc=disc:ford]
    ^-  schematic:ford
    ::
    :+  %ntbs
      (build-mark-differ mark disc)
    [[%ntdt start] [%ntdt end]]
  ::  +build-mark-differ: build a gate to diff two nouns of mark :mark
  ::
  ++  build-mark-differ
    |=  [=mark disc=disc:ford]
    ^-  schematic:ford
    ::
    :+  %ntls  [%ntts %analyzed (build-mark-analyzer mark disc)]
    ::
    :^    %ntwt
        [%ntcb ^~((ream '?=(%| -.grad.descriptor.analyzed)'))]
      :-  %ntcb
      ^-  hoon
      ::  :mark defines its own diffing; build a gate from that
      ::
      :+  %brts
        :-  %bscl
        :~  [%bsts %start [%bscb [%cnts ~[[%& 6] %core %analyzed] ~]]]
            [%bsts %end [%bscb [%cnts ~[[%& 6] %core %analyzed] ~]]]
        ==
      ::
      :+  %clhp
        :+  %ktts  %mark
        :+  %wtbn
          ^~((ream '?=(%| -.grad.descriptor.analyzed)'))
        ^~((ream 'p.grad.descriptor.analyzed'))
      ::
      ^~((ream '(diff:~(grad core.analyzed start) end)'))
    ::  :mark delegates its diffing to the :delegate mark; recurse on that
    ::
    :+  %ntls  :+  %ntts  %delegate
      :-  %ntcb
      ^-  hoon
      :+  %wtbn
        ^~((ream '?=(%& -.grad.descriptor.analyzed)'))
      ^~((ream 'p.grad.descriptor.analyzed'))
    ::
    :+  %ntls  [%ntts %build-mark-converter [%ntdt !>(build-mark-converter)]]
    :+  %ntls  [%ntts %build-mark-differ [%ntdt !>(build-mark-differ)]]
    ::  create a :converter gate that can convert data to the :delegate mark
    ::
    :+  %ntls  :+  %ntts  %converter
      :+  %ntnt
        [%ntcb ^~((ream '.'))]
      :-  %ntcb
      ^-  hoon
      ^~((ream '(build-mark-converter [[mark disc] [delegate disc]])'))
    ::  produce the delegate differ gate by recursing
    ::
    :+  %ntls  :+  %ntts  %delegate-differ
      :+  %ntnt
        [%ntcb ^~((ream '.'))]
      [%ntcb ^~((ream '(build-mark-differ delegate disc)'))]
    ::  now produce the full differ gate
    ::
    :-  %ntcb
    ^-  hoon
    :+  %brts
      :-  %bscl
        :~  [%bsts %start [%bscb [%cnts ~[[%& 6] %core %analyzed] ~]]]
            [%bsts %end [%bscb [%cnts ~[[%& 6] %core %analyzed] ~]]]
      ==
    ^~((ream '(delegate-differ (converter start) (converter end))'))
  ::
  ++  join
    !!
  ++  patch
    !!
  ::
  ++  validate
    |=  [data=* =mark disc=disc:ford]
    ^-  schematic:ford
    ::
    :+  %ntls  [%ntts %mark-core (build-mark-loader mark disc)]
    ::
    :+  %ntkt  [%ntcb ^~((ream '_+<.mark-core'))]
    ::
    :+  %ntbs
      [%ntcb ^~((ream 'noun:grab:mark-core'))]
    [%ntdt %noun data]
  ::
  ++  build-mark-loader
    |=  [=mark disc=disc:ford]
    ^-  schematic:ford
    ::  TODO: support marks loading libraries; maybe fix /&
    ::
    :-  %ntpd
    ::  TODO: convert mark/in/folder to mark-in-folder
    ::
    :-  %ntdt
    !>([disc /hoon/[mark]/mar])
  ::  +build-mark-analyzer: produce a ford build to analyze a mark on a disc
  ::
  ::    Analyzes a mark on a disc, producing a cell of:
  ::      :core is the mark core, as loaded and compiled from source.
  ::      :descriptor is a +mark-descriptor that is the result of the analysis.
  ::
  ++  build-mark-analyzer
    |=  [=mark disc=disc:ford]
    ^-  schematic:ford
    ::
    ::  /^  /.  mark-descriptor
    ::  /+  /=  mark-core  (build-mark-loader mark disc)
    ::  ::
    ::  /$  /.  analyze-mark-core
    ::  :-  /.  mark
    ::  !>(mark-core)
    ::
    ::  load :mark-core from source
    ::
    :+  %ntls  [%ntts %mark-core (build-mark-loader mark disc)]
    ::  produce cell of mark :core and :descriptor
    ::
    :-  [%ntts %core [%ntcb [%limb %mark-core]]]
    ::  cast :descriptor product to +mark-descriptor
    ::
    :+  %ntts  %descriptor
    :+  %ntkt  [%ntdt !>(mark-descriptor)]
    ::  call +analyze-mark-core to get the rest of the +mark-descriptor
    ::
    :+  %ntbs  [%ntdt !>(analyze-mark-core)]
    :-  [%ntdt !>(mark)]
    [%ntcb ^~((ream '!>(mark-core)'))]
  ::  +analyze-mark-core: produce [grows grabs grad] of a mark-descriptor
  ::
  ++  analyze-mark-core
    |=  [=mark mark-core=vase]
    ^-  mark-descriptor
    ::
    =/  grows  (~(gas in *(set term)) (sloe -:(slap mark-core [%limb %grow])))
    =/  grabs  (~(gas in *(set term)) (sloe -:(slap mark-core [%limb %grab])))
    ::
    =/  grad-vase  (slap mark-core [%limb %grad])
    ?@  q.grad-vase
      ~|  [%bad-grad-arm mark=mark]
      ?>  ((sane %tas) q.grad-vase)
      ::
      [grows grabs grad=[%& delegate=`@tas`q.grad-vase]]
    ::
    =/  form-vase  (slap grad-vase [%limb %form])
    ~|  [%bad-grad-form mark=mark]
    ?>  ?=(@ q.form-vase)
    ?>  ((sane %tas) q.form-vase)
    ::
    [grows grabs grad=[%| form=`@tas`q.form-vase]]
  --
