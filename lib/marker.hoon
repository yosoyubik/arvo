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
  ::  +build-list: coalesce a list of schematics into a single schematic
  ::
  ::    TODO: move to ford:zuse
  ::
  ++  build-list
    |=  schematics=(list schematic:ford)
    ^-  schematic:ford
    ::
    ?~  schematics
      [%ntdt !>(~)]
    [i.schematics $(schematics t.schematics)]
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
    :+  %ntls  [%ntdt !>([mark=mark disc=disc])]
    :+  %ntls  [%ntts %initial-mark (build-mark-analyzer mark disc)]
    ::
    :^    %ntwt
        [%ntcb ^~((ream '?=(%| -.grad.descriptor.initial-mark)'))]
      ::  :mark defines its own diffing; build a gate from that
      ::
      :+  %ntls  :+  %ntts  %form-mark
        ::  load the mark specified by +form:grad
        ::
        :+  %ntnt
          [%ntcb ^~((ream '..zuse'))]
        :+  %ntbs
          [%ntdt !>(build-mark-loader)]
        :_  [%ntcb %limb %disc]
        [%ntcb ^~((ream 'form:grad:core.initial-mark'))]
      ::
      :-  %ntcb
      ^-  hoon
      ^~  %-  ream
      '''
      |=  [start=_+<.core.initial-mark end=_+<.core.initial-mark]
      ^-  [term _+<.form-mark]
      ::
      :-  form:grad:core.initial-mark
      (diff:~(grad core.initial-mark start) end)
      '''
    ::  :mark delegates its diffing to the :delegate mark; recurse on that
    ::
    :+  %ntls  :+  %ntts  %delegate
      :-  %ntcb
      ^-  hoon
      :+  %wtbn
        ^~((ream '?=(%& -.grad.descriptor.initial-mark)'))
      ^~((ream 'p.grad.descriptor.initial-mark'))
    ::  create a :converter gate that can convert data to the :delegate mark
    ::
    :+  %ntls  :+  %ntts  %converter
      :+  %ntnt
        [%ntcb ^~((ream '..zuse'))]
      :+  %ntbs
        [%ntdt !>(build-mark-converter)]
      [%ntcb ^~((ream '[[mark disc] [delegate disc]]'))]
    ::  produce the delegate differ gate by recursing
    ::
    :+  %ntls  :+  %ntts  %delegate-differ
      :+  %ntnt
        [%ntcb ^~((ream '..zuse'))]
      :+  %ntbs
        [%ntdt !>(build-mark-differ)]
      [%ntcb ^~((ream '[delegate disc]'))]
    ::  now produce the full differ gate
    :-  %ntcb
    ^-  hoon
    ^~  %-  ream
    '''
    |=  [start=_+<.core.initial-mark end=_+<.core.initial-mark]
    (delegate-differ (converter start) (converter end))
    '''
  ::  +join: join two diffs into a single diff, or `[%null ~]` on conflict
  ::
  ++  join
    |=  [first=vase second=vase =mark disc=disc:ford]
    ^-  schematic:ford
    ::
    :+  %ntbs
      (build-mark-joiner mark disc)
    [[%ntdt first] [%ntdt second]]
  ::  +build-mark-joiner: produce a gate that can combine two diffs
  ::
  ++  build-mark-joiner
    |=  [=mark disc=disc:ford]
    ^-  schematic:ford
    ::
    :+  %ntls  [%ntts %initial-mark (build-mark-analyzer mark disc)]
    ::
    :^     %ntwt
        [%ntcb ^~((ream '?=(%| -.grad.descriptor.initial-mark)'))]
      ::  :mark defines its own joining; build a gate from that
      ::
      :+  %ntls  :+  %ntts  %form-mark
        ::  load the mark specified by +form:grad
        ::
        :+  %ntnt
          [%ntcb ^~((ream '..zuse'))]
        :+  %ntbs
          [%ntdt !>(build-mark-loader)]
        :_  [%ntdt !>(disc)]
        [%ntcb ^~((ream 'form:grad:core.initial-mark'))]
      ::  produce the gate
      ::
      ::    TODO: check equality first
      ::
      :-  %ntcb
      ^~  %-  ream
      '''
      |=  [start=_+<.form-mark end=_+<.form-mark]
      ^-  [term (unit _+<.form-mark)]
      ::
      =/  grad  grad:core.initial-mark
      ::
      =/  result=(unit _+<.form-mark)  (join:grad start end)
      ?~  result
        [%null ~]
      [form:grad result]
      '''
    ::  :mark delegates its diffing machinery to :delegate; recurse
    ::
    :+  %ntls  :+  %ntts  %delegate
      :-  %ntcb
      ^-  hoon
      :+  %wtbn
        ^~((ream '?=(%& -.grad.descriptor.initial-mark)'))
      ^~((ream 'p.grad.descriptor.initial-mark'))
    ::  produce the delegate joiner gate by recursing
    ::
    :+  %ntnt
      [%ntcb ^~((ream '..zuse'))]
    :+  %ntbs
      [%ntdt !>(build-mark-joiner)]
    [%ntcb ^~((ream '[delegate disc]'))]
  ::  +mash: merge, annotating conflicts
  ::
  ++  mash
    |=  $:  =mark
            disc=disc:ford
            first=[=mark disc=disc:ford =vase]
            second=[=mark disc=disc:ford =vase]
        ==
    ^-  schematic:ford
    ::
    :+  %ntbs
      (build-mark-masher mark disc [mark disc]:first [mark disc]:second)
    [[%ntdt vase.first] [%ntdt vase.second]]
  ::  +build-mark-masher: produce a gate that can merge and annotate two diffs
  ::
  ++  build-mark-masher
    |=  $:  =mark
            disc=disc:ford
            first=[=mark disc=disc:ford]
            second=[=mark disc=disc:ford]
        ==
    ^-  schematic:ford
    ::
    :+  %ntls  [%ntdt !>([mark=mark disc=disc first=first second=second])]
    :+  %ntls  [%ntts %initial-mark (build-mark-analyzer mark disc)]
    ::
    :^    %ntwt
        [%ntcb ^~((ream '?=(%| -.grad.descriptor.initial-mark)'))]
      ^-  schematic:ford
      ::  :mark defines its own mashing; build a gate from that
      ::
      ::    TODO: check equality first
      ::
      :+  %ntls  [%ntts %form [%ntcb ^~((ream 'form:grad:core.initial-mark'))]]
      ::
      :+  %ntls  :+  %ntts  %form-mark
        :+  %ntnt
          [%ntcb ^~((ream '..zuse'))]
        :+  %ntbs
          [%ntdt !>(build-mark-loader)]
        :_  [%ntcb %limb %disc]
        [%ntcb ^~((ream 'form:grad:core.initial-mark'))]
      ::  produce the gate
      ::
      :-  %ntcb
      ^~  %-  ream
      '''
      |=  [a=_+<.form-mark b=_+<.form-mark]
      ^+  +<.form-mark
      ::
      %+  mash:grad:core.initial-mark
        [ship.disc.first desk.disc.first a]
      [ship.disc.second desk.disc.second b]
      '''
    ^-  schematic:ford
    ::  :mark delegates its diffing to machinery to :delegate; recurse
    ::
    :+  %ntls  :+  %ntts  %delegate
      :-  %ntcb
      ^-  hoon
      :+  %wtbn
        ^~((ream '?=(%& -.grad.descriptor.initial-mark)'))
      ^~((ream 'p.grad.descriptor.initial-mark'))
    ::
    :+  %ntnt
      [%ntcb ^~((ream '..zuse'))]
    :+  %ntbs
      [%ntdt !>(build-mark-masher)]
    [%ntcb ^~((ream '[delegate disc first second]'))]
  ::  +patch: apply a diff to a marked noun
  ::
  ++  patch
    |=  [=mark disc=disc:ford start=vase diff=vase]
    ^-  schematic:ford
    ::
    :+  %ntbs
      (build-mark-patcher mark disc)
    [[%ntdt start] [%ntdt diff]]
  ::  +build-mark-patcher: produce a gate that applies a diff to a marked noun
  ::
  ++  build-mark-patcher
    |=  [=mark disc=disc:ford]
    ^-  schematic:ford
    ::
    :+  %ntls  [%ntdt !>(disc=disc)]
    :+  %ntls  [%ntts %initial-mark (build-mark-analyzer mark disc)]
    ::
    :^    %ntwt
        [%ntcb ^~((ream '?=(%| -.grad.descriptor.initial-mark)'))]
      ::  :mark defines its own patching; build a gate from that
      ::
      :+  %ntls  :+  %ntts  %form-mark
        :+  %ntnt
          [%ntcb ^~((ream '..zuse'))]
        :+  %ntbs
          [%ntdt !>(build-mark-loader)]
        :_  [%ntcb %limb %disc]
        [%ntcb ^~((ream 'form:grad:core.initial-mark'))]
      ::  produce the gate
      ::
      :-  %ntcb
      ^~  %-  ream
      '''
      |=  [start=_+<.core.initial-mark diff=_+<.form-mark]
      ^+  +<.core.initial-mark
      ::
      (pact:~(grad core.initial-mark start) diff)
      '''
    ::  :mark delegates its diffing machinery to :delegate; recurse
    ::
    :+  %ntls  :+  %ntts  %delegate
      :-  %ntcb
      ^-  hoon
      :+  %wtbn
        ^~((ream '?=(%& -.grad.descriptor.initial-mark)'))
      ^~((ream 'p.grad.descriptor.initial-mark'))
    ::  produce the delegate mark's patcher by recursing
    ::
    :+  %ntnt
      [%ntcb ^~((ream '..zuse'))]
    :+  %ntbs
      [%ntdt !>(build-mark-patcher)]
    [%ntcb ^~((ream '[delegate disc]'))]
  ::  +validate: ensure a noun nests inside :mark's sample
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
    ::
    =/  mark-paths=(list path)  (segments mark)
    =/  rails=(list rail:ford)
      (turn mark-paths |=(path [disc :(welp /hoon (flop +<) /mar)]))
    ::
    =/  loads=(list schematic:ford)
      (turn rails |=(rail:ford [%nttr %cx %ntdt !>(+<)]))
    ::
    :+  %ntls  [%ntts %mark [%ntdt !>(mark)]]
    :+  %ntls  [%ntts %rails [%ntdt !>(rails)]]
    :+  %ntls  [%ntts %loads `schematic:ford`(build-list loads)]
    ::
    :-  %ntpd
    ::
    :+  %ntbs
      [%ntdt !>(filter-mark-loads)]
    [%ntcb [%clls [%limb %mark] [%limb %rails] [%limb %loads]]]
  ::  +filter-mark-loads: try all rails for mark, producing the rail that worked
  ::
  ++  filter-mark-loads
    |=  [=mark rails=(list rail:ford) results=(list (unit [=mark data=*]))]
    ^-  rail:ford
    ::
    =/  successes=(list rail:ford)
      |-  ^-  (list rail:ford)
      ?~  rails  ~
      ::
      ?<  ?=(~ results)
      ::
      ?~  i.results
        $(rails t.rails, results t.results)
      [i.rails $(rails t.rails, results t.results)]
    ::
    ?~  successes
      ~|  [%no-file-for-mark mark %tried rails]
      !!
    ?^  t.successes
      ~|  [%two-files-for-mark mark i.successes i.t.successes]
      !!
    ::
    i.successes
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
  ::  +tear: split a +term into segments delimited by `-`
  ::
  ::  Example:
  ::  ```
  ::  dojo> (tear 'foo-bar-baz')
  ::  ['foo' 'bar' 'baz']
  ::  ```
  ::
  ++  tear
    |=  a=term
    ^-  (list term)
    ::  sym-no-heps: a parser for terms with no heps and a leading letter
    ::
    =/  sym-no-heps  (cook crip ;~(plug low (star ;~(pose low nud))))
    ::
    (fall (rush a (most hep sym-no-heps)) /[a])
  ::  +segments: get all paths from :path-part, replacing some `/`s with `-`s
  ::
  ::    For example, when passed a :path-part of 'foo-bar-baz',
  ::    the product will contain:
  ::    ```
  ::    dojo> (segments 'foo-bar-baz')
  ::    [/foo/bar/baz /foo/bar-baz /foo-bar/baz /foo-bar-baz]
  ::    ```
  ::
  ++  segments
    |=  path-part=@tas
    ^-  (list path)
    ::
    =/  join  |=([a=@tas b=@tas] (crip "{(trip a)}-{(trip b)}"))
    ::
    =/  torn=(list @tas)  (tear path-part)
    ::
    |-  ^-  (list (list @tas))
    ::
    ?<  ?=(~ torn)
    ::
    ?:  ?=([@ ~] torn)
      ~[torn]
    ::
    %-  zing
    %+  turn  $(torn t.torn)
    |=  s=(list @tas)
    ^-  (list (list @tas))
    ::
    ?>  ?=(^ s)
    ~[[i.torn s] [(join i.torn i.s) t.s]]
  --
