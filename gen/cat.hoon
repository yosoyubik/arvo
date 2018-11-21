#+  #=  here-disc
  ^-  disc:ford
  !:
  =/  her=path  /==
  ~&  [%loading %]
  ?>  ?=([* * *] her)
  [(slav %p i.her) (slav %tas i.t.her)]
::
#+  #=  pretty-file  #&  :-  here-disc  #.  /hoon/pretty-file/lib
#+  #=  show-dir     #&  :-  here-disc  #.  /hoon/show-dir/lib
::
:-  %say
|=  {^ {arg/(list path)} vane/?($g $c)}
=-  tang+(flop `tang`(zing -))
%+  turn  arg
|=  pax/path
^-  tang
=+  ark=.^(arch (cat 3 vane %y) pax)
?^  fil.ark
  ?:  =(%sched -:(flop pax))
    [>.^((map @da cord) (cat 3 vane %x) pax)<]~
  [leaf+(spud pax) (pretty-file .^(noun (cat 3 vane %x) pax))]
?-     dir.ark                                          ::  handle ambiguity
    ~
  [rose+[" " `~]^~[leaf+"~" (smyt pax)]]~
::
    {{@t ~} ~ ~}
  $(pax (welp pax /[p.n.dir.ark]))
::
    *
  =-  [palm+[": " ``~]^-]~
  :~  rose+[" " `~]^~[leaf+"*" (smyt pax)]
      `tank`(show-dir vane pax dir.ark)
  ==
==
