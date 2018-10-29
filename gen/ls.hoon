::  LiSt directory subnodes
::
#+  #=  here-disc
  ^-  disc:ford
  !:
  =/  her=path  /==
  ~&  [%loading %]
  ?>  ?=([* * *] her)
  [(slav %p i.her) (slav %tas i.t.her)]
::
#+  #=  show-dir  #&  :-  here-disc  #.  /hoon/show-dir/lib
::
~&  %
:-  %say
|=  {^ {arg/path ~} vane/?($g $c)}
=+  lon=.^(arch (cat 3 vane %y) arg)
tang+[?~(dir.lon leaf+"~" (show-dir vane arg dir.lon))]~
