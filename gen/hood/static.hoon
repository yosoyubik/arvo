/?  309
::
/=  pre-process
  /^  (map path [@tas @t])
  /:  /===/web  /*
  /|  /;  |=(@t [%html +<])  /&html&/urb/
      /;  |=(@t [%raw +<])   /atom/
  ==
::
~&  %finished-preprocessing
:-  %say
|=  $:  [now=@da eny=@uv bec=beak]
        ~
        ~
    ==
=>
|%
++  convert-link
  |=  [pre=tape link=tape]
  =/  parsed=(unit (list coin))
  ~&  [%pre pre]
  ~&  [%link link]
  %+  rust  link
  ;~  pose
    ;~(pfix net (more net nuck:so))
    (more net nuck:so)
  ==
  ?~  parsed
    link
  ^-  tape
  %+  welp
  =<  + 
  %^  spin  u.parsed  pre
  |=  [c=coin s=path]
  ^-  [* out=tape]
  ?>  ?=([%$ dime] c)
  [0 (weld "{s}/" (scow +.c))]
  ::
  ".html"
::
++  convert-file
  |=  [pre=tape fil=tape]
  ^-  tape
  =/  idc=(list @ud)  (fand "<a href=" fil)
  =<  +>
  %^  spin  idc  [0 fil]
  |=  [i=@ud f=@ud h=tape]
  ^-  [p=* f=@ud out=tape]
  =/  a  (scag :(add 9 i f) h)
  =/  b  (slag :(add 9 i f) h)
  =/  c  (need (find "\">" b))
  =/  old-link=tape  (scag c b)
  =/  new-file=tape  :(welp a old-link (slag c b))
  [0 f new-file]
--
::
:-  %dill-blit
=/  trio  /(scot %p p.bec)/[q.bec]/(scot r.bec)
=/  dirs  .^((list path) %ct (weld trio /web))
::
:-  %mor
%+  roll  dirs
|=  [pax=path out=(list [%sav path @t])]
~&  [%path pax]
=/  path-prefix=path  (scag (dec (lent pax)) pax)
=/  pre=[@tas @t]  (~(got by pre-process) path-prefix)
:_  out
:-  %sav
?:  =(%raw -.pre)
  [pax +.pre]
::  find and update links
=/  root=tape
  ?~  path-prefix  ""
  (slag 1 (spud (scag 1 (flop path-prefix))))
=/  fil=tape  (convert-file root (trip +.pre))
=/  fil-name  (spud (scag 1 (flop path-prefix)))
::  if the file has a .html extension, then make a dir
::  with a index.html
?:  =(-.pre %html)
  :-  :(welp path-prefix /index /[-.pre])
    (crip fil)
:-  (weld path-prefix /[-.pre]) 
  (crip fil)
