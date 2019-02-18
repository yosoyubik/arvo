::
::::  /hoon/opera/app
  ::
/-   *opera
|%
+$  card  [%wait wire @da]
+$  conditio
  $:  propio=(set opus)
      missiones=(set missio)
  ==
--
|_  [bowl:gall cond=conditio]
::
++  hoc  .
++  prep  |=((unit *) [~ ..prep])
::+|  %helpers
::  find a missio by guid, insert given opus
++  missiones-novas
  |=  [id=@uvH ones=(set missio) supo=opus]
  ^-  (set missio)
  %-  sy
  %+  turn
    ~(tap in ones)
  |=  ssio=missio
  ?:  =(guid.ssio id)
    $(opera.ssio (weld opera.ssio (limo ~[supo])))
  ssio
::
++  poke-noun
  |=  a=*
  ~&  cond
  [~ hoc]
++  poke-opera-mandatum
  |=  man=mandatum
  ^-  (quip move _hoc)
  ?+  -.man  [~ hoc]
    %missio-novo
      :_  hoc(missiones.cond (~(put in missiones.cond) +.man))
      ~
    %opus-novus
      ?~  guid.man
        :_  %=  hoc
              propio.cond  (~(put in propio.cond) novus.man)
            ==
        ~
    :_  %=  hoc
         missiones.cond
          %^    missiones-novas
              (need guid.man)
            missiones.cond
          novus.man
        ==
    ~
  ==
::
++  wake
  |=  [wir=wire ~]
  ?>  ?=([@ ~] wir)
  ~&  [%took `@dr`(sub now (slav %da i.wir))]
  [~ +>.$]
--
