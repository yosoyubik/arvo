/-  *chess
/+  *server
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chess/index  /html/
/=  chess-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chess/chessboard-js  /js/
/=  chess-css
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chess/chessboard-css  /css/
/=  chess-png
  /^  (map knot @)
  /:  /===/app/chess/img  /_  /png/
::
|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%poke wire dock poke]
      [%http-response =http-event:http]
      [%diff %json json]
  ==
+$  poke
  $%  [%modulo-bind app=term]
      [%modulo-unbind app=term]
  ==
--
::
|_  [bol=bowl:gall sta=@t]
::
++  this  .
::
++  poke-noun
  |=  asd=?(%bind %unbind)
  ^-  (quip move _this)
  :_  this
  ?:  =(%bind asd)
    [ost.bol %poke /subapp [our.bol %modulo] `poke`[%modulo-bind %chess]]~
  [ost.bol %poke /subapp [our.bol %modulo] `poke`[%modulo-unbind %chess]]~
++  prep
  |=  old=(unit @t)
  ^-  (quip move _this)
  ~&  %prep
  :-  [ost.bol %poke /subapp [our.bol %modulo] [%modulo-bind %chess]]~
  ?~  old
    this
  this(sta u.old)
::
++  peer-game
  |=  [pax=path]
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %json [%s sta]]~
::
++  poke-handle-http-request
  %-  (require-authorization ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  =+  request-line=(parse-request-line url.request.inbound-request)
  =+  back-path=(flop site.request-line)
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ~&  back-path
    ?~  back-path
      'World'
    i.back-path
  ::
  ~&  name
  ~&  back-path
  ?~  back-path
    :_  this  ~
  ~&  &2:back-path
  ?:  =(&2:back-path 'img')
    =/  img  (as-octs:mimes:html (~(got by chess-png) `@ta`name))

    :_  this
    :~  ^-  move
        :-  ost.bol
        :*  %http-response
            [%start [200 ['content-type' 'image/png']~] [~ img] %.y]
        ==
    ==
  ?:  =(name 'chessboard-js')
    :_  this
    :~  ^-  move
        :-  ost.bol
        :*  %http-response
            [%start [200 ['content-type' 'application/javascript']~] [~ chess-js] %.y]
        ==
    ==
  ?:  =(name 'chessboard-css')
    :_  this
    :~  ^-  move
        :-  ost.bol
        :*  %http-response
            [%start [200 ['content-type' 'text/css']~] [~ chess-css] %.y]
        ==
    ==
  :_  this
  :~  ^-  move
      :-  ost.bol
      :*  %http-response
          [%start [200 ['content-type' 'text/html']~] [~ index] %.y]
      ==
  ==
++  poke-chess-command
  |=  com=command
  ^-  (quip move _this)
  ~&  com
  :_  this(sta +.com)
  %+  turn  (prey:pubsub:userlib /game bol)
  |=  [=bone ^]
  [bone %diff %json [%s +.com]]
--
