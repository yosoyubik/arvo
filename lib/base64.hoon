::  |base64: flexible base64 encoding for little-endian atoms
::
::  pad: include padding when encoding, require when decoding
::  url: use url-safe characters '-' for '+' and '_' for '/'
::
=+  [pad=& url=|]
|%
::
+$  byte  @D
::
++  div-ceil
  ::  divide, rounding up.
  |=  [x=@ y=@]
  ?:  =(0 (mod x y))
    (div x y)
  +((div x y))
::
++  explode-bytes
  ::  Explode a bytestring into list of bytes. Result is in LSB order.
  |=  =octs
  ^-  (list byte)
  =/  atom-byte-width  (met 3 q.octs)
  =/  leading-zeros    (sub p.octs atom-byte-width)
  (weld (reap leading-zeros 0) (rip 3 q.octs))
::
++  explode-words
  ::  Explode a bytestring to words of bit-width `wid`. Result is in LSW order.
  |=  [=octs wid=@]
  ^-  (list @)
  =/  atom-bit-width   (met 0 q.octs)
  =/  octs-bit-width   (mul 8 p.octs)
  =/  atom-word-width  (div-ceil atom-bit-width wid)
  =/  rslt-word-width  (div-ceil octs-bit-width wid)
  =/  pad              (sub rslt-word-width atom-word-width)
  (weld (ripn wid q.octs) (reap pad 0))
::
::  +en:base64: encode +octs to base64 cord
::
++  word24  @
++  en
  ^-  $-(octs cord)
  ::
  =/  cha
    ?:  url
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  ::
  |^  |=  bs=octs
      ^-  cord
      =/  x  (octs-to-blocks bs)
      (crip (flop (unfudge-padding pad.x (encode-blocks blocks.x))))
  ::
  ++  octs-to-blocks
    :: Construct a list of 24-bit words from a bytestring. Also return
    :: the number of extra bytes added.
    ::
    :: TODO Not sure I understand preciely why this particular process
    :: is right. Where should the padding bytes that we add go?
    ::
    |=  bs=octs
    ^-  [pad=@ud blocks=(list word24)]
    =/  pad=@ud  (~(dif fo 3) 0 p.bs)
    =/  fudge=@  (lsh 3 pad (rev 3 bs))
    =/  focts    [(add pad p.bs) fudge]
    [pad (explode-words focts 24)]
  ::
  ++  unfudge-padding
    ::  Drop `ext` bytes from the front of a reversed base64-encoded
    ::  string and (optionally) replace them with `=` chars.
    |=  [ext=@ t=tape]  ^-  tape
    %+  weld  ?.(pad ~ (reap ext '='))
    (slag ext t)
  ::
  ++  encode-blocks
    ::  Build a reversed base64 tape given a reversed list of 24-bit blocks.
    |=  ws=(list word24)  ^-  tape
    ::  ~&  %encode-blocks
    ::  =-  ~&  %end-encode-blocks  -
    (zing (turn ws encode-block))
  ::
  ++  encode-block
    ::  Encode a 24-bit word into a reversed, base64-encoded tape.
    |=  w=word24  ^-  tape
    =/  a  (cut 3 [(cut 0 [0 6] w) 1] cha)
    =/  b  (cut 3 [(cut 0 [6 6] w) 1] cha)
    =/  c  (cut 3 [(cut 0 [12 6] w) 1] cha)
    =/  d  (cut 3 [(cut 0 [18 6] w) 1] cha)
    ~[a b c d]
  ::
  --
::
::  +de:base64: decode base64 cord to (unit @)
::
++  de
  |=  a=cord
  ^-  (unit octs)
  (rush a parse)
::  +parse:base64: parse base64 cord to +octs
::
++  parse
  =<  ^-  $-(nail (like octs))
      %+  sear  reduce
      ;~  plug
        %-  plus  ;~  pose
          (cook |=(a=@ (sub a 'A')) (shim 'A' 'Z'))
          (cook |=(a=@ (sub a 'G')) (shim 'a' 'z'))
          (cook |=(a=@ (add a 4)) (shim '0' '9'))
          (cold 62 (just ?:(url '-' '+')))
          (cold 63 (just ?:(url '_' '/')))
        ==
        (stun 0^2 (cold %0 tis))
      ==
  |%
  ::  +reduce:parse:base64: reduce, measure, and swap base64 digits
  ::
  ++  reduce
    |=  [dat=(list @) dap=(list @)]
    ^-  (unit octs)
    =/  lat  (lent dat)
    =/  lap  (lent dap)
    =/  dif  (~(dif fo 4) 0 lat)
    ?:  &(pad !=(dif lap))
      ::  padding required and incorrect
      ~&(%base-64-padding-err-one ~)
    ?:  &(!pad !=(0 lap))
      ::  padding not required but present
      ~&(%base-64-padding-err-two ~)
    =/  len  (sub (mul 3 (div (add lat dif) 4)) dif)
    :+  ~  len
    %+  swp  3
    ::  %+  base  64
    %+  roll
      (weld dat (reap dif 0))
    |=([p=@ q=@] (add p (mul 64 q)))
  --
--

