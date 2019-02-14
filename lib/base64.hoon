::  |base64: flexible base64 encoding for little-endian atoms
::
::  pad: include padding when encoding, require when decoding
::  url: use url-safe characters '-' for '+' and '_' for '/'
::
=+  [pad=& url=|]
|%
::
::  +en:base64: encode +octs to base64 cord
::
::  The basic inner-loop algorithm is:
::
::    - Examine the lowest three bytes of a block.
::    - Convert those to bytes into four characters.
::    - Loop on the block *without* those three bytes.
::
::  However, since not all blocks have sizes that are multiples of three
::  bytes, we will have some bullshit at the end. We replace that bullshit
::  with '=' characters.
::
::  To setup the loop, we create `dif` and `dap`:
::
::    - dif: number of zero bytes added to get block to be 3 blocks.
::    - dap: The input block, reversed, and padded with `dif` 0 bytes.
::
::      `dap` needs to be reversed, because we're dealing with atoms,
::      not with bytestrings, and we can't prepend zeros to a nat,
::      since that does nothing.
::
::  Then, we execute the loop and get back a the result encoded as a
::  backwards tape.
::
::  We need to replace `dif` characters with '=' (or just drop them, if
::  `pad` isn't set), since those are the result of the padding bytes
::  we inserted earlier. Since the list is backwards, we just `slag`
::  `dif` characters and then weld `dif` '='s to the front.
::
::  Finally, we reverse the tape and convert it to a cord.
::
++  en
  |=  inp=octs
  ^-  cord
  =/  dif=@ud  (~(dif fo 3) 0 p.inp)
  =/  dap=@ux  (lsh 3 dif (rev 3 inp))
  =/  cha
    ?:  url
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  %-  crip
  %-  flop
  %+  weld                                            ::  prepend `dif` '='s
    ?.(pad ~ (reap dif '='))
  %+  slag  dif                                       ::  drop dif chars
  !.
  =/  acc=tape  ~
  |-  ^-  tape
  ?:  =(0x0 dap)  (flop acc)                          ::  done
  =/  x  (end 3 3 dap)
  =/  a  (cut 3 [(cut 0 [0 6] x) 1] cha)
  =/  b  (cut 3 [(cut 0 [6 6] x) 1] cha)
  =/  c  (cut 3 [(cut 0 [12 6] x) 1] cha)
  =/  d  (cut 3 [(cut 0 [18 6] x) 1] cha)
  $(acc [d c b a acc], dap (rsh 3 3 dap))             ::  loop
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

