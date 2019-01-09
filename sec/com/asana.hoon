::  Test url +https://app.asana.com/api/1.0/users/me
::
::::  /hoon/asana/com/sec
  ::
/+    token-auth
::
|_  {bal/(bale:eyre keys:token-auth) ~}
++  aut  ~(standard token-auth bal ~)
++  filter-request  out-adding-header:aut
--
