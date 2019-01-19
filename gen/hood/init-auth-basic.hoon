::  API: input auth token for domain
::
::::  /hoon/init-auth-token/hood/gen
  ::
/?  314
/-  sole
/+  generators
::
::::
  ::
=,  generators
:-  %ask
|=  $:  {now/@da eny/@uvJ bec/beak}
        {arg/$@(~ {dom/path ~})}
        ~
    ==
^-  (sole-result:sole {$write-sec-atom p/host:eyre q/@})
=-  ?~  arg  -
    (fun.q.q [%& dom.arg])
%+  prompt
  [%& %oauth-hostname "api hostname: https://"]
%+  parse  thos:de-purl:html
|=  hot/host:eyre
?:  ?=(%| -.hot)
  ~|(%ips-unsupported !!)
%+  prompt
  [%& %api-token "token: "]
%+  parse  (boss 256 (star prn))
|=  pas/@t
%+  produce  %write-sec-atom    :: XX typed pair
[hot pas]
