::
::::  /hoon/labora/app
  ::
/?    310
|%
++  card
  $%  [%hiss wire (unit ~) %httr %hiss hiss:eyre]
  ==
++  tasks-url
  'https://app.asana.com/api/1.0/tasks?assignee=me&limit=20&workspace=9315823581768'
--
|_  {bowl:gall ~}
++  this  .
++  poke-noun
  |=  *
  ^-  (quip move _this)
  ~&  [%this this]
  :_  [%hiss /tasks [~ ~] %httr %hiss (need (de-purl:html tasks-url)) *moth:eyre]
  ~
::
--
