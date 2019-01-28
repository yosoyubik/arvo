/-  asana
=,  dejs-soft:format
|%
++  task
  ^-  $-(json (unit task:asana))
  %-  ot  :~
      gid+so
      name+so
      ['resource_type' so]
  ==
++  tasks
  ^-  $-(json (unit (list task:asana)))
  %-  ot  :~
      :-  %data
      %-  ar  task
  ==
--
