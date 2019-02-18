|%
::  a group of tasks
+$  missio
  $:  guid=@uvH
      name=@t
      description=wain
      creator=@p
      created=@da
      followers=(set @p)
      opera=(list opus)
  ==
::  a task or an item of work
+$  opus
  $:  guid=@uvH
      name=@t
      description=wain
      assignee=(unit @p)
      creator=@p
      followers=(set @p)
      created=@da
      due=@da
      completed=?
      tags=(set @ta)
  ==
::  the actions/commands
+$  mandatum
  $%  [%opus-novus guid=(unit @uvH) novus=opus]
      [%missio-novo missio]
      [%opus guid=@uvH]
      [%missio guid=@uvH]
  ==
--
