{application, tasks,
 [{description, "various tasks for ditz application"},
  {vsn, "0.0.1"},
  {modules, [
      cloudant,
      httpc
   ]},
  {registered, []},
  {applications, [kernel,stdlib]},
  {env, []}
]}.
