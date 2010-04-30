%% ditz app resource file

{application, ditz,
 [{description, "distributed integration testing zoo"},
  {vsn, "0.0.1"},
  {modules, [
      ditz,
      ditz_app,
      ditz_run,
      ditz_server,
      ditz_sup,
      mustache
   ]},
  {registered, []},
  {included_applications, []},
  {applications, [kernel, stdlib, sasl, ibrowse]},
  {mod, {ditz_app, []}},
  {start_phases, []}
 ]}.
