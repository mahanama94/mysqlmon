{application, mysqlmon,
	[{description, "MySQL SNMP application!"},
		{vsn, "0.1"},
		{modules, [mysqlmon_app,
				mysqlmon_sup,
				mysqlmon_service_sup,
				mysqlmon_notification_sup]},
		{registered, []},
		{applications, [kernel, stdlib]},
		{env, []},
		{mod, {mysqlmon_app, []}}]}.