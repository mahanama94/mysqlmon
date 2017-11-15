{application, mysqlmon,
	[{description, "MySQL SNMP application!"},
		{vsn, "0.1"},
		{modules, [mysqlmon_app,
				mysqlmon_sup,
				mysqlmon_service_sup,
				mysqlmon_notification_sup]},
		{registered, []},
		{applications, [kernel, stdlib]},
		{env, [
		    {services, [mysqlmon_ndb_mem_server]},
		    {notification_services, [mysqlmon_eventlog_server]},
		    {mysqlmon_pidfile_server, [
                    {check_interval, 3000},
                    {pidfile_path, "/var/run/mysqld/"},
                    {pidfile_name, "mysqld.pid"}]
                },
            {mysqlmon_ndbpidfile_server, [
                    {check_interval, 3000},
                    {pidfile_path, "/var/run/mysqld/"},
                    {node_id, 1},
                    {node_type, ndb}]
                },
            {mysqlmon_txcount_server, [
                    {warn_threshold, 100},
                    {crit_threshold, 1000},
                    {check_interval, 100000}]
                },
            {mysqlmon_concount_server, [
                    {warn_threshold, 100},
                    {crit_threshold, 1000},
                    {check_interval, 100000},
                    {dsn, "UID=root;DSN=css_dev"}]
                },
            {mysqlmon_ndb_mem_server, [
                    {warn_index, 70},
                    {crit_index, 80},
                    {warn_data, 75},
                    {crit_data, 85},
                    {check_interval, 3000}]
                }
            ]
		},
		{mod, {mysqlmon_app, []}}]}.
