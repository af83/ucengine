{application, uce, [
	      {description, "Secure event recorder"},
	      {vsn, "0.5"},
	      {modules, [
	      		config,
			uce_app,
			rest,
			routes,
			test,
			triggers,
			utils,
			user
			]},
	      {registered, [routes]},
	      {mod, {uce_app, []}},
	      {applications, [kernel, stdlib]}
]}.
