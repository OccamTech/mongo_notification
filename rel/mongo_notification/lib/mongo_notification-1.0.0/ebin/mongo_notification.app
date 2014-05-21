%% app generated at {2014,5,21} {10,55,4}
{application,mongo_notification,
             [{description,"Use to send APNS and GCM Messages to Mongo User"},
              {vsn,"1.0.0"},
              {id,[]},
              {modules,[mongo_apns,mongo_gcm,mongo_notification,
                        mongo_notification_app,mongo_notification_sup,
                        mongo_server,mongo_wall_notification]},
              {registered,[mongo_notification_sup]},
              {applications,[kernel,stdlib,eredis,mochiweb]},
              {included_applications,[]},
              {env,[]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{mongo_notification_app,[]}}]}.

