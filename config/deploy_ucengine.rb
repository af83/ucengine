set :use_sudo, false
set :user, 'encre'

set :scm, :git
set :deploy_via, :remote_cache

desc "Local Environment"
task :ucengine_local do
  set :branch, 'master'
  set :repository,  "git://github.com/AF83/ucengine.git"
  set :application, "localhost"
  set :application_type, "development"
  set :deploy_to, "/home/encre/ucengine/"
  set :config_file, "local.cfg"
  server "#{user}@#{application}", :app, :web, :db, :primary => true
end

desc "Test Environment"
task :ucengine_test do
  set :branch, 'master'
  set :repository,  "git://github.com/AF83/ucengine.git"
  set :application, "encre-test.af83.com"
  set :application_type, "development"
  set :deploy_to, "/var/www/encre/ucengine"
  set :config_file, "test.cfg"
  server "#{user}@#{application}", :app, :web, :db, :primary => true
  set :default_environment, {"ERL_LIBS" => "/var/www/encre/lib/erlang/lib"}
end

namespace :deploy do
  desc "Start the ucengine server"
  task :start do
    run "make start -C #{current_path}"
  end
  desc "Stop the ucengine server"
  task :stop do
    run "if [ -d #{current_path} ]; then make stop -C #{current_path}; fi"
  end
  desc "Restart the ucengine server"
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "make restart -C #{current_path}"
  end

  desc "Copy the configuration file"
  task :config do
    config = "#{current_path}/config/#{config_file}"
    run "cp #{config} #{current_path}/etc/uce.cfg"
  end

end

before 'deploy:update', 'deploy:stop'
after 'deploy:update', 'deploy:config'

