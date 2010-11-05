set :use_sudo, false
set :user, 'encre'

set :scm, :git
set :deploy_via, :remote_cache

desc "Local Environment"
task :env_local do
  set :branch, 'master'
  set :repository,  "af83@git.af83.com:encre-proto.git"
  set :application, "localhost"
  set :application_type, "development"
  set :deploy_to, "/home/encre"
  set :config_file, "local.cfg"
  server "#{user}@#{application}", :app, :web, :db, :primary => true
end

desc "Test Environment"
task :env_test do
  set :branch, 'master'
  set :repository,  "af83@git.af83.com:encre-proto.git"
  set :application, "encre-test.af83.com"
  set :application_type, "development"
  set :deploy_to, "/var/www/encre/encre-proto"
  set :config_file, "test.cfg"
  server "#{user}@#{application}", :app, :web, :db, :primary => true
  set :default_environment, {"ERL_LIBS" => "/var/www/encre/lib/erlang/lib"}
end

namespace :deploy do
  desc "Start the encre server"
  task :start do
    run "make start -C #{current_path}/encre"
  end
  desc "Stop the encre server"
  task :stop do
    run "if [ -d #{current_path} ]; then make stop -C #{current_path}/encre; fi"
  end
  desc "Restart the encre server"
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "make restart -C #{current_path}/encre"
  end

  desc "Copy the configuration file"
  task :config do
    config = "#{current_path}/encre/config/#{config_file}"
    run "cp #{config} #{current_path}/encre/etc/encre.cfg"
  end
end

before 'deploy:update', 'deploy:stop'
after 'deploy:update', 'deploy:config'

