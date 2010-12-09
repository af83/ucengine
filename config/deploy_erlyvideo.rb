set :use_sudo, false
set :user, 'encre'

set :scm, :git
set :deploy_via, :remote_cache

desc "Local Environment"
task :erlyvideo_local do
  set :branch, 'master'
  set :repository,  "git://github.com/erlyvideo/erlyvideo.git"
  set :application, "localhost"
  set :application_type, "development"
  set :deploy_to, "/home/encre/erlyvideo"
  set :config_file, "erlyvideo.conf.local"
  server "#{user}@#{application}", :app, :web, :db, :primary => true
end

desc "Test Environment"
task :erlyvideo_test do
  set :branch, 'master'
  set :repository,  "git://github.com/erlyvideo/erlyvideo.git"
  set :application, "encre-test.af83.com"
  set :application_type, "development"
  set :deploy_to, "/var/www/encre/erlyvideo"
  set :config_file, "erlyvideo.conf.test"
  server "#{user}@#{application}", :app, :web, :db, :primry => true
  set :default_environment, {"ERL_LIBS" => "/var/www/encre/erlyvideo/current/"}
end

namespace :deploy do
  desc "Build the erlyvideo server"
  task :build, :once => true do
    run "mkdir #{current_path}/erlyvideo"
    run "mv #{current_path}/* #{current_path}/erlyvideo/. ; true"
    run "cd #{current_path} && git clone git://github.com/AF83/erlyvideo-ucengine.git erlyvideo/plugins/erlyvideo-ucengine"
    run "cd #{current_path} && git clone https://github.com/dizzyd/ibrowse.git erlyvideo/deps/ibrowse"
    run "make -C #{current_path}/erlyvideo/deps/ibrowse/"
    run "make -C #{current_path}/erlyvideo"
  end  

  desc "Start the erlyvideo server"
  task :start do
    deploy.stop
    run "cd #{current_path}/erlyvideo && ./contrib/erlyctl start"
  end

  desc "Stop the erlyvideo server"
  task :stop do
    run "cd #{current_path}/erlyvideo && ./contrib/erlyctl stop"
  end

  desc "Restart the erlyvideo server"
  task :restart, :roles => :app, :except => { :no_release => true } do
    deploy.stop
    deploy.start
  end

  desc "Copy the configuration file"
  task :config do
    config = "#{current_path}/erlyvideo/priv/#{config_file}"
    put(File.read("config/#{config_file}"), config, :via => :scp)
    run "cp #{config} #{current_path}/erlyvideo/priv/erlyvideo.conf"
  end
end

after 'deploy:update', 'deploy:build'
after 'deploy:update', 'deploy:config'
