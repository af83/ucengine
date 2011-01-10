set :use_sudo, false
set :user, 'encre'

set :scm, :none
set :deploy_via, :copy

desc "Test Environment"
task :brick_test do 

  $BRICK = brick # ugly

  set :repository, "bricks/#{brick}"
  set :application, "encre-test.af83.com"
  set :application_type, "development"
  set :deploy_to, "/var/www/encre/bricks/#{brick}"
  set :config_file, "test.cfg"
  server "#{user}@#{application}", :app, :web, :db, :primary => true
  set :default_environment, {"RUBYLIB" => "/var/www/encre/lib/", "GEM_PATH" => "/var/www/encre/gem"}
end

namespace :deploy do
  desc "Start the brick"
  task :start do
    run "#{current_path}/#{$BRICK}.rb start"
  end

  desc "Stop the brick"
  task :stop do
    run "#{current_path}/#{$BRICK}.rb stop"
  end

  desc "Restart the brick"
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "#{current_path}/#{$BRICK}.rb restart"
  end
end

before 'deploy:update', 'deploy:stop'

