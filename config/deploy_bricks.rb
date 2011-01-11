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
  server "#{user}@#{application}", :app, :web, :db, :primary => true
  set :default_environment, {"RUBYLIB" => "/var/www/encre/lib/", "GEM_PATH" => "/var/www/encre/gem"}
end

namespace :deploy do
  desc "Start the brick"
  task :start do
    run "cd #{current_path} && bin/#{$BRICK} start"
  end

  desc "Stop the brick"
  task :stop do
#    run "cd #{current_path} && bin/#{$BRICK} stop"
  end

  desc "Restart the brick"
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "cd #{current_path} && bin/#{$BRICK} restart"
  end
end

before 'deploy:update', 'deploy:stop'

