require 'spore'
require 'spore/middleware/format'

gh = Spore.new(File.join(File.dirname(__FILE__), 'api.json'))

gh.enable(Spore::Middleware::Format, :format => 'json') # will deserialize JSON responses

# API call
r = gh.time(Hash.new)
r = gh.checkRight(:user => "root", 
                  :object => "event", 
                  :action => "list", 
                  :uid => "root", 
                  :sid => "05236488174693651359782945048547",
                  :conditions => {"type" => "plop"})

puts "HTTP status => " + r.code                        # 200
puts "Time    => " + r.body['result']
