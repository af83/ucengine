#!/usr/bin/env ruby

require 'rubygems'
require 'rsay'

require './ucengine'

begin
  UCEngine.new("localhost", 5280).connect("translation",
                                          :token => "d713ab03c0280f82709f865ffa2240a38c26f09b") do |uce|
    uce.subscribe(["af83", "demo"], :type => "chat.message.new", :start => uce.time) do |event|
      if event['metadata']['lang'] and event['metadata']['text']
        uce.publish([event['org'], event['meeting']],
                    "chat.translation.new",
                    event['id'],
                    :text => Translate.t(event['metadata']['text'], event['metadata']['lang'], 'en'))
      end
    end
  end
rescue => error
  puts "Fatal error: #{error}"
  puts "Retry in 5 seconds ..."
  sleep(5)
  retry
end
