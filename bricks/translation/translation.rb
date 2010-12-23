#!/usr/bin/env ruby

require 'rubygems'
require 'rsay'
require 'daemons'

require 'ucengine'


Daemons.run_proc('translation') do
  begin
    languages = ["fr", "en", "it"]
    UCEngine.new("localhost", 5280, UCEngine::DEBUG).connect("translation",
                                            :credential => "d713ab03c0280f82709f865ffa2240a38c26f09b") do |uce|
      uce.subscribe(["demo"], :type => "chat.message.new", :start => uce.time) do |event|
        if event['metadata']['lang'] and event['metadata']['text']
          languages.each do |language|
            next if language == event['metadata']['lang']
            begin
              text = Translate.t(event['metadata']['text'], event['metadata']['lang'], language)
              uce.publish(:location => [event['org'], event['meeting']],
                          :type => "chat.translation.new",
                          :parent => event['id'],
                          :metadata => {:text => text,
                            :lang => language,
                            :from => event['from']})
            rescue => error
              if error.to_s == "can't convert Translate::UnsupportedLanguagePair into String"
                puts "Unsupported language pair: #{event['metadata']['lang']} to #{language}"
              else
                raise
              end
            end
          end
        end
      end
    end
  rescue => error
    puts "Fatal error: #{error}"
    puts error.backtrace
    puts "Retry in 5 seconds ..."
    sleep(5)
    retry
  end
end
