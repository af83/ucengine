#!/usr/bin/env ruby

require 'rubygems'
require 'daemons'
require 'net/http'
require 'tempfile'

require 'RMagick'

require 'ucengine'

Daemons.run_proc('translation') do
  begin
    languages = ["fr", "en", "it"]
    UCEngine.new("localhost", 5280, UCEngine::DEBUG).connect("document",
                                                             :credential => "4efb7c6f7edf5c6392e1b107dde0621140fca97f") do |uce|
      uce.subscribe(["af83", "demo"], :type => "internal.file.add", :start => uce.time) do |event|
        if event['metadata']['mime'] == 'application/pdf'
          Thread.new do
            content = uce.download([event['org'], event['meeting']], event['metadata']['id'])
            file = Tempfile.new(["content-", ".pdf"])
            file.write(content)
            Magick::ImageList.new(file.path).each do |image|
              
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
