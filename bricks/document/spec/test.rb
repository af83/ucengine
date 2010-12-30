#!/usr/bin/env ruby

$: << File.expand_path(File.dirname(__FILE__))

require 'spec_helper'
require 'ucengine_mock'

begin
  Thread.new do
    begin
      $files = ["small.pdf", "big.pdf", "ugly.pdf"]
      $uploaded_files = 0
      $final_event = 0
      Timeout::timeout(300) do
        UCEngineMock.run! :host => UCE_HOST, :port => UCE_PORT
      end
    rescue => error
      puts error
      puts error.backtrace
      exit_test(1)
    end
  end
rescue => error
  puts error
  puts error.backtrace
  exit_test(1)
end

sleep 5
system("bin/document run -t")
