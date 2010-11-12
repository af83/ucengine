#!/usr/bin/env ruby

require 'rubygems'
require 'twitterstream'
require 'set'

require './ucengine'

class UCEngineTwitterStream
  def initialize(login, password, &callback)
    @stream = TwitterStream.new(login, password)
    @hashtags = Hash.new
    @callback = callback
    connect
  end

  def subscribe(location, hashtag)
    begin
      @hashtags[hashtag] = Array.new if !@hashtags[hashtag]
      @hashtags[hashtag] << location if !@hashtags[hashtag].include?(location)
      puts "#{location}: subscribe: #{hashtag}"
      reload
    rescue => error
      puts "#{location}: unable to subscribe to '#{hashtag}': #{error}"
    end
  end

  def unsubscribe(location, hashtag)
    begin
      @hashtags[hashtag].delete(location)
      puts "#{location}: unsubscribe: #{hashtag}"
      reload
    rescue => error
      puts "#{location}: unable to unsubscribe to '#{hashtag}': #{error}"
    end
  end

  def reload
    shutdown
    connect
  end

  def connect
    begin
      @thread = Thread.new do
        synchronous_connect
      end
    rescue => error
      puts "Unable to create twitter thread: #{error}"
    end
  end

  def synchronous_connect
    @stream.track(@hashtags.to_a.map{|pair| pair[0]}) do |tweet|
      begin
        text = tweet['text']
        @hashtags.each_key do |hashtag|
          if text.match(hashtag)
            @hashtags[hashtag].each do |location|
              @callback.call(location, tweet, hashtag)
            end
          end
        end
      rescue => error
        puts "Unable to broadcast tweet: #{error}"
      end
    end
  end

  def shutdown
    begin
      @thread.kill
      @thread.join
    rescue => error
      puts "Unable to kill twitter thread: #{error}"
    end
  end
end


begin
  UCEngine.new("localhost", 5280).connect("twitter",
                                          :token => "da39a3ee5e6b4b0d3255bfef95601890afd80709") do |uce|
    
    twitter = UCEngineTwitterStream.new("encre_af83", "3ncr3_4f8E") do |location, tweet, hashtag|
      puts "Publish: " + tweet['text']
      uce.publish(location, 'twitter.tweet.new', nil,
                    :text => tweet['text'], :from => tweet['user']['name'], :hashtags => hashtag)
    end
    
    uce.subscribe([], :type => "twitter.hashtag.add") do |event|
      twitter.subscribe([event['org'], event['meeting']], event['metadata']['hashtag'].downcase)
    end
    
    uce.subscribe([], :type => "twitter.hashtag.del") do |event|
      twitter.unsubscribe([event['org'], event['meeting']], event['metadata']['hashtag'].downcase)
    end
  end
rescue => error
  puts "Fatal error: #{error}"
  retry
end
