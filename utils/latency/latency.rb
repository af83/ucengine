#!/usr/bin/env ruby
# coding: utf-8

require "em-ucengine"
require "./lib/score"

# The purpose of this script is to mesure two things:
#
#  - The time an event take to be received via the streaming API
#  - The time an event take to be published
#
# To do so, we run multiple batches which sent events.
#
# The COUNTER_LIMIT and EVENTS_LIMIT constants configure the number of batch to
# run. For instance, if COUNTER_LIMIT equals 5 and EVENTS_LIMIT equals 8 we
# got the following:
#
# First 1 event is sent every 1 second 5 times
# Then 2 events is sent every 1 second 5 times
# Then 4 events is sent every 1 second 5 times
# Then 8 events is sent every 1 second 5 times
# Here we stop because we reached EVENTS_LIMIT

COUNTER_LIMIT = 5 # Number of runs per batch
EVENTS_LIMIT = 21 # Maximum number of event sent per batch
NB_CLIENTS = 3 # Number of simultaneous users

$test_number = 0
$broadcast_scores = Score.new
$publishing_scores = Score.new
$finished_users = nil

def latency(filters={})
  EventMachine::UCEngine.run('localhost', 5280) do |uce|
    uce.connect("root", "root") do |error, session|
      create_user = Proc.new do |i|
        user = "user#{i}"
        session.create_user({ :name => user,
                              :auth => "password",
                              :credential => "pwd",
                              :metadata => {}}) do
          if i > 0
            i -= 1
            create_user.call(i)
          else
            NB_CLIENTS.times do |i|
              run_client(user, uce, filters)
            end
          end
        end
      end

      $finished_users = NB_CLIENTS
      create_user.call(NB_CLIENTS)
    end
  end
end

def run_client(user, uce, filters)
  p "run_client"

  filters[:types] ||= 'test.latency'
  filters[:event_generator] ||= Proc.new do |cpt|
    'test.latency'
  end
  filters[:parent] ||= false
  parent_event = nil

  uce.connect(user, "pwd") do |error, session|
    session.publish("parent_event", "demo") do |err, event_id|
      parent_event = (filters[:parent]) ? event_id : nil
      params = {
        :type => filters[:types],
        :parent => parent_event,
      }

      subcription = session.subscribe("demo", params) do |err, events|
        events.each do |event|
          received = Time.new
          batch = event['metadata']['batch']
          score = (received.to_f * 1000000).to_i - event['metadata']['timer']
          $broadcast_scores[batch] << score
        end
      end

      nb_events = 1
      repeat = 0
      timer = EM::PeriodicTimer.new(1) do
        nb_events.times do |i|
          sent = Time.new
          event =filters[:event_generator].call(repeat + i, parent_event)
          metadata = {
            :timer => (sent.to_f * 1000000).to_i,
            :batch => nb_events
          }.merge(event[:metadata] || {})

          session.publish(event[:type], "demo", metadata, event[:parent]) do |err, event_id|
            score = ((Time.new - sent) * 1000000).to_i
            $publishing_scores[nb_events] << score
            last_event = event_id
          end

          print "."
        end

        repeat += 1

        p nb_events
        if nb_events >= EVENTS_LIMIT
          timer.cancel
          EM::add_timer 2 do
            $finished_users -= 1
            write_results if $finished_users <= 0
            subcription.cancel { EM.stop }
          end
        end

        # We reached the end of a batch
        if repeat == COUNTER_LIMIT
          nb_events += 10
          repeat = 0
          puts
        end

      end
    end
  end
end

def write_results
  p "write_results"

  # Waiting for the last events
  EM::add_timer 2 do
    $test_number += 1
    $broadcast_scores.sort!
    #File.open("scores#{$test_number}.csv",'w') do |f|
    #  f.write $broadcast_scores.to_all_csv
    #end
    $broadcast_scores.each do |batch, scores|
      p "90% of #{batch} events travel in less than #{$broadcast_scores.get_9_decile(batch)} µs, min: #{scores[0]} µs, max: #{scores[-1]} µs"
    end

    puts
    $publishing_scores.sort!
    #File.open("publishing#{$test_number}.csv",'w') do |f|
    #  f.write $publishing_scores.to_csv
    #end

    # $publishing_scores.values.map(&:sort).each_with_index do |score, n|
    # p "90% of #{n*10} events were published in less than #{score[score.length * 0.9]} µs, min: #{score[0]} µs, max: #{score[-1]} µs"
  end
end

## Test with a single type
latency({
    :types => 'test.latency',
    :parent => false,
    :event_generator => Proc.new do |cpt|
        { :type => (cpt % 2 == 1 ) ? 'test.latency' : 'test.doomed' }
    end
})

## Test with two types
latency({
    :types => 'test.latency,test.again',
    :event_generator => Proc.new do |cpt, last_event|
        { :type =>
            ['test.latency', 'test.doomed', 'test.again'][cpt % 3]
        }
    end
})

## Test with parent
latency({
    :types => 'test.latency',
    :event_generator => Proc.new do |cpt, parent|
        { :type   => 'test.latency',
          :parent => ((cpt %2) == 0) ? parent : nil
        }
    end
})

