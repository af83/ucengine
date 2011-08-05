#!/usr/bin/env ruby
# coding: utf-8

require "em-ucengine"

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
EVENTS_LIMIT = 20 # Maximum number of event sent per batch

def latency(filters={})
    filters[:types] ||= 'test.latency'
    filters[:event_generator] ||= Proc.new do |cpt|
        'test.latency'
    end

    EventMachine::UCEngine.run('localhost', 5280) do |uce|
        broadcast_scores = {}
        publishing_scores = {}

        uce.connect("participant", "pwd") do |error, session|
            subcription = session.subscribe("demo", :type => filters[:types]) do |err, events|
                events.each do |event|
                    received = Time.new
                    batch = event['metadata']['batch']
                    score = (received.to_f * 1000000).to_i - event['metadata']['timer']

                    (broadcast_scores[batch] ||= []) << score
                end
            end

            nb_events = 1
            repeat = 0
            timer = EM::PeriodicTimer.new(1) do
                nb_events.times do
                    sent = Time.new
                    session.publish(filters[:event_generator].call(repeat), "demo", {
                        :timer => (sent.to_f * 1000000).to_i,
                        :batch => nb_events
                    }) do |err, event_id|
                        score = ((Time.new - sent) * 1000000).to_i
                        (publishing_scores[nb_events] ||= []) << score
                    end

                    print "."
                end

                # We reached the event limit, so we stop
                if nb_events >= EVENTS_LIMIT
                    timer.cancel
                    subcription.cancel { EM.stop }
                    File.open('scores.csv','w') do |f|
                        broadcast_scores.values.map(&:sort).each_with_index do |score, n|
                            p "90% of #{n*10} events travel in less than #{score[score.length * 0.9]} µs, min: #{score[0]} µs, max: #{score[-1]} µs"
                            f.write("#{n*10};#{score[score.length * 0.9]}\n")
                        end
                    end

                    publishing_scores.values.map(&:sort).each_with_index do |score, n|
                        p "90% of #{n*10} events were published in less than #{score[score.length * 0.9]} µs, min: #{score[0]} µs, max: #{score[-1]} µs"
                    end
                end

                repeat += 1

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

#latency()

## Test with a single type
=begin
latency({
    :types => 'test.latency',
    :event_generator => Proc.new do |cpt|
        (cpt % 2 == 1 ) ? 'test.latency' : 'test.doomed'
    end
})
=end

## Test with two types
latency({
    :types => 'test.latency,test.again',
    :event_generator => Proc.new do |cpt|
        ['test.latency', 'test.doomed', 'test.again'][cpt % 3]
    end
})


## Test with parent

## Test with search
