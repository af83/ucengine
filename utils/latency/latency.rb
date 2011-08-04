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

COUNTER_LIMIT = 10 # Number of runs per batch
EVENTS_LIMIT = 256 # Maximum number of event sent per batch

EventMachine::UCEngine.run('localhost', 5280) do |uce|
    broadcast_scores = {}
    publishing_scores = {}

    uce.connect("participant", "pwd") do |error, session|
        subcription = session.subscribe("demo", :type => "ucengine.timer.ping" ) do |err, events|
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
                session.publish("ucengine.timer.ping", "demo", {
                    :timer => (sent.to_f * 1000000).to_i,
                    :batch => nb_events
                }) do |err, event_id|
                    score = ((Time.new - sent) * 1000000).to_i
                    (publishing_scores[nb_events] ||= []) << score
                end

                print "."
            end

            # We reached the end of a batch
            if repeat == COUNTER_LIMIT
                nb_events *= 2
                repeat = 0
                puts
            end

            # We reached the event limit, so we stop
            if nb_events > EVENTS_LIMIT
                timer.cancel
                subcription.cancel { EM.stop }
                File.open('scores.csv','w') do |f|
                    broadcast_scores.values.map(&:sort).each_with_index do |score, n|
                        p "90% of #{2**n} events travel in less than #{score[score.length * 0.9]} µs, min: #{score[0]} µs, max: #{score[-1]} µs"
                        f.write("#{2**n};#{score[score.length * 0.9]}\n")
                    end
                end

                publishing_scores.values.map(&:sort).each_with_index do |score, n|
                    p "90% of #{2**n} events were published in less than #{score[score.length * 0.9]} µs, min: #{score[0]} µs, max: #{score[-1]} µs"
                end
            end

            repeat += 1
        end
    end
end
