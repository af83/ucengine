#!/usr/bin/env ruby

require "minitest/autorun"
require "../lib/score"

describe Score do

    it "append value in a new score" do
        s = Score.new
        s['toto'] << 42
        s['toto'].must_equal [42]
    end

    it "sort a score" do
        s = Score.new
        s['toto'] << 42
        s['toto'] << 41
        s['toto'].must_equal [42, 41]
        s.sort!
        s['toto'].must_equal [41, 42]
    end

    it "display a score as CSV" do
        s = Score.new
        s['toto'] << 42
        s['toto'] << 41
        s['toto'].must_equal [42, 41]
        s.sort!
        s.to_csv.must_equal "toto;42\n"
    end
 
end
