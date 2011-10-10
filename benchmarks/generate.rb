#!/usr/bin/env ruby

require 'yaml'
require 'erb'

cwd = File.dirname(__FILE__)

scenarii = "#{cwd}/scenarii/#{ARGV[0]}.yml"

if !File.exists?(scenarii)
  puts "#{scenarii} doesn't exists."
  exit 1
end

config = YAML.load_file(scenarii)

class SessionBinding
  def get_binding
    binding
  end
end

class TsungBinding
  attr_reader :load, :arrivals, :level, :sessions

  def initialize(config)
    @load = config['load']
    @arrivals = load['arrivals']
    @level = config['levels'][ARGV[1]]
    @sessions = config['sessions']
  end

  def get_binding
    binding
  end

  def render_session(session)
    cwd = File.dirname(__FILE__)
    file = File.read("#{cwd}/scenarii/templates/#{session['type']}.erb")
    render = ERB.new(file)

    sessionbinding = SessionBinding.new
    session['variables']['level'] = level
    session['variables'].each do |name, value|
      sessionbinding.instance_variable_set "@" + name, value
      SessionBinding.class_eval{attr_reader name}
    end
    render.result(sessionbinding.get_binding)
  end
end


file = File.read("#{cwd}/scenarii/templates/base.erb")
renderer = ERB.new(file)
b = TsungBinding.new(config).get_binding
renderer.run(b)
