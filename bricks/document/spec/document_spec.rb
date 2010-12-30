require 'spec_helper'
require 'ucengine_mock'

describe  do

  before(:all) do
  end

  after do
  end

  it "converts files" do
    system(File.expand_path(File.dirname(__FILE__)) + "/test.rb").should == true
  end

end
