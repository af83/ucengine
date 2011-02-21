require 'spec_helper'
include UCEngineDocument

describe UCEngineDocument do
  before do
    @uce = UCEngine.new("localhost", 1000)
  end

  it "should not transform non pdf document" do
    UCEngineDocument::handle_upload_event(@uce, {"location" => "testmeeting",
                                                 "id" => "id",
                                                 "metadata" => {"id" => "small_12345",
                                                                "mime" => "text/plain"}})
  end

  it "should transform small pdf to images" do
    value = File.read('./spec/samples/small.pdf')
    @uce.should_receive(:download).once.with('testmeeting', 'small_12345').and_return(value)
    @uce.should_receive(:upload).once.with('testmeeting', an_instance_of(Tempfile)).and_return({"result" => "id1"})
    @uce.should_receive(:publish).once.with(hash_including(:location => "testmeeting",
                                                           :parent   => "id",
                                                           :from     => "document",
                                                           :type     => "document.conversion.done",
                                                           :metadata => {0 => "id1"}))

    UCEngineDocument::handle_upload_event(@uce, {"location" => "testmeeting",
                                                 "id" => "id",
                                                 "metadata" => {"id" => "small_12345",
                                                                "mime" => "application/pdf"}})
  end

  it "should transform big pdf to images" do
    value = File.read('./spec/samples/big.pdf')
    @uce.should_receive(:download).once.with('testmeeting', 'small_12345').and_return(value)
    @uce.should_receive(:upload).exactly(9).times.with('testmeeting', an_instance_of(Tempfile)).and_return({"result" => "id1"})
    @uce.should_receive(:publish).once.with(hash_including(:location => "testmeeting",
                                                           :parent   => "id",
                                                           :from     => "document",
                                                           :type     => "document.conversion.done"))

    UCEngineDocument::handle_upload_event(@uce, {"location" => "testmeeting",
                                                 "id" => "id",
                                                 "metadata" => {"id" => "small_12345",
                                                                "mime" => "application/pdf"}})
  end

  it "should not send sent document.conversion.done event if pdf is dammaged" do
    value = File.read('./spec/samples/ugly.pdf')
    @uce.should_receive(:download).once.with('testmeeting', 'small_12345').and_return(value)
    @uce.should_not_receive(:upload).with(any_args())
    @uce.should_not_receive(:publish).with(any_args())

    UCEngineDocument::handle_upload_event(@uce, {"location" => "testmeeting",
                                                 "id" => "id",
                                                 "metadata" => {"id" => "small_12345",
                                                 "mime" => "application/pdf"}})
  end
end
