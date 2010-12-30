require 'sinatra'
require 'json'

require 'spec_helper'

class UCEngineMock < Sinatra::Base
  use Rack::MethodOverride

  VERSION = '0.1'

  put "/api/#{VERSION}/presence/:uid" do
    status 200
    {:result => "test_sid"}.to_json
  end

  get "/api/#{VERSION}/time" do
    status 200
    {:result => 1234}.to_json
  end

  get "/api/#{VERSION}/event/:org/:meeting" do
    if params[:start].to_i > 1234
      sleep 10
      status 200
      {:result => []}.to_json
    elsif params[:type] == "internal.file.add"
      status 200
      {:result => [{:id => "12345",
                     :type => "internal.file.add",
                     :datetime => 1235,
                     :org => UCE_ORG,
                     :meeting => UCE_MEETING,
                     :metadata => {
                       :id => SMALL_FILE,
                       :mime => "application/pdf"
                     },
                     :from => "frank"
                   },
                   {:id => "12346",
                     :type => "internal.file.add",
                     :datetime => 1235,
                     :org => UCE_ORG,
                     :meeting => UCE_MEETING,
                     :metadata => {
                       :id => BIG_FILE,
                       :mime => "application/pdf"
                     },
                     :from => "frank"
                   },
                   {:id => "12347",
                     :type => "internal.file.add",
                     :datetime => 1235,
                     :org => UCE_ORG,
                     :meeting => UCE_MEETING,
                     :metadata => {
                       :id => UGLY_FILE,
                       :mime => "application/pdf"
                     },
                     :from => "frank"
                   }]}.to_json
    else
      status 400
      {:error => "bad_parameters"}.to_json
    end
  end

  get "/api/#{VERSION}/file/#{UCE_ORG}/#{UCE_MEETING}/:id" do
    puts $files
    if !$files.include?(params[:id])
      puts "Try do download an unexpected file: #{params[:id]}"
      status 500
    end
    $files.delete(params[:id])
    samples = "#{File.dirname(File.expand_path(__FILE__))}/samples"
    File.open("#{samples}/#{params[:id]}").read
  end

  post "/api/#{VERSION}/file/:org/:meeting" do
    200
    $uploaded_files += 1
    {:result => "test_file_id"}.to_json
  end

  put "/api/#{VERSION}/event/:org/:meeting" do
    if params[:type] == "document.conversion.done"
      if ["12345", "12346", "12347"].include?(params[:parent])
        $final_event += 1
        if $final_event == 2
          if $files != []
            puts "Error: #{$files.join(", ")} not downloaded"
            exit_test(1)
          end
          if $uploaded_files != 10
            puts "Error: wrong number of pages uploaded: #{$uploaded_files}, should be 10"
            exit_test(1)
          end
          exit_test(0)
        end
        {:result => "event_id"}.to_json
      end
    else
      status 404
      {:error => "not_found"}.to_json
    end
  end
  
end
