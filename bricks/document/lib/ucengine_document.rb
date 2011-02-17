require 'RMagick'

module UCEngineDocument
  def handle_upload_event(uce, event)
    if event['metadata']['mime'] == 'application/pdf'
      content = uce.download(event['location'], event['metadata']['id'])
      file = Tempfile.new(["content-", ".pdf"])
      file.write(content)
      file.flush()
      images = Hash.new
      begin
        Magick::ImageList.new(file.path).each_with_index do |image, i|
          image.format = "jpg"
          tmp_image = Tempfile.new(["content-", ".jpg"])
          image.write(tmp_image.path)
          result = uce.upload(event['location'], tmp_image)
          tmp_image.close!
          images[i] = result['result']
        end
        uce.publish(:location => event['location'],
                    :parent => event['id'],
                    :from => 'document',
                    :type => 'document.conversion.done',
                    :metadata => images)
      rescue => error
        puts "Fatal error: #{error}"
      end
      file.close!
    end
  end
end
