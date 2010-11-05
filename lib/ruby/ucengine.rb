require 'json'

require 'net/http'
require 'cgi'

class UCEngine

  def UCEngine.encode(params)
    params.collect { |k,v| "#{k}=#{CGI::escape(v.to_s)}" }.join('&')
  end

  def initialize(host, port)
    @host = host
    @port = port
    @http = Net::HTTP.new(host, port)
    @threads = []
  end

  def get(path, params, http = @http)
    params[:uid] = @uid if @uid
    params[:sid] = @sid if @sid
    JSON.parse(http.get("/api/0.1/#{path}?#{UCEngine.encode(params)}").body)
  end

  def post(path, params, http = @http)
    params[:uid] = @uid if @uid
    params[:sid] = @sid if @sid
    JSON.parse(http.post("/api/0.1/#{path}", UCEngine.encode(params)).body)
  end

  def put(path, params)
    puts "#{path} - #{UCEngine.encode(params)}"
    
    params['_method'] = "PUT"
    post(path, params)
  end

  def delete(path, params)
    params['_method'] = "DELETE"
    post(path, params)
  end

  def connect(uid, credential)
    @uid = uid
    response = put("/presence/af83/#{@uid}", {:auth => 'token', :credential => credential[:token]})
    @sid = response['result']
    yield self
    @threads.each do |thread|
      thread.join
    end
  end

  def subscribe(location, params = {})
    @threads << Thread.new do
      Net::HTTP.start(@host, @port) do |http|
        params[:_async] = "lp"
        params[:start] = 0 if !params[:start]
        while true

          begin
            events = get("/event/#{location.join("/")}", params, http)['result']
          rescue Timeout::Error
            retry
          rescue EOFError
            sleep 10
            retry
          end

          events.each do |event|
            yield event
          end
          params[:start] = events[-1]['datetime'] + 1
        end
      end
    end
  end

  def publish(location, type, metadata)
    params = Hash.new
    params[:type] = type
    metadata.each_key do |key|
      params["metadata[#{key}]"] = metadata[key]
    end
    put("/event/#{location.join("/")}", params)
  end

end
