require 'json'

require 'net/http'
require 'cgi'

class UCEngine

  DEBUG = 0
  INFO = 1
  WARNING = 2
  ERROR = 3
  CRITICAL = 4
  QUIET = 5

  def UCEngine.encode(params)
    params.collect { |k,v| "#{k}=#{CGI::escape(v.to_s)}" }.join('&')
  end

  def initialize(host, port, debug = UCEngine::QUIET)
    @host = host
    @port = port
    @http = Net::HTTP.new(host, port)
    @threads = []
    @debug = debug
    debug(UCEngine::INFO, "Initialisation complete for #{host}:#{port}.")
  end

  def debug(level, message)
    $stderr.write("#{message}\n\n") if level >= @debug
  end

  def get(path, params, http = @http)
    params[:uid] = @uid if @uid
    params[:sid] = @sid if @sid
    result = JSON.parse(http.get("/api/0.1/#{path}?#{UCEngine.encode(params)}").body)
    debug(UCEngine::DEBUG, "Request: GET /api/0.1/#{path}?#{UCEngine.encode(params)}\nResult: #{result}")
    return result
  end

  def post(path, params, http = @http)
    params[:uid] = @uid if @uid
    params[:sid] = @sid if @sid
    result = JSON.parse(http.post("/api/0.1/#{path}", UCEngine.encode(params)).body)
    debug(UCEngine::DEBUG, "Request: POST /api/0.1/#{path}?#{UCEngine.encode(params)}\nResult: #{result}")
    return result
  end

  def put(path, params)
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
    debug(UCEngine::INFO, "Authentification complete for #{@uid}/#{@sid}.")
    yield self
    @threads.each do |thread|
      thread.join
    end
  end

  def subscribe(location, params = {})
    debug(UCEngine::INFO, "Subscribe to #{location} with #{params}.")
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

  def publish(location, type, parent, metadata)
    debug(UCEngine::INFO, "Publish to #{location}, type: #{location}, parent: #{parent}, metadata: #{metadata}")
    params = Hash.new
    params[:type] = type
    params[:parent] = parent if parent
    metadata.each_key do |key|
      params["metadata[#{key}]"] = metadata[key]
    end
    put("/event/#{location.join("/")}", params)
  end

  def time
    time = get("/time", Hash.new)['result'].to_i
    debug(UCEngine::INFO, "Fecth timestamp from UCEngine: #{time}")
    return time
  end

end
