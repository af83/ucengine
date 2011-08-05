class Score < Hash

    def initialize(*args)
        super(*args)
        self.default_proc = proc do |hash, key|
            hash[key] = []
        end
    end

    def sort!
        self.each do |key, value|
            self[key].sort!
        end
        self
    end
end
