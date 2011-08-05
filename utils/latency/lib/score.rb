## Score is a hash with any key but value can only be an array.
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

    def to_csv
        buff = ""
        self.each do |k, v|
            buff += "#{k};#{self.get_9_decile k}\n"
        end
        buff
    end

    ## get the 9nth decile
    def get_9_decile key
        v = self[key]
        v[v.length * 0.9]
    end
end
