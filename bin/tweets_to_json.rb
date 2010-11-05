#!/usr/bin/env ruby

begin
  if ARGV.size < 3
    puts "./tweets_to_json.rb file org meeting"
    exit
  end
  puts '['
  file = File.open(ARGV[0])
  file.each do |line|
    offset, author, text = line.split(';')
    hashtags = ["#simonsinek", "#sinek", "#TED"]
    first = rand(3)
    second = rand(2)
    first_hashtag = hashtags[first]
    hashtags.delete_at(first)
    second_hashtag = hashtags[second]
    puts("\t{")
    puts("\t\t\"from\": \"twitter\",")
    puts("\t\t\"org\": \"#{ARGV[1]}\",")
    puts("\t\t\"meeting\": \"#{ARGV[2]}\",")
    puts("\t\t\"offset\": \"#{offset.to_i * 1000}\",")
    puts("\t\t\"type\": \"twitter.tweet.new\",")
    puts("\t\t\"metadata\": {")
    puts("\t\t\t\"from\": \"#{author}\",")
    puts("\t\t\t\"text\": \"#{text}\",")
    puts("\t\t\t\"hashtags\": \"#{first_hashtag},#{second_hashtag}\"")
    puts("\t\t}")
    if file.eof?
      puts("\t}")
    else
      puts("\t},")
    end
  end
  puts ']'
end
