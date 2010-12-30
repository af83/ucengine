require 'ucengine'

UCE_HOST = "localhost"
UCE_PORT = 5280
UCE_ORG = "test_org"
UCE_MEETING = "test_meeting"
SMALL_FILE = "small.pdf"
BIG_FILE = "big.pdf"
UGLY_FILE = "ugly.pdf"

def exit_test(status)
  puts "EXIT"
  system("bin/document stop")
  exit! status
end
