#!/usr/bin/env ruby
require "#{ENV['TM_SUPPORT_PATH']}/lib/scriptmate.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/progress.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/exit_codes.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/ui.rb"
require 'socket'
require 'timeout'
line_number = ENV['TM_LINE_NUMBER'].to_i
line_index = ENV['TM_LINE_INDEX'].to_i
support = ENV['TM_BUNDLE_SUPPORT']
ebin = "#{ENV['TM_PROJECT_DIRECTORY']}/#{ENV['TM_ERLANG_BIN']}"
module_name = ENV['TM_FILEPATH'].split('/')[-1].split('.')[0]
begin
    client = TCPSocket.new('127.0.0.1', 2345)
rescue
    TextMate.call_with_progress(:title => "Erlang Code Completion", :message => "Launching Erlang Code Completion Server... Please wait") do
        `cd "#{support}/completion"; make`
        my_popen3 "#{ENV['ERLANG_HOME']}/bin/erl -noshell -detached -pa \"#{support}/completion/ebin\" -s tm_complete_server"
    end
else
    client.print "{#{module_name}, [\"#{ebin}\"]}.\n"
    current_line_number = 1
    while $stdin.gets
        if current_line_number == line_number
            client.print $_[0..line_index-1] + "\n"
        end
        current_line_number += 1
    end
    client.print "\f\n"
    TextMate.exit_insert_snippet(client.gets(nil))
end