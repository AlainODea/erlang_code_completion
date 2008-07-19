#!/usr/bin/env ruby
require "#{ENV['TM_SUPPORT_PATH']}/lib/scriptmate.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/progress.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/exit_codes.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/ui.rb"
line_number = ENV['TM_LINE_NUMBER'].to_i
line_index = ENV['TM_LINE_INDEX'].to_i
support = ENV['TM_BUNDLE_SUPPORT']
ebin = "#{ENV['TM_PROJECT_DIRECTORY']}/#{ENV['TM_ERLANG_BIN']}"
module_name = ENV['TM_FILEPATH'].split('/')[-1].split('.')[0]
to_complete = STDIN.readlines
to_complete = to_complete[line_number-1][0..line_index-1].gsub('\\', '\\\\\\\\').gsub('"', '\"')
erl_call=`find #{ENV['ERLANG_HOME']} -name erl_call`.split("\n")[0]
response = `#{erl_call} -n tm_complete -a 'code add_path [\"#{ebin}\"]' -a 'tm_complete complete [#{module_name}, \"#{to_complete}\"]' 2>/dev/null`
if (response =~ /^".*"$/) == nil
    TextMate.call_with_progress(:title => "Erlang Code Completion", :message => "Launching Erlang Code Completion Server... Please wait") do
        `cd "#{support}/completion"; make --silent >/dev/null 2>&1`
        `#{erl_call} -s -n tm_complete -a 'code add_path [\"#{support}/completion/ebin\"]'`
    end
    response = `#{erl_call} -n tm_complete -a 'code add_path [\"#{ebin}\"]' -a 'tm_complete complete [#{module_name}, \"#{to_complete}\"]' 2>/dev/null`
end
TextMate.exit_insert_snippet(response[1..-2])