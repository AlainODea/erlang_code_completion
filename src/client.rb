#!/usr/bin/env ruby
require "#{ENV['TM_SUPPORT_PATH']}/lib/scriptmate.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/progress.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/exit_codes.rb"
require "#{ENV['TM_SUPPORT_PATH']}/lib/ui.rb"
line_number = ENV['TM_LINE_NUMBER'].to_i - 1
line_index = ENV['TM_LINE_INDEX'].to_i
support = ENV['TM_BUNDLE_SUPPORT']
ebin = "#{ENV['TM_PROJECT_DIRECTORY']}/#{ENV['TM_ERLANG_BIN']}"
module_name = ENV['TM_FILEPATH'].split('/')[-1].split('.')[0]
erl_call=`find #{ENV['ERLANG_HOME']} -name erl_call`.split("\n")[0]

lines = STDIN.readlines
data = lines[[0,line_number-10].max..[0,line_number-1].max].join("\n") + "\n" + lines[[0,line_number].max][0..line_index-1]
data = data.gsub('\\', '\\\\\\\\').gsub('"', '\"')

if `#{erl_call} -n tm_complete -a 'code add_path ["#{ebin}"]' 2>&1` =~ /^erl_call: failed to connect to node.*$/
    TextMate.call_with_progress(:title => "Code Completion", :message => "Preparing Code Completion…") do
        `cd "#{support}/completion"; make --silent >/dev/null 2>&1`
        `#{erl_call} -s -n tm_complete -a 'code add_path ["#{support}/completion/ebin"]' 2>&1`
    end
end

pipe = IO.popen("#{erl_call} -n tm_complete -e", 'w+')
pipe.write "tm_complete:complete(#{module_name},\n\"#{data}\").\n"
pipe.close_write
response = pipe.read

if match = /\{ok, "(.*)"\}/.match(response)
    TextMate.exit_insert_snippet match[1]
else
    TextMate.exit_show_tooltip 'no completions'
end
