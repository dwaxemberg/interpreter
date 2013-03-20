task :test => [:clean] do
  Dir.entries("tests").select{|a| a=~/^test(?:[01][0-9]|2[0-4])\.c$/}.collect{|t| File.join("tests", t)}.sort.each do |f|
    r = File.read(f)
    d = r.lines[0].match(/description: (.*)\n/)[1]
    v = r.lines[1].match(/value: (.*)\n/)[1]
    puts "Test for #{f}"
    puts d
    puts v
    code = r.lines[2..-1].join
    File.open("tests/temp.c", 'w'){|f| f.puts(code)}
    puts `racket -e '(load "interpreter.scm")(interpret (parser "tests/temp.c"))'`
    puts
    sleep(1)
  end
  File.delete("tests/temp.c")
end

task :clean do
  Dir.entries("tests").select{|a| a=~/^\..*[^.]+$/}.collect{|t| File.join("tests", t)}.each{|f| File.delete(f)}
end

