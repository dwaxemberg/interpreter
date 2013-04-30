task :test => [:clean] do
  if n=ENV['n']
    test("tests/part4-test#{n}.c")
  else
    Dir.entries("tests").select{|a| a=~/^part4-test\d+\.c$/}.collect{|t| File.join("tests", t)}.sort.each do |f|
      test(f)
    end
  end
  File.delete("tests/temp.c")
end

def test(f)
  r = File.read(f)
  d = r.lines.to_a[0].match(/description: ?(.*)\n/)[1]
  v = r.lines.to_a[1].match(/value: ?(.*)\n/)[1]
  c = r.lines.to_a[2].match(/class: ?(.*)\n/)[1]
  puts "Test for #{f}"
  puts d
  puts v
  puts c
  code = r.lines.to_a[4..-1].join
  File.open("tests/temp.c", 'w'){|f| f.puts(code)}
  puts %x{racket -e "(load \\"interpreter.scm\\")(interpret \\"tests/temp.c\\" '#{c})"}
  puts
  STDIN.gets
end

task :clean do
  Dir.entries("tests").select{|a| a=~/^\..*[^.]+$/}.collect{|t| File.join("tests", t)}.each{|f| File.delete(f)}
end

