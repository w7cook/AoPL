# this program computes the start/end line of latex includes for 
# references to any files
# usage: 

# Example 1 - Read File and close
def scan(fileName, content, location)
  file = File.new(fileName, "r")
  $stderr << "Scanning '#{fileName}'\n"
  active = {}
  file.each do |line|
    hasBeginOrEnd = false
    line.scan(/BEGIN\:([a-zA-Z][a-zA-Z0-9]*)/).flatten.each do |name|
      if content[name] || active[name]
        $stderr << "Symbol '#{name}' multiply defined\n"
      else
        # $stderr << "  Found '#{name}'\n"
      end
      active[name] = []
      location[name] = fileName
      hasBeginOrEnd = true
    end
    line.scan(/END\:([a-zA-Z][a-zA-Z0-9]*)/).flatten.each do |name|
      content[name] = active[name]
      active.delete(name)
      hasBeginOrEnd = true
    end
    if !hasBeginOrEnd
      active.values.each do |list|
        list << line
      end
    end
  end
  active.keys.each do |name|
    $stderr << "Missing close for #{name}\n"
  end
  file.close
end

def process(input, output, marker, content, location)
  included = false
  name = ""
  unused = content.keys
  sectionFiles = []
  section = ""
  input.each do |line|
    if line =~ /^ *\#+ /
      if sectionFiles.size > 0
        $stderr << "#{section}: #{sectionFiles}\n"
        sectionFiles = []
      end
      section = line.strip
    end
      
    if line =~ /INCLUDE\:([a-zA-Z][a-zA-Z0-9]*)/
      name = $1
      output.write(line)
      included = true
      if content[name]
        sectionFiles << location[name] if !sectionFiles.include? location[name]
        unused.delete(name)
        content[name].each do |out|
          output.write("#{marker} #{out}")
        end
        included = true
      else
        $stderr << "NO CONTENT for #{name}\n"
      end
    elsif included && line =~ /^#{marker}/
      # skip this line!
    else
      if included
        output.write("#{marker} -- %#{name}\n")
        included = false
      end
      output.write(line)
    end
  end
  unused.each do |name|
    $stderr << "UNUSED #{name}\n"
  end
end

content = {}
location = {}

Dir[ARGV[0]].each do |file| 
  scan(file, content, location)
end

marker = ARGV[1]

process($stdin, $stdout, marker, content, location)

# to run:
#  ruby "src/*.hs" ">"


