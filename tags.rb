require 'fileutils'

existingTags = {}
input = File.new("anatomy.lhs", "r")
while (line = input.gets)
  line = line.rstrip
  if line =~ /%([a-zA-Z0-9]+)$/
    existingTags[$1] = true
  end
end
input.close

checkTags = {}
counter = 1
prev = nil
num = 0
input = File.new("anatomy.lhs", "r")
while (line = input.gets)
  line = line.rstrip
  # if its a section header
  if line =~ / *#+ ([a-zA-Z0-9]+)/
    tag = $1.slice(0,4)
    num = 0
  end
  needNewTag = false
  if prev =~ /%([a-zA-Z0-9]+)$/
    if checkTags[$1]
      needNewTag = true 
      $stderr << "Warning: Creating new tag for %#{$1}\n"
    end
    checkTags[$1] = true
  end
  if (prev && prev != "" && prev[0] != "\\" && prev[0] != "%" && prev[0] != "`" && !(prev =~ /END-HIDE/) &&
      (needNewTag || (!(prev =~ /%[a-zA-Z0-9]+$/) && !(prev =~ /^ *#/))) && 
      (line == "" || line =~ /^ *\* / || line =~ /^ *[1-9]+\. / || line =~ /^   *[a-z]\. / || line[0] == "`"))
    # need a new tag
    $stderr << "FOO: [#{prev}]\n" if line[0] == "`"
    num = num + 1
    while existingTags.include?("#{tag}#{num}")
      num = num + 1
    end
    existingTags["#{tag}#{num}"] = true
    checkTags["#{tag}#{num}"] = true
    if line[0] == "`"
      puts prev if !needNewTag
      puts "%#{tag}#{num}"
    elsif prev[0] == "<" || prev[0] == ">"
      puts prev if !needNewTag
      puts "#{prev[0]} -- %#{tag}#{num}"
    else
      prev = prev.split('%')[0] if needNewTag
      puts "#{prev} %#{tag}#{num}"
    end
  else
    puts prev if prev
  end
  prev = line
end
puts prev if prev
input.close