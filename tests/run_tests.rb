inputs = Dir["inputs/test_*.evm"]

inputs.each do |input|
	test_name = input[7...(input.length - 4)]
	output_file = "outputs/#{test_name}.out"
	result = `../main.byte "#{input}" | diff -Bw "#{output_file}" -`
	if result.empty?
		puts "Pass: #{test_name}" 
	else
		puts "Fail: #{test_name}\t#{result.gsub("\n", "")}"
	end
end