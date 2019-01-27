def print_test_result(test_name, passed, message=nil)
	if passed
			puts "Pass -> #{test_name}" 
	else
			puts "Fail -> #{test_name}\nMessage:#{message}"
	end
end

number_passed = 0
(input_dir = Dir["inputs/test_*.evm"]).each do |input|
	test_name = input[7...(input.length - 4)]
	output = `../main.byte "#{input}"`

	if (out_dir = Dir["outputs/#{test_name}/*.out"]).empty?
		output_file = "outputs/#{test_name}.out"
		
		passed = (expected_output = File.read(output_file)) == output
		message = "Expected:\n#{[expected_output]}\nBut output was:\n#{[output]}" unless passed
		print_test_result test_name, passed, message
		number_passed += 1 if passed
	else
		passed = false
		expected_outputs = []
		
		out_dir.find do |output_file|
			passed ||= (expected_output = File.read(output_file)) == output
			expected_outputs << expected_output unless passed
			passed
		end
		unless passed
			message = "Expected one of: #{expected_outputs}\nBut output was:\n#{[output]}"
		end
		print_test_result test_name, passed, message
		number_passed += 1 if passed
	end
end
	puts "Passed (#{number_passed} / #{input_dir.count} - #{(number_passed.to_f / input_dir.count) * 100}%)"