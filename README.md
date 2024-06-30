# spoobmoney
A text file parser, designed for keeping track of items and their prices (and links), written in Haskell.

I made this project to assist with shopping for PC parts.

Note: this readme sucks lol

# INSTALLATION
- Install ghc using ghcup if you haven't already
- Run 'make' in the repo directory
- You now have a binary for spoobmoney located at src/bin/Main
- Move src/bin/Main anywhere you like, and rename it whatever you want (only if the name is "spoobmoney")
(very professional and cool installation instructions)

# USAGE
	spoobmoney [FLAGS] <FILE>

	The program will error if no flags are specified.
	(Note: Only one flag will be acknowledged by the program.)
	(To avoid unintended output, please only specify a single flag.)
	
	FLAGS: 
		-h: 		Display this help message

		--sum:		Output the total cost of the list
		--names:	Output the names of all items
		--prices:	Output the prices of all items
		--links:	Output the links of all items
		
	FILE:
		The infile to read

This program will take an input file, and spit out a space-separated list of the desired field.
(Example input files can be found in the examples directory)

# .spoobm file format specification:
	
	Whitespace will be ignored.

	Each line will be formatted as follows:
		<name>: $<price> (<link>)

