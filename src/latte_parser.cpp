#include <fstream>
#include <memory>

#include "latte_parser.h"
#include "bnfc/Parser.H"
#include "bnfc/Absyn.H"

std::shared_ptr<Program> parse_file(std::string filename) {
	FILE *input;

	input = fopen(filename.c_str(), "r");
	if (!input) {
		std::string msg = std::string("Cannot open file ") + filename;
		throw std::runtime_error(msg);
	}
	Program *parsed_program = pProgram(input);
	if (!parsed_program) {
		std::string msg = "Syntax error in file " + filename;
		throw std::runtime_error(msg);
	}
	std::shared_ptr<Program> res(parsed_program);
	return res;
}
