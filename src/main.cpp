#include <iostream>
#include <fstream>
#include <memory>

#include "latte_parser.h"
#include "ir.h"

#include "bnfc/Absyn.H"

static std::string get_filename(int argc, char *argv[]);

int main(int argc, char *argv[])
{
	try {
		std::string filename = get_filename(argc, argv);
		std::shared_ptr<Program> program = parse_file(filename);
		ir::ir ir(program);
		ir.add_function("fun");
		std::cout << ir << std::endl;
	} catch (const std::exception &e) {
		std::cout << "ERROR" << std::endl << e.what() << std::endl;
		return 1;
	}
	return 0;
}

static std::string get_filename(int argc, char *argv[]) {
	if (argc != 2) {
		std::string msg = "Usage: " + std::string(argv[0]) + " input.lat";
		throw std::runtime_error(msg);
	}
	return std::string(argv[1]);
}

