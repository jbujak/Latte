#include <map>
#include <memory>
#include <iostream>
#include <string>

#include "bnfc/Absyn.H"
#include "ir.h"


/* ir */

namespace ir {

ir::ir(std::shared_ptr<Program> program) {
}

std::ostream& operator<< (std::ostream& out, const ir& ir) {
	for (auto const& [name, function] : ir._functions) {
		out << *function << "\n";
	}
	return out;
}

void ir::add_function(const std::string &name) {
	if (_functions.find(name) != _functions.end()) {
		throw std::runtime_error("Double function definition");		
	}
	_functions[name] = std::shared_ptr<ir_function>(new ir_function(name));
}

/* ir_function */

ir_function::ir_function(const std::string &name) :
	_name(name),
	_commands()
{
	_commands.push_back(std::shared_ptr<ir_command>(new ir_move(1, 2)));
	_commands.push_back(std::shared_ptr<ir_command>(new ir_call("elo", {1, 2, 3})));
}

std::ostream& operator<< (std::ostream& out, const ir_function& ir_function) {
	out << ir_function._name << "\n";
	for (auto cmd : ir_function._commands) {
		out << "  " << *cmd << "\n";
	}
	return out;
}

/* ir_command */

std::ostream& operator<< (std::ostream& out, const ir_command& command) {
	out << command.to_string();
	return out;
}


/* ir_move */

ir_move::ir_move(ir_reg dst, ir_reg src) :
	_dst(dst),
	_src(src)
{
}

std::string ir_move::to_string() const {
	std::string res("move(");
	res += std::to_string(_dst);
	res += ", ";
	res += std::to_string(_src);
	res += ")";
	return res;
}

/* ir_call */

ir_call::ir_call(const std::string &name, const std::vector<ir_reg> &args) :
	_name(name),
	_args(args)
{
}

std::string ir_call::to_string() const {
	std::string res("call(");
	res += _name;
	res += ", {";
	bool first = true;
	for (ir_reg reg : _args) {
		if (!first) {
			res += ", ";
		}
		res += std::to_string(reg);
		first = false;
	}
	res += ")";
	return res;
}

}
