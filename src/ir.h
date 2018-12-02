#ifndef IR_H
#define IR_H

#include <map>
#include <memory>
#include <variant>
#include <iostream>

#include "bnfc/Absyn.H"

class ir;
class ir_function;
class ir_command;
class ir_move;
class ir_call;

using ir_reg = int;

class ir {
public:
	ir(std::shared_ptr<Program> program);
	friend std::ostream& operator<<(std::ostream &out, const ir &ir);
	void add_function(const std::string &name);
private:
	std::map<std::string, std::shared_ptr<ir_function>> _functions;
};

class ir_function {
public:
	ir_function(const std::string &name);
	friend std::ostream& operator<<(std::ostream &out, const ir_function &ir_function);
private:
	std::string _name;
	std::vector<std::shared_ptr<ir_command>> _commands;
};

class ir_command {
public:
	virtual std::string to_string() const = 0;
	friend std::ostream& operator<<(std::ostream &out, const ir_command &ir_command);
};

class ir_move : public ir_command {
public:
	ir_move(ir_reg dst, ir_reg src);
	virtual std::string to_string() const;
private:
	ir_reg _dst;
	ir_reg _src;
};

class ir_call : public ir_command {
public:
	ir_call(const std::string &name, const std::vector<ir_reg> &args);
	virtual std::string to_string() const;
private:
	std::string _name;
	std::vector<ir_reg> _args;
};

#endif //IR_H
