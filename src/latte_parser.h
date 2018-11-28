#ifndef LATTE_PARSER_H
#define LATTE_PARSER_H
#include <string>
#include <memory>

#include "bnfc/Parser.H"

std::shared_ptr<Program> parse_file(std::string filename);

#endif //LATTE_PARSER_H
