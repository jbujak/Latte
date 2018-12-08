#ifndef IR_H
#define IR_H

#include <map>
#include <memory>
#include <variant>
#include <iostream>

#include "bnfc/Absyn.H"

namespace ir {

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
		void add_function(const std::string &name, std::shared_ptr<ir_function> fun);
	private:
		std::map<std::string, std::shared_ptr<ir_function>> _functions;
};

class ir_function {
	public:
		ir_function(const std::string &name);
		void add_command(std::shared_ptr<ir_command> command);
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


class ir_generator : public Visitor
{
public:
	ir_generator(ir *ir);
	~ir_generator(void);
	char *print(Visitable *v);

	void visitProgram(Program *p); /* abstract class */
	void visitProg(Prog *p);
	void visitTopDef(TopDef *p); /* abstract class */
	void visitFnDef(FnDef *p);
	void visitListTopDef(ListTopDef *p);
	void visitArg(Arg *p); /* abstract class */
	void visitAr(Ar *p);
	void visitListArg(ListArg *p);
	void visitBlock(Block *p); /* abstract class */
	void visitBlk(Blk *p);
	void visitListStmt(ListStmt *p);
	void visitStmt(Stmt *p); /* abstract class */
	void visitEmpty(Empty *p);
	void visitBStmt(BStmt *p);
	void visitDecl(Decl *p);
	void visitAss(Ass *p);
	void visitIncr(Incr *p);
	void visitDecr(Decr *p);
	void visitRet(Ret *p);
	void visitVRet(VRet *p);
	void visitCond(Cond *p);
	void visitCondElse(CondElse *p);
	void visitWhile(While *p);
	void visitSExp(SExp *p);
	void visitItem(Item *p); /* abstract class */
	void visitNoInit(NoInit *p);
	void visitInit(Init *p);
	void visitListItem(ListItem *p);
	void visitType(Type *p); /* abstract class */
	void visitInt(Int *p);
	void visitStr(Str *p);
	void visitBool(Bool *p);
	void visitVoid(Void *p);
	void visitFun(Fun *p);
	void visitListType(ListType *p);
	void visitExpr(Expr *p); /* abstract class */
	void visitEVar(EVar *p);
	void visitELitInt(ELitInt *p);
	void visitELitTrue(ELitTrue *p);
	void visitELitFalse(ELitFalse *p);
	void visitEApp(EApp *p);
	void visitEString(EString *p);
	void visitNeg(Neg *p);
	void visitNot(Not *p);
	void visitEMul(EMul *p);
	void visitEAdd(EAdd *p);
	void visitERel(ERel *p);
	void visitEAnd(EAnd *p);
	void visitEOr(EOr *p);
	void visitListExpr(ListExpr *p);
	void visitAddOp(AddOp *p); /* abstract class */
	void visitPlus(Plus *p);
	void visitMinus(Minus *p);
	void visitMulOp(MulOp *p); /* abstract class */
	void visitTimes(Times *p);
	void visitDiv(Div *p);
	void visitMod(Mod *p);
	void visitRelOp(RelOp *p); /* abstract class */
	void visitLTH(LTH *p);
	void visitLE(LE *p);
	void visitGTH(GTH *p);
	void visitGE(GE *p);
	void visitEQU(EQU *p);
	void visitNE(NE *p);

	void visitInteger(Integer i);
	void visitDouble(Double d);
	void visitChar(Char c);
	void visitString(String s);
	void visitIdent(String s);

private:
	ir *_ir;

	std::shared_ptr<ir_function> _current_function;
		
};

}

#endif //IR_H
