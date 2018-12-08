#include "ir.h"
#include "common.h"

namespace ir {

ir_generator::ir_generator(ir *ir) : _ir(ir)
{
}

ir_generator::~ir_generator() {
}

void ir_generator::visitProgram(UNUSED(Program *p)) {} /* abstract class */

void ir_generator::visitProg(Prog *p) {
	if(p->listtopdef_) {
		p->listtopdef_->accept(this);
	}
}

void ir_generator::visitTopDef(UNUSED(TopDef *p)) {} /* abstract class */

void ir_generator::visitFnDef(FnDef *p) {
	_current_function = std::shared_ptr<ir_function>(new ir_function(p->ident_));
	_ir->add_function(p->ident_, _current_function);
	p->block_->accept(this);
}

void ir_generator::visitListTopDef(ListTopDef *listtopdef) {
	for (auto i = listtopdef->begin() ; i != listtopdef->end() ; ++i) {
		(*i)->accept(this);
	}
}

void ir_generator::visitArg(UNUSED(Arg *p)) {} /* abstract class */

void ir_generator::visitAr(Ar *p) {
}

void ir_generator::visitListArg(ListArg *p) {
}

void ir_generator::visitBlock(UNUSED(Block *p)) {} /* abstract class */

void ir_generator::visitBlk(Blk *p) {
	p->liststmt_->accept(this);
}

void ir_generator::visitListStmt(ListStmt *liststmt) {
	for (auto i = liststmt->begin() ; i != liststmt->end() ; ++i) {
		(*i)->accept(this);
	}
}

void ir_generator::visitStmt(UNUSED(Stmt *p)) {} /* abstract class */

void ir_generator::visitEmpty(Empty *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitBStmt(BStmt *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDecl(Decl *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitAss(Ass *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitIncr(Incr *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDecr(Decr *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitRet(Ret *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitVRet(VRet *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitCond(Cond *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitCondElse(CondElse *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitWhile(While *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitSExp(SExp *p) {
	p->expr_->accept(this);
}

void ir_generator::visitItem(UNUSED(Item *p)) {} /* abstract class */

void ir_generator::visitNoInit(NoInit *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitInit(Init *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitListItem(ListItem *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitType(UNUSED(Type *p)) {} /* abstract class */

void ir_generator::visitInt(Int *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitStr(Str *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitBool(Bool *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitVoid(Void *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitFun(Fun *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitListType(ListType *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitExpr(UNUSED(Expr *p)) {} /* abstract class */

void ir_generator::visitEVar(EVar *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitELitInt(ELitInt *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitELitTrue(ELitTrue *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitELitFalse(ELitFalse *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEApp(EApp *p) {
	//TODO generate call
	_current_function->add_command(std::shared_ptr<ir_command>(new ir_call(p->ident_, {})));
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEString(EString *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitNeg(Neg *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitNot(Not *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEMul(EMul *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEAdd(EAdd *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitERel(ERel *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEAnd(EAnd *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEOr(EOr *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitListExpr(ListExpr *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitAddOp(UNUSED(AddOp *p)) {} /* abstract class */

void ir_generator::visitPlus(Plus *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitMinus(Minus *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitMulOp(UNUSED(MulOp *p)) {} /* abstract class */

void ir_generator::visitTimes(Times *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDiv(Div *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitMod(Mod *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitRelOp(UNUSED(RelOp *p)) {} /* abstract class */

void ir_generator::visitLTH(LTH *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitLE(LE *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitGTH(GTH *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitGE(GE *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEQU(EQU *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitNE(NE *p) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitInteger(Integer i) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDouble(Double d) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitChar(Char c) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitString(String s) {
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitIdent(String s) {
	std::cout << __FUNCTION__ << std::endl;
}

}
