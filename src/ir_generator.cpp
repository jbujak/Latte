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
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitListArg(ListArg *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
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
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitBStmt(BStmt *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDecl(Decl *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitAss(Ass *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitIncr(Incr *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDecr(Decr *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitRet(Ret *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitVRet(VRet *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitCond(Cond *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitCondElse(CondElse *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitWhile(While *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitSExp(SExp *p) {
	p->expr_->accept(this);
}

void ir_generator::visitItem(UNUSED(Item *p)) {} /* abstract class */

void ir_generator::visitNoInit(NoInit *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitInit(Init *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitListItem(ListItem *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitType(UNUSED(Type *p)) {} /* abstract class */

void ir_generator::visitInt(Int *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitStr(Str *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitBool(Bool *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitVoid(Void *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitFun(Fun *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitListType(ListType *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitExpr(UNUSED(Expr *p)) {} /* abstract class */

void ir_generator::visitEVar(EVar *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitELitInt(ELitInt *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitELitTrue(ELitTrue *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitELitFalse(ELitFalse *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEApp(EApp *p) {
	//TODO generate call
	_current_function->add_command(std::shared_ptr<ir_command>(new ir_call(p->ident_, {})));
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEString(EString *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitNeg(Neg *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitNot(Not *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEMul(EMul *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEAdd(EAdd *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitERel(ERel *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEAnd(EAnd *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEOr(EOr *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitListExpr(ListExpr *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitAddOp(UNUSED(AddOp *p)) {} /* abstract class */

void ir_generator::visitPlus(Plus *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitMinus(Minus *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitMulOp(UNUSED(MulOp *p)) {} /* abstract class */

void ir_generator::visitTimes(Times *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDiv(Div *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitMod(Mod *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitRelOp(UNUSED(RelOp *p)) {} /* abstract class */

void ir_generator::visitLTH(LTH *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitLE(LE *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitGTH(GTH *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitGE(GE *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitEQU(EQU *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitNE(NE *p) {
	(void)p;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitInteger(Integer i) {
	(void)i;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitDouble(Double d) {
	(void)d;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitChar(Char c) {
	(void)c;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitString(String s) {
	(void)s;
	std::cout << __FUNCTION__ << std::endl;
}

void ir_generator::visitIdent(String s) {
	(void)s;
	std::cout << __FUNCTION__ << std::endl;
}

}
