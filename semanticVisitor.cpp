#include "semanticVisitor.hpp"
#include <iostream>
using namespace ast;
using output::ScopePrinter;

static std::string toString(ast::BuiltInType type) {
    switch (type) {
        case ast::BuiltInType::INT:
            return "int";
        case ast::BuiltInType::BOOL:
            return "bool";
        case ast::BuiltInType::BYTE:
            return "byte";
        case ast::BuiltInType::VOID:
            return "void";
        case ast::BuiltInType::STRING:
            return "string";
        default:
            return "unknown";
    }
}

void SemanticVisitor::pushScope() {
    scopes.emplace_back();
    printer.beginScope();
    nextLocalOffset = 0;
}
void SemanticVisitor::popScope() {
    printer.endScope();
    scopes.pop_back();
}
bool SemanticVisitor::insert(const std::string& id,const Symbol& s){
    auto& tbl = scopes.back();
    if(tbl.count(id)) return false;
    tbl[id]=s;
    return true;
}
Symbol* SemanticVisitor::lookup(const std::string& id){
    for(int i=scopes.size()-1;i>=0;--i){
        auto it = scopes[i].find(id);
        if(it!=scopes[i].end()) return &it->second;
    }
    return nullptr;
}

void SemanticVisitor::setType(const ast::Node* n, ast::BuiltInType t){
    exprType[n]=t;
}

ast::BuiltInType SemanticVisitor::getType(const ast::Node* n) const{
    auto it = exprType.find(n);
    return it==exprType.end()? ast::BuiltInType::VOID : it->second;
}


SemanticVisitor::SemanticVisitor() {
    pushScope();

    printer.emitFunc("print",  BuiltInType::VOID,{BuiltInType::STRING});
    insert("print",{Symbol::FUNC,BuiltInType::VOID,{BuiltInType::STRING},BuiltInType::VOID,0});

    printer.emitFunc("printi", BuiltInType::VOID,{BuiltInType::INT});
    insert("printi",{Symbol::FUNC,BuiltInType::VOID,{BuiltInType::INT},BuiltInType::VOID,0});

    printer.emitFunc("read",BuiltInType::INT,{BuiltInType::INT});
    insert("read",{Symbol::FUNC,BuiltInType::VOID,{BuiltInType::INT},BuiltInType::INT,0});

    printer.emitFunc("readi",BuiltInType::INT,{BuiltInType::INT});
    insert("readi",{Symbol::FUNC,BuiltInType::VOID,{BuiltInType::INT},BuiltInType::INT,0});
}

void SemanticVisitor::finish() {
    if(lookup("main")==nullptr ||
       lookup("main")->ret!=BuiltInType::VOID)
        output::errorMainMissing();
    std::cout << printer;
}

static int constLen(ast::Exp *e){
    if(auto *n = dynamic_cast<ast::Num*>(e))   return n->value;
    if(auto *b = dynamic_cast<ast::NumB*>(e))  return b->value;
    output::errorMismatch(e->line);
    return 0;
}

static bool compatible(ast::BuiltInType expected, ast::BuiltInType actual) {
    if (expected == actual) return true;
    return expected == ast::BuiltInType::INT && actual == ast::BuiltInType::BYTE;
}

static ast::BuiltInType getBuiltin(ast::Type *t) {
    if (auto *p = dynamic_cast<ast::PrimitiveType*>(t)) return p->type;
    output::errorMismatch(t->line);
    return ast::BuiltInType::VOID;
}

static bool isNumeric(ast::BuiltInType t){
    return t==ast::BuiltInType::INT || t==ast::BuiltInType::BYTE;
}

static bool isBool(ast::BuiltInType t){ return t==ast::BuiltInType::BOOL; }

void SemanticVisitor::visit(ast::Bool &n) {
    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::ID &n) {
    Symbol* s = lookup(n.value);
    if(!s) { output::errorUndef(n.line,n.value); }
    if(s->kind==Symbol::FUNC) { output::errorDefAsVar(n.line,n.value); }
    setType(&n, s->isArray ? ast::BuiltInType::VOID : s->type);
}

void SemanticVisitor::visit(ast::Formal &n) {}

void SemanticVisitor::visit(ast::Formals &n) {
    for (auto &f : n.formals) f->accept(*this);
}

void SemanticVisitor::visit(ast::FuncDecl &n) {
    ast::BuiltInType retT = getBuiltin(n.return_type.get());
    if(dynamic_cast<ast::ArrayType*>(n.return_type.get())) output::errorMismatch(n.line);
    std::vector<ast::BuiltInType> paramT;
    for (auto &f : n.formals->formals){ 
        if(dynamic_cast<ast::ArrayType*>(f->type.get())) output::errorMismatch(f->line);
        paramT.push_back(getBuiltin(f->type.get()));
    }

    if (lookup(n.id->value)) { output::errorDef(n.line, n.id->value); }

    printer.emitFunc(n.id->value, retT, paramT);
    insert(n.id->value, {Symbol::FUNC, ast::BuiltInType::VOID, paramT, retT, 0});

    currentFuncRet = retT;
    pushScope();

    int off = -1;
    for (auto &f : n.formals->formals) {
        std::string name = f->id->value;
        if (lookup(name)) { output::errorDef(f->line, name); }
        ast::BuiltInType t = getBuiltin(f->type.get());
        printer.emitVar(name, t, off);
        insert(name, {Symbol::VAR, t, {}, ast::BuiltInType::VOID, off});
        --off;
    }

    n.body->accept(*this);

    popScope();
    currentFuncRet = ast::BuiltInType::VOID;
}

void SemanticVisitor::visit(ast::VarDecl &n){
    if(lookup(n.id->value)) output::errorDef(n.line,n.id->value);

    bool isArr=false; int len=1; ast::BuiltInType elemT;
    if(auto *p=dynamic_cast<ast::PrimitiveType*>(n.type.get())){
        elemT=p->type;
    }else if(auto *a=dynamic_cast<ast::ArrayType*>(n.type.get())){
        isArr=true; elemT=a->type;
        a->length->accept(*this);
        if(!isNumeric(getType(a->length.get()))) output::errorMismatch(a->line);
        len=constLen(a->length.get());
        if(len<=0) output::errorMismatch(a->line);
    }else output::errorMismatch(n.line);

    if(n.init_exp){
        if(isArr) output::errorMismatch(n.line);
        n.init_exp->accept(*this);
        if(auto *idE=dynamic_cast<ast::ID*>(n.init_exp.get())){
            Symbol* rhs=lookup(idE->value);
            if(rhs&&rhs->isArray) output::ErrorInvalidAssignArray(n.line,idE->value);
        }
        if(!compatible(elemT,getType(n.init_exp.get()))) output::errorMismatch(n.line);
    }

    int off=nextLocalOffset;
    printer.emitVar(n.id->value,elemT,off);
    insert(n.id->value,{Symbol::VAR,elemT,{},ast::BuiltInType::VOID,off,isArr,len});
    nextLocalOffset+=len;
}




void SemanticVisitor::visit(ast::Statements &n) {
    for (auto &s : n.statements) s->accept(*this);
}

void SemanticVisitor::visit(ast::If &n) {
    n.condition->accept(*this);
    if (getType(n.condition.get()) != ast::BuiltInType::BOOL)
        output::errorMismatch(n.line);
    n.then->accept(*this);
    if (n.otherwise) n.otherwise->accept(*this);
}

void SemanticVisitor::visit(ast::While &n) {
    n.condition->accept(*this);
    if (getType(n.condition.get()) != ast::BuiltInType::BOOL)
        output::errorMismatch(n.line);

    ++loopDepth;
    n.body->accept(*this);
    --loopDepth;
}

void SemanticVisitor::visit(ast::Break &n) {
    if (loopDepth == 0) output::errorUnexpectedBreak(n.line);
}

void SemanticVisitor::visit(ast::Continue &n) {
    if (loopDepth == 0) output::errorUnexpectedContinue(n.line);
}

void SemanticVisitor::visit(ast::Return &n) {
    if (currentFuncRet == ast::BuiltInType::VOID) {
        if (n.exp) output::errorMismatch(n.line);
        return;
    }
    if (!n.exp) output::errorMismatch(n.line);

    n.exp->accept(*this);
    auto t = getType(n.exp.get());
    if (!compatible(currentFuncRet, t))
        output::errorMismatch(n.line);
}

void SemanticVisitor::visit(ast::Num &n){
    setType(&n, ast::BuiltInType::INT);
}

void SemanticVisitor::visit(ast::NumB &n){
    if(n.value>255) output::errorByteTooLarge(n.line,n.value);
    setType(&n, ast::BuiltInType::BYTE);
}

void SemanticVisitor::visit(ast::String &n){
    setType(&n, ast::BuiltInType::STRING);
}

void SemanticVisitor::visit(ast::BinOp &n){
    n.left->accept(*this);
    n.right->accept(*this);
    auto lt=getType(n.left.get()), rt=getType(n.right.get());
    if(!isNumeric(lt)||!isNumeric(rt)) output::errorMismatch(n.line);
    setType(&n, (lt==ast::BuiltInType::INT||rt==ast::BuiltInType::INT)
                   ? ast::BuiltInType::INT : ast::BuiltInType::BYTE);
}

void SemanticVisitor::visit(ast::RelOp &n){
    n.left->accept(*this);
    n.right->accept(*this);
    auto lt=getType(n.left.get()), rt=getType(n.right.get());
    if(!isNumeric(lt)||!isNumeric(rt)) output::errorMismatch(n.line);
    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::Not &n){
    n.exp->accept(*this);
    if(!isBool(getType(n.exp.get()))) output::errorMismatch(n.line);
    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::And &n){
    n.left->accept(*this);
    n.right->accept(*this);
    if(!isBool(getType(n.left.get()))||!isBool(getType(n.right.get())))
        output::errorMismatch(n.line);
    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::Or &n){
    n.left->accept(*this);
    n.right->accept(*this);
    if(!isBool(getType(n.left.get()))||!isBool(getType(n.right.get())))
        output::errorMismatch(n.line);
    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::Assign &n){
    Symbol* s = lookup(n.id->value);
    if(!s || s->kind==Symbol::FUNC) output::errorUndef(n.line,n.id->value);
    if(s->isArray) output::ErrorInvalidAssignArray(n.line,n.id->value);
    n.exp->accept(*this);

    if(auto *idExp = dynamic_cast<ast::ID*>(n.exp.get())){
        Symbol* rhs = lookup(idExp->value);
        if(rhs && rhs->isArray) output::ErrorInvalidAssignArray(n.line,idExp->value);
    }

    auto rhs = getType(n.exp.get());
    if(!compatible(s->type,rhs)) output::errorMismatch(n.line);
    setType(&n,s->type);
}

void SemanticVisitor::visit(ast::Cast &n){
    n.exp->accept(*this);
    auto src = getType(n.exp.get());
    ast::BuiltInType dst = getBuiltin(n.target_type.get());
    bool ok = false;
    if(dst==ast::BuiltInType::INT)  ok = isNumeric(src);
    if(dst==ast::BuiltInType::BYTE) ok = isNumeric(src);
    if(dst==ast::BuiltInType::BOOL) ok = isBool(src);
    if(!ok) output::errorMismatch(n.line);
    setType(&n,dst);
}

void SemanticVisitor::visit(ast::Call &n){
    Symbol* s = lookup(n.func_id->value);
    if(!s || s->kind==Symbol::VAR) output::errorUndefFunc(n.line,n.func_id->value);

    std::vector<ast::BuiltInType> actual;
    for(auto &arg : n.args->exps){
        arg->accept(*this);

        if(auto *idA = dynamic_cast<ast::ID*>(arg.get())){
            Symbol* aSym = lookup(idA->value);
            if(aSym && aSym->isArray){
                std::vector<std::string> expect;
                for(auto t:s->params) expect.push_back(toString(t));
                output::errorPrototypeMismatch(n.line,n.func_id->value,expect);
            }
        }
        actual.push_back(getType(arg.get()));
    }

    if(actual.size()!=s->params.size()) {
        std::vector<std::string> expect;
        for(auto t : s->params) expect.push_back(toString(t));
        output::errorPrototypeMismatch(n.line,n.func_id->value,expect);
    }

    for(size_t i=0;i<actual.size();++i)
        if(!compatible(s->params[i],actual[i])) {
            std::vector<std::string> expect;
            for(auto t : s->params) expect.push_back(toString(t));
            output::errorPrototypeMismatch(n.line,n.func_id->value,expect);
        }
    setType(&n,s->ret);
}

void SemanticVisitor::visit(ast::ArrayDereference &n){
    Symbol* s = lookup(n.id->value);
    if(!s || s->kind==Symbol::FUNC || !s->isArray) output::errorUndef(n.line,n.id->value);
    n.index->accept(*this);
    if(!isNumeric(getType(n.index.get()))) output::errorMismatch(n.line);
    setType(&n, s->type);
}

void SemanticVisitor::visit(ast::ArrayAssign &n){
    Symbol* s = lookup(n.id->value);
    if(!s || s->kind==Symbol::FUNC || !s->isArray) output::errorUndef(n.line,n.id->value);

    n.index->accept(*this);
    if(!isNumeric(getType(n.index.get()))) output::errorMismatch(n.line);

    n.exp->accept(*this);
    if(!compatible(s->type,getType(n.exp.get()))) output::errorMismatch(n.line);
}

void SemanticVisitor::visit(ast::PrimitiveType &){ }

void SemanticVisitor::visit(ast::ArrayType &){ }

void SemanticVisitor::visit(ast::Funcs &n){
    for (auto &f : n.funcs) f->accept(*this);
}

void SemanticVisitor::visit(ast::ExpList &n){
    for (auto &e : n.exps) e->accept(*this);
}
