#include "semanticVisitor.hpp"
#include <iostream>
using namespace ast;
using output::ScopePrinter;

// this is same toString func as in output.cpp
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
    bool shouldPrint = !scopes.empty();
    scopes.emplace_back();
    if (shouldPrint)
        printer.beginScope();
}

void SemanticVisitor::popScope() {
    if (scopes.size() > 1)
        printer.endScope();

    scopes.pop_back();
}

bool SemanticVisitor::insert(const std::string& id,const Symbol& s) {
    auto& table = scopes.back();
    if (table.count(id)) 
        return false;

    table[id]= s;
    return true;
}

Symbol* SemanticVisitor::lookup(const std::string& id) {
    for (int i = scopes.size() -1; i >= 0; --i) {
        auto iter = scopes[i].find(id);
        if(iter != scopes[i].end()) 
            return &iter->second;
    }
    return nullptr;
}

void SemanticVisitor::setType(const ast::Node* n, ast::BuiltInType t) {
    exprType[n]= t;
}

ast::BuiltInType SemanticVisitor::getType(const ast::Node* n) const {
    auto iter = exprType.find(n);
    return iter == exprType.end() ? ast::BuiltInType::VOID : iter->second;
}


SemanticVisitor::SemanticVisitor() {
    pushScope();

    printer.emitFunc("print",  BuiltInType::VOID, {BuiltInType::STRING});
    insert("print", {Symbol::FUNC, BuiltInType::VOID, {BuiltInType::STRING}, BuiltInType::VOID,0});

    printer.emitFunc("printi", BuiltInType::VOID, {BuiltInType::INT});
    insert("printi", {Symbol::FUNC, BuiltInType::VOID, {BuiltInType::INT}, BuiltInType::VOID,0});
}

void SemanticVisitor::finish() {
    Symbol* mainSym = lookup("main");
    if (mainSym == nullptr || mainSym->kind != Symbol::FUNC || mainSym->ret  != BuiltInType::VOID || !mainSym->params.empty())
        output::errorMainMissing();

    std::cout << printer;
}

// start of helper funcs

static int constLen(ast::Exp *e) {
    if (auto *n = dynamic_cast<ast::Num*>(e))
        return n->value;

    if (auto *b = dynamic_cast<ast::NumB*>(e))
        return b->value;

    output::errorMismatch(e->line);
    return 0;
}

static bool typesCompatible(ast::BuiltInType expected, ast::BuiltInType actual) {
    if (expected == actual) 
        return true;
    // byte to int is fine, however int to byte is a big no-no O~O
    return expected == ast::BuiltInType::INT && actual == ast::BuiltInType::BYTE;
}

static ast::BuiltInType getBuiltin(ast::Type *t) {
    if (auto *p = dynamic_cast<ast::PrimitiveType*>(t)) 
        return p->type;

    output::errorMismatch(t->line);
    return ast::BuiltInType::VOID;
}

static bool isTypeNum(ast::BuiltInType t) {
    return t == ast::BuiltInType::INT || t == ast::BuiltInType::BYTE;
}

static bool isTypeBool(ast::BuiltInType t) { 
    return t==ast::BuiltInType::BOOL; 
}

// end of helper funcs

void SemanticVisitor::visit(ast::Bool &n) {
    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::ID &n) {
    Symbol* s = lookup(n.value);
    if (!s) 
        output::errorUndef(n.line,n.value);

    if (s->kind == Symbol::FUNC) 
        output::errorDefAsVar(n.line,n.value);

    setType(&n, s->isArray ? ast::BuiltInType::VOID : s->type);
}

void SemanticVisitor::visit(ast::Formal &n) {}

void SemanticVisitor::visit(ast::Formals &n) {
    for (auto &f : n.formals) f->accept(*this);
}

void SemanticVisitor::visit(ast::FuncDecl &n) {
    ast::BuiltInType returnType = getBuiltin(n.return_type.get());
    if (dynamic_cast<ast::ArrayType*>(n.return_type.get())) 
        output::errorMismatch(n.line);

    std::vector<ast::BuiltInType> parameterT;
    for (auto &f : n.formals->formals) { 
        if (dynamic_cast<ast::ArrayType*>(f->type.get())) 
            output::errorMismatch(f->line);

        parameterT.push_back(getBuiltin(f->type.get()));
    }

    Symbol *decl = lookup(n.id->value);
    if (!decl) {
        printer.emitFunc(n.id->value,  returnType, parameterT);
        insert(n.id->value,{Symbol::FUNC, BuiltInType::VOID, parameterT, returnType, 0});
    } 
    else {
        if (decl->kind != Symbol::FUNC)
            output::errorDefAsVar(n.id->line, n.id->value);
    }


    currentFuncRet = returnType;
    pushScope();
    nextLocalOffset = 0;
    int off = -1;
    for (auto &f : n.formals->formals) {
        std::string name = f->id->value;
        if (lookup(name)) 
            output::errorDef(f->line, name);

        ast::BuiltInType t = getBuiltin(f->type.get());
        printer.emitVar(name, t, off);
        insert(name, {Symbol::VAR, t, {}, ast::BuiltInType::VOID, off});
        --off;
    }
    skipNextStatementsScope = true;
    n.body->accept(*this);
    popScope();
    currentFuncRet = ast::BuiltInType::VOID;
}

void SemanticVisitor::visit(ast::VarDecl &n) {
    if (lookup(n.id->value)) output::errorDef(n.line, n.id->value);

    bool isArr = false;
    int  len   = 1;
    BuiltInType elementT;

    if (auto *p = dynamic_cast<PrimitiveType*>(n.type.get())) {
        elementT = p->type;
    } 
    else if (auto *a = dynamic_cast<ArrayType*>(n.type.get())) {
        isArr = true;
        elementT = a->type;
        a->length->accept(*this);
        if (!isTypeNum(getType(a->length.get()))) 
            output::errorMismatch(a->line);

        len = constLen(a->length.get());
        if (len <= 0) 
            output::errorMismatch(a->line);

    } 
    else 
        output::errorMismatch(n.line);

    if (n.init_exp) {
        if (isArr) 
            output::errorMismatch(n.line);

        n.init_exp->accept(*this);
        if (auto *idRhs = dynamic_cast<ID*>(n.init_exp.get())) {
            Symbol *sRhs = lookup(idRhs->value);
            if (sRhs && sRhs->isArray) 
                output::ErrorInvalidAssignArray(n.line, idRhs->value);
        }
        if (!typesCompatible(elementT, getType(n.init_exp.get()))) 
            output::errorMismatch(n.line);
    }
    
    int off = nextLocalOffset;
    if (isArr)
        printer.emitArr(n.id->value, elementT, len, off);
    else 
        printer.emitVar(n.id->value, elementT, off);

    insert(n.id->value, {Symbol::VAR, elementT, {}, BuiltInType::VOID, off, isArr, len});
    nextLocalOffset += len;
}


void SemanticVisitor::visit(ast::Statements &n) {
    if (skipNextStatementsScope) {
        skipNextStatementsScope = false;
        for (auto &s : n.statements) {
            s->accept(*this);
        }
        return;
    }
    pushScope();
    for (auto &s : n.statements) {
        s->accept(*this);
    }
    popScope();
}

void SemanticVisitor::visit(ast::If &n) {
    n.condition->accept(*this);
    if (getType(n.condition.get()) != BuiltInType::BOOL)
        output::errorMismatch(n.condition->line);

    pushScope();
    n.then->accept(*this);
    popScope();
    if (n.otherwise) {
        pushScope();
        n.otherwise->accept(*this);
        popScope();
    }
}

void SemanticVisitor::visit(ast::While &n) {
    n.condition->accept(*this);
    if (getType(n.condition.get()) != BuiltInType::BOOL)
        output::errorMismatch(n.condition->line);

    ++loopDepth;
    pushScope();
    n.body->accept(*this);
    popScope();
    --loopDepth;
}


void SemanticVisitor::visit(ast::Break &n) {
    if (loopDepth == 0){
        output::errorUnexpectedBreak(n.line);
    }
}
void SemanticVisitor::visit(ast::Continue &n) {
    if (loopDepth == 0){
        output::errorUnexpectedContinue(n.line);
    }
}

void SemanticVisitor::visit(ast::Return &n) {
    if (currentFuncRet == ast::BuiltInType::VOID) {
        if (n.exp) 
            output::errorMismatch(n.line);
        return;
    }
    if (!n.exp)
        output::errorMismatch(n.line);

    n.exp->accept(*this);
    auto t = getType(n.exp.get());
    if (!typesCompatible(currentFuncRet, t))
        output::errorMismatch(n.line);
}

void SemanticVisitor::visit(ast::Num &n) {
    setType(&n, ast::BuiltInType::INT);
}

void SemanticVisitor::visit(ast::NumB &n) {
    if (n.value>255) {output::errorByteTooLarge(n.line,n.value);}
    setType(&n, ast::BuiltInType::BYTE);
}

void SemanticVisitor::visit(ast::String &n) {
    setType(&n, ast::BuiltInType::STRING);
}

void SemanticVisitor::visit(ast::BinOp &n) {
    n.left->accept(*this);
    n.right->accept(*this);

    auto leftT = getType(n.left.get()), rightT = getType(n.right.get());
    if (!isTypeNum(leftT) || !isTypeNum(rightT)) 
        output::errorMismatch(n.line);

    setType(&n, (leftT == ast::BuiltInType::INT || rightT== ast::BuiltInType::INT) ? ast::BuiltInType::INT : ast::BuiltInType::BYTE);
}

void SemanticVisitor::visit(ast::RelOp &n) {
    n.left->accept(*this);
    n.right->accept(*this);
    auto lt = getType(n.left.get()), rt = getType(n.right.get());
    if (!isTypeNum(lt) || !isTypeNum(rt)) {
        output::errorMismatch(n.line);
    }

    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::Not &n){
    n.exp->accept(*this);
    if (!isTypeBool(getType(n.exp.get()))) 
        output::errorMismatch(n.line);

    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::And &n){
    n.left->accept(*this);
    n.right->accept(*this);

    if (!isTypeBool(getType(n.left.get())) || !isTypeBool(getType(n.right.get())))
        output::errorMismatch(n.line);

    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::Or &n){
    n.left->accept(*this);
    n.right->accept(*this);

    if (!isTypeBool(getType(n.left.get())) || !isTypeBool(getType(n.right.get())))
        output::errorMismatch(n.line);
    setType(&n, ast::BuiltInType::BOOL);
}

void SemanticVisitor::visit(ast::Assign &n) {
    Symbol* sym = lookup(n.id->value);
    if (!sym || sym->kind == Symbol::FUNC) 
        output::errorUndef(n.line,n.id->value);
    if (sym->isArray) 
        output::ErrorInvalidAssignArray(n.line,n.id->value);

    n.exp->accept(*this);
    if (auto *idExp = dynamic_cast<ast::ID*>(n.exp.get())) {
        Symbol* rhs = lookup(idExp->value);
        if (rhs && rhs->isArray) 
            output::ErrorInvalidAssignArray(n.line,idExp->value);
    }

    auto rhs = getType(n.exp.get());
    if (!typesCompatible(sym->type,rhs)) 
        output::errorMismatch(n.line);
    setType(&n, sym->type);
}

void SemanticVisitor::visit(ast::Cast &n) {
    n.exp->accept(*this);
    auto src = getType(n.exp.get());
    ast::BuiltInType dest = getBuiltin(n.target_type.get());
    bool isCastOK = false;

    if (dest == ast::BuiltInType::INT){
        isCastOK = isTypeNum(src);
    }
    if (dest == ast::BuiltInType::BYTE){
        isCastOK = isTypeNum(src);
    }
    if (dest == ast::BuiltInType::BOOL){
        isCastOK = isTypeBool(src);
    }
    if (!isCastOK){
        output::errorMismatch(n.line);
    }
    setType(&n, dest);
}


void SemanticVisitor::visit(ast::Call &n) {
    Symbol* sym = lookup(n.func_id->value);
    if (!sym || sym->kind== Symbol::VAR) 
        output::errorUndefFunc(n.line,n.func_id->value);

    std::vector<ast::BuiltInType> actual;
    for (auto &arg : n.args->exps){
        arg->accept(*this);
        if (auto *idA = dynamic_cast<ast::ID*>(arg.get())) {
            Symbol* aSym = lookup(idA->value);
            if (aSym && aSym->isArray){
                std::vector<std::string> expect;
                for (auto t:sym->params) {
                    expect.push_back(toString(t));
                }
                output::errorPrototypeMismatch(n.line,n.func_id->value,expect);
            }
        }
        actual.push_back(getType(arg.get()));
    }

    if (actual.size()!=sym->params.size()) {
        std::vector<std::string> expect;
        for (auto t : sym->params) {
            expect.push_back(toString(t));
        }
        output::errorPrototypeMismatch(n.line,n.func_id->value,expect);
    }

    for (size_t i = 0; i< actual.size(); ++i){
        if (!typesCompatible(sym->params[i],actual[i])){
            std::vector<std::string> expect;
            for (auto t : sym->params){
                expect.push_back(toString(t));
            }
            output::errorPrototypeMismatch(n.line,n.func_id->value,expect);
        }
    }
    setType(&n, sym->ret);
}

void SemanticVisitor::visit(ast::ArrayDereference &n) {
    Symbol* s = lookup(n.id->value);
    if (!s || s->kind == Symbol::FUNC || !s->isArray) {
        output::errorUndef(n.line,n.id->value);
    }

    n.index->accept(*this);
    if (!isTypeNum(getType(n.index.get()))) {
        output::errorMismatch(n.line);
    }

    setType(&n, s->type);
}

void SemanticVisitor::visit(ast::ArrayAssign &n) {
    Symbol* s = lookup(n.id->value);
    if (!s || s->kind == Symbol::FUNC || !s->isArray) {
        output::errorUndef(n.line,n.id->value);
    }

    n.index->accept(*this);
    if (!isTypeNum(getType(n.index.get()))) {
        output::errorMismatch(n.line);
    }

    n.exp->accept(*this);
    if (!typesCompatible(s->type, getType(n.exp.get()))) {
        output::errorMismatch(n.line);
    }
}

void SemanticVisitor::visit(ast::PrimitiveType &){}

void SemanticVisitor::visit(ast::ArrayType &){}


void SemanticVisitor::visit(ast::Funcs &n) {
    for (auto &f : n.funcs) {
        BuiltInType ret = getBuiltin(f->return_type.get());
        if (dynamic_cast<ast::ArrayType*>(f->return_type.get()))
            output::errorMismatch(f->line);

        std::vector<BuiltInType> params;
        for (auto &formal : f->formals->formals) {
            if (dynamic_cast<ast::ArrayType*>(formal->type.get()))
                output::errorMismatch(formal->line);
            params.push_back(getBuiltin(formal->type.get()));
        }

        if (lookup(f->id->value))
            output::errorDef(f->id->line, f->id->value);

        printer.emitFunc(f->id->value, ret, params);
        insert(f->id->value,{Symbol::FUNC, BuiltInType::VOID, params, ret, 0});
    }

    for (auto &f : n.funcs){
        f->accept(*this);
    }
}

void SemanticVisitor::visit(ast::ExpList &n) {
    for (auto &e : n.exps) {
        e->accept(*this);
    }
}
