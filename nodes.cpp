#include "nodes.hpp"
#include <string>
#include <utility>

extern int yylineno;

namespace ast {

    Node::Node() : line(yylineno) {}

    Num::Num(const char *str) : Exp(), value(std::stoi(str)) {}

    NumB::NumB(const char *str) : Exp(), value(std::stoi(str)) {}

    String::String(const char *str) : Exp(), value(str) {
        // Remove the quotes
        value = value.substr(1, value.size() - 2);
    }

    Bool::Bool(bool value) : Exp(), value(value) {}

    ID::ID(const char *str) : Exp(), value(str) {}

    BinOp::BinOp(std::shared_ptr<Exp> left, std::shared_ptr<Exp> right, BinOpType op)
            : Exp(), left(std::move(left)), right(std::move(right)), op(op) {}

    RelOp::RelOp(std::shared_ptr<Exp> left, std::shared_ptr<Exp> right, RelOpType op)
            : Exp(), left(std::move(left)), right(std::move(right)), op(op) {}

    PrimitiveType::PrimitiveType(BuiltInType type) : Node(), type(type) {}

    ArrayType::ArrayType(BuiltInType type, std::shared_ptr<Exp> length) : Node(), type(type), length(length) {}

    Cast::Cast(std::shared_ptr<Exp> exp, std::shared_ptr<PrimitiveType> target_type)
            : Exp(), exp(std::move(exp)), target_type(std::move(target_type)) {}

    Not::Not(std::shared_ptr<Exp> exp) : Exp(), exp(std::move(exp)) {}

    And::And(std::shared_ptr<Exp> left, std::shared_ptr<Exp> right)
            : Exp(), left(std::move(left)), right(std::move(right)) {}

    Or::Or(std::shared_ptr<Exp> left, std::shared_ptr<Exp> right)
            : Exp(), left(std::move(left)), right(std::move(right)) {}

    ExpList::ExpList(std::shared_ptr<Exp> exp) : Node(), exps({std::move(exp)}) {}

    void ExpList::push_front(const std::shared_ptr<Exp> &exp) {
        exps.insert(exps.begin(), exp);
    }

    void ExpList::push_back(const std::shared_ptr<Exp> &exp) {
        exps.push_back(exp);
    }

    Call::Call(std::shared_ptr<ID> func_id, std::shared_ptr<ExpList> args)
            : Exp(), func_id(std::move(func_id)), args(std::move(args)) {}

    Call::Call(std::shared_ptr<ID> func_id)
            : Exp(), func_id(std::move(func_id)), args(std::make_shared<ExpList>()) {}

    Statements::Statements(std::shared_ptr<Statement> statement) : Statement(), statements({std::move(statement)}) {}

    void Statements::push_front(const std::shared_ptr<Statement> &statement) {
        statements.insert(statements.begin(), statement);
    }

    void Statements::push_back(const std::shared_ptr<Statement> &statement) {
        statements.push_back(statement);
    }

    Return::Return(std::shared_ptr<Exp> exp) : Statement(), exp(std::move(exp)) {}

    If::If(std::shared_ptr<Exp> condition, std::shared_ptr<Statement> then, std::shared_ptr<Statement> otherwise)
            : Statement(), condition(std::move(condition)), then(std::move(then)), otherwise(std::move(otherwise)) {}

    While::While(std::shared_ptr<Exp> condition, std::shared_ptr<Statement> body)
            : Statement(), condition(std::move(condition)),
              body(std::move(body)) {}

    VarDecl::VarDecl(std::shared_ptr<ID> id, std::shared_ptr<Type> type, std::shared_ptr<Exp> init_exp)
            : Statement(), id(std::move(std::move(id))), type(std::move(type)), init_exp(std::move(init_exp)) {}

    Assign::Assign(std::shared_ptr<ID> id, std::shared_ptr<Exp> exp)
            : Statement(), id(std::move(id)), exp(std::move(exp)) {}
    
    ArrayAssign::ArrayAssign(std::shared_ptr<ID> id, std::shared_ptr<Exp> exp, std::shared_ptr<Exp> index)
            : Statement(), id(std::move(id)), exp(std::move(exp)), index(std::move(index)) {}
    
    ArrayDereference::ArrayDereference(std::shared_ptr<ID> id, std::shared_ptr<Exp> index)
            : Exp(), id(std::move(id)), index(std::move(index)) {}
    
    Formal::Formal(std::shared_ptr<ID> id, std::shared_ptr<Type> type)
            : Node(), id(std::move(id)), type(std::move(type)) {}

    Formals::Formals(std::shared_ptr<Formal> formal) : Node(), formals({std::move(formal)}) {}

    void Formals::push_front(const std::shared_ptr<Formal> &formal) {
        formals.insert(formals.begin(), formal);
    }

    void Formals::push_back(const std::shared_ptr<Formal> &formal) {
        formals.push_back(formal);
    }

    FuncDecl::FuncDecl(std::shared_ptr<ID> id, std::shared_ptr<Type> return_type, std::shared_ptr<Formals> formals, std::shared_ptr<Statements> body)
            : Node(), id(std::move(id)), return_type(std::move(return_type)), formals(std::move(formals)), body(std::move(body)) {}

    Funcs::Funcs(std::shared_ptr<FuncDecl> func) : Node(), funcs({std::move(func)}) {}

    void Funcs::push_front(const std::shared_ptr<FuncDecl> &func) {
        funcs.insert(funcs.begin(), func);
    }

    void Funcs::push_back(const std::shared_ptr<FuncDecl> &func) {
        funcs.push_back(func);
    }

}