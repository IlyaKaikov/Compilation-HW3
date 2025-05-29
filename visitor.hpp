#ifndef VISITOR_HPP
#define VISITOR_HPP

namespace ast {
    class Num;
    class NumB;
    class String;
    class Bool;
    class ID;
    class BinOp;
    class RelOp;
    class Not;
    class And;
    class Or;
    // class Type;
    class Cast;
    class ExpList;
    class Call;
    class Statements;
    class Break;
    class Continue;
    class Return;
    class If;
    class While;
    class VarDecl;
    class Assign;
    class Formal;
    class Formals;
    class FuncDecl;
    class Funcs;
    class ArrayType;
    class PrimitiveType;
    class ArrayDereference;
    class ArrayAssign;
}

class Visitor {
public:
    virtual void visit(ast::Num &node) = 0;

    virtual void visit(ast::NumB &node) = 0;

    virtual void visit(ast::String &node) = 0;

    virtual void visit(ast::Bool &node) = 0;

    virtual void visit(ast::ID &node) = 0;

    virtual void visit(ast::BinOp &node) = 0;

    virtual void visit(ast::RelOp &node) = 0;

    virtual void visit(ast::Not &node) = 0;

    virtual void visit(ast::And &node) = 0;

    virtual void visit(ast::Or &node) = 0;

    // virtual void visit(ast::Type &node) = 0;

    virtual void visit(ast::ArrayType &node) = 0;

    virtual void visit(ast::PrimitiveType &node) = 0;

    virtual void visit(ast::ArrayDereference &node) = 0;

    virtual void visit(ast::ArrayAssign &node) = 0;

    virtual void visit(ast::Cast &node) = 0;

    virtual void visit(ast::ExpList &node) = 0;

    virtual void visit(ast::Call &node) = 0;

    virtual void visit(ast::Statements &node) = 0;

    virtual void visit(ast::Break &node) = 0;

    virtual void visit(ast::Continue &node) = 0;

    virtual void visit(ast::Return &node) = 0;

    virtual void visit(ast::If &node) = 0;

    virtual void visit(ast::While &node) = 0;

    virtual void visit(ast::VarDecl &node) = 0;

    virtual void visit(ast::Assign &node) = 0;

    virtual void visit(ast::Formal &node) = 0;

    virtual void visit(ast::Formals &node) = 0;

    virtual void visit(ast::FuncDecl &node) = 0;

    virtual void visit(ast::Funcs &node) = 0;
};

#endif //VISITOR_HPP
