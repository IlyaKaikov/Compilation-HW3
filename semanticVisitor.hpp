#ifndef SEMANTICVISITOR_HPP
#define SEMANTICVISITOR_HPP

#include "output.hpp"
#include "nodes.hpp"
#include <stack>
#include <unordered_map>

struct Symbol {
    enum Kind { VAR, FUNC } kind;
    ast::BuiltInType type;
    std::vector<ast::BuiltInType> params;
    ast::BuiltInType ret;
    int offset;
    bool isArray;
    int  arrLen;
};

class SemanticVisitor : public Visitor {
public:
    SemanticVisitor();
    void finish();

    void visit(ast::Num &n) override;
    void visit(ast::Bool &n) override;
    void visit(ast::ID &n) override;
    void visit(ast::VarDecl &n) override;
    void visit(ast::Formal &n) override;
    void visit(ast::Formals &n) override;
    void visit(ast::FuncDecl &n) override;
    void visit(ast::Return &n) override;
    void visit(ast::Break &n) override;
    void visit(ast::Continue &n) override;
    void visit(ast::While &n) override;
    void visit(ast::If &n) override;
    void visit(ast::Statements &n) override;
    void visit(ast::NumB &n) override;
    void visit(ast::String &n) override;
    void visit(ast::BinOp &n) override;
    void visit(ast::RelOp &n) override;
    void visit(ast::Not &n) override;
    void visit(ast::And &n) override;
    void visit(ast::Or &n) override;
    void visit(ast::Assign &n) override;
    void visit(ast::Cast &n) override;
    void visit(ast::Call &n) override;
    void visit(ast::ArrayAssign &n) override;
    void visit(ast::ArrayDereference &n) override;
    void visit(ast::PrimitiveType &n) override;
    void visit(ast::ArrayType &n) override;
    void visit(ast::Funcs &n) override;
    void visit(ast::ExpList &n) override;




private:
    using Table = std::unordered_map<std::string,Symbol>;
    std::unordered_map<const ast::Node*,ast::BuiltInType> exprType;
    std::vector<Table> scopes;
    output::ScopePrinter printer;

    ast::BuiltInType currentFuncRet = ast::BuiltInType::VOID;
    bool insideLoop = false;
    bool skipNextStatementsScope = false;
    int nextLocalOffset = 0;
    int loopDepth = 0;

    void pushScope();
    void popScope();
    bool insert(const std::string&, const Symbol&);
    Symbol* lookup(const std::string&);
    void  setType(const ast::Node*, ast::BuiltInType);
    ast::BuiltInType getType(const ast::Node*) const;

};

#endif //SEMANTICVISITOR_HPP