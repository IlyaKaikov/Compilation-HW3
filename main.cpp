#include "semanticVisitor.hpp"
#include "nodes.hpp"

extern int yyparse();
extern std::shared_ptr<ast::Node> program;

int main() {
    yyparse();
    SemanticVisitor sem;
    program->accept(sem);
    sem.finish();
    return 0;
}
