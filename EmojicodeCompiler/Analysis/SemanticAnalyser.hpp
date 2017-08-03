//
//  SemanticAnalyser.hpp
//  EmojicodeCompiler
//
//  Created by Theo Weidmann on 28/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#ifndef SemanticAnalyser_hpp
#define SemanticAnalyser_hpp

#include "../Parsing/ASTNode.hpp"
#include "PathAnalyser.hpp"
#include "../Scoping/SemanticScoper.hpp"
#include "../Types/Type.hpp"
#include "../Types/TypeExpectation.hpp"
#include "../Types/TypeContext.hpp"
#include "../FunctionType.hpp"
#include "../CompilerError.hpp"
#include <utility>
#include <memory>

namespace EmojicodeCompiler {

class SemanticAnalyser {
public:
    SemanticAnalyser(TypeContext context, Function *function)
    : scoper_(SemanticScoper::scoperForFunction(function)), typeContext_(context), function_(function) {}
    void analyse(std::shared_ptr<ASTNode> node);
private:
    PathAnalyser pathAnalyser_;
    /// The scoper responsible for scoping the function being compiled.
    SemanticScoper scoper_;
    TypeContext typeContext_;

    Function *function_;

    void analyseStatement(std::shared_ptr<ASTNode> node);
    void analyseBlock(std::shared_ptr<ASTNode> node);
    Type analyseExpression(std::shared_ptr<ASTNode>, const TypeExpectation & = TypeExpectation());
    Type analyseMethodCall(std::shared_ptr<ASTNodeWithArguments> node, const Type &type, size_t offset);
    std::pair<Type, std::shared_ptr<ASTNodeWithType>> analyseTypeExpr(std::shared_ptr<ASTNode> node, const TypeExpectation &exp) {
        auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
        auto type = analyseExpression(typeNode, exp).resolveOnSuperArgumentsAndConstraints(typeContext_);
        return std::make_pair(type, typeNode);
    }

    /// Tries to pop the temporary scope (see SemanticScoper) and wraps the node into a @c Scoped node if necessary.
    void popTemporaryScope(const std::shared_ptr<ASTNode> &node) {
        if (scoper_.popTemporaryScope()) {
            node->cloneAndAppend();
            node->setNodeType(ASTNodeType::Scoped);
        }
    }

    void analyseReturn(const std::shared_ptr<ASTNode> &);
    void analyseInitializationRequirements();

    /// Parses an expression node, verifies it return type and boxes it according to the given expectation.
    /// Calls @c expect internally.
    Type expectType(const Type &type, std::shared_ptr<ASTNode> node, std::vector<CommonTypeFinder> *ctargs = nullptr);
    /// Parses an expression node and boxes it according to the given expectation. Calls @c box internally.
    Type expect(const TypeExpectation &expectation, std::shared_ptr<ASTNode> node);
    /// Performs boxing on a node exactly as @c expect does. Only use this if @c expect has been ruled out.
    Type box(Type exprType, const TypeExpectation &expectation, std::shared_ptr<ASTNode> node);

    Type analyseFunctionCall(std::shared_ptr<ASTNodeWithArguments> node, const Type &type,
                             size_t offset, Function *function);
    Type analysePrimitiveOperatorNode(const Type &valueType, const std::shared_ptr<ASTNode> &node);

    void validateMetability(const Type &originalType, const SourcePosition &p) const {
        if (!originalType.allowsMetaType()) {
            auto string = originalType.toString(typeContext_, true);
            throw CompilerError(p, "Can’t get metatype of %s.", string.c_str());
        }
    }

    void validateMethodCapturability(const Type &type, const SourcePosition &p) const {
        if (type.type() == TypeContent::ValueType) {
            if (type.size() > 1) {
                throw CompilerError(p, "Type not eligible for method capturing.");  // TODO: Improve
            }
        }
        else if (type.type() != TypeContent::Class) {
            throw CompilerError(p, "You can’t capture method calls on this kind of type.");
        }
    }
    void validateAccessLevel(Function *function, const SourcePosition &p) const;

    bool typeIsEnumerable(const Type &type, Type *elementType);
};

}  // namespace Emojicode

#endif /* SemanticAnalyser_hpp */
