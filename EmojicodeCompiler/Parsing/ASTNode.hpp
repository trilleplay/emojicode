//
//  ASTNode.hpp
//  EmojicodeCompiler
//
//  Created by Theo Weidmann on 28/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#ifndef ASTNode_hpp
#define ASTNode_hpp

#include "../Types/Type.hpp"
#include "../Types/TypeAvailability.hpp"
#include "ASTNodeType.hpp"
#include "../Lex/SourcePosition.hpp"
#include <memory>
#include <vector>

namespace EmojicodeCompiler {

class ASTNode {
public:
    ASTNode(ASTNodeType type, const SourcePosition &p) : nodeType_(type), sourcePosition_(p) {}
    std::shared_ptr<ASTNode> appendNode(ASTNodeType type, const SourcePosition &p) {
        nodes_.emplace_back(std::make_shared<ASTNode>(type, p));
        return nodes_.back();
    }

    void appendNodeP(const std::shared_ptr<ASTNode> &node) {
        nodes_.emplace_back(node);
    };
    template <typename T>
    std::shared_ptr<T> appendNode(T &&node) {
        nodes_.emplace_back(new T(std::move(node)));
        return std::static_pointer_cast<T>(nodes_.back());
    };
    void setValue(const EmojicodeString &string) { value_ = string; }

    const std::vector<std::shared_ptr<ASTNode>>& nodes() const { return nodes_; }
    ASTNodeType nodeType() const { return nodeType_; }
    void setNodeType(ASTNodeType type) { nodeType_ = type; }
    const EmojicodeString& value() const { return value_; }
    void setIntValue(unsigned int value) { intValue_ = value; }
    unsigned int intValue() const { return intValue_; }
    const SourcePosition& position() const { return sourcePosition_; }

    void setExpressionType(const Type &type) { expressionType_ = type; }
    
    /// Set after semantic analysis and transformation.
    /// Iff this node represents an expression type this type is the exact type produced by this node.
    const Type& expressionType() const { return expressionType_; }

    /// Copies the content of this node into a new node, clears the nodes of this object and appends the clone
    /// to this object.
    /// @returns The cloned node which was appendend to the callee.
    std::shared_ptr<ASTNode> cloneAndAppend() {
        auto newNode = clone();
        nodes_.clear();
        nodes_.emplace_back(newNode);
        return newNode;
    }

    void swapSubnodes() {
        std::swap(nodes_[0], nodes_[1]);
    }

    void clearNodes() {
        nodes_.clear();
    }
private:
    virtual std::shared_ptr<ASTNode> clone() { return std::make_shared<ASTNode>(*this); }
    ASTNodeType nodeType_;
    std::vector<std::shared_ptr<ASTNode>> nodes_;
    SourcePosition sourcePosition_;
    Type expressionType_ = Type::nothingness();
    EmojicodeString value_;
    unsigned int intValue_;
};

class ASTNodeWithType : public ASTNode {
public:
    ASTNodeWithType(ASTNodeType nodeType, const SourcePosition &p, const Type &type)
    : ASTNode(nodeType, p), type_(type) {}
    void setAvailability(TypeAvailability a) { availability_ = a; }
    TypeAvailability availability() const { return availability_; }
    const Type& type() const { return type_; }
    void setType(const Type &type) { type_ = type; }
private:
    virtual std::shared_ptr<ASTNode> clone() { return std::make_shared<ASTNodeWithType>(*this); }
    Type type_;
    TypeAvailability availability_ = TypeAvailability::StaticAndUnavailable;
};

class ASTNodeWithArguments : public ASTNodeWithType {
public:
    ASTNodeWithArguments(ASTNodeType nodeType, const SourcePosition &p, const Type &type)
    : EmojicodeCompiler::ASTNodeWithType(nodeType, p, type) {}
    ASTNodeWithArguments(ASTNodeType nodeType, const SourcePosition &p)
    : EmojicodeCompiler::ASTNodeWithType(nodeType, p, Type::nothingness()) {}
    void addGenericArgument(const Type &type) { genericArguments_.emplace_back(type); }
    std::vector<Type>& genericArguments() { return genericArguments_; }
private:
    virtual std::shared_ptr<ASTNode> clone() { return std::make_shared<ASTNodeWithArguments>(*this); }
    std::vector<Type> genericArguments_;
};

}  // namespace Emojicode

#endif /* ASTNode_hpp */
