//
//  FunctionParser.hpp
//  EmojicodeCompiler
//
//  Created by Theo Weidmann on 28/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#ifndef FunctionParser_hpp
#define FunctionParser_hpp

#include "../Analysis/OperatorHelper.hpp"
#include "AbstractParser.hpp"
#include "../Lex/TokenStream.hpp"
#include "../Types/TypeContext.hpp"
#include "ASTNodeType.hpp"
#include "ASTNode.hpp"

namespace EmojicodeCompiler {

class ASTNode;

class FunctionParser : AbstractParser {
public:
    FunctionParser(Package *pkg, TokenStream stream, const TypeContext &context) : AbstractParser(pkg, stream), typeContext_(context) {}
    std::shared_ptr<ASTNode> parse();
private:
    TypeContext typeContext_;
    std::shared_ptr<ASTNode> fnode_ = std::make_shared<ASTNode>(ASTNodeType::Block, SourcePosition(0, 0, ""));
    void parseStatement(const std::shared_ptr<ASTNode> &);
    void flowControlBlock(const std::shared_ptr<ASTNode> &);
    void parseCondition(const std::shared_ptr<ASTNode> &);
    void parseExprTokens(const std::shared_ptr<ASTNode> &pnode, const Token &token, int precendence);
    void parseExprIdentifier(const std::shared_ptr<ASTNode> &pnode, const Token &token);
    void parseArguments(const std::shared_ptr<ASTNodeWithArguments> &pnode);
    void parseTypeExpr(const std::shared_ptr<ASTNode> &pnode, const SourcePosition &p);

    void parseExpr(const std::shared_ptr<ASTNode> &pnode, int precedence = 0) {
        return parseExprTokens(pnode, stream_.consumeToken(), precedence);
    }

    void parseUnaryPrefix(const std::shared_ptr<ASTNode> &pnode, ASTNodeType type, const Token &token) {
        auto node = pnode->appendNode(type, token.position());
        parseExpr(node, kPrefixPrecedence);
    }

    void parseListingLiteral(const std::shared_ptr<ASTNode> &pnode, ASTNodeType type, Emojis end, const Token &token) {
        auto node = pnode->appendNode(type, token.position());
        while (stream_.nextTokenIsEverythingBut(end)) {
            parseExpr(node);
        }
        stream_.consumeToken();
    }

    int peakOperatorPrecedence();
};

}  // namespace Emojicode

#endif /* FunctionParser_hpp */
