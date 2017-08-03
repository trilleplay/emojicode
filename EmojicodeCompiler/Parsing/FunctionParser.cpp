//
//  FunctionParser.cpp
//  EmojicodeCompiler
//
//  Created by Theo Weidmann on 28/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#include "../Lex/Token.hpp"
#include "ASTNode.hpp"
#include "FunctionParser.hpp"

namespace EmojicodeCompiler {

std::shared_ptr<ASTNode> FunctionParser::parse() {
    while (stream_.nextTokenIsEverythingBut(E_WATERMELON)) {
        parseStatement(fnode_);
    }
    return fnode_;
}

void FunctionParser::flowControlBlock(const std::shared_ptr<ASTNode> &pnode) {
    auto &token = stream_.requireIdentifier(E_GRAPES);

    auto node = pnode->appendNode(ASTNode(ASTNodeType::Block, token.position()));
    while (stream_.nextTokenIsEverythingBut(E_WATERMELON)) {
        parseStatement(node);
    }
    stream_.consumeToken();
}

void FunctionParser::parseCondition(const std::shared_ptr<ASTNode> &pnode) {
    if (stream_.consumeTokenIf(E_SOFT_ICE_CREAM)) {
        auto &varName = stream_.consumeToken(TokenType::Variable);

        auto node = pnode->appendNode(ASTNode(ASTNodeType::ConditionalAssignment, varName.position()));
//        node->setValue(varName.value());
    }
    else {
        parseExpr(pnode);
    }
}

void FunctionParser::parseArguments(const std::shared_ptr<ASTNodeWithArguments> &pnode) {
    while (stream_.consumeTokenIf(E_SPIRAL_SHELL)) {
        pnode->addGenericArgument(parseType(typeContext_, TypeDynamism::AllKinds));
    }

    while (stream_.nextTokenIsEverythingBut(TokenType::EndOfArguments)) {
        parseExpr(pnode);
    }
    stream_.consumeToken();
}

void FunctionParser::parseStatement(const std::shared_ptr<ASTNode> &pnode) {
    const Token &token = stream_.consumeToken();
    if (token.type() == TokenType::Identifier) {
        switch (token.value()[0]) {
            case E_SHORTCAKE: {
                auto &varName = stream_.consumeToken(TokenType::Variable);

                Type type = parseType(typeContext_, TypeDynamism::AllKinds);
                auto node = pnode->appendNode(ASTNodeWithType(ASTNodeType::VariableDeclaration, token.position(), type));
                node->setValue(varName.value());
                return;
            }
            case E_CUSTARD: {
                if (stream_.nextToken().type() == TokenType::Identifier) {
                    auto &method = stream_.consumeToken();
                    auto &varName = stream_.consumeToken(TokenType::Variable);

                    auto node = pnode->appendNode(ASTNodeType::VariableAssignment, token.position());
                    node->setValue(varName.value());
                    auto callNode = node->appendNode(ASTNodeWithArguments(ASTNodeType::MethodCall,
                                                                          method.position()));
                    callNode->setValue(method.value());
                    auto varNode = callNode->appendNode(ASTNodeType::Variable, token.position());
                    varNode->setValue(varName.value());
                    parseArguments(callNode);
                }
                else {
                    auto &varName = stream_.consumeToken(TokenType::Variable);
                    auto node = pnode->appendNode(ASTNodeType::VariableAssignment, token.position());
                    node->setValue(varName.value());
                    if (stream_.nextToken().type() == TokenType::Operator) {
                        auto &token = stream_.consumeToken();
                        auto oper = node->appendNode(ASTNodeType::Operator, token.position());
                        oper->setValue(token.value());
                        auto varNode = oper->appendNode(ASTNodeType::Variable, token.position());
                        varNode->setValue(varName.value());
                        parseExpr(oper, operatorPrecedence(operatorType(token.value())));
                    }
                    else {
                        parseExpr(node);
                    }
                }
                return;
            }
            case E_SOFT_ICE_CREAM: {
                auto &varName = stream_.consumeToken(TokenType::Variable);
                auto node = pnode->appendNode(ASTNodeType::FrozenDeclaration, token.position());
                node->setValue(varName.value());
                parseExpr(node);
                return;
            }
            case E_TANGERINE: {
                auto node = pnode->appendNode(ASTNodeType::IfStatement, token.position());
                do {
                    parseCondition(node);
                    flowControlBlock(node);
                } while (stream_.consumeTokenIf(E_LEMON));

                if (stream_.consumeTokenIf(E_STRAWBERRY)) {
                    flowControlBlock(node);
                }
                return;
            }
            case E_AVOCADO: {
                auto &variableToken = stream_.consumeToken(TokenType::Variable);

                auto node = pnode->appendNode(ASTNodeType::ErrorCheckControl, token.position());
                node->setValue(variableToken.value());
                parseExpr(node);
                flowControlBlock(node);

                auto &errorVariableToken = stream_.consumeToken(TokenType::Variable);
                // TODO: node->setValue2(errorVariableToken.value());

                flowControlBlock(node);
                return;
            }
            case E_CLOCKWISE_RIGHTWARDS_AND_LEFTWARDS_OPEN_CIRCLE_ARROWS: {
                auto node = pnode->appendNode(ASTNodeType::RepeatWhile, token.position());
                parseCondition(node);
                flowControlBlock(node);
                return;
            }
            case E_CLOCKWISE_RIGHTWARDS_AND_LEFTWARDS_OPEN_CIRCLE_ARROWS_WITH_CIRCLED_ONE_OVERLAY: {
                auto &variableToken = stream_.consumeToken(TokenType::Variable);
                auto node = pnode->appendNode(ASTNodeType::ForIn, token.position());
                node->setValue(variableToken.value());
                parseExpr(node);
                flowControlBlock(node);
                return;
            }
            case E_GOAT: {
                auto node = pnode->appendNode(ASTNodeWithArguments(ASTNodeType::Superinitializer, token.position()));
                auto &initializerToken = stream_.consumeToken(TokenType::Identifier);
                node->setValue(initializerToken.value());
                parseArguments(node);
                return;
            }
            case E_POLICE_CARS_LIGHT: {
                auto node = pnode->appendNode(ASTNodeType::Error, token.position());
                parseExpr(node);
                return;
            }
            case E_RED_APPLE: {
                auto node = pnode->appendNode(ASTNodeType::Return, token.position());
                parseExpr(node);
                return;
            }
        }
    }
    auto node = pnode->appendNode(ASTNodeType::Expression, token.position());
    parseExprTokens(node, token, 0);
}

int FunctionParser::peakOperatorPrecedence() {
    if (stream_.hasMoreTokens() && stream_.nextTokenIs(TokenType::Operator)) {
        return operatorPrecedence(operatorType(stream_.nextToken().value()));
    }
    return 0;
}

void FunctionParser::parseExprTokens(const std::shared_ptr<ASTNode> &pnode, const Token &token, int precendence) {
    switch (token.value().front()) {
        case E_CLOUD:
            parseUnaryPrefix(pnode, ASTNodeType::IsNothingness, token);
            break;
        case E_TRAFFIC_LIGHT:
            parseUnaryPrefix(pnode, ASTNodeType::IsError, token);
            break;
        case E_WHITE_LARGE_SQUARE:
            parseUnaryPrefix(pnode, ASTNodeType::MetaTypeInstanceFromInstance, token);
            break;
        case E_BEER_MUG:
            parseUnaryPrefix(pnode, ASTNodeType::Unwrap, token);
            break;
        case E_METRO:
            parseUnaryPrefix(pnode, ASTNodeType::PerfectExtraction, token);
            break;
        case E_WHITE_SQUARE_BUTTON: {
            Type t = parseType(typeContext_, TypeDynamism::None);
            pnode->appendNode(ASTNodeWithType(ASTNodeType::MetaTypeInstance, token.position(), t));
            break;
        }
        default:
            switch (token.type()) {
                case TokenType::String: {
                    auto node = pnode->appendNode(ASTNodeType::StringLiteral, token.position());
                    node->setValue(token.value());
                    break;
                }
                case TokenType::BooleanTrue:
                    pnode->appendNode(ASTNodeType::TrueLiteral, token.position());
                    break;
                case TokenType::BooleanFalse:
                    pnode->appendNode(ASTNodeType::FalseLiteral, token.position());
                    break;
                case TokenType::Integer: {
                    auto node = pnode->appendNode(ASTNodeType::IntegerLiteral, token.position());
                    node->setValue(token.value());
                    break;
                }
                case TokenType::Double: {
                    auto node = pnode->appendNode(ASTNodeType::DoubleLiteral, token.position());
                    node->setValue(token.value());
                    break;
                }
                case TokenType::Symbol: {
                    auto node = pnode->appendNode(ASTNodeType::SymbolLiteral, token.position());
                    node->setValue(token.value());
                    break;
                }
                case TokenType::Variable: {
                    auto node = pnode->appendNode(ASTNodeType::Variable, token.position());
                    node->setValue(token.value());
                    break;
                }
                case TokenType::Identifier:
                    parseExprIdentifier(pnode, token);
                    break;
                case TokenType::DocumentationComment:
                    throw CompilerError(token.position(), "Misplaced documentation comment.");
                default:
                    throw CompilerError(token.position(), "Unexpected token %s.", token.stringName());
            }
    }


    int peakedPre;
    while (precendence < (peakedPre = peakOperatorPrecedence())) {
        auto &token = stream_.consumeToken();
        auto oper = pnode->nodes().back();
        oper->cloneAndAppend();
        oper->setNodeType(ASTNodeType::Operator);
        oper->setValue(token.value());
        parseExpr(oper, peakedPre);
    }
}

void FunctionParser::parseExprIdentifier(const std::shared_ptr<ASTNode> &pnode, const Token &token) {
    switch (token.value()[0]) {
        case E_COOKIE:
            parseListingLiteral(pnode, ASTNodeType::ConcatenateLiteral, E_COOKIE, token);
            return;
        case E_ICE_CREAM:
            parseListingLiteral(pnode, ASTNodeType::ListLiteral, E_AUBERGINE, token);
            return;
        case E_HONEY_POT:
            parseListingLiteral(pnode, ASTNodeType::DictionaryLiteral, E_AUBERGINE, token);
            return;
        case E_DOG: {
            pnode->appendNode(ASTNodeType::This, token.position());
            return;
        }
        case E_HIGH_VOLTAGE_SIGN: {
            pnode->appendNode(ASTNodeType::Nothingness, token.position());
            return;
        }
        case E_BLACK_SQUARE_BUTTON: {
            auto node = pnode->appendNode(ASTNodeType::Cast, token.position());
            parseExpr(node);
            parseTypeExpr(node, token.position());
            return;
        }
        case E_HOT_PEPPER: {
            if (stream_.consumeTokenIf(E_DOUGHNUT)) {
                auto node = pnode->appendNode(ASTNodeWithType(ASTNodeType::TypeMethodCapture, token.position(),
                                                              Type::nothingness()));
                node->setValue(stream_.consumeToken(TokenType::Identifier).value());
                parseExpr(node);
            }
            else {
                auto node = pnode->appendNode(ASTNodeWithType(ASTNodeType::MethodCapture, token.position(),
                                                              Type::nothingness()));
                node->setValue(stream_.consumeToken(TokenType::Identifier).value());
                parseTypeExpr(node, node->position());
            }
            return;
        }
        case E_GRAPES: {
            return;
        }
        case E_LOLLIPOP: {
            auto node = pnode->appendNode(ASTNodeWithArguments(ASTNodeType::CallableCall, token.position()));
            parseExpr(node);
            parseArguments(node);
            return;
        }
        case E_CHIPMUNK:  {
            auto node = pnode->appendNode(ASTNodeWithArguments(ASTNodeType::SupermethodCall, token.position()));
            node->setValue(stream_.consumeToken(TokenType::Identifier).value());
            parseArguments(node);
            return;
        }
        case E_LARGE_BLUE_DIAMOND: {
            auto node = pnode->appendNode(ASTNodeWithArguments(ASTNodeType::Initialization, token.position()));
            parseTypeExpr(node, token.position());
            node->setValue(stream_.consumeToken(TokenType::Identifier).value());
            parseArguments(node);
            return;
        }
        case E_DOUGHNUT: {
            auto node = pnode->appendNode(ASTNodeWithArguments(ASTNodeType::TypeMethodCall, token.position()));
            node->setValue(stream_.consumeToken(TokenType::Identifier).value());
            parseTypeExpr(node, node->position());
            parseArguments(node);
            return;
        }
        default: {
            auto node = pnode->appendNode(ASTNodeWithArguments(ASTNodeType::MethodCall, token.position(), Type::nothingness()));
            node->setValue(token.value());
            parseExpr(node);
            parseArguments(node);
            return;
        }
        case E_LEMON:
        case E_STRAWBERRY:
        case E_WATERMELON:
        case E_AUBERGINE:
        case E_SHORTCAKE:
        case E_CUSTARD:
        case E_SOFT_ICE_CREAM:
        case E_TANGERINE:
        case E_CLOCKWISE_RIGHTWARDS_AND_LEFTWARDS_OPEN_CIRCLE_ARROWS:
        case E_CLOCKWISE_RIGHTWARDS_AND_LEFTWARDS_OPEN_CIRCLE_ARROWS_WITH_CIRCLED_ONE_OVERLAY:
        case E_GOAT:
        case E_RED_APPLE:
        case E_POLICE_CARS_LIGHT:
        case E_AVOCADO:
            throw CompilerError(token.position(), "Unexpected statement %s.", token.value().utf8().c_str());
    }
}

void FunctionParser::parseTypeExpr(const std::shared_ptr<ASTNode> &pnode, const SourcePosition &p) {
    if (stream_.consumeTokenIf(E_BLACK_LARGE_SQUARE)) {
        auto node = pnode->appendNode(ASTNodeWithType(ASTNodeType::TypeFromExpression, p, Type::nothingness()));
        parseExpr(pnode);
        node->setAvailability(TypeAvailability::DynamicAndAvailable);
        return;
    }
    if (stream_.consumeTokenIf(E_MEDIUM_BLACK_CIRCLE)) {
        pnode->appendNode(ASTNodeWithType(ASTNodeType::InferType, p, Type::nothingness()));
        return;
    }
    Type ot = parseType(typeContext_, TypeDynamism::AllKinds);
    switch (ot.type()) {
        case TypeContent::GenericVariable:
            throw CompilerError(p, "Generic Arguments are not yet available for reflection.");
        case TypeContent::Class: {
            auto node = pnode->appendNode(ASTNodeWithType(ASTNodeType::StaticType, p, ot));
            node->setAvailability(TypeAvailability::StaticAndAvailabale);
            return;
        }
        case TypeContent::Self: {
            auto node = pnode->appendNode(ASTNodeWithType(ASTNodeType::ThisType, p, ot));
            node->setAvailability(TypeAvailability::DynamicAndAvailable);
            return;
        }
        case TypeContent::LocalGenericVariable:
            throw CompilerError(p, "Function Generic Arguments are not available for reflection.");
        default:
            break;
    }
    auto node = pnode->appendNode(ASTNodeWithType(ASTNodeType::StaticType, p, ot));
    node->setAvailability(TypeAvailability::StaticAndUnavailable);
}

}  // namespace EmojicodeCompiler
