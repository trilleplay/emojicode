//
//  AbstractParser.cpp
//  Emojicode
//
//  Created by Theo Weidmann on 27/04/16.
//  Copyright © 2016 Theo Weidmann. All rights reserved.
//

#include "AbstractParser.hpp"
#include "../Function.hpp"
#include "../Types/Protocol.hpp"
#include "../Types/TypeContext.hpp"
#include <map>
#include <vector>

namespace EmojicodeCompiler {

TypeIdentifier AbstractParser::parseTypeIdentifier() {
    if (stream_.nextTokenIs(TokenType::Variable)) {
        throw CompilerError(stream_.consumeToken().position(), "Generic variables not allowed here.");
    }

    if (stream_.nextTokenIs(E_CANDY)) {
        throw CompilerError(stream_.consumeToken().position(), "Unexpected 🍬.");
    }

    EmojicodeString enamespace;

    if (stream_.consumeTokenIf(E_ORANGE_TRIANGLE)) {
        enamespace = stream_.consumeToken(TokenType::Identifier).value();
    }
    else {
        enamespace = globalNamespace;
    }

    auto &typeName = stream_.consumeToken(TokenType::Identifier);
    return TypeIdentifier(typeName.value(), enamespace, typeName);
}

Type AbstractParser::parseType(const TypeContext &typeContext, TypeDynamism dynamism) {
    if (stream_.nextTokenIs(E_MEDIUM_BLACK_CIRCLE)) {
        throw CompilerError(stream_.consumeToken().position(), "⚫️ not allowed here.");
    }
    if (stream_.nextTokenIs(E_WHITE_SQUARE_BUTTON)) {
        auto &token = stream_.consumeToken();
        Type type = parseType(typeContext, dynamism);
        if (!type.allowsMetaType() || type.meta()) {
            throw CompilerError(token.position(), "Meta type of %s is restricted.",
                                type.toString(typeContext, true).c_str());
        }
        type.setMeta(true);
        return type;
    }

    bool optional = false;
    if (stream_.consumeTokenIf(E_CANDY)) {
        optional = true;
    }

    if ((dynamism & TypeDynamism::GenericTypeVariables) != TypeDynamism::None &&
        (typeContext.calleeType().canHaveGenericArguments() || typeContext.function() != nullptr) &&
        stream_.nextTokenIs(TokenType::Variable)) {

        auto &variableToken = stream_.consumeToken(TokenType::Variable);

        if (typeContext.function() != nullptr) {
            auto it = typeContext.function()->genericArgumentVariables.find(variableToken.value());
            if (it != typeContext.function()->genericArgumentVariables.end()) {
                Type type = it->second;
                if (optional) {
                    type.setOptional();
                }
                return type;
            }
        }
        if (typeContext.calleeType().canHaveGenericArguments()) {
            Type type = Type::nothingness();
            if (typeContext.calleeType().typeDefinition()->fetchVariable(variableToken.value(), optional, &type)) {
                return type;
            }
        }

        throw CompilerError(variableToken.position(), "No such generic type variable \"%s\".",
                            variableToken.value().utf8().c_str());
    }
    else if (stream_.nextTokenIs(E_DOG)) {
        auto &selfToken = stream_.consumeToken(TokenType::Identifier);
        if ((dynamism & TypeDynamism::Self) == TypeDynamism::None) {
            throw CompilerError(selfToken.position(), "🐕 not allowed here.");
        }
        return Type::self(optional);
    }
    else if (stream_.nextTokenIs(E_BENTO_BOX)) {
        auto &bentoToken = stream_.consumeToken(TokenType::Identifier);
        Type type = Type(TypeContent::MultiProtocol, optional);
        while (stream_.nextTokenIsEverythingBut(E_BENTO_BOX)) {
            auto protocolType = parseType(typeContext, dynamism);
            if (protocolType.type() != TypeContent::Protocol || protocolType.optional() || protocolType.meta()) {
                throw CompilerError(bentoToken.position(), "🍱 may only consist of non-optional protocol types.");
            }
            type.genericArguments_.push_back(protocolType);
        }
        if (type.protocols().empty()) {
            throw CompilerError(bentoToken.position(), "An empty 🍱 is invalid.");
        }
        type.sortMultiProtocolType();
        stream_.consumeToken(TokenType::Identifier);
        return type;
    }
    else if (stream_.nextTokenIs(E_POLICE_CARS_LIGHT)) {
        auto &token = stream_.consumeToken(TokenType::Identifier);
        Type errorType = parseErrorEnumType(typeContext, dynamism, token.position());
        if (optional) {
            throw CompilerError(token.position(), "The error type itself cannot be an optional. "
                                "Maybe you meant to make the contained type an optional?");
        }
        Type type = Type::error();
        type.genericArguments_.emplace_back(errorType);
        type.genericArguments_.emplace_back(parseType(typeContext, dynamism));
        return type;
    }
    else if (stream_.consumeTokenIf(E_GRAPES)) {
        Type t = Type::callableIncomplete(optional);
        t.genericArguments_.push_back(Type::nothingness());

        while (stream_.nextTokenIsEverythingBut(E_WATERMELON) &&
               stream_.nextTokenIsEverythingBut(E_RIGHTWARDS_ARROW, TokenType::Operator)) {
            t.genericArguments_.push_back(parseType(typeContext, dynamism));
        }

        if (stream_.consumeTokenIf(E_RIGHTWARDS_ARROW, TokenType::Operator)) {
            t.genericArguments_[0] = parseType(typeContext, dynamism);
        }

        stream_.requireIdentifier(E_WATERMELON);
        return t;
    }
    else {
        auto parsedType = parseTypeIdentifier();

        auto type = Type::nothingness();
        if (!package_->fetchRawType(parsedType, optional, &type)) {
            throw CompilerError(parsedType.token.position(), "Could not find type %s in enamespace %s.",
                                parsedType.name.utf8().c_str(), parsedType.ns.utf8().c_str());
        }

        parseGenericArgumentsForType(&type, typeContext, dynamism, parsedType.token.position());
        return type;
    }
}

Type AbstractParser::parseErrorEnumType(const TypeContext &typeContext, TypeDynamism dynamism, const SourcePosition &p) {
    auto errorType = parseType(typeContext, dynamism);
    if (errorType.type() != TypeContent::Enum || errorType.optional() || errorType.meta()) {
        throw CompilerError(p, "Error type must be a non-optional 🦃.");
    }
    return errorType;
}

void AbstractParser::parseGenericArgumentsForType(Type *type, const TypeContext &typeContext, TypeDynamism dynamism,
                                                  const SourcePosition &p) {
    if (type->canHaveGenericArguments()) {
        auto typeDef = type->typeDefinition();
        auto offset = typeDef->superGenericArguments().size();
        type->genericArguments_ = typeDef->superGenericArguments();

        if (!typeDef->ownGenericArgumentVariables().empty()) {
            size_t count = 0;
            while (stream_.nextTokenIs(E_SPIRAL_SHELL)) {
                auto &token = stream_.consumeToken(TokenType::Identifier);

                Type ta = parseType(typeContext, dynamism);

                auto i = count + offset;
                if (typeDef->numberOfGenericArgumentsWithSuperArguments() <= i) {
                    break;  // and throw an error below
                }
                if (!ta.compatibleTo(typeDef->genericArgumentConstraints()[i], typeContext)) {
                    auto thisName = typeDef->genericArgumentConstraints()[i].toString(typeContext, true);
                    auto thatName = ta.toString(typeContext, true);
                    throw CompilerError(token.position(), "Generic argument for %s is not compatible to constraint %s.",
                                        thatName.c_str(), thisName.c_str());
                }

                type->genericArguments_.push_back(ta);

                count++;
            }

            if (count != typeDef->ownGenericArgumentVariables().size()) {
                auto str = type->toString(Type::nothingness(), true);
                throw CompilerError(p, "Type %s requires %d generic arguments, but %d were given.", str.c_str(),
                                    typeDef->ownGenericArgumentVariables().size(), count);
            }
        }
    }
}

void AbstractParser::parseArgumentList(Function *function, const TypeContext &typeContext, bool initializer) {
    bool argumentToVariable;

    while ((argumentToVariable = stream_.nextTokenIs(E_BABY_BOTTLE)) || stream_.nextTokenIs(TokenType::Variable)) {
        if (argumentToVariable) {
            auto &token = stream_.consumeToken(TokenType::Identifier);
            if (!initializer) {
                throw CompilerError(token.position(), "🍼 can only be used with initializers.");
            }
        }

        auto &variableToken = stream_.consumeToken(TokenType::Variable);
        auto type = parseType(typeContext, TypeDynamism::GenericTypeVariables);

        function->arguments.emplace_back(variableToken.value(), type);

        if (argumentToVariable) {
            static_cast<Initializer *>(function)->addArgumentToVariable(variableToken.value(), variableToken.position());
        }
    }

    if (function->arguments.size() > UINT8_MAX) {
        throw CompilerError(function->position(), "A function cannot take more than 255 arguments.");
    }
}

void AbstractParser::parseReturnType(Function *function, const TypeContext &typeContext) {
    if (stream_.consumeTokenIf(E_RIGHTWARDS_ARROW, TokenType::Operator)) {
        function->returnType = parseType(typeContext, TypeDynamism::GenericTypeVariables);
    }
}

void AbstractParser::parseGenericArgumentsInDefinition(Function *function, const TypeContext &ct) {
    while (stream_.consumeTokenIf(E_SPIRAL_SHELL)) {
        auto &variable = stream_.consumeToken(TokenType::Variable);

        Type t = parseType(TypeContext(function->owningType(), function), TypeDynamism::GenericTypeVariables);
        function->genericArgumentConstraints.push_back(t);

        Type rType = Type(TypeContent::LocalGenericVariable, false,
                          static_cast<int>(function->genericArgumentVariables.size()), function);

        if (function->genericArgumentVariables.count(variable.value()) > 0) {
            throw CompilerError(variable.position(),
                                "A generic argument variable with the same name is already in use.");
        }
        function->genericArgumentVariables.insert(std::map<EmojicodeString, Type>::value_type(variable.value(), rType));
    }
}

void AbstractParser::parseBody(Function *function, bool allowNative) {
    if (stream_.nextTokenIs(E_RADIO)) {
        auto &radio = stream_.consumeToken(TokenType::Identifier);
        if (!allowNative) {
            throw CompilerError(radio.position(), "Native code is not allowed in this context.");
        }
        auto &indexToken = stream_.consumeToken(TokenType::Integer);
        auto index = std::stoll(indexToken.value().utf8(), nullptr, 0);
        if (index < 1 || index > UINT16_MAX) {
            throw CompilerError(indexToken.position(), "Linking Table Index is not in range.");
        }
        function->setLinkingTableIndex(static_cast<int>(index));
    }
    else {
        stream_.requireIdentifier(E_GRAPES);
        parseBodyBlock(function);
    }
}

void AbstractParser::parseBodyBlock(Function *function) {
    function->setTokenStream(stream_);

    int depth = 0;
    while (true) {
        auto &token = stream_.consumeToken();
        if (token.isIdentifier(E_GRAPES)) {
            depth++;
        }
        else if (token.isIdentifier(E_WATERMELON)) {
            if (depth == 0) {
                break;
            }
            depth--;
        }
    }
}

}  // namespace EmojicodeCompiler
