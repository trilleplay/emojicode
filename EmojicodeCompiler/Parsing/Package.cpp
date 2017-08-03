//
//  Package.cpp
//  Emojicode
//
//  Created by Theo Weidmann on 01/04/16.
//  Copyright © 2016 Theo Weidmann. All rights reserved.
//

#include "ASTNode.hpp"
#include "../Scoping/SemanticScoper.hpp"
#include "../Analysis/SemanticAnalyser.hpp"
#include "../CompilerError.hpp"
#include "../Generation/FnCodeGenerator.hpp"
#include "FunctionParser.hpp"
#include "Package.hpp"
#include "PackageParser.hpp"
#include "../Types/ValueType.hpp"
#include <algorithm>
#include <cstring>
#include <list>
#include <map>
#include <string>

// TODO: delete
#include <iomanip>
#include <iostream>

namespace EmojicodeCompiler {

inline const char* nodeName(ASTNodeType type)  {
    switch (type) {
        case ASTNodeType::StringLiteral: return "StringLiteral";
        case ASTNodeType::IntegerLiteral: return "IntegerLiteral";
        case ASTNodeType::DoubleLiteral: return "DoubleLiteral";
        case ASTNodeType::TrueLiteral: return "TrueLiteral";
        case ASTNodeType::FalseLiteral: return "FalseLiteral";
        case ASTNodeType::SymbolLiteral: return "SymbolLiteral";
        case ASTNodeType::ConcatenateLiteral: return "ConcatenateLiteral";
        case ASTNodeType::ListLiteral: return "ListLiteral";
        case ASTNodeType::DictionaryLiteral: return "DictionaryLiteral";
        case ASTNodeType::VariableDeclaration: return "VariableDeclaration";
        case ASTNodeType::FrozenDeclaration: return "FrozenDeclaration";
        case ASTNodeType::VariableDeclarationAssignment: return "VariableDeclarationAssignment";
        case ASTNodeType::VariableAssignment: return "VariableAssignment";
        case ASTNodeType::VariableAssignmentInstance: return "VariableAssignmentInstance";
        case ASTNodeType::Variable: return "Variable";
        case ASTNodeType::VariableInstance: return "VariableInstance";
        case ASTNodeType::ConditionalAssignment: return "ConditionalAssignment";
        case ASTNodeType::This: return "This";
        case ASTNodeType::Nothingness: return "Nothingness";
        case ASTNodeType::IsNothingness: return "IsNothingness";
        case ASTNodeType::IsError: return "IsError";
        case ASTNodeType::IsSameObject: return "IsSameObject";
        case ASTNodeType::MetaTypeInstance: return "MetaTypeInstance";
        case ASTNodeType::MetaTypeInstanceFromInstance: return "MetaTypeInstanceFromInstance";
        case ASTNodeType::Unwrap: return "Unwrap";
        case ASTNodeType::PerfectExtraction: return "PerfectExtraction";
        case ASTNodeType::MethodCapture: return "MethodCapture";
        case ASTNodeType::TypeMethodCapture: return "TypeMethodCapture";
        case ASTNodeType::CallableCall: return "CallableCall";
        case ASTNodeType::SupermethodCall: return "SupermethodCall";
        case ASTNodeType::Cast: return "Cast";
        case ASTNodeType::TypeFromExpression: return "TypeFromExpression";
        case ASTNodeType::StaticType: return "StaticType";
        case ASTNodeType::InferType: return "InferType";
        case ASTNodeType::ThisType: return "ThisType";
        case ASTNodeType::MethodCall: return "MethodCall";
        case ASTNodeType::TypeMethodCall: return "TypeMethodCall";
        case ASTNodeType::Initialization: return "Initialization";
        case ASTNodeType::Superinitializer: return "Superinitializer";
        case ASTNodeType::IfStatement: return "IfStatement";
        case ASTNodeType::ErrorCheckControl: return "ErrorCheckControl";
        case ASTNodeType::RepeatWhile: return "RepeatWhile";
        case ASTNodeType::ForIn: return "ForIn";
        case ASTNodeType::Return: return "Return";
        case ASTNodeType::Error: return "Error";
        case ASTNodeType::Block: return "Block";
        case ASTNodeType::Expression: return "Expression";
        case ASTNodeType::Scoped: return "Scoped";
        case ASTNodeType::BoxToSimpleOptional: return "BoxToSimpleOptional";
        case ASTNodeType::SimpleToSimpleOptional: return "SimpleToSimpleOptional";
        case ASTNodeType::SimpleOptionalToBox: return "SimpleOptionalToBox";
        case ASTNodeType::SimpleToBox: return "SimpleToBox";
        case ASTNodeType::BoxToSimple: return "BoxToSimple";
        case ASTNodeType::StoreTemporarily: return "StoreTemporarily";
        case ASTNodeType::Dereference: return "Dereference";
        case ASTNodeType::EnumIntialization: return "EnumIntialization";
        case ASTNodeType::VariableReferenceInstance: return "VariableReferenceInstance";
        case ASTNodeType::VariableReference: return "VariableReference";
        case ASTNodeType::ObjectMethodDispatch: return "ObjectMethodDispatch";
        case ASTNodeType::ContextedFunctionDispatch: return "ContextedFunctionDispatch";
        case ASTNodeType::ValueTypeTypeMethodCall: return "ValueTypeTypeMethodCall";
        case ASTNodeType::ProtocolMethodDispatch: return "ProtocolMethodDispatch";
        case ASTNodeType::ValueTypeInitialization: return "ValueTypeInitialization";
        case ASTNodeType::EqualOperator: return "EqualOperator";
        case ASTNodeType::Operator: return "Operator";
        case ASTNodeType::IntegerAddOperator: return "IntegerAddOperator";
        case ASTNodeType::IntegerSubtractOperator: return "IntegerSubtractOperator";
        case ASTNodeType::IntegerMultiplyOperator: return "IntegerMultiplyOperator";
        case ASTNodeType::IntegerDivideOperator: return "IntegerDivideOperator";
        case ASTNodeType::IntegerRemainderOperator: return "IntegerRemainderOperator";
        case ASTNodeType::IntegerAndOperator: return "IntegerAndOperator";
        case ASTNodeType::IntegerOrOperator: return "IntegerOrOperator";
        case ASTNodeType::IntegerXorOperator: return "IntegerXorOperator";
        case ASTNodeType::IntegerNotOperator: return "IntegerNotOperator";
        case ASTNodeType::IntegerShiftLeftOperator: return "IntegerShiftLeftOperator";
        case ASTNodeType::IntegerShiftRightOperator: return "IntegerShiftRightOperator";
        case ASTNodeType::IntegerGreaterOrEqualOperator: return "IntegerGreaterOrEqualOperator";
        case ASTNodeType::IntegerGreaterOperator: return "IntegerGreaterOperator";
        case ASTNodeType::IntegerToDoubleConversion: return "IntegerToDoubleConversion";
        case ASTNodeType::DoubleAddOperator: return "DoubleAddOperator";
        case ASTNodeType::DoubleSubtractOperator: return "DoubleSubtractOperator";
        case ASTNodeType::DoubleMultiplyOperator: return "DoubleMultiplyOperator";
        case ASTNodeType::DoubleDivideOperator: return "DoubleDivideOperator";
        case ASTNodeType::DoubleRemainderOperator: return "DoubleRemainderOperator";
        case ASTNodeType::DoubleGreaterOrEqualOperator: return "DoubleGreaterOrEqualOperator";
        case ASTNodeType::DoubleGreaterOperator: return "DoubleGreaterOperator";
        case ASTNodeType::DoubleEqualOperator: return "DoubleEqualOperator";
        case ASTNodeType::BooleanAnd: return "BooleanAnd";
        case ASTNodeType::BooleanOr: return "BooleanOr";
        case ASTNodeType::DownCast: return "DownCast";
        case ASTNodeType::CastToClass: return "CastToClass";
        case ASTNodeType::CastToProtocol: return "CastToProtocol";
        case ASTNodeType::CastToValueType: return "CastToValueType";
    }
}

void printTree(const std::shared_ptr<ASTNode> &node, int indent, const TypeContext &tc) {
    std::cout << std::setw(indent) << ' ' << "|\n";
    std::cout << std::setw(indent) << ' ' << "+----------------------------+\n";
    std::cout << std::setw(indent) << ' ' << "|" << std::setw(28) << nodeName(node->nodeType()) << "|\n";
    if (!node->value().empty()) {
        std::cout << std::setw(indent) << ' ' << "|" << std::setw(28) << node->value().utf8() << "|\n";
    }
    switch (node->nodeType()) {
        case ASTNodeType::VariableAssignment:
        case ASTNodeType::Variable:
        case ASTNodeType::VariableReference:
        case ASTNodeType::VariableDeclaration:
        case ASTNodeType::FrozenDeclaration:
        case ASTNodeType::VariableReferenceInstance:
        case ASTNodeType::VariableAssignmentInstance:
        case ASTNodeType::VariableInstance:
            std::cout << std::setw(indent) << ' ' << "|" << std::setw(28) << node->intValue() << "|\n";
        default:
            break;
    }
    auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
    if (typeNode != nullptr && typeNode->type().type() != TypeContent::Nothingness) {
        std::cout << std::setw(indent) << ' ' << "|" << std::setw(28) << typeNode->type().toString(tc, true) << "|\n";
    }
    std::cout << std::setw(indent) << ' ' << "+----------------------------+\n";
    for (auto &n : node->nodes()) {
        printTree(n, indent + 3, tc);
    }
}

Class* getStandardClass(const EmojicodeString &name, Package *_, const SourcePosition &errorPosition) {
    Type type = Type::nothingness();
    _->fetchRawType(name, globalNamespace, false, errorPosition, &type);
    if (type.type() != TypeContent::Class) {
        throw CompilerError(errorPosition, "s package class %s is missing.", name.utf8().c_str());
    }
    return type.eclass();
}

Protocol* getStandardProtocol(const EmojicodeString &name, Package *_, const SourcePosition &errorPosition) {
    Type type = Type::nothingness();
    _->fetchRawType(name, globalNamespace, false, errorPosition, &type);
    if (type.type() != TypeContent::Protocol) {
        throw CompilerError(errorPosition, "s package protocol %s is missing.", name.utf8().c_str());
    }
    return type.protocol();
}

ValueType* getStandardValueType(const EmojicodeString &name, Package *_, const SourcePosition &errorPosition,
                                unsigned int boxId) {
    Type type = Type::nothingness();
    _->fetchRawType(name, globalNamespace, false, errorPosition, &type);
    if (type.type() != TypeContent::ValueType) {
        throw CompilerError(errorPosition, "s package value type %s is missing.", name.utf8().c_str());
    }
    if (type.boxIdentifier() != boxId) {
        throw CompilerError(errorPosition, "s package value type %s has improper box id.", name.utf8().c_str());
    }
    return type.valueType();
}

void loadStandard(Package *s, const SourcePosition &errorPosition) {
    // Order of the following calls is important as they will cause Box IDs to be assigned
    VT_BOOLEAN = getStandardValueType(EmojicodeString(E_OK_HAND_SIGN), s, errorPosition, T_BOOLEAN);
    VT_INTEGER = getStandardValueType(EmojicodeString(E_STEAM_LOCOMOTIVE), s, errorPosition, T_INTEGER);
    VT_DOUBLE = getStandardValueType(EmojicodeString(E_ROCKET), s, errorPosition, T_DOUBLE);
    VT_SYMBOL = getStandardValueType(EmojicodeString(E_INPUT_SYMBOL_FOR_SYMBOLS), s, errorPosition, T_SYMBOL);

    CL_STRING = getStandardClass(EmojicodeString(0x1F521), s, errorPosition);
    CL_LIST = getStandardClass(EmojicodeString(0x1F368), s, errorPosition);
    CL_DATA = getStandardClass(EmojicodeString(0x1F4C7), s, errorPosition);
    CL_DICTIONARY = getStandardClass(EmojicodeString(0x1F36F), s, errorPosition);

    PR_ENUMERATOR = getStandardProtocol(EmojicodeString(0x1F361), s, errorPosition);
    PR_ENUMERATEABLE = getStandardProtocol(EmojicodeString(E_CLOCKWISE_RIGHTWARDS_AND_LEFTWARDS_OPEN_CIRCLE_ARROWS_WITH_CIRCLED_ONE_OVERLAY), s, errorPosition);
}

std::vector<Package *> Package::packagesLoadingOrder_;
std::map<std::string, Package *> Package::packages_;

Package* Package::loadPackage(const std::string &name, const EmojicodeString &ns, const SourcePosition &p) {
    Package *package = findPackage(name);

    if (package != nullptr) {
        if (!package->finishedLoading()) {
            throw CompilerError(p, "Circular dependency detected: %s (loaded first) and %s depend on each other.",
                                this->name().c_str(), name.c_str());
        }
    }
    else {
        auto path = packageDirectory + "/" + name + "/header.emojic";

        package = new Package(name, p);

        if (name != "s") {
            package->loadPackage("s", globalNamespace, p);
        }

        package->parse(path);
    }

    package->loadInto(this, ns, p);
    return package;
}

void Package::parse(const std::string &path) {
    packages_.emplace(name(), this);

    PackageParser(this, lex(path)).parse();

    if (!validVersion()) {
        throw CompilerError(SourcePosition(0, 0, path), "Package %s does not provide a valid version.",
                                     name().c_str());
    }

    if (name_ == "s") {
        loadStandard(this, position_);
        requiresNativeBinary_ = false;
    }

    packagesLoadingOrder_.push_back(this);

    for (auto vt : valueTypes_) {
        vt->prepareForSemanticAnalysis();
    }
    for (auto eclass : classes()) {
        eclass->prepareForSemanticAnalysis();
    }

    for (auto eclass : classes()) {
        for (auto function : eclass->methodList()) {
            parseFunction(function);
        }
        for (auto function : eclass->initializerList()) {
            parseFunction(function);
        }
        for (auto function : eclass->typeMethodList()) {
            parseFunction(function);
        }
    }

    for (auto function : functions()) {
        parseFunction(function);
    }

    finishedLoading_ = true;
}

void Package::parseFunction(Function *function) {
    if (function->isNative() || function->functionType() == FunctionType::BoxingLayer) {
        return;
    }

    auto type = function->owningType();
    if (type.type() == TypeContent::ValueType || type.type() == TypeContent::Enum) {
        type.setReference();
    }
    auto context = TypeContext(type, function);
    try {
        std::shared_ptr<ASTNode> ast = FunctionParser(function->package(), function->tokenStream(), context).parse();
        SemanticAnalyser(context, function).analyse(ast);
        function->setTokenStream(TokenStream());  // release tokens
        function->setAst(ast);
#ifdef DEBUG
        std::cout << function->name().utf8() << "\n";
        printTree(ast, 0, context);
#endif
    }
    catch (CompilerError &ce) {
        printError(ce);
    }
}

Package* Package::findPackage(const std::string &name) {
    auto it = packages_.find(name);
    return it != packages_.end() ? it->second : nullptr;
}

bool Package::fetchRawType(TypeIdentifier ptn, bool optional, Type *type) {
    return fetchRawType(ptn.name, ptn.ns, optional, ptn.token.position(), type);
}

bool Package::fetchRawType(const EmojicodeString &name, const EmojicodeString &ns, bool optional,
                           const SourcePosition &p, Type *type) {
    if (ns == globalNamespace && ns.size() == 1) {
        switch (name.front()) {
            case E_MEDIUM_WHITE_CIRCLE:
                if (optional) {
                    throw CompilerError(p, "🍬⚪️ is identical to ⚪️. Do not specify 🍬.");
                }
                *type = Type::something();
                return true;
            case E_LARGE_BLUE_CIRCLE:
                *type = Type::someobject(optional);
                return true;
            case E_SPARKLES:
                throw CompilerError(p, "The Nothingness type may not be referenced to.");
        }
    }

    EmojicodeString key = EmojicodeString(ns);
    key.append(name);
    auto it = types_.find(key);

    if (it != types_.end()) {
        auto xtype = it->second;
        if (optional) {
            xtype.setOptional();
        }
        *type = xtype;
        return true;
    }
    return false;
}

void Package::exportType(Type t, EmojicodeString name, const SourcePosition &p) {
    if (finishedLoading()) {
        throw std::logic_error("The package did already finish loading. No more types can be exported.");
    }
    if (std::any_of(exportedTypes_.begin(), exportedTypes_.end(), [&name](auto &type) { return type.name == name; })) {
        throw CompilerError(p, "A type named %s was already exported.", name.utf8().c_str());
    }
    exportedTypes_.emplace_back(t, name);
}

void Package::registerType(Type t, const EmojicodeString &name, const EmojicodeString &ns, bool exportFromPkg,
                           const SourcePosition &p) {
    EmojicodeString key = EmojicodeString(ns);
    key.append(name);
    types_.emplace(key, t);

    if (exportFromPkg) {
        exportType(t, name, p);
    }
}

void Package::loadInto(Package *destinationPackage, const EmojicodeString &ns, const SourcePosition &p) const {
    for (auto exported : exportedTypes_) {
        Type type = Type::nothingness();
        if (destinationPackage->fetchRawType(exported.name, ns, false, p, &type)) {
            throw CompilerError(p, "Package %s could not be loaded into namespace %s of package %s: %s collides with a type of the same name in the same namespace.", name().c_str(), ns.utf8().c_str(), destinationPackage->name().c_str(),
                                         exported.name.utf8().c_str());
        }

        destinationPackage->registerType(exported.type, exported.name, ns, false, p);
    }
}

}  // namespace EmojicodeCompiler
