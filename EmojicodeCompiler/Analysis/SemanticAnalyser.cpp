//
//  SemanticAnalyser.cpp
//  EmojicodeCompiler
//
//  Created by Theo Weidmann on 28/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#include "../Parsing/ASTNodeType.hpp"
#include "../Types/TypeDefinition.hpp"
#include "../Function.hpp"
#include "../Scoping/VariableNotFoundError.hpp"
#include "SemanticAnalyser.hpp"
#include "../Types/Enum.hpp"
#include "../Types/Protocol.hpp"
#include "OperatorHelper.hpp"
#include "../Types/CommonTypeFinder.hpp"
#include <memory>

namespace EmojicodeCompiler {

void SemanticAnalyser::analyse(std::shared_ptr<ASTNode> node) {
    Scope &methodScope = scoper_.pushArgumentsScope(function_->arguments, node->position());

    if (hasInstanceScope(function_->functionType())) {
        scoper_.instanceScope()->setVariableInitialization(!isFullyInitializedCheckRequired(function_->functionType()));
    }

    if (isFullyInitializedCheckRequired(function_->functionType())) {
        auto initializer = static_cast<Initializer *>(function_);
        for (auto &var : initializer->argumentsToVariables()) {
            if (!scoper_.instanceScope()->hasLocalVariable(var)) {
                throw CompilerError(initializer->position(),
                                    "🍼 was applied to \"%s\" but no matching instance variable was found.",
                                    var.utf8().c_str());
            }
            auto &instanceVariable = scoper_.instanceScope()->getLocalVariable(var);
            auto &argumentVariable = methodScope.getLocalVariable(var);
            if (!argumentVariable.type().compatibleTo(instanceVariable.type(), typeContext_)) {
                throw CompilerError(initializer->position(),
                                    "🍼 was applied to \"%s\" but instance variable has incompatible type.",
                                    var.utf8().c_str());
            }
            instanceVariable.initialize();
            auto assign = node->appendNode(ASTNodeType::VariableAssignmentInstance, initializer->position());
            assign->setIntValue(instanceVariable.id());
            auto varg = assign->appendNode(ASTNodeType::Variable, initializer->position());
            varg->setIntValue(argumentVariable.id());
            varg->setExpressionType(argumentVariable.type());
            box(argumentVariable.type(), TypeExpectation(instanceVariable.type()), varg);
        }
    }

    analyseBlock(node);

    analyseReturn(node);
    analyseInitializationRequirements();

    scoper_.popScope();
    function_->setVariableCount(scoper_.variableIdCount());
}

void SemanticAnalyser::analyseReturn(const std::shared_ptr<ASTNode> &node) {
    if (function_->functionType() == FunctionType::ObjectInitializer) {
        auto returnNode = node->appendNode(ASTNodeType::Return, node->position());
        returnNode->appendNode(ASTNodeType::This, node->position());
    }
    else if (function_->functionType() == FunctionType::ValueTypeInitializer) {
        auto returnNode = node->appendNode(ASTNodeType::Return, node->position());
    }
    else if (!pathAnalyser_.hasCertainly(PathAnalyserIncident::Returned)) {
        if (function_->returnType.type() != TypeContent::Nothingness) {
            throw CompilerError(function_->position(), "An explicit return is missing.");
        }
        else {
            node->appendNode(ASTNodeType::Return, node->position());
        }
    }
}

void SemanticAnalyser::analyseInitializationRequirements() {
    if (isFullyInitializedCheckRequired(function_->functionType())) {
        scoper_.instanceScope()->initializerUnintializedVariablesCheck(function_->position(),
                                                                       "Instance variable \"%s\" must be initialized.");
    }

    if (isSuperconstructorRequired(function_->functionType())) {
        if (typeContext_.calleeType().eclass()->superclass() != nullptr &&
            !pathAnalyser_.hasCertainly(PathAnalyserIncident::CalledSuperInitializer)) {
            if (pathAnalyser_.hasPotentially(PathAnalyserIncident::CalledSuperInitializer)) {
                throw CompilerError(function_->position(), "Superinitializer is potentially not called.");
            }
            else {
                throw CompilerError(function_->position(), "Superinitializer is not called.");
            }
        }
    }
}

void SemanticAnalyser::analyseBlock(std::shared_ptr<ASTNode> node) {
    for (auto statement : node->nodes()) {
        analyseStatement(statement);
        popTemporaryScope(statement);
    }
}

Type SemanticAnalyser::expectType(const Type &type, std::shared_ptr<ASTNode> node, std::vector<CommonTypeFinder> *ctargs) {
    auto returnType = ctargs ? analyseExpression(node) : expect(TypeExpectation(type), node);
    if (!returnType.compatibleTo(type, typeContext_, ctargs)) {
        auto cn = returnType.toString(typeContext_, true);
        auto tn = type.toString(typeContext_, true);
        throw CompilerError(node->position(), "%s is not compatible to %s.", cn.c_str(), tn.c_str());
    }
    return returnType;
}

void SemanticAnalyser::validateAccessLevel(Function *function, const SourcePosition &p) const {
    if (function->accessLevel() == AccessLevel::Private) {
        if (typeContext_.calleeType().type() != function->owningType().type()
            || function->owningType().typeDefinition() != typeContext_.calleeType().typeDefinition()) {
            throw CompilerError(p, "%s is 🔒.", function->name().utf8().c_str());
        }
    }
    else if (function->accessLevel() == AccessLevel::Protected) {
        if (typeContext_.calleeType().type() != function->owningType().type()
            || !this->typeContext_.calleeType().eclass()->inheritsFrom(function->owningType().eclass())) {
            throw CompilerError(p, "%s is 🔐.", function->name().utf8().c_str());
        }
    }
}

Type SemanticAnalyser::analyseFunctionCall(std::shared_ptr<ASTNodeWithArguments> node, const Type &type,
                                           size_t offset, Function *function) {
    if (node->nodes().size() - offset != function->arguments.size()) {
        throw CompilerError(node->position(), "%s expects %ld arguments but %ld were supplied.",
                            function->name().utf8().c_str(), function->arguments.size(), node->nodes().size() - offset);
    }

    TypeContext typeContext = TypeContext(type, function, &node->genericArguments());
    if (node->genericArguments().empty() && !function->genericArgumentVariables.empty()) {
        std::vector<CommonTypeFinder> genericArgsFinders(function->genericArgumentVariables.size(), CommonTypeFinder());

        for (auto arg : function->arguments) {
            expectType(arg.type.resolveOn(typeContext), node->nodes()[offset++], &genericArgsFinders);
        }
        for (auto finder : genericArgsFinders) {
            typeContext.functionGenericArguments()->emplace_back(finder.getCommonType(node->position()));
        }
    }
    else if (node->genericArguments().size() != function->genericArgumentVariables.size()) {
        throw CompilerError(node->position(), "Too few generic arguments provided.");
    }

    for (size_t i = 0; i < node->genericArguments().size(); i++) {
        if (!node->genericArguments()[i].compatibleTo(function->genericArgumentConstraints[i], typeContext)) {
            throw CompilerError(node->position(),
                                "Generic argument %d of type %s is not compatible to constraint %s.",
                                i + 1, node->genericArguments()[i].toString(typeContext, true).c_str(),
                                function->genericArgumentConstraints[i].toString(typeContext, true).c_str());
        }
    }

    for (auto arg : function->arguments) {
        expectType(arg.type.resolveOn(typeContext), node->nodes()[offset++]);
    }
    function->deprecatedWarning(node->position());
    validateAccessLevel(function, node->position());
    return function->returnType.resolveOn(typeContext);
}

Type SemanticAnalyser::box(Type exprType, const TypeExpectation &expectation, std::shared_ptr<ASTNode> node) {
    node->setExpressionType(exprType);
    if (exprType.type() == TypeContent::Nothingness) {
        return exprType;
    }
    if (!expectation.shouldPerformBoxing()) {
        return exprType;
    }

    // TODO: callable boxing

    switch (expectation.simplifyType(exprType)) {
        case StorageType::SimpleOptional:
            switch (exprType.storageType()) {
                case StorageType::SimpleOptional:
                    break;
                case StorageType::Box:
                    exprType.unbox();
                    exprType.setOptional();  // TODO: ERROR?!
                    node->cloneAndAppend();
                    node->setNodeType(ASTNodeType::BoxToSimpleOptional);
                    node->setExpressionType(exprType);
                    break;
                case StorageType::Simple:
                    if (expectation.type() != TypeContent::Error) {
                        exprType.setOptional();
                    }
                    else {
                        auto prty = exprType;
                        exprType = expectation;
                        exprType.setGenericArgument(1, prty);
                    }
                    node->cloneAndAppend();
                    node->setNodeType(ASTNodeType::SimpleToSimpleOptional);
                    node->setExpressionType(exprType);
                    break;
            }
            break;
        case StorageType::Box:
            switch (exprType.storageType()) {
                case StorageType::Box:
                    break;
                case StorageType::SimpleOptional:
                    node->cloneAndAppend();
                    node->setNodeType(ASTNodeType::SimpleOptionalToBox);
                    exprType.forceBox();
                    node->setExpressionType(exprType);
                    break;
                case StorageType::Simple:
                    node->cloneAndAppend();
                    node->setNodeType(ASTNodeType::SimpleToBox);
                    exprType.forceBox();
                    node->setExpressionType(exprType);
                    break;
            }
            break;
        case StorageType::Simple:
            switch (exprType.storageType()) {
                case StorageType::Simple:
                case StorageType::SimpleOptional:
                    break;
                case StorageType::Box:
                    exprType.unbox();
                    node->cloneAndAppend();
                    node->setNodeType(ASTNodeType::BoxToSimple);
                    node->setExpressionType(exprType);
                    break;
            }
            break;
    }

    if (!exprType.isReference() && expectation.isReference() && exprType.isReferencable()) {
        if (node->nodeType() == ASTNodeType::Variable) {
            node->setNodeType(ASTNodeType::VariableReference);
        }
        else if (node->nodeType() == ASTNodeType::VariableInstance) {
            node->setNodeType(ASTNodeType::VariableReferenceInstance);
        }
        else {
            node->cloneAndAppend();
            node->setNodeType(ASTNodeType::StoreTemporarily);
            scoper_.pushTemporaryScope();
            auto &var = scoper_.currentScope().declareInternalVariable(exprType, node->position());
            node->setIntValue(var.id());
        }
        exprType.setReference(true);
        node->setExpressionType(exprType);
    }
    else if (exprType.isReference() && !expectation.isReference()) {
        node->cloneAndAppend();
        node->setNodeType(ASTNodeType::Dereference);
        exprType.setReference(false);
        node->setExpressionType(exprType);
    }
    return exprType;
}

Type SemanticAnalyser::expect(const TypeExpectation &expectation, std::shared_ptr<ASTNode> node) {
    return box(analyseExpression(node, expectation), expectation, node);
}

Type SemanticAnalyser::analyseMethodCall(std::shared_ptr<ASTNodeWithArguments> node, const Type &type, size_t offset) {
    if (type.type() == TypeContent::MultiProtocol) {
        for (auto &protocol : type.protocols()) {
            Function *method;
            if ((method = protocol.protocol()->lookupMethod(node->value())) != nullptr) {
                node->setNodeType(ASTNodeType::ProtocolMethodDispatch);
                node->setType(protocol);
                return analyseFunctionCall(node, protocol, 1, method);
            }
        }
        throw CompilerError(node->position(), "No type in %s provides a method called %s.",
                            type.toString(typeContext_, true).c_str(), node->value().utf8().c_str());
    }

    auto method = type.typeDefinition()->getMethod(node->value(), type, typeContext_, node->position());
    node->setType(type);

    if (type.type() == TypeContent::ValueType) {
        if (method->mutating()) {
            if (!type.isMutable()) {
                throw CompilerError(node->position(), "%s was marked 🖍 but callee is not mutable.",
                                    method->name().utf8().c_str());
            }
            auto varNode = node->nodes().front();
            assert(varNode->nodeType() == ASTNodeType::VariableReference);
            scoper_.currentScope().getLocalVariable(node->value()).mutate(node->position());
        }
        node->setNodeType(ASTNodeType::ContextedFunctionDispatch);
    }
    else if (type.type() == TypeContent::Protocol) {
        node->setNodeType(ASTNodeType::ProtocolMethodDispatch);
    }
    else if (type.type() == TypeContent::Enum) {
        node->setNodeType(ASTNodeType::ContextedFunctionDispatch);
    }
    else if (type.type() == TypeContent::Class) {
        node->setNodeType(ASTNodeType::ObjectMethodDispatch);
    }
    else {
        auto typeString = type.toString(typeContext_, true);
        throw CompilerError(node->position(), "You cannot call methods on %s.", typeString.c_str());
    }
    return analyseFunctionCall(node, type, offset, method);
}

Type SemanticAnalyser::analyseExpression(std::shared_ptr<ASTNode> node, const TypeExpectation &expectation) {
    switch (node->nodeType()) {
        case ASTNodeType::StringLiteral:
            return Type(CL_STRING, false);
        case ASTNodeType::ListLiteral: {
            Type type = Type(CL_LIST, false);

            node->setNodeType(ASTNodeType::Scoped);
            auto nodes = node->nodes();
            node->clearNodes();

            auto variable = scoper_.currentScope().declareInternalVariable(type, node->position());

            auto varAsNode = node->appendNode(ASTNodeType::VariableDeclarationAssignment, node->position());
            varAsNode->setIntValue(variable.id());

            auto initNode = varAsNode->appendNode(ASTNodeWithArguments(ASTNodeType::Initialization, node->position()));
            initNode->setType(type);
            initNode->setExpressionType(type);
            initNode->appendNode(ASTNodeWithType(ASTNodeType::StaticType, node->position(), type));
            initNode->setValue(EmojicodeString(0x1F438));

            CommonTypeFinder finder;
            for (auto &valueNode : nodes) {
                auto mc = node->appendNode(ASTNodeWithArguments(ASTNodeType::ObjectMethodDispatch,
                                                                valueNode->position(), type));
                mc->setValue(EmojicodeString(0x1F43B));
                auto varNode = mc->appendNode(ASTNodeType::Variable, node->position());
                varNode->setExpressionType(type);
                varNode->setIntValue(variable.id());

                Type type = expect(TypeExpectation(false, true, false), valueNode);
                finder.addType(type, typeContext_);

                mc->appendNodeP(valueNode);
            }

            auto varNode = node->appendNode(ASTNodeType::Variable, node->position());
            varNode->setExpressionType(type);
            varNode->setIntValue(variable.id());

            type.setGenericArgument(0, finder.getCommonType(node->position()));
            return type;
        }
        case ASTNodeType::DictionaryLiteral:
            return Type(CL_DICTIONARY, false);  // TODO: infer and transform
        case ASTNodeType::IntegerLiteral:
            if (expectation.type() == TypeContent::ValueType && expectation.valueType() == VT_DOUBLE) {
                node->setNodeType(ASTNodeType::DoubleLiteral);
                return Type::doubl();
            }
            return Type::integer();
        case ASTNodeType::DoubleLiteral:
            return Type::doubl();
        case ASTNodeType::SymbolLiteral:
            return Type::symbol();
        case ASTNodeType::TrueLiteral:
        case ASTNodeType::FalseLiteral:
            return Type::boolean();
        case ASTNodeType::Variable: {
            auto var = scoper_.getVariable(node->value(), node->position());
            if (var.inInstanceScope) {
                node->setNodeType(ASTNodeType::VariableInstance);
            }
            node->setIntValue(var.variable.id());
            return var.variable.type();
        }
        case ASTNodeType::IsSameObject:
            expectType(Type::someobject(), node->nodes()[0]);
            expectType(Type::someobject(), node->nodes()[1]);
            return Type::boolean();
        case ASTNodeType::ConcatenateLiteral: {
            Type type = Type::nothingness();
            function_->package()->fetchRawType(EmojicodeString(0x1F520), globalNamespace, false, node->position(), &type);

            node->setNodeType(ASTNodeType::Scoped);
            auto nodes = node->nodes();
            node->clearNodes();

            auto variable = scoper_.currentScope().declareInternalVariable(type, node->position());

            auto varAsNode = node->appendNode(ASTNodeType::VariableDeclarationAssignment, node->position());
            varAsNode->setIntValue(variable.id());

            auto initNode = varAsNode->appendNode(ASTNodeWithArguments(ASTNodeType::Initialization, node->position()));
            initNode->setType(type);
            initNode->setExpressionType(type);
            initNode->appendNode(ASTNodeWithType(ASTNodeType::StaticType, node->position(), type));
            initNode->setValue(EmojicodeString(0x1F195));

            for (auto &stringNode : nodes) {
                auto mc = node->appendNode(ASTNodeWithArguments(ASTNodeType::ObjectMethodDispatch,
                                                                stringNode->position(), type));
                mc->setValue(EmojicodeString(0x1F43B));
                auto varNode = mc->appendNode(ASTNodeType::Variable, node->position());
                varNode->setExpressionType(type);
                varNode->setIntValue(variable.id());
                expectType(Type(CL_STRING, false), stringNode);
                mc->appendNodeP(stringNode);
            }

            auto getNode = node->appendNode(ASTNodeWithArguments(ASTNodeType::ObjectMethodDispatch,
                                                                 node->position(), type));
            getNode->setValue(EmojicodeString(0x1F521));
            auto varNode = getNode->appendNode(ASTNodeType::Variable, node->position());
            varNode->setExpressionType(type);
            varNode->setIntValue(variable.id());
            
            return Type(CL_STRING, false);
        }
        case ASTNodeType::Nothingness:
            return Type::nothingness();
        case ASTNodeType::IsNothingness: {
            Type type = expect(TypeExpectation(true, false), node->nodes().front());
            if (!type.optional() && type.type() != TypeContent::Something) {
                throw CompilerError(node->position(), "☁️ can only be used with optionals and ⚪️.");
            }
            return Type::boolean();
        }
        case ASTNodeType::IsError: {
            Type type = expect(TypeExpectation(true, false), node->nodes().front());
            if (type.type() != TypeContent::Error) {
                throw CompilerError(node->position(), "🚥 can only be used with 🚨.");
            }
            return Type::boolean();
        }
        case ASTNodeType::Unwrap: {
            Type t = expect(TypeExpectation(true, false), node->nodes().front());

            if (!t.optional()) {
                throw CompilerError(node->position(), "🍺 can only be used with optionals.");
            }

            assert(t.isReference());
            t.setReference(false);
            t.setOptional(false);
            return t;
        }
        case ASTNodeType::PerfectExtraction: {
            Type t = expect(TypeExpectation(true, false), node->nodes().front());

            if (t.type() != TypeContent::Error) {
                throw CompilerError(node->position(), "🚇 can only be used with 🚨.");
            }

            assert(t.isReference());
            t.setReference(false);
            
            return t.genericArguments()[1];
        }
        case ASTNodeType::MetaTypeInstance: {
            Type originalType = std::dynamic_pointer_cast<ASTNodeWithType>(node)->type();
            validateMetability(originalType, node->position());
            originalType.setMeta(true);
            return originalType;
        }
        case ASTNodeType::MetaTypeInstanceFromInstance: {
            Type originalType = expect(TypeExpectation(false, false, false), node->nodes().front());
            validateMetability(originalType, node->position());
            originalType.setMeta(true);
            return originalType;
        }
        case ASTNodeType::This:
            if (isSuperconstructorRequired(function_->functionType()) &&
                !pathAnalyser_.hasCertainly(PathAnalyserIncident::CalledSuperInitializer) &&
                typeContext_.calleeType().eclass()->superclass() != nullptr) {
                throw CompilerError(node->position(), "Attempt to use 🐕 before superinitializer call.");
            }
            if (isFullyInitializedCheckRequired(function_->functionType())) {
                scoper_.instanceScope()->initializerUnintializedVariablesCheck(node->position(),
                                                                               "Instance variable \"%s\" must be "
                                                                               "initialized before the use of 🐕.");
            }

            if (!isSelfAllowed(function_->functionType())) {
                throw CompilerError(node->position(), "Illegal use of 🐕.");
            }
            pathAnalyser_.recordIncident(PathAnalyserIncident::UsedSelf);
            return typeContext_.calleeType();
        case ASTNodeType::CallableCall: {
            Type type = expect(TypeExpectation(false, false, false), node->nodes().front());
            if (type.type() != TypeContent::Callable) {
                throw CompilerError(node->position(), "Given value is not callable.");
            }
            for (size_t i = 1; i < type.genericArguments().size(); i++) {
                expectType(type.genericArguments()[i], node->nodes()[i]);
            }
            return type.genericArguments()[0];
        }
        case ASTNodeType::MethodCapture: {
            Type type = expect(TypeExpectation(false, false, false), node->nodes().front());

            validateMethodCapturability(type, node->position());
            auto function = type.typeDefinition()->getMethod(node->value(), type,
                                                                       typeContext_, node->position());
            function->deprecatedWarning(node->position());
            std::dynamic_pointer_cast<ASTNodeWithType>(node)->setType(type);
            return function->type();
        }
        case ASTNodeType::TypeMethodCapture: {
            auto [ type, typeNode ] = analyseTypeExpr(node->nodes().front(), expectation);
            validateMethodCapturability(type, node->position());
            auto function = type.typeDefinition()->getMethod(node->value(), type,
                                                                       typeContext_, node->position());
            function->deprecatedWarning(node->position());
            node->setExpressionType(type);
            std::dynamic_pointer_cast<ASTNodeWithType>(node)->setType(type);
            return function->type();
        }
        case ASTNodeType::SupermethodCall: {
            if (function_->functionType() != FunctionType::ObjectMethod) {
                throw CompilerError(node->position(), "Not within an object-context.");
            }

            Class *superclass = typeContext_.calleeType().eclass()->superclass();

            if (superclass == nullptr) {
                throw CompilerError(node->position(), "Class has no superclass.");
            }

            auto argumentsNode = std::dynamic_pointer_cast<ASTNodeWithArguments>(node);

            Function *method = superclass->getMethod(node->value(), Type(superclass, false),
                                                     typeContext_, node->position());
            argumentsNode->setType(Type(superclass, true));
            return analyseFunctionCall(argumentsNode, Type(superclass, true), 0, method);
        }
        case ASTNodeType::Cast: {
            auto [ type, typeNode ] = analyseTypeExpr(node->nodes()[1], expectation);

            Type originalType = analyseExpression(node->nodes().front());

            if (originalType.compatibleTo(type, typeContext_)) {
                throw CompilerError(node->position(), "Unnecessary cast.");
            }
            else if (!type.compatibleTo(originalType, typeContext_)) {
                auto typeString = type.toString(typeContext_, true);
                throw CompilerError(node->position(), "Cast to unrelated type %s will always fail.",
                                    typeString.c_str());
            }

            if (type.type() == TypeContent::Class) {
                if (!type.genericArguments().empty()) {
                    throw CompilerError(node->position(), "Class casts with generic arguments are not available.");
                }

                if (originalType.type() == TypeContent::Someobject || originalType.type() == TypeContent::Class) {
                    if (originalType.optional()) {
                        throw CompilerError(node->position(), "Downcast on classes with optionals not possible.");
                    }
                    assert(originalType.storageType() == StorageType::Simple && originalType.size() == 1);
                }
                else {
                    assert(originalType.storageType() == StorageType::Box);
                }
            }
            else if (type.type() == TypeContent::Protocol && isStatic(typeNode->availability())) {
                if (!type.genericArguments().empty()) {
                    throw CompilerError(node->position(), "Cannot cast to generic protocols.");
                }
                assert(originalType.storageType() == StorageType::Box);
            }
            else if ((type.type() == TypeContent::ValueType || type.type() == TypeContent::Enum)
                     && isStatic(typeNode->availability())) {
                assert(originalType.storageType() == StorageType::Box);
                type.forceBox();
            }
            else {
                auto typeString = type.toString(typeContext_, true);
                throw CompilerError(node->position(), "You cannot cast to %s.", typeString.c_str());
            }

            type.setOptional(true);
            return type;
        }
        case ASTNodeType::Initialization: {
            auto [ type, typeNode ] = analyseTypeExpr(node->nodes().front(), expectation);

            auto argumentsNode = std::dynamic_pointer_cast<ASTNodeWithArguments>(node);

            if (type.type() == TypeContent::Enum) {
                node->setNodeType(ASTNodeType::EnumIntialization);
                notStaticError(typeNode->availability(), node->position(), "Enums");

                auto v = type.eenum()->getValueFor(node->value());
                if (!v.first) {
                    throw CompilerError(node->position(), "%s does not have a member named %s.",
                                        type.eenum()->name().utf8().c_str(), node->value().utf8().c_str());
                }
                return type;
            }

            if (type.type() == TypeContent::ValueType) {
                notStaticError(typeNode->availability(), node->position(), "Value Types");
                argumentsNode->setNodeType(ASTNodeType::ValueTypeInitialization);
            }

            auto initializer = type.typeDefinition()->getInitializer(node->value(), type,
                                                                               typeContext_, node->position());
            analyseFunctionCall(argumentsNode, type, 1, initializer);
            argumentsNode->setType(type);
            return type;
        }
        case ASTNodeType::Operator: {
            Type otype = analyseExpression(node->nodes().front());

            if ((otype.type() != TypeContent::ValueType && otype.type() != TypeContent::Enum) ||
                !otype.valueType()->isPrimitive()) {
                Type type = box(otype, TypeExpectation(true, false), node->nodes().front())
                                .resolveOnSuperArgumentsAndConstraints(typeContext_);
                node->setNodeType(ASTNodeType::Expression);
                auto a = node->nodes().front();
                auto b = node->nodes().back();
                node->clearNodes();
                auto callNode = node->appendNode(ASTNodeWithArguments(ASTNodeType::MethodCall, node->position()));
                callNode->appendNodeP(a);
                callNode->appendNodeP(b);
                callNode->setValue(node->value());
                return analyseMethodCall(callNode, type, 1);
            }

            Type type = box(otype, TypeExpectation(false, false, false), node->nodes().front());

            expectType(type, node->nodes().back());
            return analysePrimitiveOperatorNode(type, node);
        }
        case ASTNodeType::MethodCall: {
            Type rtype = expect(TypeExpectation(true, false), node->nodes().front())
            .resolveOnSuperArgumentsAndConstraints(typeContext_);
            return analyseMethodCall(std::dynamic_pointer_cast<ASTNodeWithArguments>(node), rtype, 1);
        }
        case ASTNodeType::TypeMethodCall: {
            auto [ type, typeNode ] = analyseTypeExpr(node->nodes().front(), expectation);

            if (type.optional()) {
                compilerWarning(node->position(), "You cannot call optionals on 🍬.");
            }

            auto argumentsNode = std::dynamic_pointer_cast<ASTNodeWithArguments>(node);
            argumentsNode->setType(type);

            Function *method;
            if (type.type() == TypeContent::Class) {
                method = type.typeDefinition()->getTypeMethod(node->value(), type,
                                                                        typeContext_, node->position());
            }
            else if ((type.type() == TypeContent::ValueType || type.type() == TypeContent::Enum)
                     && isStatic(typeNode->availability())) {
                method = type.typeDefinition()->getTypeMethod(node->value(), type,
                                                                        typeContext_, node->position());
                argumentsNode->setNodeType(ASTNodeType::ValueTypeTypeMethodCall);
            }
            else {
                throw CompilerError(node->position(), "You can’t call type methods on %s.",
                                    type.toString(typeContext_, true).c_str());
            }
            return analyseFunctionCall(argumentsNode, type, 1, method);
        }
        case ASTNodeType::StaticType:
            return std::dynamic_pointer_cast<ASTNodeWithType>(node)->type();
        case ASTNodeType::TypeFromExpression: {
            auto type = std::dynamic_pointer_cast<ASTNodeWithType>(node)->type();
            if (!type.meta()) {
                throw CompilerError(node->position(), "Expected meta type.");
            }
            if (type.optional()) {
                throw CompilerError(node->position(), "🍬 can’t be used as meta type.");
            }
            type.setMeta(false);
            return type;
        }
        case ASTNodeType::ThisType: {
            return typeContext_.calleeType();
        }
        case ASTNodeType::InferType: {
            if (expectation.type() == TypeContent::StorageExpectation || expectation.type() == TypeContent::Nothingness) {
                throw CompilerError(node->position(), "Cannot infer ⚫️.");
            }
            node->setNodeType(ASTNodeType::StaticType);
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            typeNode->setAvailability(expectation.type() == TypeContent::Class ? TypeAvailability::StaticAndAvailabale
                                      : TypeAvailability::StaticAndAvailabale);
            auto type = expectation;
            type.setOptional(false);
            typeNode->setType(type);
            return type;
        }
        case ASTNodeType::ConditionalAssignment: {
            Type t = expect(TypeExpectation(false, false), node->nodes().front());
            if (!t.optional()) {
                throw CompilerError(node->position(), "Condition assignment can only be used with optionals.");
            }

            t.setReference(false);
            t.setOptional(false);

            auto &variable = scoper_.currentScope().declareVariable(node->value(), t, true, node->position());
            variable.initialize();
            
            return Type::boolean();
        }
        case ASTNodeType::IfStatement:
        case ASTNodeType::VariableDeclaration:
        case ASTNodeType::VariableAssignment:
        case ASTNodeType::RepeatWhile:
        case ASTNodeType::ForIn:
        case ASTNodeType::Superinitializer:
        case ASTNodeType::Return:
        case ASTNodeType::Error:
        case ASTNodeType::ErrorCheckControl:
        case ASTNodeType::FrozenDeclaration:
        case ASTNodeType::Expression:
        case ASTNodeType::Block:
        case ASTNodeType::VariableAssignmentInstance:
            throw std::invalid_argument("Malformed AST tree. Statement appears as expression.");
        GENERATOR_ONLY_NODE_TYPES_CASES
            throw std::invalid_argument("Malformed AST tree. Generator-only node appeared during analysis.");
    }
}

void SemanticAnalyser::analyseStatement(std::shared_ptr<ASTNode> node) {
    switch (node->nodeType()) {
        case ASTNodeType::Expression:
            analyseExpression(node->nodes().front());
            break;
        case ASTNodeType::VariableDeclaration: {
            auto type = std::dynamic_pointer_cast<ASTNodeWithType>(node)->type();
            auto &var = scoper_.currentScope().declareVariable(node->value(), type, false, node->position());
            node->setExpressionType(type);
            node->setIntValue(var.id());
            if (type.optional()) {
                node->cloneAndAppend();
                node->setNodeType(ASTNodeType::Block);
                auto assign = node->appendNode(ASTNodeType::VariableAssignment, node->position());
                assign->setIntValue(var.id());
                assign->appendNode(ASTNodeType::Nothingness, node->position());
                assign->setExpressionType(Type::nothingness());
            }
            break;
        }
        case ASTNodeType::VariableAssignment: {
            try {
                auto rvar = scoper_.getVariable(node->value(), node->position());
                if (rvar.inInstanceScope && !typeContext_.calleeType().isMutable() &&
                    !isFullyInitializedCheckRequired(function_->functionType())) {
                    throw CompilerError(node->position(),
                                        "Can’t mutate instance variable as method is not marked with 🖍.");
                }

                if (rvar.inInstanceScope) {
                    node->setNodeType(ASTNodeType::VariableAssignmentInstance);
                }

                node->setIntValue(rvar.variable.id());

                rvar.variable.initialize();
                rvar.variable.mutate(node->position());

                expectType(rvar.variable.type(), node->nodes().front());
                return;
            }
            catch (VariableNotFoundError &vne) {
                // Not declared, declaring as local variable
                node->setNodeType(ASTNodeType::VariableDeclarationAssignment);
                Type t = expect(TypeExpectation(false, true), node->nodes().front());
                popTemporaryScope(node->nodes().front());
                auto &var = scoper_.currentScope().declareVariable(node->value(), t, false, node->position());
                var.initialize();
                node->setIntValue(var.id());
                return;
            }
        }
        case ASTNodeType::FrozenDeclaration: {
            Type t = expect(TypeExpectation(false, false), node->nodes().front());
            popTemporaryScope(node->nodes().front());
            auto &var = scoper_.currentScope().declareVariable(node->value(), t, true, node->position());
            var.initialize();
            node->setIntValue(var.id());
            node->setExpressionType(t);
            return;
        }
        case ASTNodeType::Superinitializer: {
            if (!isSuperconstructorRequired(function_->functionType())) {
                throw CompilerError(node->position(), "🐐 can only be used inside initializers.");
            }
            if (typeContext_.calleeType().eclass()->superclass() == nullptr) {
                throw CompilerError(node->position(), "🐐 can only be used if the class inherits from another.");
            }
            if (pathAnalyser_.hasPotentially(PathAnalyserIncident::CalledSuperInitializer)) {
                throw CompilerError(node->position(), "Superinitializer might have already been called.");
            }

            scoper_.instanceScope()->initializerUnintializedVariablesCheck(node->position(),
                                                                           "Instance variable \"%s\" must be "
                                                                           "initialized before superinitializer.");

            auto argumentsNode = std::dynamic_pointer_cast<ASTNodeWithArguments>(node);

            Class *eclass = typeContext_.calleeType().eclass();
            auto initializer = eclass->superclass()->getInitializer(node->value(), Type(eclass, false),
                                                                    typeContext_, node->position());
            argumentsNode->setType(Type(eclass->superclass(), false));
            analyseFunctionCall(argumentsNode, Type(eclass, false), 0, initializer);

            pathAnalyser_.recordIncident(PathAnalyserIncident::CalledSuperInitializer);
            return;
        }
        case ASTNodeType::RepeatWhile: {
            pathAnalyser_.beginBranch();
            scoper_.pushScope();
            expectType(Type::boolean(), node->nodes().front());
            popTemporaryScope(node->nodes().front());
            analyseBlock(node->nodes()[1]);
            scoper_.popScope();
            pathAnalyser_.endBranch();
            pathAnalyser_.endUncertainBranches();
            return;
        }
        case ASTNodeType::IfStatement: {
            for (auto it = node->nodes().begin(); it < node->nodes().end(); it++) {
                auto condNode = *it;
                if (condNode->nodeType() == ASTNodeType::Block) {
                    pathAnalyser_.beginBranch();
                    scoper_.pushScope();
                    analyseBlock(condNode);
                    scoper_.popScope();
                    pathAnalyser_.beginBranch();
                    assert(it + 1 == node->nodes().end());
                    break;
                }
                pathAnalyser_.beginBranch();
                scoper_.pushScope();
                expectType(Type::boolean(), condNode);
                popTemporaryScope(condNode);
                analyseBlock(*(++it));
                scoper_.popScope();
                pathAnalyser_.beginBranch();
            }
            if (node->nodes().size() % 2 != 0) {
                pathAnalyser_.endUncertainBranches();
            }
            else {
                pathAnalyser_.endMutualExclusiveBranches();
            }
            return;
        }
        case ASTNodeType::Return: {
            pathAnalyser_.recordIncident(PathAnalyserIncident::Returned);
            if (function_->returnType.type() == TypeContent::Nothingness) {
                return;
            }

            if (isOnlyNothingnessReturnAllowed(function_->functionType())) {
                throw CompilerError(node->position(), "🍎 cannot be used inside an initializer.");
            }

            expectType(function_->returnType, node->nodes().front());
            return;

        }
        case ASTNodeType::ErrorCheckControl: {
            Type type = expect(TypeExpectation(false, false), node->nodes().front());

            if (type.type() != TypeContent::Error) {
                throw CompilerError(node->position(), "🥑 can only be used with 🚨.");
            }

            pathAnalyser_.beginBranch();
            scoper_.pushScope();
            scoper_.currentScope().declareVariable(node->value(), type.genericArguments()[1], true,
                                                    node->position()).initialize();
            analyseBlock(node->nodes()[1]);
            scoper_.popScope();
            pathAnalyser_.endBranch();

            pathAnalyser_.beginBranch();
            scoper_.pushScope();
            scoper_.currentScope().declareVariable(node->value(), type.genericArguments()[0], true,
                                                    node->position()).initialize();
            analyseBlock(node->nodes()[2]);
            scoper_.popScope();
            pathAnalyser_.endBranch();
            pathAnalyser_.endMutualExclusiveBranches();
            return;
        }
        case ASTNodeType::ForIn: {
            scoper_.pushScope();

            Type iteratee = expect(TypeExpectation(true, true, false), node->nodes().front());
            popTemporaryScope(node->nodes().front());

            Type itemType = Type::nothingness();
            if (!typeIsEnumerable(iteratee, &itemType)) {
                auto iterateeString = iteratee.toString(typeContext_, true);
                throw CompilerError(node->position(), "%s does not conform to s🔂.", iterateeString.c_str());
            }

            pathAnalyser_.beginBranch();
            scoper_.currentScope().declareVariable(node->value(), itemType, true,
                                                    node->position()).initialize();
            analyseBlock(node->nodes()[1]);
            scoper_.popScope();
            pathAnalyser_.endBranch();
            pathAnalyser_.endUncertainBranches();

            // TODO: Transform
            return;
        }
        case ASTNodeType::Error: {
            pathAnalyser_.recordIncident(PathAnalyserIncident::Returned);
            if (isOnlyNothingnessReturnAllowed(function_->functionType())) {
                auto *initializer = dynamic_cast<Initializer *>(function_);
                if (!initializer->errorProne()) {
                    throw CompilerError(node->position(), "Initializer is not declared error-prone.");
                }
                expectType(initializer->errorType(), node->nodes().front());
                return;
            }

            if (function_->returnType.type() != TypeContent::Error) {
                throw CompilerError(node->position(), "Function is not declared to return a 🚨.");
            }

            expectType(function_->returnType.genericArguments()[0], node->nodes().front());
            return;
        }
        case ASTNodeType::VariableAssignmentInstance:
            return;  // Ignore baby bottle initialization
        case ASTNodeType::Block:
            throw std::invalid_argument("Malformed AST tree. Block is not a statement.");
        GENERATOR_ONLY_NODE_TYPES_CASES
            throw std::invalid_argument("Malformed AST tree. Generator-only node appeared during analysis.");
        default:
            throw std::invalid_argument("Malformed AST tree. Expression appears as statement.");
    }
}

Type SemanticAnalyser::analysePrimitiveOperatorNode(const Type &valueType, const std::shared_ptr<ASTNode> &node) {
    if (valueType.valueType() == VT_DOUBLE) {
        switch (operatorType(node->value())) {
            case OperatorType::MultiplicationOperator:
                node->setNodeType(ASTNodeType::DoubleMultiplyOperator);
                return Type::doubl();
            case OperatorType::LessOperator:
                node->swapSubnodes();
            case OperatorType::GreaterOperator:
                node->setNodeType(ASTNodeType::DoubleGreaterOperator);
                return Type::boolean();
            case OperatorType::LessOrEqualOperator:
                node->swapSubnodes();
            case OperatorType::GreaterOrEqualOperator:
                node->setNodeType(ASTNodeType::DoubleGreaterOrEqualOperator);
                return Type::boolean();
            case OperatorType::DivisionOperator:
                node->setNodeType(ASTNodeType::DoubleDivideOperator);
                return Type::doubl();
            case OperatorType::PlusOperator:
                node->setNodeType(ASTNodeType::DoubleAddOperator);
                return Type::doubl();
            case OperatorType::MinusOperator:
                node->setNodeType(ASTNodeType::DoubleSubtractOperator);
                return Type::doubl();
            case OperatorType::RemainderOperator:
                node->setNodeType(ASTNodeType::DoubleRemainderOperator);
                return Type::doubl();
            case OperatorType::EqualOperator:
                node->setNodeType(ASTNodeType::DoubleEqualOperator);
                return Type::boolean();
            default:
                break;
        }
    }
    else if (valueType.valueType() == VT_INTEGER) {
        switch (operatorType(node->value())) {
            case OperatorType::MultiplicationOperator:
                node->setNodeType(ASTNodeType::IntegerMultiplyOperator);
                return Type::integer();
            case OperatorType::BitwiseAndOperator:
                node->setNodeType(ASTNodeType::IntegerAndOperator);
                return Type::integer();
            case OperatorType::BitwiseOrOperator:
                node->setNodeType(ASTNodeType::IntegerOrOperator);
                return Type::integer();
            case OperatorType::BitwiseXorOperator:
                node->setNodeType(ASTNodeType::IntegerXorOperator);
                return Type::integer();
            case OperatorType::LessOperator:
                node->swapSubnodes();
            case OperatorType::GreaterOperator:
                node->setNodeType(ASTNodeType::IntegerGreaterOperator);
                return Type::boolean();
            case OperatorType::LessOrEqualOperator:
                node->swapSubnodes();
            case OperatorType::GreaterOrEqualOperator:
                node->setNodeType(ASTNodeType::IntegerGreaterOrEqualOperator);
                return Type::boolean();
            case OperatorType::ShiftLeftOperator:
                node->setNodeType(ASTNodeType::IntegerShiftLeftOperator);
                return Type::integer();
            case OperatorType::ShiftRightOperator:
                node->setNodeType(ASTNodeType::IntegerShiftRightOperator);
                return Type::integer();
            case OperatorType::DivisionOperator:
                node->setNodeType(ASTNodeType::IntegerDivideOperator);
                return Type::integer();
            case OperatorType::PlusOperator:
                node->setNodeType(ASTNodeType::IntegerAddOperator);
                return Type::integer();
            case OperatorType::MinusOperator:
                node->setNodeType(ASTNodeType::IntegerSubtractOperator);
                return Type::integer();
            case OperatorType::RemainderOperator:
                node->setNodeType(ASTNodeType::IntegerRemainderOperator);
                return Type::integer();
            default:
                break;
        }
    }
    else if (valueType.valueType() == VT_BOOLEAN) {
        switch (operatorType(node->value())) {
            case OperatorType::LogicalAndOperator:
                node->setNodeType(ASTNodeType::BooleanAnd);
                return Type::boolean();
            case OperatorType::LogicalOrOperator:
                node->setNodeType(ASTNodeType::BooleanOr);
                return Type::boolean();
            default:
                break;
        }
    }

    if (operatorType(node->value()) == OperatorType::EqualOperator) {
        node->setNodeType(ASTNodeType::EqualOperator);
        return Type::boolean();
    }

    throw CompilerError(node->position(), "Type %s does not support operator.",
                        valueType.toString(typeContext_, true).c_str(), node->value().utf8().c_str());
}

bool SemanticAnalyser::typeIsEnumerable(const Type &type, Type *elementType) {
    if (type.type() == TypeContent::Class && !type.optional()) {
        for (Class *a = type.eclass(); a != nullptr; a = a->superclass()) {
            for (auto &protocol : a->protocols()) {
                if (protocol.protocol() == PR_ENUMERATEABLE) {
                    auto itemType = Type(TypeContent::GenericVariable, false, 0, PR_ENUMERATEABLE);
                    *elementType = itemType.resolveOn(protocol.resolveOn(type));
                    return true;
                }
            }
        }
    }
    else if (type.canHaveProtocol() && !type.optional()) {
        for (auto &protocol : type.typeDefinition()->protocols()) {
            if (protocol.protocol() == PR_ENUMERATEABLE) {
                auto itemType = Type(TypeContent::GenericVariable, false, 0, PR_ENUMERATEABLE);
                *elementType = itemType.resolveOn(protocol.resolveOn(type));
                return true;
            }
        }
    }
    else if (type.type() == TypeContent::Protocol && type.protocol() == PR_ENUMERATEABLE) {
        *elementType = Type(TypeContent::GenericVariable, false, 0, type.protocol()).resolveOn(type);
        return true;
    }
    return false;
}

}  // namespace Emojicode
