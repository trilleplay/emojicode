//
//  FnCodeGenerator.cpp
//  Emojicode
//
//  Created by Theo Weidmann on 29/07/2017.
//  Copyright © 2017 Theo Weidmann. All rights reserved.
//

#include "StringPool.hpp"
#include "../Types/Enum.hpp"
#include "FnCodeGenerator.hpp"
#include "../Types/Protocol.hpp"
#include <cassert>
#include <cstdlib>

namespace EmojicodeCompiler {

void FnCodeGenerator::generate() {
    if (fn_->isNative()) {
        wr().writeInstruction({ INS_TRANSFER_CONTROL_TO_NATIVE, INS_RETURN });
        return;
    }
    if (fn_->functionType() == FunctionType::BoxingLayer) {
        return;
    }
    scoper_.pushScope();

    unsigned int i = 0;
    for (auto arg : fn_->arguments) {
        scoper_.declareVariable(i++, arg.type);
    }

    generateBlock(fn_->ast());
    fn_->setFullSize(scoper_.size());
    scoper_.popScope(wr().count());
}

void FnCodeGenerator::generateBlock(const std::shared_ptr<ASTNode> &node) {
    for (auto &node : node->nodes()) {
        generateNode(node);
    }
}

void FnCodeGenerator::writeInteger(long long value)  {
    if (std::llabs(value) > INT32_MAX) {
        wr().writeInstruction(INS_GET_64_INTEGER);

        wr().writeInstruction(value >> 32);
        wr().writeInstruction(static_cast<EmojicodeInstruction>(value));
        return;
    }

    wr().writeInstruction(INS_GET_32_INTEGER);
    value += INT32_MAX;
    wr().writeInstruction(static_cast<EmojicodeInstruction>(value));
}

EmojicodeInstruction FnCodeGenerator::generateArguments(const std::shared_ptr<ASTNode> &node, size_t offset) {
    size_t argumentsSize = 0;
    for (size_t i = offset; i < node->nodes().size(); i++) {
        generateNode(node->nodes()[i]);
        argumentsSize += node->nodes()[i]->expressionType().size();
    }
    return static_cast<EmojicodeInstruction>(argumentsSize);
}

void FnCodeGenerator::generateNode(const std::shared_ptr<ASTNode> &node) {
    switch (node->nodeType()) {
        case ASTNodeType::Block:
            generateBlock(node);
            return;
        case ASTNodeType::Expression:
            generateNode(node->nodes().front());
            return;
        case ASTNodeType::Scoped:
            scoper_.pushScope();
            generateBlock(node);
            scoper_.popScope(wr().count());
            return;

        case ASTNodeType::TrueLiteral:
            wr().writeInstruction(INS_GET_TRUE);
            break;
        case ASTNodeType::FalseLiteral:
            wr().writeInstruction(INS_GET_FALSE);
            break;
        case ASTNodeType::StringLiteral:
            wr().writeInstruction(INS_GET_STRING_POOL);
            wr().writeInstruction(StringPool::theStringPool().poolString(node->value()));
            break;
        case ASTNodeType::IntegerLiteral: {
            long long value = std::stoll(node->value().utf8(), nullptr, 0);
            writeInteger(value);
            break;
        }
        case ASTNodeType::DoubleLiteral: {
            wr().writeInstruction(INS_GET_DOUBLE);

            double d = std::stod(node->value().utf8());
            wr().writeDoubleCoin(d);
            break;
        }
        case ASTNodeType::SymbolLiteral:
            wr().writeInstruction(INS_GET_SYMBOL);
            wr().writeInstruction(node->value()[0]);
            break;
        case ASTNodeType::VariableDeclaration: {
            Type t = node->expressionType();
            scoper_.declareVariable(node->intValue(), t);
            break;
        }
        case ASTNodeType::FrozenDeclaration: {
            generateNode(node->nodes().front());

            auto &var = scoper_.declareVariable(node->intValue(), node->expressionType());
            copyToVariable(var.stackIndex, false, node->nodes().front()->expressionType());
            var.initialize(wr().count());
            break;
        }
        case ASTNodeType::VariableAssignment: {
            generateNode(node->nodes().front());
            auto &var = scoper_.getVariable(node->intValue());
            copyToVariable(var.stackIndex, false, node->nodes().front()->expressionType());
            var.initialize(wr().count());
            break;
        }
        case ASTNodeType::VariableAssignmentInstance: {
            generateNode(node->nodes().front());
            auto &var = instanceScoper_->getVariable(node->intValue());
            copyToVariable(var.stackIndex, true, node->nodes().front()->expressionType());
            break;
        }
        case ASTNodeType::VariableDeclarationAssignment: {
            generateNode(node->nodes().front());
            auto &var = scoper_.declareVariable(node->intValue(), node->nodes().front()->expressionType());
            copyToVariable(var.stackIndex, false, node->nodes().front()->expressionType());
            break;
        }
        case ASTNodeType::Variable: {
            auto &var = scoper_.getVariable(node->intValue());
            if (var.type.size() == 1) {
                wr().writeInstruction(INS_PUSH_SINGLE_STACK);
                wr().writeInstruction(var.stackIndex);
            }
            else {
                wr().writeInstruction(INS_PUSH_WITH_SIZE_STACK);
                wr().writeInstruction(var.stackIndex);
                wr().writeInstruction(var.type.size());
            }
            break;
        }
        case ASTNodeType::VariableInstance: {
            auto &var = instanceScoper_->getVariable(node->intValue());
            if (var.type.size() == 1) {
                writeValueTypeOrObject(INS_PUSH_SINGLE_OBJECT, INS_PUSH_SINGLE_VT);
                wr().writeInstruction(var.stackIndex);
            }
            else {
                writeValueTypeOrObject(INS_PUSH_SINGLE_OBJECT, INS_PUSH_SINGLE_VT);
                wr().writeInstruction(var.stackIndex);
                wr().writeInstruction(var.type.size());
            }
            break;
        }
        case ASTNodeType::VariableReference: {
            CGScoper::Variable &var = scoper_.getVariable(node->intValue());
            wr().writeInstruction(INS_PUSH_VT_REFERENCE_STACK);
            wr().writeInstruction(var.stackIndex);
            break;
        }
        case ASTNodeType::VariableReferenceInstance: {
            auto &var = instanceScoper_->getVariable(node->intValue());
            writeValueTypeOrObject(INS_PUSH_VT_REFERENCE_OBJECT, INS_PUSH_VT_REFERENCE_VT);
            wr().writeInstruction(var.stackIndex);
            break;
        }
        case ASTNodeType::Nothingness: {
            wr().writeInstruction(INS_GET_NOTHINGNESS);
            break;
        }
        case ASTNodeType::ThisType:
        case ASTNodeType::This: {
            wr().writeInstruction(INS_THIS);
            break;
        }
        case ASTNodeType::EnumIntialization: {
            writeInteger(node->expressionType().eenum()->getValueFor(node->value()).second);
            break;
        }
        case ASTNodeType::MetaTypeInstance: {
            wr().writeInstruction(INS_GET_CLASS_FROM_INDEX);
            wr().writeInstruction(node->expressionType().eclass()->index);
            break;
        }
        case ASTNodeType::MetaTypeInstanceFromInstance: {
            wr().writeInstruction(INS_GET_CLASS_FROM_INSTANCE);
            break;
        }
        case ASTNodeType::SimpleToSimpleOptional: {
            wr().writeInstruction(INS_SIMPLE_OPTIONAL_PRODUCE);
            generateNode(node->nodes().front());
            break;
        }
        case ASTNodeType::SimpleToBox: {
            auto rtype = node->nodes().front()->expressionType();
            if (rtype.remotelyStored()) {
                generateNode(node->nodes().front());
                wr().writeInstruction({ INS_BOX_PRODUCE_REMOTE,
                    static_cast<EmojicodeInstruction>(rtype.size()),
                    static_cast<EmojicodeInstruction>(rtype.boxIdentifier()) });
            }
            else {
                wr().writeInstruction({ INS_BOX_PRODUCE,
                    static_cast<EmojicodeInstruction>(rtype.boxIdentifier()) });
                generateNode(node->nodes().front());
                wr().writeInstruction(INS_PUSH_N);
                wr().writeInstruction(kBoxValueSize - rtype.size() - 1);
            }
            break;
        }
        case ASTNodeType::BoxToSimple: {
            generateNode(node->nodes().front());
            auto rtype = node->nodes().front()->expressionType();
            if (rtype.remotelyStored()) {
                wr().writeInstruction({ INS_UNBOX_REMOTE, static_cast<EmojicodeInstruction>(rtype.size()) });
            }
            else {
                wr().writeInstruction({ INS_UNBOX, static_cast<EmojicodeInstruction>(rtype.size()) });
            }
            break;
        }
        case ASTNodeType::SimpleOptionalToBox: {
            generateNode(node->nodes().front());
            auto rtype = node->nodes().front()->expressionType();
            if (rtype.remotelyStored()) {
                wr().writeInstruction({ INS_SIMPLE_OPTIONAL_TO_BOX_REMOTE,
                    static_cast<EmojicodeInstruction>(rtype.boxIdentifier()),
                    static_cast<EmojicodeInstruction>(rtype.size() - 1) });
            }
            else {
                wr().writeInstruction({ INS_SIMPLE_OPTIONAL_TO_BOX,
                    static_cast<EmojicodeInstruction>(rtype.boxIdentifier()),
                static_cast<EmojicodeInstruction>(rtype.size() - 1) });
            }
            break;
        }
        case ASTNodeType::BoxToSimpleOptional: {
            generateNode(node->nodes().front());
            auto rtype = node->nodes().front()->expressionType();
            rtype.unbox();
            if (rtype.remotelyStored()) {
                wr().writeInstruction({ INS_BOX_TO_SIMPLE_OPTIONAL_PRODUCE_REMOTE,
                    static_cast<EmojicodeInstruction>(rtype.size()) });
            }
            else {
                wr().writeInstruction({ INS_BOX_TO_SIMPLE_OPTIONAL_PRODUCE,
                    static_cast<EmojicodeInstruction>(rtype.size()) });
            }
            break;
        }
        case ASTNodeType::StoreTemporarily: {
            auto rtype = node->nodes().front()->expressionType();
            auto &variable = scoper_.declareVariable(node->intValue(), rtype);
            generateNode(node->nodes().front());
            copyToVariable(variable.stackIndex, false, rtype);
            variable.initialize(wr().count());
            wr().writeInstruction({ INS_PUSH_VT_REFERENCE_STACK, variable.stackIndex });
            break;
        }
        case ASTNodeType::Dereference: {
            generateNode(node->nodes().front());
            wr().writeInstruction(INS_PUSH_VALUE_FROM_REFERENCE);
            wr().writeInstruction(node->expressionType().size());
            break;
        }
        case ASTNodeType::IsSameObject: {
            generateNode(node->nodes().front());
            generateNode(node->nodes().back());
            wr().writeInstruction(INS_SAME_OBJECT);
            break;
        }
        case ASTNodeType::IsNothingness:
            generateUnaryOperator(INS_IS_NOTHINGNESS, node);
            break;
        case ASTNodeType::IsError:
            generateUnaryOperator(INS_IS_ERROR, node);
            break;
        case ASTNodeType::Unwrap: {
            generateNode(node->nodes().front());
            auto type = node->nodes().front()->expressionType();
            if (type.storageType() == StorageType::Box) {
                wr().writeInstruction(INS_UNWRAP_BOX_OPTIONAL);
            }
            else {
                wr().writeInstruction(INS_UNWRAP_SIMPLE_OPTIONAL);
                wr().writeInstruction(type.size() - 1);
            }
            break;
        }
        case ASTNodeType::PerfectExtraction: {
            generateNode(node->nodes().front());
            auto type = node->nodes().front()->expressionType();
            if (type.storageType() == StorageType::Box) {
                wr().writeInstruction(INS_ERROR_CHECK_BOX_OPTIONAL);
            }
            else {
                wr().writeInstruction(INS_ERROR_CHECK_SIMPLE_OPTIONAL);
                wr().writeInstruction(type.size());
            }
            break;
        }
        case ASTNodeType::Error:
        case ASTNodeType::Return: {
            if (!node->nodes().empty()) {
                generateNode(node->nodes().front());
            }
            wr().writeInstruction(INS_RETURN);
            break;
        }
        case ASTNodeType::TypeFromExpression: {
            generateNode(node->nodes().front());
            break;
        }
        case ASTNodeType::StaticType: {
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            if (typeNode->type().type() == TypeContent::Class) {
                wr().writeInstruction(INS_GET_CLASS_FROM_INDEX);
                wr().writeInstruction(typeNode->type().eclass()->index);
                break;
            }
            assert(typeNode->availability() == TypeAvailability::StaticAndUnavailable);
            break;
        }
        case ASTNodeType::RepeatWhile: {
            wr().writeInstruction(INS_JUMP_FORWARD);
            auto placeholder = wr().writeInstructionsCountPlaceholderCoin();

            auto delta = wr().count();
            generateNode(node->nodes().back());
            placeholder.write();
            generateNode(node->nodes().front());

            wr().writeInstruction(INS_JUMP_BACKWARD_IF);
            wr().writeInstruction(wr().count() - delta + 1);
            break;
        }
        case ASTNodeType::IfStatement: {
            // This list will contain all placeholders that need to be replaced with a relative jump value
            // to jump past the whole if
            auto endPlaceholders = std::vector<std::pair<InstructionCount, FunctionWriterPlaceholder>>();

            std::experimental::optional<FunctionWriterCountPlaceholder> placeholder;
            for (auto it = node->nodes().begin(); it < node->nodes().end(); it++) {
                auto condNode = *it;
                if (condNode->nodeType() == ASTNodeType::Block) {
                    wr().writeInstruction(INS_JUMP_FORWARD);
                    auto elseCountPlaceholder = wr().writeInstructionsCountPlaceholderCoin();
                    placeholder->write();
                    scoper_.pushScope();
                    generateBlock(condNode);
                    scoper_.popScope(0);
                    elseCountPlaceholder.write();
                    assert(it + 1 == node->nodes().end());
                    placeholder = std::experimental::nullopt;
                    break;
                }
                if (placeholder) {
                    wr().writeInstruction(INS_JUMP_FORWARD);
                    auto endPlaceholder = wr().writeInstructionPlaceholder();
                    endPlaceholders.emplace_back(wr().count(), endPlaceholder);
                    placeholder->write();
                }

                scoper_.pushScope();
                generateNode(condNode);
                wr().writeInstruction(INS_JUMP_FORWARD_IF_NOT);
                placeholder = wr().writeInstructionsCountPlaceholderCoin();
                scoper_.pushScope();
                generateBlock(*(++it));
                scoper_.popScope(0);
            }

            if (placeholder) {
                placeholder->write();
            }

            for (auto endPlaceholder : endPlaceholders) {
                endPlaceholder.second.write(wr().count() - endPlaceholder.first);
            }
            break;
        }
        case ASTNodeType::ObjectMethodDispatch: {
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            auto argSize = generateArguments(typeNode, 1);
            generateNode(typeNode->nodes().front());
            wr().writeInstruction(INS_DISPATCH_METHOD);
            wr().writeInstruction(typeNode->type().eclass()->lookupMethod(typeNode->value())->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::ContextedFunctionDispatch: {
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            auto argSize = generateArguments(typeNode, 1);
            generateNode(typeNode->nodes().front());
            wr().writeInstruction(INS_CALL_CONTEXTED_FUNCTION);
            auto function = typeNode->type().typeDefinition()->lookupMethod(typeNode->value());
            wr().writeInstruction(function->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::ProtocolMethodDispatch: {
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            auto argSize = generateArguments(typeNode, 1);
            generateNode(typeNode->nodes().front());
            wr().writeInstruction(INS_DISPATCH_PROTOCOL);
            auto function = typeNode->type().protocol()->lookupMethod(typeNode->value());
            wr().writeInstruction(typeNode->type().protocol()->index);
            wr().writeInstruction(function->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::Initialization: {
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            auto argSize = generateArguments(typeNode, 1);
            generateNode(typeNode->nodes().front());
            wr().writeInstruction(INS_NEW_OBJECT);
            wr().writeInstruction(typeNode->type().eclass()->lookupInitializer(typeNode->value())->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::ValueTypeInitialization: {
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            auto argSize = generateArguments(typeNode, 1);
            generateNode(typeNode->nodes().front());  // Reference to where to store the instance
            wr().writeInstruction(INS_CALL_CONTEXTED_FUNCTION);
            wr().writeInstruction(typeNode->type().eclass()->lookupInitializer(typeNode->value())->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::MethodCapture: {
            generateNode(node->nodes().front());
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            wr().writeInstruction(INS_CAPTURE_METHOD);
            wr().writeInstruction(typeNode->type().eclass()->lookupMethod(node->value())->vtiForUse());
            break;
        }
        case ASTNodeType::TypeMethodCapture: {
            generateNode(node->nodes().front());
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            wr().writeInstruction(INS_CAPTURE_METHOD);
            wr().writeInstruction(typeNode->type().eclass()->lookupTypeMethod(node->value())->vtiForUse());
            break;
        }
        case ASTNodeType::SupermethodCall: {
            auto argSize = generateArguments(node, 0);
            wr().writeInstruction(INS_GET_CLASS_FROM_INDEX);
            auto superclass = std::dynamic_pointer_cast<ASTNodeWithArguments>(node)->type().eclass();
            wr().writeInstruction(superclass->index);
            wr().writeInstruction(INS_DISPATCH_SUPER);
            wr().writeInstruction(superclass->lookupMethod(node->value())->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::CallableCall: {
            auto argSize = generateArguments(node, 1);
            generateNode(node->nodes().front());
            wr().writeInstruction(INS_EXECUTE_CALLABLE);
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::TypeMethodCall: {
            auto argSize = generateArguments(node, 1);
            generateNode(node->nodes().front());
            wr().writeInstruction(INS_DISPATCH_TYPE_METHOD);
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            wr().writeInstruction(typeNode->type().eclass()->lookupTypeMethod(typeNode->value())->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::ValueTypeTypeMethodCall: {
            auto argSize = generateArguments(node, 1);
            wr().writeInstruction(INS_CALL_FUNCTION);
            auto typeNode = std::dynamic_pointer_cast<ASTNodeWithType>(node);
            wr().writeInstruction(typeNode->type().valueType()->lookupTypeMethod(typeNode->value())->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::ErrorCheckControl: {
            throw;
            // TODO: implement
        }
        case ASTNodeType::Superinitializer: {
            auto argSize = generateArguments(node, 0);
            wr().writeInstruction(INS_GET_CLASS_FROM_INDEX);
            auto superclass = std::dynamic_pointer_cast<ASTNodeWithArguments>(node)->type().eclass();
            wr().writeInstruction(superclass->index);
            wr().writeInstruction(INS_SUPER_INITIALIZER);
            wr().writeInstruction(superclass->lookupInitializer(node->value())->vtiForUse());
            wr().writeInstruction(argSize);
            break;
        }
        case ASTNodeType::EqualOperator:
            generateBinaryOperator(INS_EQUAL_PRIMITIVE, node);
            break;
        case ASTNodeType::DoubleAddOperator:
            generateBinaryOperator(INS_ADD_DOUBLE, node);
            break;
        case ASTNodeType::DoubleSubtractOperator:
            generateBinaryOperator(INS_SUBTRACT_DOUBLE, node);
            break;
        case ASTNodeType::DoubleMultiplyOperator:
            generateBinaryOperator(INS_MULTIPLY_DOUBLE, node);
            break;
        case ASTNodeType::DoubleDivideOperator:
            generateBinaryOperator(INS_DIVIDE_DOUBLE, node);
            break;
        case ASTNodeType::DoubleGreaterOperator:
            generateBinaryOperator(INS_GREATER_DOUBLE, node);
            break;
        case ASTNodeType::DoubleGreaterOrEqualOperator:
            generateBinaryOperator(INS_GREATER_OR_EQUAL_DOUBLE, node);
            break;
        case ASTNodeType::DoubleRemainderOperator:
            generateBinaryOperator(INS_REMAINDER_DOUBLE, node);
            break;
        case ASTNodeType::DoubleEqualOperator:
            generateBinaryOperator(INS_EQUAL_DOUBLE, node);
            break;
        case ASTNodeType::IntegerAddOperator:
            generateBinaryOperator(INS_ADD_INTEGER, node);
            break;
        case ASTNodeType::IntegerSubtractOperator:
            generateBinaryOperator(INS_SUBTRACT_INTEGER, node);
            break;
        case ASTNodeType::IntegerMultiplyOperator:
            generateBinaryOperator(INS_MULTIPLY_INTEGER, node);
            break;
        case ASTNodeType::IntegerDivideOperator:
            generateBinaryOperator(INS_DIVIDE_INTEGER, node);
            break;
        case ASTNodeType::IntegerRemainderOperator:
            generateBinaryOperator(INS_REMAINDER_INTEGER, node);
            break;
        case ASTNodeType::IntegerGreaterOperator:
            generateBinaryOperator(INS_GREATER_INTEGER, node);
            break;
        case ASTNodeType::IntegerGreaterOrEqualOperator:
            generateBinaryOperator(INS_GREATER_OR_EQUAL_INTEGER, node);
            break;
        case ASTNodeType::IntegerShiftLeftOperator:
            generateBinaryOperator(INS_SHIFT_LEFT_INTEGER, node);
            break;
        case ASTNodeType::IntegerShiftRightOperator:
            generateBinaryOperator(INS_SHIFT_RIGHT_INTEGER, node);
            break;
        case ASTNodeType::IntegerAndOperator:
            generateBinaryOperator(INS_BINARY_AND_INTEGER, node);
            break;
        case ASTNodeType::IntegerXorOperator:
            generateBinaryOperator(INS_BINARY_XOR_INTEGER, node);
            break;
        case ASTNodeType::IntegerOrOperator:
            generateBinaryOperator(INS_BINARY_OR_INTEGER, node);
            break;
        case ASTNodeType::IntegerNotOperator:
            generateUnaryOperator(INS_BINARY_NOT_INTEGER, node);
            break;
        case ASTNodeType::IntegerToDoubleConversion:
            generateUnaryOperator(INS_INT_TO_DOUBLE, node);
            break;
        case ASTNodeType::BooleanOr:
            generateBinaryOperator(INS_AND_BOOLEAN, node);
            break;
        case ASTNodeType::BooleanAnd:
            generateBinaryOperator(INS_OR_BOOLEAN, node);
            break;

        case ASTNodeType::ConcatenateLiteral:
        case ASTNodeType::MethodCall:
        case ASTNodeType::ForIn:
        case ASTNodeType::InferType:
        case ASTNodeType::DictionaryLiteral:
        case ASTNodeType::ListLiteral:
        case ASTNodeType::Operator:
        case ASTNodeType::ConditionalAssignment:
        case ASTNodeType::Cast:
            throw std::invalid_argument("Semantic-only node type. Was this AST semantically analysed and transformed?");
    }
}

}  // namespace EmojicodeCompiler
