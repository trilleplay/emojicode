//
//  Type.c
//  Emojicode
//
//  Created by Theo Weidmann on 04.03.15.
//  Copyright (c) 2015 Theo Weidmann. All rights reserved.
//

#include "Type.hpp"
#include "../../utf8.h"
#include "../Types/Class.hpp"
#include "../EmojicodeCompiler.hpp"
#include "../Types/Enum.hpp"
#include "../Function.hpp"
#include "../Types/Protocol.hpp"
#include "../Types/TypeContext.hpp"
#include "../Types/ValueType.hpp"
#include <algorithm>
#include <cstring>
#include <vector>

namespace EmojicodeCompiler {

Class *CL_STRING;
Class *CL_LIST;
Class *CL_DATA;
Class *CL_DICTIONARY;
Protocol *PR_ENUMERATEABLE;
Protocol *PR_ENUMERATOR;
ValueType *VT_BOOLEAN;
ValueType *VT_SYMBOL;
ValueType *VT_INTEGER;
ValueType *VT_DOUBLE;

Type::Type(Protocol *protocol, bool o) : typeContent_(TypeContent::Protocol), typeDefinition_(protocol), optional_(o) {
}

Type::Type(Enum *enumeration, bool o) : typeContent_(TypeContent::Enum), typeDefinition_(enumeration), optional_(o) {
}

Type::Type(ValueType *valueType, bool o)
: typeContent_(TypeContent::ValueType), typeDefinition_(valueType), optional_(o), mutable_(false) {
    for (size_t i = 0; i < valueType->numberOfGenericArgumentsWithSuperArguments(); i++) {
        genericArguments_.emplace_back(TypeContent::GenericVariable, false, i, valueType);
    }
}

Type::Type(Class *c, bool o) : typeContent_(TypeContent::Class), typeDefinition_(c), optional_(o) {
    for (size_t i = 0; i < c->numberOfGenericArgumentsWithSuperArguments(); i++) {
        genericArguments_.emplace_back(TypeContent::GenericVariable, false, i, c);
    }
}

Class* Type::eclass() const {
    return static_cast<Class *>(typeDefinition_);
}

Protocol* Type::protocol() const {
    return static_cast<Protocol *>(typeDefinition_);
}

Enum* Type::eenum() const {
    return static_cast<Enum *>(typeDefinition_);
}

ValueType* Type::valueType() const {
    return static_cast<ValueType *>(typeDefinition_);
}

TypeDefinition* Type::typeDefinition() const  {
    return typeDefinition_;
}

bool Type::canHaveGenericArguments() const {
    return type() == TypeContent::Class || type() == TypeContent::Protocol || type() == TypeContent::ValueType;
}

void Type::sortMultiProtocolType() {
    std::sort(genericArguments_.begin(), genericArguments_.end(), [](const Type &a, const Type &b) {
        return a.protocol()->index < b.protocol()->index;
    });
}

size_t Type::genericVariableIndex() const {
    if (type() != TypeContent::GenericVariable && type() != TypeContent::LocalGenericVariable) {
        throw std::domain_error("Tried to get reference from non-reference type");
    }
    return genericArgumentIndex_;
}

bool Type::allowsMetaType() const {
    return type() == TypeContent::Class || type() == TypeContent::Enum || type() == TypeContent::ValueType;
}

Type Type::resolveReferenceToBaseReferenceOnSuperArguments(const TypeContext &typeContext) const {
    TypeDefinition *c = typeContext.calleeType().typeDefinition();
    Type t = *this;

    auto maxReferenceForSuper = c->numberOfGenericArgumentsWithSuperArguments() - c->ownGenericArgumentVariables().size();
    // Try to resolve on the generic arguments to the superclass.
    while (t.type() == TypeContent::GenericVariable && c->canBeUsedToResolve(t.typeDefinition()) &&
           t.genericVariableIndex() < maxReferenceForSuper) {
        Type tn = c->superGenericArguments()[t.genericVariableIndex()];
        if (tn.type() == TypeContent::GenericVariable && tn.genericVariableIndex() == t.genericVariableIndex()
            && tn.typeDefinition() == t.typeDefinition()) {
            break;
        }
        t = tn;
    }
    return t;
}

Type Type::resolveOnSuperArgumentsAndConstraints(const TypeContext &typeContext, bool resolveSelf) const {
    if (typeContext.calleeType().type() == TypeContent::Nothingness) {
        return *this;
    }
    TypeDefinition *c = typeContext.calleeType().typeDefinition();
    Type t = *this;
    if (type() == TypeContent::Nothingness) {
        return t;
    }
    bool optional = t.optional();
    bool box = t.storageType() == StorageType::Box;

    if (resolveSelf && t.type() == TypeContent::Self) {
        t = typeContext.calleeType();
    }

    auto maxReferenceForSuper = c->numberOfGenericArgumentsWithSuperArguments() - c->ownGenericArgumentVariables().size();
    // Try to resolve on the generic arguments to the superclass.
    while (t.type() == TypeContent::GenericVariable && t.genericVariableIndex() < maxReferenceForSuper) {
        t = c->superGenericArguments()[t.genericVariableIndex()];
    }
    while (t.type() == TypeContent::LocalGenericVariable && typeContext.function() == t.localResolutionConstraint_) {
        t = typeContext.function()->genericArgumentConstraints[t.genericVariableIndex()];
    }
    while (t.type() == TypeContent::GenericVariable
           && typeContext.calleeType().typeDefinition()->canBeUsedToResolve(t.typeDefinition())) {
        t = typeContext.calleeType().typeDefinition()->genericArgumentConstraints()[t.genericVariableIndex()];
    }

    if (optional) {
        t.setOptional();
    }
    if (box) {
        t.forceBox_ = true;
    }
    return t;
}

Type Type::resolveOn(const TypeContext &typeContext, bool resolveSelf) const {
    Type t = *this;
    if (type() == TypeContent::Nothingness) {
        return t;
    }
    bool optional = t.optional();
    bool box = t.storageType() == StorageType::Box;

    if (resolveSelf && t.type() == TypeContent::Self && typeContext.calleeType().resolveSelfOn_) {
        t = typeContext.calleeType();
    }

    while (t.type() == TypeContent::LocalGenericVariable && typeContext.function() == t.localResolutionConstraint_
           && typeContext.functionGenericArguments() != nullptr) {
        t = (*typeContext.functionGenericArguments())[t.genericVariableIndex()];
    }

    if (typeContext.calleeType().canHaveGenericArguments()) {
        while (t.type() == TypeContent::GenericVariable &&
               typeContext.calleeType().typeDefinition()->canBeUsedToResolve(t.typeDefinition())) {
            Type tn = typeContext.calleeType().genericArguments_[t.genericVariableIndex()];
            if (tn.type() == TypeContent::GenericVariable && tn.genericVariableIndex() == t.genericVariableIndex()
                && tn.typeDefinition() == t.typeDefinition()) {
                break;
            }
            t = tn;
        }
    }

    if (optional) {
        t.setOptional();
    }

    if (t.canHaveGenericArguments()) {
        for (size_t i = 0; i < t.typeDefinition()->numberOfGenericArgumentsWithSuperArguments(); i++) {
            t.genericArguments_[i] = t.genericArguments_[i].resolveOn(typeContext);
        }
    }
    else if (t.type() == TypeContent::Callable) {
        for (Type &genericArgument : t.genericArguments_) {
            genericArgument = genericArgument.resolveOn(typeContext);
        }
    }

    if (box) {
        t.forceBox_ = true;
    }
    return t;
}

bool Type::identicalGenericArguments(Type to, const TypeContext &typeContext, std::vector<CommonTypeFinder> *ctargs) const {
    if (!to.typeDefinition()->ownGenericArgumentVariables().empty()) {
        for (size_t l = to.typeDefinition()->ownGenericArgumentVariables().size(),
             i = to.typeDefinition()->numberOfGenericArgumentsWithSuperArguments() - l; i < l; i++) {
            if (!this->genericArguments_[i].identicalTo(to.genericArguments_[i], typeContext, ctargs)) {
                return false;
            }
        }
    }
    return true;
}

bool Type::compatibleTo(Type to, const TypeContext &ct, std::vector<CommonTypeFinder> *ctargs) const {
    if (type() == TypeContent::Error) {
        return to.type() == TypeContent::Error && genericArguments()[0].identicalTo(to.genericArguments()[0], ct, ctargs)
                && genericArguments()[1].compatibleTo(to.genericArguments()[1], ct);
    }
    if (to.type() == TypeContent::Something) {
        return true;
    }
    if (to.meta_ != meta_) {
        return false;
    }
    if (this->optional() && !to.optional()) {
        return false;
    }

    if (to.type() == TypeContent::Someobject && this->type() == TypeContent::Class) {
        return true;
    }
    if (to.type() == TypeContent::Error) {
        return compatibleTo(to.genericArguments()[1], ct);
    }
    if (this->type() == TypeContent::Class && to.type() == TypeContent::Class) {
        return this->eclass()->inheritsFrom(to.eclass()) && identicalGenericArguments(to, ct, ctargs);
    }
    if ((this->type() == TypeContent::Protocol && to.type() == TypeContent::Protocol) ||
        (this->type() == TypeContent::ValueType && to.type() == TypeContent::ValueType)) {
        return this->typeDefinition() == to.typeDefinition() && identicalGenericArguments(to, ct, ctargs);
    }
    if (type() == TypeContent::MultiProtocol && to.type() == TypeContent::MultiProtocol) {
        return std::equal(protocols().begin(), protocols().end(), to.protocols().begin(), to.protocols().end(),
                          [ct, ctargs](const Type &a, const Type &b) {
                              return a.compatibleTo(b, ct, ctargs);
                          });
    }
    if (to.type() == TypeContent::MultiProtocol) {
        return std::all_of(to.protocols().begin(), to.protocols().end(), [this, ct](const Type &p) {
            return compatibleTo(p, ct);
        });
    }
    if (type() == TypeContent::Class && to.type() == TypeContent::Protocol) {
        for (Class *a = this->eclass(); a != nullptr; a = a->superclass()) {
            for (auto &protocol : a->protocols()) {
                if (protocol.resolveOn(*this).compatibleTo(to.resolveOn(ct), ct, ctargs)) {
                    return true;
                }
            }
        }
        return false;
    }
    if ((type() == TypeContent::ValueType || type() == TypeContent::Enum) && to.type() == TypeContent::Protocol) {
        for (auto &protocol : typeDefinition()->protocols()) {
            if (protocol.resolveOn(*this).compatibleTo(to.resolveOn(ct), ct, ctargs)) {
                return true;
            }
        }
        return false;
    }
    if (this->type() == TypeContent::Nothingness) {
        return to.optional() || to.type() == TypeContent::Nothingness;
    }
    if (this->type() == TypeContent::Enum && to.type() == TypeContent::Enum) {
        return this->eenum() == to.eenum();
    }
    if ((this->type() == TypeContent::GenericVariable && to.type() == TypeContent::GenericVariable) ||
        (this->type() == TypeContent::LocalGenericVariable && to.type() == TypeContent::LocalGenericVariable)) {
        return (this->genericVariableIndex() == to.genericVariableIndex() &&
                this->typeDefinition() == to.typeDefinition()) ||
        this->resolveOnSuperArgumentsAndConstraints(ct)
        .compatibleTo(to.resolveOnSuperArgumentsAndConstraints(ct), ct, ctargs);
    }
    if (this->type() == TypeContent::GenericVariable) {
        return this->resolveOnSuperArgumentsAndConstraints(ct).compatibleTo(to, ct, ctargs);
    }
    if (to.type() == TypeContent::GenericVariable) {
        return this->compatibleTo(to.resolveOnSuperArgumentsAndConstraints(ct), ct, ctargs);
    }
    if (this->type() == TypeContent::LocalGenericVariable) {
        return ctargs != nullptr || this->resolveOnSuperArgumentsAndConstraints(ct).compatibleTo(to, ct, ctargs);
    }
    if (to.type() == TypeContent::LocalGenericVariable) {
        if (ctargs != nullptr) {
            (*ctargs)[to.genericVariableIndex()].addType(*this, ct);
            return true;
        }
        return this->compatibleTo(to.resolveOnSuperArgumentsAndConstraints(ct), ct, ctargs);
    }
    if (to.type() == TypeContent::Self) {
        return this->type() == to.type();
    }
    if (this->type() == TypeContent::Self) {
        return this->resolveOnSuperArgumentsAndConstraints(ct).compatibleTo(to, ct, ctargs);
    }
    if (this->type() == TypeContent::Callable && to.type() == TypeContent::Callable) {
        if (this->genericArguments_[0].compatibleTo(to.genericArguments_[0], ct, ctargs)
            && to.genericArguments_.size() == this->genericArguments_.size()) {
            for (size_t i = 1; i < to.genericArguments_.size(); i++) {
                if (!to.genericArguments_[i].compatibleTo(this->genericArguments_[i], ct, ctargs)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    return this->type() == to.type();
}

bool Type::identicalTo(Type to, const TypeContext &tc, std::vector<CommonTypeFinder> *ctargs) const {
    if (optional() != to.optional()) {
        return false;
    }
    if (ctargs != nullptr && to.type() == TypeContent::LocalGenericVariable) {
        (*ctargs)[to.genericVariableIndex()].addType(*this, tc);
        return true;
    }

    if (type() == to.type()) {
        switch (type()) {
            case TypeContent::Class:
            case TypeContent::Protocol:
            case TypeContent::ValueType:
                return typeDefinition() == to.typeDefinition()
                && identicalGenericArguments(to, tc, ctargs);
            case TypeContent::Callable:
                return to.genericArguments_.size() == this->genericArguments_.size()
                && identicalGenericArguments(to, tc, ctargs);
            case TypeContent::Enum:
                return eenum() == to.eenum();
            case TypeContent::GenericVariable:
            case TypeContent::LocalGenericVariable:
                return resolveReferenceToBaseReferenceOnSuperArguments(tc).genericVariableIndex() ==
                to.resolveReferenceToBaseReferenceOnSuperArguments(tc).genericVariableIndex();
            case TypeContent::Self:
            case TypeContent::Something:
            case TypeContent::Someobject:
            case TypeContent::Nothingness:
                return true;
            case TypeContent::Error:
                return genericArguments_[0].identicalTo(to.genericArguments_[0], tc, ctargs) &&
                genericArguments_[1].identicalTo(to.genericArguments_[1], tc, ctargs);
            case TypeContent::MultiProtocol:
                return std::equal(protocols().begin(), protocols().end(), to.protocols().begin(), to.protocols().end(),
                                  [&tc, ctargs](const Type &a, const Type &b) { return a.identicalTo(b, tc, ctargs); });
            case TypeContent::StorageExpectation:
                return false;
        }
    }
    return false;
}


// MARK: Storage

int Type::size() const {
    int basesize = 0;
    switch (storageType()) {
        case StorageType::SimpleOptional:
            basesize = 1;
        case StorageType::Simple:
            switch (type()) {
                case TypeContent::ValueType:
                case TypeContent::Enum:
                    return basesize + typeDefinition()->size();
                case TypeContent::Callable:
                case TypeContent::Class:
                case TypeContent::Someobject:
                case TypeContent::Self:
                    return basesize + 1;
                case TypeContent::Error:
                    if (genericArguments()[1].storageType() == StorageType::SimpleOptional) {
                        return std::max(2, genericArguments()[1].size());
                    }
                    return std::max(1, genericArguments()[1].size()) + basesize;
                case TypeContent::Nothingness:
                    return 1;
                default:
                    throw std::logic_error("Type is wrongly simply stored");
            }
        case StorageType::Box:
            return 4;
        default:
            throw std::logic_error("Type has invalid storage type");
    }
}

StorageType Type::storageType() const {
    if (forceBox_ || requiresBox()) {
        return StorageType::Box;
    }
    if (type() == TypeContent::Error) {
        return StorageType::SimpleOptional;
    }
    return optional() ? StorageType::SimpleOptional : StorageType::Simple;
}

EmojicodeInstruction Type::boxIdentifier() const {
    EmojicodeInstruction value;
    switch (type()) {
        case TypeContent::ValueType:
        case TypeContent::Enum:
            value = valueType()->boxIdFor(genericArguments_);
            break;
        case TypeContent::Callable:
        case TypeContent::Class:
        case TypeContent::Someobject:
            value = T_OBJECT;
            break;
        case TypeContent::Nothingness:
        case TypeContent::Protocol:
        case TypeContent::MultiProtocol:
        case TypeContent::Something:
        case TypeContent::GenericVariable:
        case TypeContent::LocalGenericVariable:
        case TypeContent::Self:
        case TypeContent::Error:
            return 0;  // This can only be executed in the case of a semantic error, return any value
        case TypeContent::StorageExpectation:
            throw std::logic_error("Box identifier for StorageExpectation");
    }
    return remotelyStored() ? value | REMOTE_MASK : value;
}

bool Type::requiresBox() const {
    switch (type()) {
        case TypeContent::ValueType:
        case TypeContent::Enum:
        case TypeContent::Callable:
        case TypeContent::Class:
        case TypeContent::Someobject:
        case TypeContent::Self:
        case TypeContent::Nothingness:
        case TypeContent::StorageExpectation:
            return false;
        case TypeContent::Error:
            return genericArguments()[1].storageType() == StorageType::Box;
        case TypeContent::Something:
        case TypeContent::GenericVariable:
        case TypeContent::LocalGenericVariable:
        case TypeContent::Protocol:
        case TypeContent::MultiProtocol:
            return true;
    }
}

bool Type::isReferencable() const {
    switch (type()) {
        case TypeContent::Callable:
        case TypeContent::Class:
        case TypeContent::Someobject:
        case TypeContent::GenericVariable:
        case TypeContent::LocalGenericVariable:
        case TypeContent::Self:
            return storageType() != StorageType::Simple;
        case TypeContent::Nothingness:
            return false;
        case TypeContent::ValueType:
        case TypeContent::Enum:
        case TypeContent::Protocol:
        case TypeContent::MultiProtocol:
        case TypeContent::Something:
        case TypeContent::Error:
            return true;
        case TypeContent::StorageExpectation:
            throw std::logic_error("isReferenceWorthy for StorageExpectation");
    }
}

template <typename T, typename... Us>
void Type::objectVariableRecords(int index, std::vector<T> *information, Us... args) const {
    switch (type()) {
        case TypeContent::ValueType: {
            if (storageType() == StorageType::Box) {
                information->push_back(T(index, ObjectVariableType::Box, args...));
                return;
            }

            auto optional = storageType() == StorageType::SimpleOptional;
            auto size = information->size();
            for (auto variable : valueType()->instanceScope().map()) {
                // TODO:               auto vindex = index + variable.second.id() + (optional ? 1 : 0);
                // variable.second.type().objectVariableRecords(vindex, information, args...);
            }
            if (optional) {
                auto info = T(static_cast<unsigned int>(information->size() - size), index,
                              ObjectVariableType::ConditionalSkip, args...);
                information->insert(information->begin() + size, info);
            }
            return;
        }
        case TypeContent::Class:
        case TypeContent::Self:
        case TypeContent::Someobject:
        case TypeContent::Callable:
        case TypeContent::Protocol:
        case TypeContent::MultiProtocol:
        case TypeContent::Something:
        case TypeContent::GenericVariable:
        case TypeContent::LocalGenericVariable:
        case TypeContent::Error:  // Is or may be an object pointer
            switch (storageType()) {
                case StorageType::SimpleOptional:
                    information->push_back(T(index + 1, index, ObjectVariableType::Condition, args...));
                    return;
                case StorageType::Simple:
                    information->push_back(T(index, ObjectVariableType::Simple, args...));
                    return;
                case StorageType::Box:
                    information->push_back(T(index, ObjectVariableType::Box, args...));
                    return;
                default:
                    throw std::domain_error("invalid storage type");
            }
        case TypeContent::Enum:
        case TypeContent::Nothingness:
        case TypeContent::StorageExpectation:
            return;  // Can't be object pointer
    }
}

template void Type::objectVariableRecords(int, std::vector<ObjectVariableInformation>*) const;
template void Type::objectVariableRecords(int, std::vector<FunctionObjectVariableInformation>*,
                                          InstructionCount, InstructionCount) const;

// MARK: Type Visulisation

std::string Type::typePackage() const {
    switch (this->type()) {
        case TypeContent::Class:
        case TypeContent::ValueType:
        case TypeContent::Protocol:
        case TypeContent::Enum:
            return this->typeDefinition()->package()->name();
        case TypeContent::Nothingness:
        case TypeContent::Something:
        case TypeContent::Someobject:
        case TypeContent::GenericVariable:
        case TypeContent::LocalGenericVariable:
        case TypeContent::Callable:
        case TypeContent::Self:
        case TypeContent::MultiProtocol:  // should actually never come in here
        case TypeContent::Error:
            return "";
        case TypeContent::StorageExpectation:
            throw std::logic_error("typePackage for StorageExpectation");
    }
}

void Type::typeName(Type type, const TypeContext &typeContext, bool includePackageAndOptional, std::string &string) const {
    if (type.meta_) {
        string.append("🔳");
    }

    if (includePackageAndOptional) {
        if (type.optional()) {
            string.append("🍬");
        }

        string.append(type.typePackage());
    }

    switch (type.type()) {
        case TypeContent::Class:
        case TypeContent::Protocol:
        case TypeContent::Enum:
        case TypeContent::ValueType:
            string.append(type.typeDefinition()->name().utf8());
            break;
        case TypeContent::MultiProtocol:
            string.append("🍱");
            for (auto &protocol : type.protocols()) {
                typeName(protocol, typeContext, includePackageAndOptional, string);
            }
            string.append("🍱");
            return;
        case TypeContent::Nothingness:
            string.append("✨");
            return;
        case TypeContent::Something:
            string.append("⚪️");
            return;
        case TypeContent::Someobject:
            string.append("🔵");
            return;
        case TypeContent::Self:
            string.append("🐕");
            return;
        case TypeContent::Callable:
            string.append("🍇");

            for (size_t i = 1; i < type.genericArguments_.size(); i++) {
                typeName(type.genericArguments_[i], typeContext, includePackageAndOptional, string);
            }

            string.append("➡️");
            typeName(type.genericArguments_[0], typeContext, includePackageAndOptional, string);
            string.append("🍉");
            return;
        case TypeContent::Error:
            string.append("🚨");
            typeName(type.genericArguments_[0], typeContext, includePackageAndOptional, string);
            typeName(type.genericArguments_[1], typeContext, includePackageAndOptional, string);
            return;
        case TypeContent::GenericVariable: {
            if (typeContext.calleeType().type() == TypeContent::Class) {
                Class *eclass = typeContext.calleeType().eclass();
                do {
                    for (auto it : eclass->ownGenericArgumentVariables()) {
                        if (it.second.genericVariableIndex() == type.genericVariableIndex()) {
                            string.append(it.first.utf8());
                            return;
                        }
                    }
                } while ((eclass = eclass->superclass()) != nullptr);
            }
            else if (typeContext.calleeType().canHaveGenericArguments()) {
                for (auto it : typeContext.calleeType().typeDefinition()->ownGenericArgumentVariables()) {
                    if (it.second.genericVariableIndex() == type.genericVariableIndex()) {
                        string.append(it.first.utf8());
                        return;
                    }
                }
            }

            string.append("T" + std::to_string(type.genericVariableIndex()) + "?");
            return;
        }
        case TypeContent::LocalGenericVariable:
            if (typeContext.function() != nullptr) {
                for (auto it : typeContext.function()->genericArgumentVariables) {
                    if (it.second.genericVariableIndex() == type.genericVariableIndex()) {
                        string.append(it.first.utf8());
                        return;
                    }
                }
            }

            string.append("L" + std::to_string(type.genericVariableIndex()) + "?");
            return;
        case TypeContent::StorageExpectation:
            return;
    }

    if (type.canHaveGenericArguments()) {
        auto typeDef = type.typeDefinition();
        if (typeDef->ownGenericArgumentVariables().empty()) {
            return;
        }

        for (auto &argumentType : type.genericArguments()) {
            string.append("🐚");
            typeName(argumentType, typeContext, includePackageAndOptional, string);
        }
    }
}

std::string Type::toString(const TypeContext &typeContext, bool optionalAndPackages) const {
    std::string string;
    typeName(*this, typeContext, optionalAndPackages, string);
    return string;
}

}  // namespace EmojicodeCompiler
