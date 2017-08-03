//
//  Function.h
//  Emojicode
//
//  Created by Theo Weidmann on 04/01/16.
//  Copyright © 2016 Theo Weidmann. All rights reserved.
//

#ifndef Function_hpp
#define Function_hpp

#include "CompilerError.hpp"

#include "Lex/TokenStream.hpp"
#include "Types/Type.hpp"
#include "FunctionType.hpp"
#include "Generation/FunctionWriter.hpp"
#include "Types/Class.hpp"
#include <algorithm>
#include <queue>
#include <map>
#include <vector>
#include <numeric>
#include <experimental/optional>

namespace EmojicodeCompiler {

class VTIProvider;
class ASTNode;

enum class AccessLevel {
    Public, Private, Protected
};

struct Argument {
    Argument(EmojicodeString n, Type t) : variableName(n), type(t) {}

    /// The name of the variable
    EmojicodeString variableName;
    /// The type
    Type type;
};

/** Functions are callables that belong to a class or value type as either method, type method or initializer. */
class Function {
    friend void Class::prepareForCG();
    friend Protocol;
    friend void generateCode(Writer &writer);
public:
    static bool foundStart;
    static Function *start;
    static std::queue<Function *> compilationQueue;
    /// The VTIProvider used for functions created by the compiler that do not belong to any type. E.g. the start flag
    /// function, closures.
    static ValueTypeVTIProvider pureFunctionsProvider;

    /** Returns a VTI for a function. */
    static int nextFunctionVti() { return nextVti_++; }
    /** Returns the number of funciton VTIs assigned. This should be equal to the number of compiled functions. */
    static int functionCount() { return nextVti_; }

    Function(EmojicodeString name, AccessLevel level, bool final, Type owningType, Package *package, SourcePosition p,
             bool overriding, EmojicodeString documentationToken, bool deprecated, bool mutating,
             FunctionType type)
    : position_(p),
    name_(name),
    final_(final),
    overriding_(overriding),
    deprecated_(deprecated),
    mutating_(mutating),
    access_(level),
    owningType_(owningType),
    package_(package),
    documentation_(documentationToken),
    functionType_(type) {}

    EmojicodeString name() const { return name_; }

    EmojicodeString protocolBoxingLayerName(EmojicodeString protocolName) {
        return EmojicodeString({ protocolName[0], name()[0] });
    }

    /** Whether the method is implemented natively and Run-Time Native Linking must occur. */
    bool isNative() const { return linkingTableIndex_ > 0; }
    unsigned int linkingTabelIndex() const { return linkingTableIndex_; }
    void setLinkingTableIndex(int index);
    /** Whether the method was marked as final and can’t be overriden. */
    bool final() const { return final_; }
    /** Whether the method is intended to override a super method. */
    bool overriding() const { return overriding_; }
    /** Whether the method is deprecated. */
    bool deprecated() const { return deprecated_; }
    /** Returns the access level to this method. */
    AccessLevel accessLevel() const { return access_; }

    std::vector<Argument> arguments;
    /** Return type of the method */
    Type returnType = Type::nothingness();

    /** Returns the position at which this callable was defined. */
    const SourcePosition& position() const { return position_; }

    /** Returns a copy of the token stream intended to be used to parse this callable. */
    const TokenStream& tokenStream() const { return tokenStream_; }
    void setTokenStream(TokenStream ts) { tokenStream_ = ts; }

    /// The type of this function when used as value.
    Type type() const;

    /** Type to which this function belongs.
     This can be Nothingness if the function doesn’t belong to any type (e.g. 🏁). */
    Type owningType() const { return owningType_; }

    const EmojicodeString& documentation() const { return documentation_; }

    /** The types for the generic arguments. */
    std::vector<Type> genericArgumentConstraints;
    /** Generic type arguments as variables */
    std::map<EmojicodeString, Type> genericArgumentVariables;

    /** The namespace in which the function was defined.
     This does not necessarily match the package of @c owningType. */
    Package* package() const { return package_; }

    /// Issues a warning at the given position if the function is deprecated.
    void deprecatedWarning(const SourcePosition &p) const;

    /// Returns true if the method is validly overriding a method or false if it does not override.
    /// @throws CompilerError if the override is improper, e.g. implicit
    bool checkOverride(Function *superFunction) const;
    /// Makes this method properly inherit from @c super.
    void override(Function *super, Type superSource, Type typeContext) {
        enforcePromises(super, typeContext, superSource, std::experimental::nullopt);
        setVti(super->getVti());
        super->registerOverrider(this);
    }
    /// Checks that no promises were broken and applies boxing if necessary.
    /// Returns false iff a value for protocol was given and the arguments or the return type are storage incompatible.
    bool enforcePromises(Function *super, const TypeContext &typeContext, const Type &superSource,
                         std::experimental::optional<TypeContext> protocol);

    void registerOverrider(Function *f) { overriders_.push_back(f); }

    /** Returns the VTI for this function or fetches one by calling the VTI Assigner and marks the function as used.
     @warning This method must only be called if the function will be needed at run-time and
     should be assigned a VTI. */
    int vtiForUse();
    /// Assigns this method a VTI without marking it as used.
    void assignVti();
    /// Returns the VTI this function was assigned.
    /// @throws std::logic_error if the function wasn’t assigned a VTI
    int getVti() const;

    void setVti(int vti);

    void markUsed(bool addToCompilationQueue = true);
    /** Sets the @c VTIProvider which should be used to assign this method a VTI and to update the VTI counter. */
    void setVtiProvider(VTIProvider *provider);
    /// Whether the function was used.
    bool used() const { return used_; }
    /// Whether the function was assigned a VTI
    bool assigned() const;

    /// Whether the function mutates the callee. Only relevant for value type instance methods.
    bool mutating() const { return mutating_; }

    FunctionType functionType() const { return functionType_; }

    virtual ContextType contextType() const {
        switch (functionType()) {
            case FunctionType::ObjectMethod:
            case FunctionType::ObjectInitializer:
                return ContextType::Object;
                break;
            case FunctionType::ValueTypeMethod:
            case FunctionType::ValueTypeInitializer:
                return ContextType::ValueReference;
                break;
            case FunctionType::ClassMethod:
            case FunctionType::Function:
                return ContextType::None;
                break;
            case FunctionType::BoxingLayer:
                throw std::logic_error("contextType for BoxingLayer called on Function class");
        }
    }

    void setAst(const std::shared_ptr<ASTNode> &ast) { ast_ = ast; }
    const std::shared_ptr<ASTNode>& ast() const { return ast_; }

    int fullSize() const { return fullSize_; }
    void setFullSize(int c) { fullSize_ = c; }

    FunctionWriter writer_;
    std::vector<FunctionObjectVariableInformation>& objectVariableInformation() { return objectVariableInformation_; }
    size_t variableCount() const { return variableCount_; }
    void setVariableCount(size_t variableCount) { variableCount_ = variableCount; }
private:
    TokenStream tokenStream_;
    std::shared_ptr<ASTNode> ast_;
    SourcePosition position_;
    EmojicodeString name_;
    static int nextVti_;
    int vti_ = -1;
    bool final_;
    bool overriding_;
    bool deprecated_;
    bool mutating_;
    bool used_ = false;
    unsigned int linkingTableIndex_ = 0;
    AccessLevel access_;
    Type owningType_;
    Package *package_;
    EmojicodeString documentation_;
    VTIProvider *vtiProvider_ = nullptr;
    FunctionType functionType_;
    int fullSize_ = -1;
    std::vector<Function*> overriders_;
    std::vector<FunctionObjectVariableInformation> objectVariableInformation_;
    size_t variableCount_ = 0;
};

class Initializer: public Function {
public:
    Initializer(EmojicodeString name, AccessLevel level, bool final, Type owningType, Package *package,
                SourcePosition p, bool overriding, EmojicodeString documentationToken, bool deprecated, bool r,
                std::experimental::optional<Type> errorType, FunctionType mode)
    : Function(name, level, final, owningType, package, p, overriding, documentationToken, deprecated, true, mode),
    required_(r),
    errorType_(errorType) {
        returnType = Type::nothingness();
    }

    /// Whether all subclassess are required to implement this initializer as well. Never true for non-class types.
    bool required() const { return required_; }
    /// Whether this initalizer might return an error.
    bool errorProne() const { return static_cast<bool>(errorType_); }
    const Type& errorType() const { return *errorType_; }

    /// Returns the actual type constructed with this initializer for the given initialized type @c type
    Type constructedType(Type type) {
        type.unbox();
        if (errorType_) {
            Type errorType = Type::error();
            errorType.genericArguments_ = { *errorType_, type };
            return errorType;
        }
        return type;
    }
    void addArgumentToVariable(const EmojicodeString &string, const SourcePosition &p) {
        auto find = std::find(argumentsToVariables_.begin(), argumentsToVariables_.end(), string);
        if (find != argumentsToVariables_.end()) {
            throw CompilerError(p, "Instance variable initialized with 🍼 more than once.");
        }
        argumentsToVariables_.push_back(string);
    }
    const std::vector<EmojicodeString>& argumentsToVariables() const { return argumentsToVariables_; }
private:
    bool required_;
    std::experimental::optional<Type> errorType_;
    std::vector<EmojicodeString> argumentsToVariables_;
};

}  // namespace EmojicodeCompiler

#endif /* Function_hpp */
