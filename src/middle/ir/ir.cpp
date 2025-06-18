#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Intrinsics.h"
#include <vector>
#include <map>
#include <memory>

using namespace llvm;

// 全局变量和类型定义
static LLVMContext context;
static std::unique_ptr<Module> module;
static std::map<std::string, Value*> namedValues;
static std::map<std::string, Type*> typeMap;

// 类型定义
enum ValueType {
    VT_VOID,
    VT_INT,
    VT_UINT8,
    VT_UINT32,
    VT_INT64,
    VT_FLOAT16,
    VT_FLOAT32,
    VT_STRING,
    VT_BOOL,
    VT_ANY
};
//clang++-16 -std=c++17 -gdwarf-4 -O0 ir.cpp $(llvm-config-16 --cxxflags --ldflags --libs) -o ir
// 初始化类型系统
void initTypeSystem() {
    module = std::make_unique<Module>("my_program", context);
    
    // 基本类型映射
    typeMap["VOID"] = Type::getVoidTy(context);
    typeMap["INT"] = Type::getInt32Ty(context);
    typeMap["UINT8"] = Type::getInt8Ty(context);
    typeMap["UINT32"] = Type::getInt32Ty(context);
    typeMap["INT64"] = Type::getInt64Ty(context);
    typeMap["FLOAT16"] = Type::getHalfTy(context);
    typeMap["FLOAT32"] = Type::getFloatTy(context);
    typeMap["STRING"] = PointerType::get(Type::getInt8Ty(context), 0);
    typeMap["BOOL"] = Type::getInt1Ty(context);
    typeMap["ANY"] = Type::getInt8Ty(context)->getPointerTo();
}

// 获取LLVM类型
Type* getLLVMType(const std::string& typeName) {
    auto it = typeMap.find(typeName);
    if (it != typeMap.end()) {
        return it->second;
    }
    return typeMap["ANY"];
}

// 创建字面量
Value* createLiteral(IRBuilder<>& builder, const std::string& value, const std::string& typeStr) {
    Type* type = getLLVMType(typeStr);
    
    if (type->isIntegerTy()) {
        uint64_t intValue = std::stoull(value);
        return ConstantInt::get(type, intValue);
    } else if (type->isFloatingPointTy()) {
        double floatValue = std::stod(value);
        return ConstantFP::get(type, floatValue);
    } else if (type == typeMap["STRING"]) {
        return builder.CreateGlobalStringPtr(value);
    } else if (type == typeMap["BOOL"]) {
        return ConstantInt::get(type, value == "true" ? 1 : 0);
    }
    
    return Constant::getNullValue(type);
}

// 创建变量声明
Value* createVarDecl(IRBuilder<>& builder, const std::string& name, 
                    const std::string& typeStr, bool isConst, Value* initValue) {
    Type* type = getLLVMType(typeStr);
    if (!type) return nullptr;

    // 获取当前函数
    Function* currentFunc = builder.GetInsertBlock()->getParent();
    if (!currentFunc) return nullptr;

    // 在函数入口块创建alloca
    BasicBlock* entryBB = &currentFunc->getEntryBlock();
    IRBuilder<> entryBuilder(entryBB, entryBB->begin());
    Value* alloca = entryBuilder.CreateAlloca(type, nullptr, name);

    // 初始化处理
    if (initValue) {
        if (initValue->getType() != type) {
            if (type->isFloatingPointTy() && initValue->getType()->isIntegerTy()) {
                initValue = builder.CreateSIToFP(initValue, type, "convtmp");
            } else if (type->isIntegerTy() && initValue->getType()->isFloatingPointTy()) {
                initValue = builder.CreateFPToSI(initValue, type, "convtmp");
            } else if (type->isIntegerTy() && initValue->getType()->isIntegerTy()) {
                initValue = builder.CreateIntCast(initValue, type, true, "casttmp");
            }
        }
        builder.CreateStore(initValue, alloca);
    }
    
    namedValues[name] = alloca;
    return alloca;
}

// 创建函数声明
Function* createFunctionDecl(const std::string& name, 
                           const std::vector<std::string>& params,
                           const std::vector<std::string>& paramTypes, 
                           const std::string& returnType) {
    Type* retType = getLLVMType(returnType);
    std::vector<Type*> paramLLVMTypes;
    
    for (const auto& paramType : paramTypes) {
        paramLLVMTypes.push_back(getLLVMType(paramType));
    }
    
    FunctionType* funcType = FunctionType::get(retType, paramLLVMTypes, false);
    return Function::Create(funcType, Function::ExternalLinkage, name, module.get());
}

// 创建主程序
void createMainProgram() {
    IRBuilder<> builder(context);
    
    // 1. 创建计算面积函数
    Function* calcAreaFunc = createFunctionDecl("计算面积", {"半径"}, {"Float32"}, "Float32");
    BasicBlock* calcAreaBB = BasicBlock::Create(context, "entry", calcAreaFunc);
    builder.SetInsertPoint(calcAreaBB);
    
    // 创建函数参数引用
    Value* radius = &calcAreaFunc->arg_begin()[0];
    namedValues["半径"] = radius;
    
    // 创建变量PI
    Value* pi = createVarDecl(builder, "PI", "FLOAT16", true, 
                            createLiteral(builder, "3.1415926", "FLOAT16"));
    
    // 计算面积: PI * radius²
    Value* radiusSquared = builder.CreateFMul(radius, radius, "radius_squared");
    Value* area = builder.CreateFMul(pi, radiusSquared, "area");
    builder.CreateRet(area);
    
    // 2. 创建主函数
    Function* mainFunc = createFunctionDecl("main", {}, {}, "Int32");
    BasicBlock* mainBB = BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(mainBB);
    
    // 创建年龄变量
    Value* age = createVarDecl(builder, "年龄", "UINT8", false, 
                             createLiteral(builder, "28", "UINT8"));
    
    // 调用计算面积函数
    Value* arg = createLiteral(builder, "10", "UINT8");
    builder.CreateCall(calcAreaFunc, {arg});
    
    // 创建if语句
    BasicBlock* thenBB = BasicBlock::Create(context, "then", mainFunc);
    BasicBlock* elseBB = BasicBlock::Create(context, "else", mainFunc);
    BasicBlock* mergeBB = BasicBlock::Create(context, "ifcont", mainFunc);

    // 创建条件: age > 18
    Value* cond = builder.CreateICmpUGT(
        builder.CreateZExt(age, Type::getInt8Ty(context), "age_ext"),  // 明确扩展
        createLiteral(builder, "18", "UINT8"), 
        "age_cond"
    );
    
    // 创建分支
    builder.CreateCondBr(cond, thenBB, elseBB);
    
    // 处理then块
    builder.SetInsertPoint(thenBB);
    Value* statusThen = createVarDecl(builder, "状态", "STRING", false, 
                                    createLiteral(builder, "\"成年人\"", "STRING"));
    builder.CreateBr(mergeBB);
    
    // 处理else块
    builder.SetInsertPoint(elseBB);
    Value* statusElse = createVarDecl(builder, "状态", "STRING", false, 
                                    createLiteral(builder, "\"未成年人\"", "STRING"));
    builder.CreateBr(mergeBB);
    
    // 继续在合并点生成代码
    builder.SetInsertPoint(mergeBB);
    builder.CreateRetVoid();
}

int main() {
    initTypeSystem();
    createMainProgram();
    
    // 验证并打印模块
    verifyModule(*module);
    module->print(outs(), nullptr);
    
    return 0;
}